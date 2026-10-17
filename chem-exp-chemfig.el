;;; chem-exp-chemfig.el --- Chemfig exporter -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2025 Giovanni Crisalfi

;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The chemfig exporter.  It owns chemfig atom placement, bond rendering,
;; tree walking, and output formatting.  The molecule/geometry pipeline lives
;; in `chem-indigo'; this file only turns a prepared drawing into chemfig.

;;; Code:

(require 'cl-lib)
(require 'chem-ir)
(require 'chem-export)
(require 'tochemfig-atom)
(require 'tochemfig-bond)
(require 'chem-exp-chemfig-mappings)

;;; Renderer state

(cl-defstruct (chem-exp-chemfig-atom-state (:constructor chem-exp-chemfig-atom-state-create))
  string-pos phantom phantom-pos
  first-quadrant second-quadrant charge-angle
  explicit)

(cl-defstruct (chem-exp-chemfig-bond-state (:constructor chem-exp-chemfig-bond-state-create))
  tikz-styles tikz-values
  is-last display-type)

(cl-defstruct (chem-exp-chemfig-state (:constructor chem-exp-chemfig-state-create))
  atoms bonds)

(defun chem-exp-chemfig--make-state ()
  "Create an empty renderer state."
  (chem-exp-chemfig-state-create
   :atoms (make-hash-table :test 'eql)
   :bonds (make-hash-table :test 'eq)))

(defun chem-exp-chemfig--atom-state (state atom)
  "Return the renderer state for ATOM in STATE."
  (let ((key (chem-atom-idx atom)))
    (or (gethash key (chem-exp-chemfig-state-atoms state))
        (puthash key (chem-exp-chemfig-atom-state-create)
                 (chem-exp-chemfig-state-atoms state)))))

(defun chem-exp-chemfig--bond-state (state bond)
  "Return the renderer state for BOND in STATE."
  (or (gethash bond (chem-exp-chemfig-state-bonds state))
      (puthash bond (chem-exp-chemfig-bond-state-create)
               (chem-exp-chemfig-state-bonds state))))

(defun chem-exp-chemfig--atom-explicit-p (state atom)
  "Return non-nil when ATOM is explicitly labeled in STATE."
  (chem-exp-chemfig-atom-state-explicit (chem-exp-chemfig--atom-state state atom)))

;;; Atom label placement

(defconst tochemfig--hydrogen-lefties
  '("O" "S" "Se" "Te" "F" "Cl" "Br" "I" "At")
  "Elements whose hydrogen is drawn to the left when the atom is solitary.")

(defconst tochemfig--quadrant-turf 80
  "Degrees that have to remain free on either side of a hydrogen placement.")

(defconst tochemfig--quadrants
  '((0 0 "east") (1 180 "west") (2 270 "south") (3 90 "north"))
  "Candidate quadrants for hydrogen placement: (priority angle name).")

(defconst tochemfig--charge-turf 50
  "Reserved angle for charges - big enough for 2+.")

(defconst tochemfig--charge-positions
  '((0 15 "top_right") (1 165 "top_left") (2 90 "top_center")
    (3 270 "bottom_center") (4 345 "bottom_right") (5 195 "bottom_left"))
  "Candidate positions for charges: (priority angle name).")

(defconst tochemfig--explicit-characters "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  "Characters that mark an atom as explicitly printed.")

(defun tochemfig--score-angle (a b turf)
  "Score how much angle B impinges on candidate angle A within TURF."
  (let* ((diff (mod (- a b) 360))
         (angle (min diff (- 360 diff))))
    (expt (max 0 (- turf angle)) 2)))

(defun tochemfig--score-angles (atom choices turf)
  "Return CHOICES sorted by how well they avoid ATOM's bond angles."
  (let ((aux nil))
    (dolist (choice choices)
      (let ((score 0))
        (dolist (bond-angle (tochemfig-atom-bond-angles atom))
          (setq score (+ score (tochemfig--score-angle (nth 1 choice) bond-angle turf))))
        (push (list score (nth 0 choice) (nth 2 choice)) aux)))
    (setq aux (sort aux
                    (lambda (a b)
                      (cond ((/= (nth 0 a) (nth 0 b)) (< (nth 0 a) (nth 0 b)))
                            ((/= (nth 1 a) (nth 1 b)) (< (nth 1 a) (nth 1 b)))
                            (t (string< (nth 2 a) (nth 2 b)))))))
    (mapcar (lambda (x) (nth 2 x)) aux)))

(defun tochemfig-atom-score-angles (atom state)
  "Determine preferred quadrants and charge angle for ATOM in STATE."
  (let ((atom-state (chem-exp-chemfig--atom-state state atom)))
    (if (> (length (tochemfig-atom-bond-angles atom)) 0)
        (let ((quadrants (tochemfig--score-angles atom tochemfig--quadrants tochemfig--quadrant-turf)))
          (setf (chem-exp-chemfig-atom-state-first-quadrant atom-state) (nth 0 quadrants))
          (setf (chem-exp-chemfig-atom-state-second-quadrant atom-state) (nth 1 quadrants)))
      (if (member (tochemfig-atom-element atom) tochemfig--hydrogen-lefties)
          (progn (setf (chem-exp-chemfig-atom-state-first-quadrant atom-state) "west")
                 (setf (chem-exp-chemfig-atom-state-second-quadrant atom-state) "east"))
        (setf (chem-exp-chemfig-atom-state-first-quadrant atom-state) "east")
        (setf (chem-exp-chemfig-atom-state-second-quadrant atom-state) "west")))
    (setf (chem-exp-chemfig-atom-state-charge-angle atom-state)
          (nth 0 (tochemfig--score-angles atom tochemfig--charge-positions tochemfig--charge-turf)))))

(defun tochemfig-atom-render (atom state)
  "Render ATOM using STATE, caching its phantom.
Return (atom-code . comment-code)."
  (let* ((options (tochemfig-atom-options atom))
         (idx (1+ (tochemfig-atom-idx atom)))
         (atom-state (chem-exp-chemfig--atom-state state atom))
         (rendered (tochemfig--format-atom
                    options idx (tochemfig-atom-label atom)
                    (tochemfig-atom-number atom)
                    (tochemfig-atom-element atom)
                    (tochemfig-atom-hydrogens atom)
                    (or (tochemfig-atom-charge atom) 0)
                    (or (tochemfig-atom-radical atom) 0)
                    (chem-exp-chemfig-atom-state-first-quadrant atom-state)
                    (chem-exp-chemfig-atom-state-second-quadrant atom-state)
                    (chem-exp-chemfig-atom-state-charge-angle atom-state)))
         (atom-code (nth 0 rendered)))
    (setf (chem-exp-chemfig-atom-state-string-pos atom-state) (nth 1 rendered))
    (setf (chem-exp-chemfig-atom-state-phantom atom-state) (nth 2 rendered))
    (setf (chem-exp-chemfig-atom-state-phantom-pos atom-state) (nth 3 rendered))
    (let* ((comment (tochemfig--format-atom-comment options idx))
           (marker-code (tochemfig--format-marker (tochemfig-atom-marker atom)))
           (comment (if marker-code " " comment)))
      (setf (chem-exp-chemfig-atom-state-explicit atom-state)
            (string-match-p "[A-Z0-9]" atom-code))
      (cons (concat (or marker-code "") atom-code) comment))))

(defun tochemfig-atom-render-phantom (atom state)
  "Render ATOM as a phantom for a ring-closing bond using STATE.
Return (phantom-code . comment-code)."
  (cons (chem-exp-chemfig-atom-state-phantom (chem-exp-chemfig--atom-state state atom))
        (tochemfig--format-closure-comment
         (tochemfig-atom-options atom) (1+ (tochemfig-atom-idx atom)))))

;;; Fancy bond geometry

(defun tochemfig--angle-penalty (angle)
  "Scoring function for choosing the side of a double bond stroke."
  (if (null angle) 0 (expt (- angle 105) 2)))

(defun tochemfig--cotan100 (angle)
  "Return 100 times the cotangent of ANGLE degrees, rounded."
  (round (/ 100.0 (tan (* angle (/ float-pi 180.0))))))

(defun tochemfig--adjoining-angles (bond atom inversion-angle)
  "Return the narrowest adjoining angles on ATOM's left and right."
  (let* ((raw (mapcar (lambda (a) (mod (round a) 360))
                      (tochemfig-atom-bond-angles atom)))
         (reference (mod (round (- (tochemfig-bond-angle bond) inversion-angle)) 360)))
    (let ((pos (cl-position reference raw :test #'equal)))
      (when pos
        (setq raw (append (cl-subseq raw 0 pos) (cl-subseq raw (1+ pos))))))
    (if (null raw)
        (list nil nil)
      (let* ((angles (sort (mapcar (lambda (a) (mod (- a reference) 360)) raw) #'<)))
        (list (round (car angles)) (round (car (last angles))))))))

(defun tochemfig-bond-upstream-angles (bond)
  "Return the narrowest upstream left and right angles of BOND."
  (let* ((pair (tochemfig--adjoining-angles bond (tochemfig-bond-start-atom bond) 0))
         (first (nth 0 pair))
         (last (nth 1 pair)))
    (when (not (null last)) (setq last (- 360 last)))
    (list (cons 'left first) (cons 'right last))))

(defun tochemfig-bond-downstream-angles (bond)
  "Return the narrowest downstream left and right angles of BOND."
  (let* ((pair (tochemfig--adjoining-angles bond (tochemfig-bond-end-atom bond) 180))
         (first (nth 0 pair))
         (last (nth 1 pair)))
    (when (not (null last)) (setq last (- 360 last)))
    (list (cons 'left last) (cons 'right first))))

(defun tochemfig-bond-shorten-stroke (_bond same-angle other-angle)
  "Determine how much to shorten the second stroke of a double bond."
  (if (null same-angle)
      0
    (let ((angle (cond
                  ((<= same-angle 180) (* 0.5 same-angle))
                  ((and (> same-angle 210) (< same-angle 270)) (- same-angle 180))
                  ((and other-angle (> other-angle 210) (< other-angle 270)) (- other-angle 180))
                  (t 90))))
      (tochemfig--cotan100 angle))))

(defun tochemfig-bond-fancy-double (bond state)
  "Return (side start end) for a fancy double BOND, or nil.
STATE supplies renderer-only atom flags."
  (let* ((start-atom (tochemfig-bond-start-atom bond))
         (end-atom (tochemfig-bond-end-atom bond))
         (start-angles (tochemfig-bond-upstream-angles bond))
         (end-angles (tochemfig-bond-downstream-angles bond))
         (s-left (cdr (assq 'left start-angles)))
         (s-right (cdr (assq 'right start-angles)))
         (e-left (cdr (assq 'left end-angles)))
         (e-right (cdr (assq 'right end-angles)))
         (clockwise (tochemfig-bond-clockwise bond))
         (s-exp (chem-exp-chemfig--atom-explicit-p state start-atom))
         (e-exp (chem-exp-chemfig--atom-explicit-p state end-atom)))
    (if (and (zerop clockwise)
             (or s-exp e-exp)
             (or (and s-exp e-exp)
                 (and s-exp (or (null e-left)
                                (and (<= 90 (abs e-left) 135)
                                     (<= 90 (abs e-right) 135))))
                 (and e-exp (or (null s-left)
                                (and (<= 90 (abs s-left) 135)
                                     (<= 90 (abs s-right) 135))))))
        nil
      (let* ((side (cond ((= clockwise -1) "left")
                         ((= clockwise 1) "right")
                         (t (let ((lp (+ (tochemfig--angle-penalty s-left)
                                         (tochemfig--angle-penalty e-left)))
                                  (rp (+ (tochemfig--angle-penalty s-right)
                                         (tochemfig--angle-penalty e-right))))
                              (cond ((< lp rp) "left")
                                    ((> lp rp) "right")
                                    ((< (abs (- (tochemfig-bond-angle bond) 44.5)) 90) "left")
                                    (t "right"))))))
             (start (if s-exp
                        0
                      (if (equal side "left")
                          (tochemfig-bond-shorten-stroke bond s-left s-right)
                        (tochemfig-bond-shorten-stroke bond s-right s-left))))
             (end (if e-exp
                      0
                    (if (equal side "left")
                        (tochemfig-bond-shorten-stroke bond e-left e-right)
                      (tochemfig-bond-shorten-stroke bond e-right e-left)))))
        (list side start end)))))

(defun tochemfig-bond-fancy-triple (bond state)
  "Return (start end) for a fancy triple BOND using STATE."
  (let* ((start-atom (tochemfig-bond-start-atom bond))
         (end-atom (tochemfig-bond-end-atom bond))
         (start (if (chem-exp-chemfig--atom-explicit-p state start-atom)
                    0
                  (let* ((vals (mapcar #'cdr (tochemfig-bond-upstream-angles bond)))
                         (non-nil (delq nil vals)))
                    (if (car vals) (tochemfig--cotan100 (* 0.5 (apply #'min non-nil))) 0))))
         (end (if (chem-exp-chemfig--atom-explicit-p state end-atom)
                  0
                (let* ((vals (mapcar #'cdr (tochemfig-bond-downstream-angles bond)))
                       (non-nil (delq nil vals)))
                  (if (car vals) (tochemfig--cotan100 (* 0.5 (apply #'min non-nil))) 0)))))
    (list start end)))

;;; Bond rendering

(defun tochemfig-bond-to-chemfig (bond state)
  "Render BOND (without its atom) to chemfig using STATE."
  (let* ((options (tochemfig-bond-options bond))
         (to-phantom (tochemfig-bond-to-phantom bond))
         (start-atom (tochemfig-bond-start-atom bond))
         (end-atom (tochemfig-bond-end-atom bond))
         (start-state (chem-exp-chemfig--atom-state state start-atom))
         (end-state (chem-exp-chemfig--atom-state state end-atom))
         (end-string-pos (if to-phantom
                             (chem-exp-chemfig-atom-state-phantom-pos end-state)
                           (chem-exp-chemfig-atom-state-string-pos end-state)))
         (bond-type (tochemfig-bond-bond-type bond))
         (bond-state (chem-exp-chemfig--bond-state state bond))
         (display-type (or (chem-exp-chemfig-bond-state-display-type bond-state)
                           bond-type))
         (tikz-styles (chem-exp-chemfig-bond-state-tikz-styles bond-state))
         (tikz-values (chem-exp-chemfig-bond-state-tikz-values bond-state)))
    (when (and (eq (tochemfig-bond-decoration bond) 'fancy)
               (memq bond-type '(double triple)))
      (if (eq bond-type 'double)
          (let ((fd (tochemfig-bond-fancy-double bond state)))
            (when fd
              (cl-pushnew "double" tikz-styles :test #'equal)
              (cl-pushnew (nth 0 fd) tikz-styles :test #'equal)
              (setq tikz-values
                    (append (list (cons 'start (nth 1 fd)) (cons 'end (nth 2 fd)))
                            tikz-values))
              (setq display-type 'decorated)))
        (cl-pushnew "triple" tikz-styles :test #'equal)
        (let ((ft (tochemfig-bond-fancy-triple bond state)))
          (setq tikz-values
                (append (list (cons 'start (nth 0 ft)) (cons 'end (nth 1 ft)))
                        tikz-values))
          (setq display-type 'decorated))))
    (setf (chem-exp-chemfig-bond-state-tikz-styles bond-state) tikz-styles)
    (setf (chem-exp-chemfig-bond-state-tikz-values bond-state) tikz-values)
    (setf (chem-exp-chemfig-bond-state-display-type bond-state) display-type)
    (tochemfig--format-bond options (tochemfig-bond-angle bond)
                            (tochemfig-bond-angle (tochemfig-bond-parent bond))
                            display-type
                            (tochemfig-bond-clockwise bond)
                            (chem-exp-chemfig-bond-state-is-last bond-state)
                            (tochemfig-bond-length bond)
                            (chem-exp-chemfig-atom-state-string-pos start-state)
                            end-string-pos
                            tikz-styles
                            tikz-values
                            (tochemfig-bond-marker bond))))

(defun tochemfig-bond-indent (bond level bond-code atom-code comment-code)
  "Indent BOND-CODE and ATOM-CODE for LEVEL, padding the bond code."
  (let* ((code-str (tochemfig--str bond-code))
         (padded (concat (make-string (max 0 (- tochemfig--bond-code-width (length code-str))) ?\s)
                         code-str))
         (indent-len (* (or (plist-get (tochemfig-bond-options bond) :indent) 4) level))
         (stuff (concat (make-string indent-len ?\s) padded (or atom-code ""))))
    (when (and comment-code (not (equal comment-code "")))
      (setq stuff (concat stuff "% " comment-code)))
    (replace-regexp-in-string "[ \t\n]+\\'" "" stuff)))

(defun tochemfig-bond-render (bond state level)
  "Render BOND and its trailing atom at LEVEL using STATE."
  (cond
   ((eq (tochemfig-bond-kind bond) 'aromatic-ring)
    (let* ((options (tochemfig-bond-options bond))
           (rendered (tochemfig--format-aromatic-ring
                      options (tochemfig-bond-angle bond)
                      (tochemfig-bond-parent-angle bond)
                      (tochemfig-bond-length bond)
                      (tochemfig-bond-radius bond))))
      (tochemfig-bond-indent bond level (nth 0 rendered) (nth 1 rendered) (nth 2 rendered))))
   ((eq (tochemfig-bond-kind bond) 'dummy-first)
    (let ((atom-render (tochemfig-atom-render (tochemfig-bond-end-atom bond) state)))
      (tochemfig-bond-indent bond level "" (car atom-render) (cdr atom-render))))
   (t
    (let* ((to-phantom (tochemfig-bond-to-phantom bond))
           (atom (tochemfig-bond-end-atom bond))
           (atom-render (if to-phantom
                            (tochemfig-atom-render-phantom atom state)
                          (tochemfig-atom-render atom state)))
           (bond-code (tochemfig-bond-to-chemfig bond state)))
      (tochemfig-bond-indent bond level bond-code (car atom-render) (cdr atom-render))))))

;;; Drawing walk

(defun tochemfig--render-branches (mol state output level bonds)
  "Append BONDS rendered as indented branches to OUTPUT.
OUTPUT is the reversed line accumulator; returns the updated accumulator."
  (let ((indent (or (plist-get (tochemfig-molecule-options mol) :indent) 4)))
    (dolist (bond bonds output)
      (let ((width (+ (* level indent) tochemfig--bond-code-width)))
        (setq output (cons (concat (make-string (1- width) ?\s) "(") output))
        (setq output (tochemfig--render mol state output bond level))
        (setq output (cons (concat (make-string (1- width) ?\s) ")") output))))))

(defun tochemfig--render (mol state output bond level)
  "Recursively render BOND into OUTPUT at LEVEL using STATE."
  (setq output (cons (tochemfig-bond-render bond state level) output))
  (let ((branches (copy-sequence (tochemfig-bond-descendants bond))))
    (cond
     ((eq bond (tochemfig-molecule-exit-bond mol))
      (tochemfig--render-branches mol state output (1+ level) branches))
     ((null branches) output)
     (t
      (let ((trunk-idx (cl-position-if #'tochemfig-bond-is-trunk branches))
            (first nil))
        (if trunk-idx
            (setq first (nth trunk-idx branches)
                  branches (append (cl-subseq branches 0 trunk-idx)
                                   (cl-subseq branches (1+ trunk-idx))))
          (setq first (car branches)
                branches (cdr branches)))
        (setq output (tochemfig--render-branches mol state output (1+ level) branches))
        (tochemfig--render mol state output first level))))))

(defun tochemfig--render-molecule (mol state)
  "Render MOL to a list of lines using STATE."
  (nreverse (tochemfig--render mol state nil (tochemfig-molecule-root mol) 0)))

;;; Exporter entry point

(defun chem-exp-chemfig--prepare-cross-state (state drawing)
  "Fill STATE with cross-bond styling for DRAWING."
  (dolist (bond (chem-tree-bonds drawing))
    (when (tochemfig-bond-cross-p bond)
      (let* ((start-angles (mapcar #'cdr (tochemfig-bond-upstream-angles bond)))
             (end-angles (mapcar #'cdr (tochemfig-bond-downstream-angles bond)))
             (start-angle (apply #'min (delq nil start-angles)))
             (end-angle (apply #'min (delq nil end-angles)))
             (start (max 10 (tochemfig--cotan100 start-angle)))
             (end (max 10 (tochemfig--cotan100 end-angle)))
             (bond-state (chem-exp-chemfig--bond-state state bond)))
        (setf (chem-exp-chemfig-bond-state-tikz-styles bond-state) (list "cross"))
        (setf (chem-exp-chemfig-bond-state-tikz-values bond-state)
              (list (cons 'bgstart start) (cons 'bgend end)))
        (setf (chem-exp-chemfig-bond-state-is-last bond-state)
              (tochemfig-bond-cross-last-p bond))))))

(defun chem-exp-chemfig-render (drawing options)
  "Render DRAWING as chemfig using OPTIONS."
  (let ((state (chem-exp-chemfig--make-state)))
    (chem-exp-chemfig--prepare-cross-state state drawing)
    (maphash (lambda (_ atom) (tochemfig-atom-score-angles atom state))
             (chem-drawing-atoms drawing))
    (tochemfig--format-output options (tochemfig--render-molecule drawing state))))

(chem-register-exporter 'chemfig #'chem-exp-chemfig-render
                        '(:label "Chemfig (LaTeX)"))

(provide 'chem-exp-chemfig)
;;; chem-exp-chemfig.el ends here
