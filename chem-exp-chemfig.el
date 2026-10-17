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
(require 'chem-bond)
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
  atoms bonds options)

(defun chem-exp-chemfig--make-state (options)
  "Create an empty renderer state with exporter OPTIONS."
  (chem-exp-chemfig-state-create
   :atoms (make-hash-table :test 'eql)
   :bonds (make-hash-table :test 'eq)
   :options options))

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

(defconst chem--hydrogen-lefties
  '("O" "S" "Se" "Te" "F" "Cl" "Br" "I" "At")
  "Elements whose hydrogen is drawn to the left when the atom is solitary.")

(defconst chem--quadrant-turf 80
  "Degrees that have to remain free on either side of a hydrogen placement.")

(defconst chem--quadrants
  '((0 0 "east") (1 180 "west") (2 270 "south") (3 90 "north"))
  "Candidate quadrants for hydrogen placement: (priority angle name).")

(defconst chem--charge-turf 50
  "Reserved angle for charges - big enough for 2+.")

(defconst chem--charge-positions
  '((0 15 "top_right") (1 165 "top_left") (2 90 "top_center")
    (3 270 "bottom_center") (4 345 "bottom_right") (5 195 "bottom_left"))
  "Candidate positions for charges: (priority angle name).")

(defconst chem--explicit-characters "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  "Characters that mark an atom as explicitly printed.")

(defun chem--score-angle (a b turf)
  "Score how much angle B impinges on candidate angle A within TURF."
  (let* ((diff (mod (- a b) 360))
         (angle (min diff (- 360 diff))))
    (expt (max 0 (- turf angle)) 2)))

(defun chem--score-angles (atom choices turf)
  "Return CHOICES sorted by how well they avoid ATOM's bond angles."
  (let ((aux nil))
    (dolist (choice choices)
      (let ((score 0))
        (dolist (bond-angle (chem-atom-bond-angles atom))
          (setq score (+ score (chem--score-angle (nth 1 choice) bond-angle turf))))
        (push (list score (nth 0 choice) (nth 2 choice)) aux)))
    (setq aux (sort aux
                    (lambda (a b)
                      (cond ((/= (nth 0 a) (nth 0 b)) (< (nth 0 a) (nth 0 b)))
                            ((/= (nth 1 a) (nth 1 b)) (< (nth 1 a) (nth 1 b)))
                            (t (string< (nth 2 a) (nth 2 b)))))))
    (mapcar (lambda (x) (nth 2 x)) aux)))

(defun chem-atom-score-angles (atom state)
  "Determine preferred quadrants and charge angle for ATOM in STATE."
  (let ((atom-state (chem-exp-chemfig--atom-state state atom)))
    (if (> (length (chem-atom-bond-angles atom)) 0)
        (let ((quadrants (chem--score-angles atom chem--quadrants chem--quadrant-turf)))
          (setf (chem-exp-chemfig-atom-state-first-quadrant atom-state) (nth 0 quadrants))
          (setf (chem-exp-chemfig-atom-state-second-quadrant atom-state) (nth 1 quadrants)))
      (if (member (chem-atom-element atom) chem--hydrogen-lefties)
          (progn (setf (chem-exp-chemfig-atom-state-first-quadrant atom-state) "west")
                 (setf (chem-exp-chemfig-atom-state-second-quadrant atom-state) "east"))
        (setf (chem-exp-chemfig-atom-state-first-quadrant atom-state) "east")
        (setf (chem-exp-chemfig-atom-state-second-quadrant atom-state) "west")))
    (setf (chem-exp-chemfig-atom-state-charge-angle atom-state)
          (nth 0 (chem--score-angles atom chem--charge-positions chem--charge-turf)))))

(defun chem-atom-render (atom state)
  "Render ATOM using STATE, caching its phantom.
Return (atom-code . comment-code)."
  (let* ((options (chem-exp-chemfig-state-options state))
         (idx (1+ (chem-atom-idx atom)))
         (atom-state (chem-exp-chemfig--atom-state state atom))
         (rendered (chem--format-atom
                    options idx (chem-atom-label atom)
                    (chem-atom-number atom)
                    (chem-atom-element atom)
                    (chem-atom-hydrogens atom)
                    (or (chem-atom-charge atom) 0)
                    (or (chem-atom-radical atom) 0)
                    (chem-exp-chemfig-atom-state-first-quadrant atom-state)
                    (chem-exp-chemfig-atom-state-second-quadrant atom-state)
                    (chem-exp-chemfig-atom-state-charge-angle atom-state)))
         (atom-code (nth 0 rendered)))
    (setf (chem-exp-chemfig-atom-state-string-pos atom-state) (nth 1 rendered))
    (setf (chem-exp-chemfig-atom-state-phantom atom-state) (nth 2 rendered))
    (setf (chem-exp-chemfig-atom-state-phantom-pos atom-state) (nth 3 rendered))
    (let* ((comment (chem--format-atom-comment options idx))
           (marker-code (chem--format-marker (chem-atom-marker atom)))
           (comment (if marker-code " " comment)))
      (setf (chem-exp-chemfig-atom-state-explicit atom-state)
            (string-match-p "[A-Z0-9]" atom-code))
      (cons (concat (or marker-code "") atom-code) comment))))

(defun chem-atom-render-phantom (atom state)
  "Render ATOM as a phantom for a ring-closing bond using STATE.
Return (phantom-code . comment-code)."
  (cons (chem-exp-chemfig-atom-state-phantom (chem-exp-chemfig--atom-state state atom))
        (chem--format-closure-comment
         (chem-exp-chemfig-state-options state) (1+ (chem-atom-idx atom)))))

;;; Fancy bond geometry

(defun chem--angle-penalty (angle)
  "Scoring function for choosing the side of a double bond stroke."
  (if (null angle) 0 (expt (- angle 105) 2)))

(defun chem--cotan100 (angle)
  "Return 100 times the cotangent of ANGLE degrees, rounded."
  (round (/ 100.0 (tan (* angle (/ float-pi 180.0))))))

(defun chem--adjoining-angles (bond atom inversion-angle)
  "Return the narrowest adjoining angles on ATOM's left and right."
  (let* ((raw (mapcar (lambda (a) (mod (round a) 360))
                      (chem-atom-bond-angles atom)))
         (reference (mod (round (- (chem-bond-angle bond) inversion-angle)) 360)))
    (let ((pos (cl-position reference raw :test #'equal)))
      (when pos
        (setq raw (append (cl-subseq raw 0 pos) (cl-subseq raw (1+ pos))))))
    (if (null raw)
        (list nil nil)
      (let* ((angles (sort (mapcar (lambda (a) (mod (- a reference) 360)) raw) #'<)))
        (list (round (car angles)) (round (car (last angles))))))))

(defun chem-bond-upstream-angles (bond)
  "Return the narrowest upstream left and right angles of BOND."
  (let* ((pair (chem--adjoining-angles bond (chem-bond-start-atom bond) 0))
         (first (nth 0 pair))
         (last (nth 1 pair)))
    (when (not (null last)) (setq last (- 360 last)))
    (list (cons 'left first) (cons 'right last))))

(defun chem-bond-downstream-angles (bond)
  "Return the narrowest downstream left and right angles of BOND."
  (let* ((pair (chem--adjoining-angles bond (chem-bond-end-atom bond) 180))
         (first (nth 0 pair))
         (last (nth 1 pair)))
    (when (not (null last)) (setq last (- 360 last)))
    (list (cons 'left last) (cons 'right first))))

(defun chem-bond-shorten-stroke (_bond same-angle other-angle)
  "Determine how much to shorten the second stroke of a double bond."
  (if (null same-angle)
      0
    (let ((angle (cond
                  ((<= same-angle 180) (* 0.5 same-angle))
                  ((and (> same-angle 210) (< same-angle 270)) (- same-angle 180))
                  ((and other-angle (> other-angle 210) (< other-angle 270)) (- other-angle 180))
                  (t 90))))
      (chem--cotan100 angle))))

(defun chem-bond-fancy-double (bond state)
  "Return (side start end) for a fancy double BOND, or nil.
STATE supplies renderer-only atom flags."
  (let* ((start-atom (chem-bond-start-atom bond))
         (end-atom (chem-bond-end-atom bond))
         (start-angles (chem-bond-upstream-angles bond))
         (end-angles (chem-bond-downstream-angles bond))
         (s-left (cdr (assq 'left start-angles)))
         (s-right (cdr (assq 'right start-angles)))
         (e-left (cdr (assq 'left end-angles)))
         (e-right (cdr (assq 'right end-angles)))
         (clockwise (chem-bond-clockwise bond))
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
                         (t (let ((lp (+ (chem--angle-penalty s-left)
                                         (chem--angle-penalty e-left)))
                                  (rp (+ (chem--angle-penalty s-right)
                                         (chem--angle-penalty e-right))))
                              (cond ((< lp rp) "left")
                                    ((> lp rp) "right")
                                    ((< (abs (- (chem-bond-angle bond) 44.5)) 90) "left")
                                    (t "right"))))))
             (start (if s-exp
                        0
                      (if (equal side "left")
                          (chem-bond-shorten-stroke bond s-left s-right)
                        (chem-bond-shorten-stroke bond s-right s-left))))
             (end (if e-exp
                      0
                    (if (equal side "left")
                        (chem-bond-shorten-stroke bond e-left e-right)
                      (chem-bond-shorten-stroke bond e-right e-left)))))
        (list side start end)))))

(defun chem-bond-fancy-triple (bond state)
  "Return (start end) for a fancy triple BOND using STATE."
  (let* ((start-atom (chem-bond-start-atom bond))
         (end-atom (chem-bond-end-atom bond))
         (start (if (chem-exp-chemfig--atom-explicit-p state start-atom)
                    0
                  (let* ((vals (mapcar #'cdr (chem-bond-upstream-angles bond)))
                         (non-nil (delq nil vals)))
                    (if (car vals) (chem--cotan100 (* 0.5 (apply #'min non-nil))) 0))))
         (end (if (chem-exp-chemfig--atom-explicit-p state end-atom)
                  0
                (let* ((vals (mapcar #'cdr (chem-bond-downstream-angles bond)))
                       (non-nil (delq nil vals)))
                  (if (car vals) (chem--cotan100 (* 0.5 (apply #'min non-nil))) 0)))))
    (list start end)))

;;; Bond rendering

(defun chem-bond-to-chemfig (node state)
  "Render NODE's bond (without its atom) to chemfig using STATE."
  (let* ((bond (chem-node-bond node))
         (options (chem-exp-chemfig-state-options state))
         (to-phantom (chem-node-phantom-p node))
         (parent-angle (let ((parent (chem-node-parent node)))
                         (when (and parent (chem-node-bond parent))
                           (chem-bond-angle (chem-node-bond parent)))))
         (start-atom (chem-bond-start-atom bond))
         (end-atom (chem-bond-end-atom bond))
         (start-state (chem-exp-chemfig--atom-state state start-atom))
         (end-state (chem-exp-chemfig--atom-state state end-atom))
         (end-string-pos (if to-phantom
                             (chem-exp-chemfig-atom-state-phantom-pos end-state)
                           (chem-exp-chemfig-atom-state-string-pos end-state)))
         (bond-type (chem-bond-bond-type bond))
         (bond-state (chem-exp-chemfig--bond-state state bond))
         (display-type (or (chem-exp-chemfig-bond-state-display-type bond-state)
                           bond-type))
         (tikz-styles (chem-exp-chemfig-bond-state-tikz-styles bond-state))
         (tikz-values (chem-exp-chemfig-bond-state-tikz-values bond-state)))
    (when (and (eq (chem-bond-decoration bond) 'fancy)
               (memq bond-type '(double triple)))
      (if (eq bond-type 'double)
          (let ((fd (chem-bond-fancy-double bond state)))
            (when fd
              (cl-pushnew "double" tikz-styles :test #'equal)
              (cl-pushnew (nth 0 fd) tikz-styles :test #'equal)
              (setq tikz-values
                    (append (list (cons 'start (nth 1 fd)) (cons 'end (nth 2 fd)))
                            tikz-values))
              (setq display-type 'decorated)))
        (cl-pushnew "triple" tikz-styles :test #'equal)
        (let ((ft (chem-bond-fancy-triple bond state)))
          (setq tikz-values
                (append (list (cons 'start (nth 0 ft)) (cons 'end (nth 1 ft)))
                        tikz-values))
          (setq display-type 'decorated))))
    (setf (chem-exp-chemfig-bond-state-tikz-styles bond-state) tikz-styles)
    (setf (chem-exp-chemfig-bond-state-tikz-values bond-state) tikz-values)
    (setf (chem-exp-chemfig-bond-state-display-type bond-state) display-type)
    (chem--format-bond options (chem-bond-angle bond)
                            parent-angle
                            display-type
                            (chem-bond-clockwise bond)
                            (chem-exp-chemfig-bond-state-is-last bond-state)
                            (chem-bond-length bond)
                            (chem-exp-chemfig-atom-state-string-pos start-state)
                            end-string-pos
                            tikz-styles
                            tikz-values
                            (chem-bond-marker bond))))

(defun chem-bond-indent (options level bond-code atom-code comment-code)
  "Indent BOND-CODE and ATOM-CODE for LEVEL, padding the bond code.
OPTIONS supplies the indentation width."
  (let* ((code-str (chem--str bond-code))
         (padded (concat (make-string (max 0 (- chem--bond-code-width (length code-str))) ?\s)
                         code-str))
         (indent-len (* (or (plist-get options :indent) 4) level))
         (stuff (concat (make-string indent-len ?\s) padded (or atom-code ""))))
    (when (and comment-code (not (equal comment-code "")))
      (setq stuff (concat stuff "% " comment-code)))
    (replace-regexp-in-string "[ \t\n]+\\'" "" stuff)))

(defun chem-bond-render (mol node state level)
  "Render NODE and its trailing atom at LEVEL using STATE.
MOL supplies the atom table for the traversal root."
  (let ((bond (chem-node-bond node)))
    (cond
     ((null bond)
      (let ((atom-render
             (chem-atom-render
              (gethash (chem-node-to node) (chem-drawing-atoms mol))
              state)))
        (chem-bond-indent (chem-exp-chemfig-state-options state) level
                          "" (car atom-render) (cdr atom-render))))
     ((eq (chem-bond-kind bond) 'aromatic-ring)
      (let* ((options (chem-exp-chemfig-state-options state))
             (rendered (chem--format-aromatic-ring
                        options (chem-bond-angle bond)
                        (chem-bond-parent-angle bond)
                        (chem-bond-length bond)
                        (chem-bond-radius bond))))
        (chem-bond-indent options level
                          (nth 0 rendered) (nth 1 rendered) (nth 2 rendered))))
     (t
      (let* ((to-phantom (chem-node-phantom-p node))
             (atom (chem-bond-end-atom bond))
             (atom-render (if to-phantom
                              (chem-atom-render-phantom atom state)
                            (chem-atom-render atom state)))
             (bond-code (chem-bond-to-chemfig node state)))
        (chem-bond-indent (chem-exp-chemfig-state-options state) level
                          bond-code (car atom-render) (cdr atom-render)))))))

;;; Drawing walk

(defun chem--render-branches (mol state output level nodes)
  "Append NODES rendered as indented branches to OUTPUT.
OUTPUT is the reversed line accumulator; returns the updated accumulator."
  (let ((indent (or (plist-get (chem-exp-chemfig-state-options state) :indent) 4)))
    (dolist (node nodes output)
      (let ((width (+ (* level indent) chem--bond-code-width)))
        (setq output (cons (concat (make-string (1- width) ?\s) "(") output))
        (setq output (chem--render mol state output node level))
        (setq output (cons (concat (make-string (1- width) ?\s) ")") output))))))

(defun chem--render (mol state output node level)
  "Recursively render NODE into OUTPUT at LEVEL using STATE."
  (setq output (cons (chem-bond-render mol node state level) output))
  (let ((branches (copy-sequence (chem-node-children node))))
    (cond
     ((eq node (chem-drawing-exit-node mol))
      (chem--render-branches mol state output (1+ level) branches))
     ((null branches) output)
     (t
      (let ((trunk-idx (cl-position-if #'chem-node-trunk-p branches))
            (first nil))
        (if trunk-idx
            (setq first (nth trunk-idx branches)
                  branches (append (cl-subseq branches 0 trunk-idx)
                                   (cl-subseq branches (1+ trunk-idx))))
          (setq first (car branches)
                branches (cdr branches)))
        (setq output (chem--render-branches mol state output (1+ level) branches))
        (chem--render mol state output first level))))))

(defun chem--render-molecule (mol state)
  "Render MOL to a list of lines using STATE."
  (nreverse (chem--render mol state nil (chem-drawing-root mol) 0)))

;;; Exporter entry point

(defun chem-exp-chemfig--prepare-cross-state (state drawing)
  "Fill STATE with cross-bond styling for DRAWING."
  (dolist (node (chem-tree-nodes drawing))
    (when (chem-node-cross-p node)
      (let* ((bond (chem-node-bond node))
             (start-angles (mapcar #'cdr (chem-bond-upstream-angles bond)))
             (end-angles (mapcar #'cdr (chem-bond-downstream-angles bond)))
             (start-angle (apply #'min (delq nil start-angles)))
             (end-angle (apply #'min (delq nil end-angles)))
             (start (max 10 (chem--cotan100 start-angle)))
             (end (max 10 (chem--cotan100 end-angle)))
             (bond-state (chem-exp-chemfig--bond-state state bond)))
        (setf (chem-exp-chemfig-bond-state-tikz-styles bond-state) (list "cross"))
        (setf (chem-exp-chemfig-bond-state-tikz-values bond-state)
              (list (cons 'bgstart start) (cons 'bgend end)))
        (setf (chem-exp-chemfig-bond-state-is-last bond-state)
              (chem-node-cross-last-p node))))))

(defun chem-exp-chemfig-render (drawing options)
  "Render DRAWING as chemfig using OPTIONS."
  (let ((state (chem-exp-chemfig--make-state options)))
    (chem-exp-chemfig--prepare-cross-state state drawing)
    (maphash (lambda (_ atom) (chem-atom-score-angles atom state))
             (chem-drawing-atoms drawing))
    (chem--format-output options (chem--render-molecule drawing state))))

(defconst chem-exp-chemfig-option-specs
  '((:key :terse
     :variable chem-exp-chemfig-default-terse
     :label "Terse output"
     :type boolean
     :group output)
    (:key :indent
     :variable chem-exp-chemfig-default-indent
     :label "Indentation"
     :type number
     :group output)
    (:key :relative-angles
     :variable chem-exp-chemfig-default-relative-angles
     :label "Relative angles"
     :type boolean
     :group output)
    (:key :legacy-lewis
     :variable chem-exp-chemfig-default-legacy-lewis
     :label "Legacy \\lewis (ChemFig < 1.6)"
     :type boolean
     :group advanced)
    (:key :wrap-chemfig
     :variable chem-exp-chemfig-default-wrap-chemfig
     :label "Wrap in \\chemfig{}"
     :type boolean
     :group output)
    (:key :submol-name
     :variable chem-exp-chemfig-default-submol-name
     :label "Submol name"
     :type string
     :group sub-molecule))
  "Chemfig exporter option metadata.")

(chem-register-exporter 'chemfig #'chem-exp-chemfig-render
                        (list :label "Chemfig (LaTeX)"
                              :options chem-exp-chemfig-option-specs))

(provide 'chem-exp-chemfig)
;;; chem-exp-chemfig.el ends here
