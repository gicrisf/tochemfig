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

(defun tochemfig-atom-score-angles (atom)
  "Determine preferred quadrants and charge angle for ATOM."
  (if (> (length (tochemfig-atom-bond-angles atom)) 0)
      (let ((quadrants (tochemfig--score-angles atom tochemfig--quadrants tochemfig--quadrant-turf)))
        (setf (tochemfig-atom-first-quadrant atom) (nth 0 quadrants))
        (setf (tochemfig-atom-second-quadrant atom) (nth 1 quadrants)))
    (if (member (tochemfig-atom-element atom) tochemfig--hydrogen-lefties)
        (progn (setf (tochemfig-atom-first-quadrant atom) "west")
               (setf (tochemfig-atom-second-quadrant atom) "east"))
      (setf (tochemfig-atom-first-quadrant atom) "east")
      (setf (tochemfig-atom-second-quadrant atom) "west")))
  (setf (tochemfig-atom-charge-angle atom)
        (nth 0 (tochemfig--score-angles atom tochemfig--charge-positions tochemfig--charge-turf))))

(defun tochemfig-atom-render (atom)
  "Render ATOM, caching its phantom.
Return (atom-code . comment-code)."
  (let* ((options (tochemfig-atom-options atom))
         (idx (1+ (tochemfig-atom-idx atom)))
         (rendered (tochemfig--format-atom
                    options idx (tochemfig-atom-element atom)
                    (tochemfig-atom-hydrogens atom)
                    (or (tochemfig-atom-charge atom) 0)
                    (or (tochemfig-atom-radical atom) 0)
                    (tochemfig-atom-first-quadrant atom)
                    (tochemfig-atom-second-quadrant atom)
                    (tochemfig-atom-charge-angle atom)))
         (atom-code (nth 0 rendered)))
    (setf (tochemfig-atom-string-pos atom) (nth 1 rendered))
    (setf (tochemfig-atom-phantom atom) (nth 2 rendered))
    (setf (tochemfig-atom-phantom-pos atom) (nth 3 rendered))
    (let* ((comment (tochemfig--format-atom-comment options idx))
           (marker-code (tochemfig--format-marker (tochemfig-atom-marker atom)))
           (comment (if marker-code " " comment)))
      (setf (tochemfig-atom-explicit atom)
            (string-match-p "[A-Z0-9]" atom-code))
      (cons (concat (or marker-code "") atom-code) comment))))

(defun tochemfig-atom-render-phantom (atom)
  "Render ATOM as a phantom for a ring-closing bond.
Return (phantom-code . comment-code)."
  (cons (tochemfig-atom-phantom atom)
        (tochemfig--format-closure-comment
         (tochemfig-atom-options atom) (1+ (tochemfig-atom-idx atom)))))

;;; Bond rendering

(defun tochemfig-bond-to-chemfig (bond)
  "Render BOND (without its atom) to chemfig."
  (let* ((options (tochemfig-bond-options bond))
         (to-phantom (tochemfig-bond-to-phantom bond))
         (end-atom (tochemfig-bond-end-atom bond))
         (end-string-pos (if to-phantom
                             (tochemfig-atom-phantom-pos end-atom)
                           (tochemfig-atom-string-pos end-atom)))
         (bond-type (tochemfig-bond-bond-type bond)))
    (when (and (plist-get options :fancy-bonds)
               (memq bond-type '(double triple)))
      (if (eq bond-type 'double)
          (let ((fd (tochemfig-bond-fancy-double bond)))
            (when fd
              (cl-pushnew "double" (tochemfig-bond-tikz-styles bond) :test #'equal)
              (cl-pushnew (nth 0 fd) (tochemfig-bond-tikz-styles bond) :test #'equal)
              (setf (tochemfig-bond-tikz-values bond)
                    (append (list (cons 'start (nth 1 fd)) (cons 'end (nth 2 fd)))
                            (tochemfig-bond-tikz-values bond)))
              (setf (tochemfig-bond-bond-type bond) 'decorated)))
        (cl-pushnew "triple" (tochemfig-bond-tikz-styles bond) :test #'equal)
        (let ((ft (tochemfig-bond-fancy-triple bond)))
          (setf (tochemfig-bond-tikz-values bond)
                (append (list (cons 'start (nth 0 ft)) (cons 'end (nth 1 ft)))
                        (tochemfig-bond-tikz-values bond)))
          (setf (tochemfig-bond-bond-type bond) 'decorated))))
    (tochemfig--format-bond options (tochemfig-bond-angle bond)
                            (tochemfig-bond-angle (tochemfig-bond-parent bond))
                            (tochemfig-bond-bond-type bond)
                            (tochemfig-bond-clockwise bond)
                            (tochemfig-bond-is-last bond)
                            (tochemfig-bond-length bond)
                            (tochemfig-atom-string-pos (tochemfig-bond-start-atom bond))
                            end-string-pos
                            (tochemfig-bond-tikz-styles bond)
                            (tochemfig-bond-tikz-values bond)
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

(defun tochemfig-bond-render (bond level)
  "Render BOND and its trailing atom at LEVEL."
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
    (let ((atom-render (tochemfig-atom-render (tochemfig-bond-end-atom bond))))
      (tochemfig-bond-indent bond level "" (car atom-render) (cdr atom-render))))
   (t
    (let* ((to-phantom (tochemfig-bond-to-phantom bond))
           (atom (tochemfig-bond-end-atom bond))
           (atom-render (if to-phantom
                            (tochemfig-atom-render-phantom atom)
                          (tochemfig-atom-render atom)))
           (bond-code (tochemfig-bond-to-chemfig bond)))
      (tochemfig-bond-indent bond level bond-code (car atom-render) (cdr atom-render))))))

;;; Drawing walk

(defun tochemfig--render-branches (mol output level bonds)
  "Append BONDS rendered as indented branches to OUTPUT.
OUTPUT is the reversed line accumulator; returns the updated accumulator."
  (let ((indent (or (plist-get (tochemfig-molecule-options mol) :indent) 4)))
    (dolist (bond bonds output)
      (let ((width (+ (* level indent) tochemfig--bond-code-width)))
        (setq output (cons (concat (make-string (1- width) ?\s) "(") output))
        (setq output (tochemfig--render mol output bond level))
        (setq output (cons (concat (make-string (1- width) ?\s) ")") output))))))

(defun tochemfig--render (mol output bond level)
  "Recursively render BOND into OUTPUT at LEVEL; returns the accumulator."
  (setq output (cons (tochemfig-bond-render bond level) output))
  (let ((branches (copy-sequence (tochemfig-bond-descendants bond))))
    (cond
     ((eq bond (tochemfig-molecule-exit-bond mol))
      (tochemfig--render-branches mol output (1+ level) branches))
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
        (setq output (tochemfig--render-branches mol output (1+ level) branches))
        (tochemfig--render mol output first level))))))

(defun tochemfig--render-molecule (mol)
  "Render MOL to a list of lines."
  (nreverse (tochemfig--render mol nil (tochemfig-molecule-root mol) 0)))

;;; Exporter entry point

(defun chem-exp-chemfig-render (drawing options)
  "Render DRAWING as chemfig using OPTIONS."
  (maphash (lambda (_ atom) (tochemfig-atom-score-angles atom))
           (chem-drawing-atoms drawing))
  (tochemfig--format-output options (tochemfig--render-molecule drawing)))

(chem-register-exporter 'chemfig #'chem-exp-chemfig-render
                        '(:label "Chemfig (LaTeX)"))

(provide 'chem-exp-chemfig)
;;; chem-exp-chemfig.el ends here
