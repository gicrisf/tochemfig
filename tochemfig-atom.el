;;; tochemfig-atom.el --- Atom representation for chemfig generation -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2025 Giovanni Crisalfi

;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Translation of mol2chemfigPy3's `atom.py'.  An atom knows its element,
;; coordinates, charge, radical state and implicit hydrogens, and can render
;; itself to chemfig (choosing where to place hydrogens and charges so that
;; they do not collide with bonds).

;;; Code:

(require 'cl-lib)
(require 'chem-ir)
(require 'tochemfig-mappings)

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

;; Transitional aliases while the atom struct lives in `chem-ir'.
(eval-and-compile
  (chem-ir-alias-accessor 'tochemfig-atom-create 'chem-atom-create)
  (chem-ir-alias-accessor 'tochemfig-atom-p 'chem-atom-p)
  (chem-ir-alias-accessor 'copy-tochemfig-atom 'copy-chem-atom)
  (dolist (slot '(options idx x y element hydrogens charge radical neighbors
                         first-quadrant second-quadrant charge-angle
                         string-pos phantom phantom-pos bond-angles explicit
                         marker))
    (chem-ir-alias-accessor
     (intern (concat "tochemfig-atom-" (symbol-name slot)))
     (intern (concat "chem-atom-" (symbol-name slot))))))

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

(provide 'tochemfig-atom)
;;; tochemfig-atom.el ends here
