;;; chem-bond.el --- Bond creation and geometry -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2025 Giovanni Crisalfi

;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Translation of mol2chemfigPy3's `bond.py'.  A bond is also a node in the
;; drawing tree: it knows its parent and descendants.  Exporter-specific
;; rendering lives elsewhere.

;;; Code:

(require 'cl-lib)
(require 'chem-ir)

;; Indigo stereo constants.
(defconst chem--stereo-up 5)
(defconst chem--stereo-down 6)
(defconst chem--stereo-either 4)

(defconst chem--bond-mapping
  '((1 . single) (2 . double) (3 . triple) (4 . either)
    (5 . upto) (6 . downto))
  "Raw Indigo bond type/stereo to mol2chemfig bond type.
Note that order 4 (aromatic) maps to `either' (wavy), mirroring the reference
implementation where the dict literal overrides the aromatic entry.")

(defconst chem--aromatic-ring-scale 1.5)

(defun chem--rad-to-deg (radians)
  "Convert RADIANS to degrees."
  (* radians (/ 180.0 float-pi)))

(defun chem--compare-positions (x1 y1 x2 y2)
  "Return (distance angle) between (X1,Y1) and (X2,Y2).
The angle follows mol2chemfig's convention and may be negative."
  (let* ((xd (- x2 x1))
         (yd (- y2 y1))
         (length (sqrt (+ (* xd xd) (* yd yd)))))
    (list length
          (cond
           ((= xd 0) (if (< yd 0) 270 90))
           (t (let ((raw (chem--rad-to-deg (atan (abs (/ yd xd))))))
                (cond ((>= yd 0) (if (> xd 0) raw (- 180 raw)))
                      (t (if (> xd 0) (- raw) (+ 180 raw))))))))))

(defun chem-bond-create-from (options start-atom end-atom bond-order stereo)
  "Create a bond from START-ATOM to END-ATOM.
BOND-ORDER is an Indigo keyword; STEREO an Indigo stereo keyword."
  (let* ((stereo-code (cond ((eq stereo :up) chem--stereo-up)
                            ((eq stereo :down) chem--stereo-down)
                            ((eq stereo :either) chem--stereo-either)
                            (t 0)))
         (flip (plist-get options :flip))
         (flop (plist-get options :flop)))
    (when (and (memq stereo-code (list chem--stereo-up chem--stereo-down))
               (not (eq flip flop)))
      (setq stereo-code (- (+ chem--stereo-up chem--stereo-down) stereo-code)))
    (let* ((bond-type
            (if (memq stereo-code (list chem--stereo-up chem--stereo-down
                                        chem--stereo-either))
                (cdr (assq stereo-code chem--bond-mapping))
              (let ((raw (pcase bond-order
                           (:single 1) (:double 2) (:triple 3) (:aromatic 4) (_ 0))))
                (or (cdr (assq raw chem--bond-mapping)) 'query))))
           (dims (chem--compare-positions
                  (chem-atom-x start-atom) (chem-atom-y start-atom)
                  (chem-atom-x end-atom) (chem-atom-y end-atom)))
           (marker-opt (plist-get options :markers))
           (ids (sort (list (1+ (chem-atom-idx start-atom))
                            (1+ (chem-atom-idx end-atom))) #'<)))
      (chem-bond-create
       :options options
       :kind 'bond
       :start-atom start-atom
       :end-atom end-atom
       :bond-type bond-type
       :descendants nil
       :length (nth 0 dims)
       :angle (+ (nth 1 dims) (or (plist-get options :angle) 0.0))
       :marker (if marker-opt (format "%s%d-%d" marker-opt (nth 0 ids) (nth 1 ids)) "")
       :to-phantom nil
       :is-trunk nil
       :parent nil
       :clockwise 0))))

(defun chem-bond-invert (bond)
  "Return a reversed copy of BOND."
  (let ((c (copy-chem-bond bond)))
    (setf (chem-bond-start-atom c) (chem-bond-end-atom bond))
    (setf (chem-bond-end-atom c) (chem-bond-start-atom bond))
    (setf (chem-bond-angle c) (mod (+ (chem-bond-angle c) 180) 360))
    (pcase (chem-bond-bond-type c)
      ('upto (setf (chem-bond-bond-type c) 'upfrom))
      ('downto (setf (chem-bond-bond-type c) 'downfrom)))
    c))

(defun chem-bond-set-link (bond)
  "Make BOND an invisible link."
  (setf (chem-bond-bond-type bond) 'link)
  (setf (chem-bond-marker bond) ""))

(defun chem-bond-is-clockwise (bond center-x center-y)
  "Determine the orientation of BOND relative to (CENTER-X, CENTER-Y)."
  (when (= (chem-bond-clockwise bond) 0)
    (let* ((dims (chem--compare-positions
                  (chem-atom-x (chem-bond-end-atom bond))
                  (chem-atom-y (chem-bond-end-atom bond))
                  center-x center-y))
           (center-angle (+ (nth 1 dims) (or (plist-get (chem-bond-options bond) :angle) 0.0)))
           (center-kink (mod (- center-angle (chem-bond-angle bond)) 360)))
      (setf (chem-bond-clockwise bond) (if (> center-kink 180) 1 -1)))))

(provide 'chem-bond)
;;; chem-bond.el ends here
