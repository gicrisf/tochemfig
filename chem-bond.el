;;; chem-bond.el --- Geometry helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2025 Giovanni Crisalfi

;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Geometry helpers shared by ingestion and exporters.

;;; Code:

(require 'cl-lib)
(require 'chem-ir)

(defun chem--rad-to-deg (radians)
  "Convert RADIANS to degrees."
  (* radians (/ 180.0 float-pi)))

(defun chem--canonical-stereo (stereo start end)
  "Return STEREO relative to the canonical (lower-index) bond orientation.
START and END are the original Indigo source and destination indices."
  (if (<= start end)
      (or stereo :none)
    (pcase (or stereo :none)
      ((or :up :down)
       (if (eq stereo :up) :down :up))
      (other other))))

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

(provide 'chem-bond)
;;; chem-bond.el ends here
