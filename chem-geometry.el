;;; chem-geometry.el --- Geometry helpers -*- lexical-binding: t; -*-

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

(defun chem-flip-stereo (stereo)
  "Return STEREO with up/down swapped."
  (pcase stereo
    (:up :down)
    (:down :up)
    (_ stereo)))

(defun chem-stereo-other-positions (atoms center wedge-neighbor)
  "Return three 2D positions for substituents at CENTER.
Exclude WEDGE-NEIGHBOR.  Explicit neighbors are sorted by atom index; an
implicit hydrogen is synthesized when only two explicit neighbors remain."
  (let* ((atom (gethash center atoms))
         (others (sort (delq wedge-neighbor
                             (copy-sequence (chem-atom-neighbors atom)))
                       #'<))
         (positions (mapcar (lambda (idx)
                              (let ((other (gethash idx atoms)))
                                (cons (chem-atom-x other)
                                      (chem-atom-y other))))
                            others)))
    (pcase (length positions)
      (3 positions)
      (2
       (let* ((wedge-atom (gethash wedge-neighbor atoms))
              (p0 (nth 0 positions))
              (p1 (nth 1 positions))
              (cx (/ (+ (chem-atom-x wedge-atom) (car p0) (car p1)) 3.0))
              (cy (/ (+ (chem-atom-y wedge-atom) (cdr p0) (cdr p1)) 3.0))
              (hx (- (* 2.0 (chem-atom-x atom)) cx))
              (hy (- (* 2.0 (chem-atom-y atom)) cy)))
         (append positions (list (cons hx hy)))))
      (_ nil))))

(defun chem-signed-area (p0 p1 p2)
  "Return the signed area of triangle P0 P1 P2."
  (- (* (- (car p1) (car p0)) (- (cdr p2) (cdr p0)))
     (* (- (car p2) (car p0)) (- (cdr p1) (cdr p0)))))

(defun chem--stereocenter-atom-position (atoms idx)
  "Return the (X . Y) position of atom IDX in ATOMS."
  (let ((atom (gethash idx atoms)))
    (cons (chem-atom-x atom) (chem-atom-y atom))))

(defun chem-stereocenter-other-positions (atoms center wedge-neighbor)
  "Return three positions in CENTER's pyramid order.
Exclude WEDGE-NEIGHBOR.  Preserve the pyramid order and substitute a
synthesized position for an implicit-hydrogen slot (`-1')."
  (let* ((atom (gethash center atoms))
         (stereocenter (chem-atom-stereocenter atom))
         (pyramid (chem-stereocenter-pyramid stereocenter))
         (indices (cl-remove wedge-neighbor pyramid :test #'=)))
    (when (= 3 (length indices))
      (let* ((explicit (cl-remove -1 indices :test #'=))
             (positions (mapcar (lambda (idx)
                                  (chem--stereocenter-atom-position atoms idx))
                                explicit)))
        (pcase (length explicit)
          (3 positions)
          (2
           (let* ((wedge-atom (gethash wedge-neighbor atoms))
                  (p0 (nth 0 positions))
                  (p1 (nth 1 positions))
                  (cx (/ (+ (chem-atom-x wedge-atom) (car p0) (car p1)) 3.0))
                  (cy (/ (+ (chem-atom-y wedge-atom) (cdr p0) (cdr p1)) 3.0))
                  (h (cons (- (* 2.0 (chem-atom-x atom)) cx)
                           (- (* 2.0 (chem-atom-y atom)) cy))))
             (mapcar (lambda (idx)
                       (if (= idx -1)
                           h
                         (chem--stereocenter-atom-position atoms idx)))
                     indices)))
          (_ nil))))))

(defun chem-stereocenter-sign (atoms center edge)
  "Return the configuration sign for stereocenter CENTER using EDGE."
  (let* ((wedge-neighbor (chem-edge-other-atom edge center))
         (direction (chem-edge-stereo-directed edge center))
         (positions (chem-stereocenter-other-positions
                     atoms center wedge-neighbor)))
    (when (and positions (memq direction '(:up :down)))
      (let ((sa (chem-signed-area (nth 0 positions)
                                  (nth 1 positions)
                                  (nth 2 positions))))
        (unless (zerop sa)
          (* (if (eq direction :up) 1 -1)
             (if (> sa 0) 1 -1)))))))

(defun chem-stereocenter-wedge-up-p (atoms center edge sign)
  "Return non-nil when EDGE renders up for CENTER given configuration SIGN."
  (when (and sign (/= sign 0))
    (let* ((wedge-neighbor (chem-edge-other-atom edge center))
           (positions (chem-stereocenter-other-positions
                       atoms center wedge-neighbor)))
      (when positions
        (let ((sa (chem-signed-area (nth 0 positions)
                                    (nth 1 positions)
                                    (nth 2 positions))))
          (> (* sign (if (> sa 0) 1 -1)) 0))))))

(defun chem-stereo-sign (atoms edge)
  "Return the configuration sign for wedge EDGE, or nil."
  (let* ((center (chem-edge-stereo-center edge))
         (a (chem-edge-a edge))
         (b (chem-edge-b edge)))
    (when (and center (memq (chem-edge-stereo edge) '(:up :down)))
      (let* ((wedge-neighbor (if (= center a) b a))
             (raw (if (= center a)
                      (chem-edge-stereo edge)
                    (chem-flip-stereo (chem-edge-stereo edge))))
             (positions (chem-stereo-other-positions
                         atoms center wedge-neighbor)))
        (when positions
          (let ((sa (chem-signed-area (nth 0 positions)
                                      (nth 1 positions)
                                      (nth 2 positions))))
            (unless (zerop sa)
              (* (if (eq raw :up) 1 -1)
                 (if (> sa 0) 1 -1)))))))))

(defun chem-stereo-wedge-up-p (atoms edge sign)
  "Return non-nil when EDGE's wedge renders up given configuration SIGN."
  (let* ((center (chem-edge-stereo-center edge))
         (a (chem-edge-a edge))
         (b (chem-edge-b edge)))
    (when (and center sign (/= sign 0))
      (let* ((wedge-neighbor (if (= center a) b a))
             (positions (chem-stereo-other-positions
                         atoms center wedge-neighbor)))
        (when positions
          (let ((sa (chem-signed-area (nth 0 positions)
                                      (nth 1 positions)
                                      (nth 2 positions))))
            (> (* sign (if (> sa 0) 1 -1)) 0)))))))

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

(provide 'chem-geometry)
;;; chem-geometry.el ends here
