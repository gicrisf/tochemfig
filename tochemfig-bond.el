;;; tochemfig-bond.el --- Bond representation for chemfig generation -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2025 Giovanni Crisalfi

;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Translation of mol2chemfigPy3's `bond.py'.  A bond is also a node in the
;; molecule tree: it knows its parent and descendants.  Rendering walks that
;; tree and asks each bond to render itself followed by its end atom.

;;; Code:

(require 'cl-lib)
(require 'tochemfig-mappings)
(require 'tochemfig-atom)

;; Indigo stereo constants.
(defconst tochemfig--stereo-up 5)
(defconst tochemfig--stereo-down 6)
(defconst tochemfig--stereo-either 4)

(defconst tochemfig--bond-mapping
  '((1 . single) (2 . double) (3 . triple) (4 . either)
    (5 . upto) (6 . downto))
  "Raw Indigo bond type/stereo to mol2chemfig bond type.
Note that order 4 (aromatic) maps to `either' (wavy), mirroring the reference
implementation where the dict literal overrides the aromatic entry.")

(defconst tochemfig--aromatic-ring-scale 1.5)

(cl-defstruct (tochemfig-bond (:constructor tochemfig-bond-create))
  options                        ; Option plist
  kind                           ; `bond', `dummy-first' or `aromatic-ring'
  start-atom end-atom            ; Atom structs (nil for aromatic ring)
  tikz-styles                    ; List of style names
  tikz-values                    ; Alist of symbol -> number
  bond-type                      ; Bond type symbol
  descendants                    ; List of child bonds
  length angle                   ; Computed geometry
  marker                         ; Marker string
  is-last to-phantom is-trunk    ; Flags
  parent                         ; Parent bond (nil for root)
  clockwise                      ; Ring double bond orientation
  parent-angle                   ; Parent angle (aromatic ring bond)
  radius)                        ; Circle radius (aromatic ring bond)

(defun tochemfig--rad-to-deg (radians)
  "Convert RADIANS to degrees."
  (* radians (/ 180.0 float-pi)))

(defun tochemfig--compare-positions (x1 y1 x2 y2)
  "Return (distance angle) between (X1,Y1) and (X2,Y2).
The angle follows mol2chemfig's convention and may be negative."
  (let* ((xd (- x2 x1))
         (yd (- y2 y1))
         (length (sqrt (+ (* xd xd) (* yd yd)))))
    (list length
          (cond
           ((= xd 0) (if (< yd 0) 270 90))
           (t (let ((raw (tochemfig--rad-to-deg (atan (abs (/ yd xd))))))
                (cond ((>= yd 0) (if (> xd 0) raw (- 180 raw)))
                      (t (if (> xd 0) (- raw) (+ 180 raw))))))))))

(defun tochemfig-bond-create-from (options start-atom end-atom bond-order stereo)
  "Create a bond from START-ATOM to END-ATOM.
BOND-ORDER is an Indigo keyword; STEREO an Indigo stereo keyword."
  (let* ((stereo-code (cond ((eq stereo :up) tochemfig--stereo-up)
                            ((eq stereo :down) tochemfig--stereo-down)
                            ((eq stereo :either) tochemfig--stereo-either)
                            (t 0)))
         (flip (plist-get options :flip))
         (flop (plist-get options :flop)))
    (when (and (memq stereo-code (list tochemfig--stereo-up tochemfig--stereo-down))
               (not (eq flip flop)))
      (setq stereo-code (- (+ tochemfig--stereo-up tochemfig--stereo-down) stereo-code)))
    (let* ((bond-type
            (if (memq stereo-code (list tochemfig--stereo-up tochemfig--stereo-down
                                        tochemfig--stereo-either))
                (cdr (assq stereo-code tochemfig--bond-mapping))
              (let ((raw (pcase bond-order
                           (:single 1) (:double 2) (:triple 3) (:aromatic 4) (_ 0))))
                (or (cdr (assq raw tochemfig--bond-mapping)) 'query))))
           (dims (tochemfig--compare-positions
                  (tochemfig-atom-x start-atom) (tochemfig-atom-y start-atom)
                  (tochemfig-atom-x end-atom) (tochemfig-atom-y end-atom)))
           (marker-opt (plist-get options :markers))
           (ids (sort (list (1+ (tochemfig-atom-idx start-atom))
                            (1+ (tochemfig-atom-idx end-atom))) #'<)))
      (tochemfig-bond-create
       :options options
       :kind 'bond
       :start-atom start-atom
       :end-atom end-atom
       :tikz-styles nil
       :tikz-values nil
       :bond-type bond-type
       :descendants nil
       :length (nth 0 dims)
       :angle (+ (nth 1 dims) (or (plist-get options :angle) 0.0))
       :marker (if marker-opt (format "%s%d-%d" marker-opt (nth 0 ids) (nth 1 ids)) "")
       :is-last nil
       :to-phantom nil
       :is-trunk nil
       :parent nil
       :clockwise 0))))

(defun tochemfig-bond-invert (bond)
  "Return a reversed copy of BOND."
  (let ((c (copy-tochemfig-bond bond)))
    (setf (tochemfig-bond-start-atom c) (tochemfig-bond-end-atom bond))
    (setf (tochemfig-bond-end-atom c) (tochemfig-bond-start-atom bond))
    (setf (tochemfig-bond-angle c) (mod (+ (tochemfig-bond-angle c) 180) 360))
    (pcase (tochemfig-bond-bond-type c)
      ('upto (setf (tochemfig-bond-bond-type c) 'upfrom))
      ('downto (setf (tochemfig-bond-bond-type c) 'downfrom)))
    c))

(defun tochemfig-bond-set-link (bond)
  "Make BOND an invisible link."
  (setf (tochemfig-bond-bond-type bond) 'link)
  (setf (tochemfig-bond-tikz-styles bond) nil)
  (setf (tochemfig-bond-tikz-values bond) nil)
  (setf (tochemfig-bond-marker bond) ""))

(defun tochemfig-bond-is-clockwise (bond center-x center-y)
  "Determine the orientation of BOND relative to (CENTER-X, CENTER-Y)."
  (when (= (tochemfig-bond-clockwise bond) 0)
    (let* ((dims (tochemfig--compare-positions
                  (tochemfig-atom-x (tochemfig-bond-end-atom bond))
                  (tochemfig-atom-y (tochemfig-bond-end-atom bond))
                  center-x center-y))
           (center-angle (+ (nth 1 dims) (or (plist-get (tochemfig-bond-options bond) :angle) 0.0)))
           (center-kink (mod (- center-angle (tochemfig-bond-angle bond)) 360)))
      (setf (tochemfig-bond-clockwise bond) (if (> center-kink 180) 1 -1)))))

;;; Fancy double / triple bond geometry

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

(defun tochemfig-bond-fancy-double (bond)
  "Return (side start end) for a fancy double bond, or nil."
  (let* ((start-atom (tochemfig-bond-start-atom bond))
         (end-atom (tochemfig-bond-end-atom bond))
         (start-angles (tochemfig-bond-upstream-angles bond))
         (end-angles (tochemfig-bond-downstream-angles bond))
         (s-left (cdr (assq 'left start-angles)))
         (s-right (cdr (assq 'right start-angles)))
         (e-left (cdr (assq 'left end-angles)))
         (e-right (cdr (assq 'right end-angles)))
         (clockwise (tochemfig-bond-clockwise bond))
         (s-exp (tochemfig-atom-explicit start-atom))
         (e-exp (tochemfig-atom-explicit end-atom)))
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

(defun tochemfig-bond-fancy-triple (bond)
  "Return (start end) for a fancy triple bond."
  (let* ((start-atom (tochemfig-bond-start-atom bond))
         (end-atom (tochemfig-bond-end-atom bond))
         (start (if (tochemfig-atom-explicit start-atom)
                    0
                  (let* ((vals (mapcar #'cdr (tochemfig-bond-upstream-angles bond)))
                         (non-nil (delq nil vals)))
                    (if (car vals) (tochemfig--cotan100 (* 0.5 (apply #'min non-nil))) 0))))
         (end (if (tochemfig-atom-explicit end-atom)
                  0
                (let* ((vals (mapcar #'cdr (tochemfig-bond-downstream-angles bond)))
                       (non-nil (delq nil vals)))
                  (if (car vals) (tochemfig--cotan100 (* 0.5 (apply #'min non-nil))) 0)))))
    (list start end)))

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

(provide 'tochemfig-bond)
;;; tochemfig-bond.el ends here
