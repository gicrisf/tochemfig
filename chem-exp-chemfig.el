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
(require 'chem-geometry)
(require 'chem-exp-chemfig-mappings)

;;; Renderer state

(cl-defstruct (chem-exp-chemfig-atom-state (:constructor chem-exp-chemfig-atom-state-create))
  string-pos phantom phantom-pos
  first-quadrant second-quadrant charge-angle
  explicit)

(cl-defstruct (chem-exp-chemfig-node-state (:constructor chem-exp-chemfig-node-state-create))
  tikz-styles tikz-values
  is-last display-type)

(defconst chem-exp-chemfig--aromatic-ring-scale 1.5
  "Scale factor for the aromatic-circle radius.")

(cl-defstruct (chem-exp-chemfig-circle (:constructor chem-exp-chemfig-circle-create))
  angle parent-angle length radius)

(cl-defstruct (chem-exp-chemfig-state (:constructor chem-exp-chemfig-state-create))
  atoms nodes options
  clockwise                      ; hash: bond -> -1/1/0
  display-type                   ; hash: bond -> display type override
  extra-angles                   ; hash: atom idx -> extra label angles
  circle-children)               ; hash: anchor node -> synthetic circle nodes

(defun chem-exp-chemfig--make-state (options)
  "Create an empty renderer state with exporter OPTIONS."
  (chem-exp-chemfig-state-create
   :atoms (make-hash-table :test 'eql)
   :nodes (make-hash-table :test 'eq)
   :options options
   :clockwise (make-hash-table :test 'eq)
   :display-type (make-hash-table :test 'eq)
   :extra-angles (make-hash-table :test 'eql)
   :circle-children (make-hash-table :test 'eq)))

(defun chem-exp-chemfig--atom-bond-angles (state atom)
  "Return ATOM's incident bond angles plus ring-center label angles."
  (append (chem-atom-bond-angles atom)
          (gethash (chem-atom-idx atom)
                   (chem-exp-chemfig-state-extra-angles state))))

(defun chem-exp-chemfig--node-edge (drawing node)
  "Return NODE's canonical edge, or nil for synthetic connectors."
  (let ((edge-id (chem-node-edge-id node)))
    (and edge-id (chem-edge-at drawing edge-id))))

(defun chem-exp-chemfig--node-geometry (drawing node)
  "Return (length angle) for NODE's directed atom segment."
  (let* ((atoms (chem-drawing-atoms drawing))
         (a (gethash (chem-node-from node) atoms))
         (b (gethash (chem-node-to node) atoms)))
    (chem--compare-positions (chem-atom-x a) (chem-atom-y a)
                             (chem-atom-x b) (chem-atom-y b))))

(defun chem-exp-chemfig--node-angle (drawing node)
  "Return NODE's directed angle."
  (nth 1 (chem-exp-chemfig--node-geometry drawing node)))

(defun chem-exp-chemfig--node-length (drawing node)
  "Return NODE's directed length."
  (nth 0 (chem-exp-chemfig--node-geometry drawing node)))

(defun chem-exp-chemfig--parent-angle (drawing node)
  "Return NODE's incoming parent angle, or nil."
  (let ((parent (chem-node-parent node)))
    (when (and parent (chem-node-from parent))
      (chem-exp-chemfig--node-angle drawing parent))))

(defun chem-exp-chemfig--order-display-type (edge)
  "Return EDGE's display type from its bond order."
  (pcase (chem-edge-order edge)
    (:single 'single)
    (:double 'double)
    (:triple 'triple)
    (:aromatic 'either)
    (_ 'query)))

(defun chem-exp-chemfig--node-display-type (state drawing node)
  "Return NODE's chemfig display type."
  (let ((edge-id (chem-node-edge-id node)))
    (cond
     ((chem-node-link-p node) 'link)
     ((null edge-id) 'link)
     (t
      (let* ((edge (chem-edge-at drawing edge-id))
             (atoms (chem-drawing-atoms drawing)))
        (cond
         ((chem-edge-link-p edge) 'link)
         ((gethash node (chem-exp-chemfig-state-display-type state)))
         (t
          (let* ((stereo (chem-edge-stereo edge))
                 (center (chem-edge-stereo-center edge))
                 (stereocenter
                  (and center (chem-atom-stereocenter (gethash center atoms))))
                 (configuration
                  (and stereocenter
                       (chem-stereocenter-configuration stereocenter))))
            (cond
             ((eq stereo :either) 'either)
             ((and center configuration (memq stereo '(:up :down)))
              (let ((wedge-up-p
                     (chem-stereocenter-wedge-up-p
                      atoms center edge configuration)))
                (if (= center (chem-node-to node))
                    (if wedge-up-p 'upfrom 'downfrom)
                  (if wedge-up-p 'upto 'downto))))
             ((memq stereo '(:up :down))
              (pcase (chem-edge-stereo-directed edge (chem-node-from node))
                (:up 'upto)
                (:down 'downto)
                (_ (chem-exp-chemfig--order-display-type edge))))
             (t (chem-exp-chemfig--order-display-type edge)))))))))))

(defun chem-exp-chemfig--node-clockwise (state node)
  "Return NODE's derived ring orientation, or 0."
  (or (gethash node (chem-exp-chemfig-state-clockwise state)) 0))

(defun chem-exp-chemfig--node-marker (drawing node)
  "Return NODE's marker string."
  (let ((edge (chem-exp-chemfig--node-edge drawing node)))
    (if (and edge (not (chem-node-link-p node)))
        (chem-edge-marker edge)
      "")))

(defun chem-exp-chemfig--node-decoration (drawing node)
  "Return NODE's decoration."
  (let ((edge (chem-exp-chemfig--node-edge drawing node)))
    (if (and edge (not (chem-node-link-p node)))
        (chem-edge-decoration edge)
      'plain)))

(defun chem-exp-chemfig--display-children (state node)
  "Return NODE's traversal children plus exporter circle items."
  (append (chem-node-children node)
          (gethash node (chem-exp-chemfig-state-circle-children state))))

(defun chem-exp-chemfig--atom-state (state atom)
  "Return the renderer state for ATOM in STATE."
  (let ((key (chem-atom-idx atom)))
    (or (gethash key (chem-exp-chemfig-state-atoms state))
        (puthash key (chem-exp-chemfig-atom-state-create)
                 (chem-exp-chemfig-state-atoms state)))))

(defun chem-exp-chemfig--node-state (state node)
  "Return the renderer state for NODE in STATE."
  (or (gethash node (chem-exp-chemfig-state-nodes state))
      (puthash node (chem-exp-chemfig-node-state-create)
               (chem-exp-chemfig-state-nodes state))))

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

(defun chem--score-angles (angles choices turf)
  "Return CHOICES sorted by how well they avoid ANGLES."
  (let ((aux nil))
    (dolist (choice choices)
      (let ((score 0))
        (dolist (bond-angle angles)
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
  (let ((atom-state (chem-exp-chemfig--atom-state state atom))
        (angles (chem-exp-chemfig--atom-bond-angles state atom)))
    (if (> (length angles) 0)
        (let ((quadrants (chem--score-angles angles chem--quadrants chem--quadrant-turf)))
          (setf (chem-exp-chemfig-atom-state-first-quadrant atom-state) (nth 0 quadrants))
          (setf (chem-exp-chemfig-atom-state-second-quadrant atom-state) (nth 1 quadrants)))
      (if (member (chem-atom-element atom) chem--hydrogen-lefties)
          (progn (setf (chem-exp-chemfig-atom-state-first-quadrant atom-state) "west")
                 (setf (chem-exp-chemfig-atom-state-second-quadrant atom-state) "east"))
        (setf (chem-exp-chemfig-atom-state-first-quadrant atom-state) "east")
        (setf (chem-exp-chemfig-atom-state-second-quadrant atom-state) "west")))
    (setf (chem-exp-chemfig-atom-state-charge-angle atom-state)
          (nth 0 (chem--score-angles angles chem--charge-positions chem--charge-turf)))))

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

(defun chem-exp-chemfig--adjoining-angles (angle atom inversion-angle)
  "Return the narrowest adjoining angles on ATOM's left and right."
  (let* ((raw (mapcar (lambda (a) (mod (round a) 360))
                      (chem-atom-bond-angles atom)))
         (reference (mod (round (- angle inversion-angle)) 360)))
    (let ((pos (cl-position reference raw :test #'equal)))
      (when pos
        (setq raw (append (cl-subseq raw 0 pos) (cl-subseq raw (1+ pos))))))
    (if (null raw)
        (list nil nil)
      (let* ((angles (sort (mapcar (lambda (a) (mod (- a reference) 360)) raw) #'<)))
        (list (round (car angles)) (round (car (last angles))))))))

(defun chem-exp-chemfig--upstream-angles (drawing node angle)
  "Return the narrowest upstream left and right angles of NODE."
  (let* ((atom (gethash (chem-node-from node) (chem-drawing-atoms drawing)))
         (pair (chem-exp-chemfig--adjoining-angles angle atom 0))
         (first (nth 0 pair))
         (last (nth 1 pair)))
    (when (not (null last)) (setq last (- 360 last)))
    (list (cons 'left first) (cons 'right last))))

(defun chem-exp-chemfig--downstream-angles (drawing node angle)
  "Return the narrowest downstream left and right angles of NODE."
  (let* ((atom (gethash (chem-node-to node) (chem-drawing-atoms drawing)))
         (pair (chem-exp-chemfig--adjoining-angles angle atom 180))
         (first (nth 0 pair))
         (last (nth 1 pair)))
    (when (not (null last)) (setq last (- 360 last)))
    (list (cons 'left last) (cons 'right first))))

(defun chem-exp-chemfig--shorten-stroke (same-angle other-angle)
  "Determine how much to shorten the second stroke of a double bond."
  (if (null same-angle)
      0
    (let ((angle (cond
                  ((<= same-angle 180) (* 0.5 same-angle))
                  ((and (> same-angle 210) (< same-angle 270)) (- same-angle 180))
                  ((and other-angle (> other-angle 210) (< other-angle 270)) (- other-angle 180))
                  (t 90))))
      (chem--cotan100 angle))))

(defun chem-exp-chemfig--fancy-double (drawing node state angle clockwise)
  "Return (side start end) for a fancy double NODE, or nil."
  (let* ((atoms (chem-drawing-atoms drawing))
         (start-atom (gethash (chem-node-from node) atoms))
         (end-atom (gethash (chem-node-to node) atoms))
         (start-angles (chem-exp-chemfig--upstream-angles drawing node angle))
         (end-angles (chem-exp-chemfig--downstream-angles drawing node angle))
         (s-left (cdr (assq 'left start-angles)))
         (s-right (cdr (assq 'right start-angles)))
         (e-left (cdr (assq 'left end-angles)))
         (e-right (cdr (assq 'right end-angles)))
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
                                    ((< (abs (- angle 44.5)) 90) "left")
                                    (t "right"))))))
             (start (if s-exp
                        0
                      (if (equal side "left")
                          (chem-exp-chemfig--shorten-stroke s-left s-right)
                        (chem-exp-chemfig--shorten-stroke s-right s-left))))
             (end (if e-exp
                      0
                    (if (equal side "left")
                        (chem-exp-chemfig--shorten-stroke e-left e-right)
                      (chem-exp-chemfig--shorten-stroke e-right e-left)))))
        (list side start end)))))

(defun chem-exp-chemfig--fancy-triple (drawing node state angle)
  "Return (start end) for a fancy triple NODE using STATE."
  (let* ((atoms (chem-drawing-atoms drawing))
         (start-atom (gethash (chem-node-from node) atoms))
         (end-atom (gethash (chem-node-to node) atoms))
         (start (if (chem-exp-chemfig--atom-explicit-p state start-atom)
                    0
                  (let* ((vals (mapcar #'cdr
                                       (chem-exp-chemfig--upstream-angles
                                        drawing node angle)))
                         (non-nil (delq nil vals)))
                    (if (car vals) (chem--cotan100 (* 0.5 (apply #'min non-nil))) 0))))
         (end (if (chem-exp-chemfig--atom-explicit-p state end-atom)
                  0
                (let* ((vals (mapcar #'cdr
                                     (chem-exp-chemfig--downstream-angles
                                      drawing node angle)))
                       (non-nil (delq nil vals)))
                  (if (car vals) (chem--cotan100 (* 0.5 (apply #'min non-nil))) 0)))))
    (list start end)))

;;; Bond rendering

(defun chem-exp-chemfig--node-to-chemfig (drawing node state)
  "Render NODE (without its atom) to chemfig using STATE."
  (let* ((options (chem-exp-chemfig-state-options state))
         (to-phantom (chem-node-phantom-p node))
         (geometry (chem-exp-chemfig--node-geometry drawing node))
         (angle (nth 1 geometry))
         (length (nth 0 geometry))
         (parent-angle (chem-exp-chemfig--parent-angle drawing node))
         (start-atom (gethash (chem-node-from node) (chem-drawing-atoms drawing)))
         (end-atom (gethash (chem-node-to node) (chem-drawing-atoms drawing)))
         (start-state (chem-exp-chemfig--atom-state state start-atom))
         (end-state (chem-exp-chemfig--atom-state state end-atom))
         (end-string-pos (if to-phantom
                             (chem-exp-chemfig-atom-state-phantom-pos end-state)
                           (chem-exp-chemfig-atom-state-string-pos end-state)))
         (bond-type (chem-exp-chemfig--node-display-type state drawing node))
         (clockwise (chem-exp-chemfig--node-clockwise state node))
         (decoration (chem-exp-chemfig--node-decoration drawing node))
         (marker (chem-exp-chemfig--node-marker drawing node))
         (bond-state (chem-exp-chemfig--node-state state node))
         (display-type (or (chem-exp-chemfig-node-state-display-type bond-state)
                           bond-type))
         (tikz-styles (chem-exp-chemfig-node-state-tikz-styles bond-state))
         (tikz-values (chem-exp-chemfig-node-state-tikz-values bond-state)))
    (when (and (eq decoration 'fancy)
               (memq bond-type '(double triple)))
      (if (eq bond-type 'double)
          (let ((fd (chem-exp-chemfig--fancy-double
                     drawing node state angle clockwise)))
            (when fd
              (cl-pushnew "double" tikz-styles :test #'equal)
              (cl-pushnew (nth 0 fd) tikz-styles :test #'equal)
              (setq tikz-values
                    (append (list (cons 'start (nth 1 fd)) (cons 'end (nth 2 fd)))
                            tikz-values))
              (setq display-type 'decorated)))
        (cl-pushnew "triple" tikz-styles :test #'equal)
        (let ((ft (chem-exp-chemfig--fancy-triple drawing node state angle)))
          (setq tikz-values
                (append (list (cons 'start (nth 0 ft)) (cons 'end (nth 1 ft)))
                        tikz-values))
          (setq display-type 'decorated))))
    (setf (chem-exp-chemfig-node-state-tikz-styles bond-state) tikz-styles)
    (setf (chem-exp-chemfig-node-state-tikz-values bond-state) tikz-values)
    (setf (chem-exp-chemfig-node-state-display-type bond-state) display-type)
    (chem--format-bond options angle parent-angle display-type clockwise
                       (chem-exp-chemfig-node-state-is-last bond-state)
                       length
                       (chem-exp-chemfig-atom-state-string-pos start-state)
                       end-string-pos
                       tikz-styles
                       tikz-values
                       marker)))

(defun chem-exp-chemfig--indent (options level bond-code atom-code comment-code)
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

(defun chem-node-render (drawing node state level)
  "Render NODE and its trailing atom at LEVEL using STATE."
  (if (null (chem-node-from node))
      (let ((atom-render
             (chem-atom-render
              (gethash (chem-node-to node) (chem-drawing-atoms drawing))
              state)))
        (chem-exp-chemfig--indent (chem-exp-chemfig-state-options state) level
                          "" (car atom-render) (cdr atom-render)))
    (let* ((to-phantom (chem-node-phantom-p node))
           (atom (gethash (chem-node-to node) (chem-drawing-atoms drawing)))
           (atom-render (if to-phantom
                            (chem-atom-render-phantom atom state)
                          (chem-atom-render atom state)))
           (bond-code (chem-exp-chemfig--node-to-chemfig drawing node state)))
      (chem-exp-chemfig--indent (chem-exp-chemfig-state-options state) level
                        bond-code (car atom-render) (cdr atom-render)))))

(defun chem-exp-chemfig--circle-render (circle state level)
  "Render CIRCLE at LEVEL using STATE."
  (let* ((options (chem-exp-chemfig-state-options state))
         (rendered (chem--format-aromatic-ring
                    options
                    (chem-exp-chemfig-circle-angle circle)
                    (chem-exp-chemfig-circle-parent-angle circle)
                    (chem-exp-chemfig-circle-length circle)
                    (chem-exp-chemfig-circle-radius circle))))
    (chem-exp-chemfig--indent options level
                      (nth 0 rendered) (nth 1 rendered) (nth 2 rendered))))

;;; Drawing walk

(defun chem--render-branches (drawing state output level items)
  "Append ITEMS rendered as indented branches to OUTPUT.
OUTPUT is the reversed line accumulator; returns the updated accumulator."
  (let ((indent (or (plist-get (chem-exp-chemfig-state-options state) :indent) 4)))
    (dolist (item items output)
      (let ((width (+ (* level indent) chem--bond-code-width)))
        (setq output (cons (concat (make-string (1- width) ?\s) "(") output))
        (setq output (chem--render drawing state output item level))
        (setq output (cons (concat (make-string (1- width) ?\s) ")") output))))))

(defun chem--render (drawing state output item level)
  "Recursively render ITEM into OUTPUT at LEVEL using STATE."
  (if (chem-exp-chemfig-circle-p item)
      (cons (chem-exp-chemfig--circle-render item state level) output)
    (setq output (cons (chem-node-render drawing item state level) output))
    (let ((branches (copy-sequence (chem-exp-chemfig--display-children state item))))
      (cond
       ((eq item (chem-drawing-exit-node drawing))
        (chem--render-branches drawing state output (1+ level) branches))
       ((null branches) output)
       (t
        (let ((trunk-idx (cl-position-if
                          (lambda (branch)
                            (and (chem-node-p branch) (chem-node-trunk-p branch)))
                          branches))
              (first nil))
          (if trunk-idx
              (setq first (nth trunk-idx branches)
                    branches (append (cl-subseq branches 0 trunk-idx)
                                     (cl-subseq branches (1+ trunk-idx))))
            (setq first (car branches)
                  branches (cdr branches)))
          (setq output (chem--render-branches drawing state output (1+ level) branches))
          (chem--render drawing state output first level)))))))

(defun chem--render-molecule (drawing state)
  "Render DRAWING to a list of lines using STATE."
  (nreverse (chem--render drawing state nil (chem-drawing-root drawing) 0)))

;;; Exporter entry point

(defun chem-exp-chemfig--prepare-cross-state (state drawing)
  "Fill STATE with cross-bond styling for DRAWING."
  (dolist (node (chem-tree-nodes drawing))
    (when (chem-node-cross-p node)
      (let* ((angle (chem-exp-chemfig--node-angle drawing node))
             (start-angles (mapcar
                            #'cdr
                            (chem-exp-chemfig--upstream-angles
                             drawing node angle)))
             (end-angles (mapcar
                          #'cdr
                          (chem-exp-chemfig--downstream-angles
                           drawing node angle)))
             (start-angle (apply #'min (delq nil start-angles)))
             (end-angle (apply #'min (delq nil end-angles)))
             (start (max 10 (chem--cotan100 start-angle)))
             (end (max 10 (chem--cotan100 end-angle)))
             (bond-state (chem-exp-chemfig--node-state state node)))
        (setf (chem-exp-chemfig-node-state-tikz-styles bond-state) (list "cross"))
        (setf (chem-exp-chemfig-node-state-tikz-values bond-state)
              (list (cons 'bgstart start) (cons 'bgend end)))
        (setf (chem-exp-chemfig-node-state-is-last bond-state)
              (chem-node-cross-last-p node))))))

(defun chem-exp-chemfig--prepare-rings (state drawing options)
  "Build render-only ring presentation caches for DRAWING."
  (let ((clockwise (chem-exp-chemfig-state-clockwise state))
        (display-type (chem-exp-chemfig-state-display-type state))
        (extra-angles (chem-exp-chemfig-state-extra-angles state))
        (circle-children (chem-exp-chemfig-state-circle-children state))
        (atoms (chem-drawing-atoms drawing))
        (aromatic-circles-p (plist-get options :aromatic-circles)))
    (clrhash clockwise)
    (clrhash display-type)
    (clrhash extra-angles)
    (clrhash circle-children)
    (dolist (ring (sort (copy-sequence (chem-drawing-rings drawing))
                        (lambda (a b)
                          (cond ((and (chem-ring-aromatic-p a)
                                      (not (chem-ring-aromatic-p b)))
                                 t)
                                ((and (not (chem-ring-aromatic-p a))
                                      (chem-ring-aromatic-p b))
                                 nil)
                                (t (> (chem-ring-sequence a)
                                      (chem-ring-sequence b)))))))
      (if (and (chem-ring-aromatic-p ring)
               (chem-ring-symmetric-p drawing ring)
               aromatic-circles-p)
          (let ((edge-ids (chem-ring-bonds ring)))
            (dolist (edge-id edge-ids)
              (when-let* ((node (chem-tree-node-for-edge drawing edge-id)))
                (puthash node 'aromatic display-type)))
            (let* ((anchor-node (chem-tree-node-for-edge
                                 drawing (chem-ring-anchor ring)))
                   (center (chem-ring-center drawing ring))
                   (atom (gethash (chem-node-to anchor-node) atoms))
                   (dims (chem--compare-positions
                          (chem-atom-x atom) (chem-atom-y atom)
                          (nth 0 center) (nth 1 center)))
                   (outer-r (nth 0 dims))
                   (angle (nth 1 dims))
                   (n (length edge-ids))
                   (alpha (- (/ float-pi 2) (/ float-pi n)))
                   (inner-r (* (sin alpha) outer-r))
                   (circle (chem-exp-chemfig-circle-create
                            :angle (mod (chem--num-round angle 1) 360)
                            :parent-angle (chem-exp-chemfig--node-angle
                                           drawing anchor-node)
                            :length (chem--num-round outer-r 2)
                            :radius (chem--num-round
                                     (* chem-exp-chemfig--aromatic-ring-scale
                                        inner-r)
                                     2))))
              (when anchor-node
                (puthash anchor-node
                         (append (gethash anchor-node circle-children)
                                 (list circle))
                         circle-children))
              (dolist (idx (chem-ring-atoms ring))
                (let* ((ring-atom (gethash idx atoms))
                       (dims (chem--compare-positions
                              (chem-atom-x ring-atom) (chem-atom-y ring-atom)
                              (nth 0 center) (nth 1 center))))
                  (puthash idx
                           (append (gethash idx extra-angles)
                                   (list (nth 1 dims)))
                           extra-angles)))))
        (dolist (edge-id (chem-ring-bonds ring))
          (when-let* ((node (chem-tree-node-for-edge drawing edge-id)))
            (when (zerop (or (gethash node clockwise) 0))
              (let* ((from (chem-node-from node))
                     (to (chem-node-to node))
                     (side (chem-ring-side drawing ring from to)))
                (puthash node
                         (pcase side
                           (:left -1)
                           (:right 1)
                           (_ 0))
                         clockwise)))))))))

(defun chem-exp-chemfig-render (drawing options)
  "Render DRAWING as chemfig using OPTIONS."
  (let ((state (chem-exp-chemfig--make-state options)))
    (chem-exp-chemfig--prepare-cross-state state drawing)
    (chem-exp-chemfig--prepare-rings state drawing options)
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
