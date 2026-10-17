;;; chem-ir.el --- Drawing intermediate representation -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2025 Giovanni Crisalfi

;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The drawing IR shared by all exporters.  It holds the molecular graph, the
;; drawing tree, and the geometry needed to render a structure.
;;
;; `chem-edge' is the canonical chemical graph: one record per real or link
;; edge, with a stable id and a canonical `a' -> `b' orientation.  The
;; `chem-bond' records in `chem-drawing-bonds' are a transitional directed view
;; used by the current chemfig pipeline; they carry the old display vocabulary
;; until the renderer consumes the canonical graph and traversal directly.
;;
;; This is a transitional extraction: the atom and bond structs still carry a
;; few slots used by the chemfig renderer.  They will move into the chemfig
;; exporter as the renderer is separated from the core.
;;
;; This file must not depend on Indigo or on any exporter.

;;; Code:

(require 'cl-lib)

(cl-defstruct (chem-edge (:constructor chem-edge-create))
  id                             ; Stable integer id within the drawing
  a b                            ; Canonical atom indices, with a < b
  order                          ; :single :double :triple :aromatic :query
  stereo                         ; :none :up :down :either :cis :trans
                                 ; relative to a -> b
  link-p                         ; Invisible fragment connector
  cross-p                        ; Draw over crossing bonds
  marker                         ; Marker string
  decoration)                    ; :plain or :fancy

(cl-defstruct (chem-atom (:constructor chem-atom-create))
  options                        ; Option plist (shared with the drawing)
  idx                            ; Integer index (0-based)
  x y                            ; 2D coordinates
  element                        ; Element symbol string ("C", "N", ...)
  hydrogens                      ; Implicit hydrogen count
  charge                         ; Integer formal charge
  radical                        ; Number of radical electrons
  neighbors                      ; List of neighbor atom indices
  bond-angles                    ; Angles of all attached bonds
  label                          ; Display label, or nil for a hidden carbon
  number                         ; 1-based number when numbering is requested
  marker)                        ; Marker string

(cl-defstruct (chem-bond (:constructor chem-bond-create))
  options                        ; Option plist
  kind                           ; `bond' or `aromatic-ring'
  edge-id                        ; Canonical `chem-edge' id, or nil for pseudo bonds
  start-atom end-atom            ; Atom structs (nil for aromatic ring)
  bond-type                      ; Bond type / display type
  descendants                    ; List of child bonds
  length angle                   ; Geometry
  marker                         ; Marker string
  decoration                     ; :plain or :fancy
  cross-p cross-last-p           ; Cross-bond drawing flags
  to-phantom is-trunk            ; Flags
  parent                         ; Parent bond (nil for root)
  clockwise                      ; Transitional ring double-bond orientation
  parent-angle                   ; Transitional aromatic ring parent angle
  radius)                        ; Transitional aromatic ring radius

(cl-defstruct (chem-node (:constructor chem-node-create))
  bond                           ; Directed chem-bond, or nil for the root
  from to                        ; Atom indices; FROM is nil for the root
  parent
  children
  closure-p                      ; Target atom already visited
  phantom-p                      ; Render the target atom as a phantom
  trunk-p
  cross-p
  cross-last-p)

(cl-defstruct (chem-ring (:constructor chem-ring-create))
  id                             ; Stable integer within the drawing
  atoms                          ; Ordered atom indices around the cycle
  bonds)                         ; Ordered canonical edge ids following the cycle

(cl-defstruct (chem-drawing (:constructor chem-drawing-create))
  options
  toolkit                        ; Transitional Indigo handle
  atoms                          ; hash: idx -> atom
  bonds                          ; Transitional hash: (start . end) -> directed bond
  edges                          ; hash: edge id -> canonical edge
  edge-lookup                    ; hash: canonical (a . b) -> edge
  adjacency                      ; hash: atom idx -> ordered (edge-id . neighbor-idx)
  rings                          ; list of chem-ring, stable order
  atom-pairs                     ; list of (start . end)
  seen-atoms                     ; hash: idx -> t
  seen-bonds                     ; hash: (start . end) -> t
  entry-atom exit-atom
  exit-bond                      ; Transitional directed exit bond
  exit-node                      ; Traversal exit node
  root                           ; Traversal root node
  bond-scale)

(defun chem-edge-at (drawing id)
  "Return DRAWING's canonical edge with ID, or nil."
  (and id (gethash id (chem-drawing-edges drawing))))

(defun chem-get-edge (drawing a b)
  "Return the canonical edge between atom indices A and B, or nil."
  (gethash (if (< a b) (cons a b) (cons b a))
           (chem-drawing-edge-lookup drawing)))

(defun chem-edge-other-atom (edge idx)
  "Return the atom index at the other end of EDGE from IDX."
  (if (= idx (chem-edge-a edge))
      (chem-edge-b edge)
    (chem-edge-a edge)))

(defun chem-edge-stereo-directed (edge from)
  "Return EDGE's stereo relative to FROM.
Wedge/hash stereo is inverted when traversing from the canonical `b' end.
`:cis' and `:trans' are orientation-independent."
  (let ((stereo (chem-edge-stereo edge)))
    (if (= from (chem-edge-a edge))
        stereo
      (pcase stereo
        ((or :up :down)
         (if (eq stereo :up) :down :up))
        (_ stereo)))))

(defun chem-atom-adjacency (drawing idx)
  "Return DRAWING's ordered adjacency for atom IDX.
Each entry is (EDGE-ID . NEIGHBOR-IDX)."
  (gethash idx (chem-drawing-adjacency drawing)))

(defun chem-edges (drawing)
  "Return DRAWING's canonical edges ordered by id."
  (let (edges)
    (maphash (lambda (_ edge) (push edge edges))
             (chem-drawing-edges drawing))
    (sort edges (lambda (a b) (< (chem-edge-id a) (chem-edge-id b))))))

(defun chem-visible-edges (drawing)
  "Return DRAWING's canonical edges that should be drawn.
Invisible fragment links are excluded."
  (cl-remove-if #'chem-edge-link-p (chem-edges drawing)))

;;; Coordinate transforms
;;
;; These operate on atom coordinates only.  Derived bond geometry is refreshed
;; separately by `chem-bond-refresh-geometry' after a transform.

(defun chem-rotate (drawing degrees)
  "Rotate DRAWING's coordinates counterclockwise by DEGREES."
  (let* ((theta (* degrees (/ float-pi 180.0)))
         (cos-theta (cos theta))
         (sin-theta (sin theta)))
    (maphash
     (lambda (_ atom)
       (let ((x (chem-atom-x atom))
             (y (chem-atom-y atom)))
         (setf (chem-atom-x atom) (- (* x cos-theta) (* y sin-theta)))
         (setf (chem-atom-y atom) (+ (* x sin-theta) (* y cos-theta)))))
     (chem-drawing-atoms drawing))
    drawing))

(defun chem-flip-horizontal (drawing)
  "Mirror DRAWING's coordinates across the vertical axis."
  (maphash (lambda (_ atom)
             (setf (chem-atom-x atom) (- (chem-atom-x atom))))
           (chem-drawing-atoms drawing))
  drawing)

(defun chem-flip-vertical (drawing)
  "Mirror DRAWING's coordinates across the horizontal axis."
  (maphash (lambda (_ atom)
             (setf (chem-atom-y atom) (- (chem-atom-y atom))))
           (chem-drawing-atoms drawing))
  drawing)

(defun chem-scale (drawing factor)
  "Scale DRAWING's coordinates by FACTOR."
  (maphash (lambda (_ atom)
             (setf (chem-atom-x atom) (* factor (chem-atom-x atom)))
             (setf (chem-atom-y atom) (* factor (chem-atom-y atom))))
           (chem-drawing-atoms drawing))
  drawing)

(defun chem-tree-nodes (drawing)
  "Return DRAWING's traversal nodes in pre-order, excluding the root."
  (let ((nodes nil))
    (cl-labels ((recurse (node)
                  (dolist (child (chem-node-children node))
                    (push child nodes)
                    (recurse child))))
      (when (chem-drawing-root drawing)
        (recurse (chem-drawing-root drawing))))
    (nreverse nodes)))

(defun chem-tree-bonds (drawing)
  "Return all directed bonds in DRAWING's traversal, excluding the root."
  (let (bonds)
    (dolist (node (chem-tree-nodes drawing))
      (when-let* ((bond (chem-node-bond node)))
        (push bond bonds)))
    (nreverse bonds)))

(defun chem-tree-node-for-bond (drawing bond)
  "Return the traversal node whose bond is BOND, or nil."
  (cl-find bond (chem-tree-nodes drawing)
           :key #'chem-node-bond :test #'eq))

;;; Rings

(defun chem-ring-at (drawing id)
  "Return DRAWING's ring with ID, or nil."
  (cl-find id (chem-drawing-rings drawing)
           :key #'chem-ring-id :test #'=))

(defun chem-edge-length (drawing edge)
  "Return the length of EDGE from atom coordinates."
  (let* ((atoms (chem-drawing-atoms drawing))
         (a (gethash (chem-edge-a edge) atoms))
         (b (gethash (chem-edge-b edge) atoms))
         (dx (- (chem-atom-x b) (chem-atom-x a)))
         (dy (- (chem-atom-y b) (chem-atom-y a))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun chem-ring-center (drawing ring)
  "Return the centroid of RING as (X Y)."
  (let ((atoms (chem-drawing-atoms drawing))
        (indices (chem-ring-atoms ring))
        (sum-x 0.0)
        (sum-y 0.0))
    (dolist (idx indices)
      (let ((atom (gethash idx atoms)))
        (setq sum-x (+ sum-x (chem-atom-x atom)))
        (setq sum-y (+ sum-y (chem-atom-y atom)))))
    (list (/ sum-x (float (length indices)))
          (/ sum-y (float (length indices))))))

(defun chem-ring-vertex-radii (drawing ring)
  "Return the center-to-vertex distances of RING."
  (let ((center (chem-ring-center drawing ring))
        (atoms (chem-drawing-atoms drawing))
        radii)
    (dolist (idx (chem-ring-atoms ring))
      (let ((atom (gethash idx atoms)))
        (push (sqrt (+ (expt (- (chem-atom-x atom) (nth 0 center)) 2)
                        (expt (- (chem-atom-y atom) (nth 1 center)) 2)))
              radii)))
    (nreverse radii)))

(defun chem-ring-circumradius (drawing ring)
  "Return the mean center-to-vertex distance of RING."
  (let ((radii (chem-ring-vertex-radii drawing ring)))
    (/ (apply #'+ radii) (float (length radii)))))

(defun chem-ring-apothem (drawing ring)
  "Return the mean center-to-edge distance of RING."
  (let ((center (chem-ring-center drawing ring))
        (atoms (chem-drawing-atoms drawing))
        (sum 0.0)
        (count 0))
    (dolist (edge-id (chem-ring-bonds ring))
      (let* ((edge (chem-edge-at drawing edge-id))
             (a (gethash (chem-edge-a edge) atoms))
             (b (gethash (chem-edge-b edge) atoms))
             (mid-x (/ (+ (chem-atom-x a) (chem-atom-x b)) 2.0))
             (mid-y (/ (+ (chem-atom-y a) (chem-atom-y b)) 2.0)))
        (setq sum (+ sum (sqrt (+ (expt (- mid-x (nth 0 center)) 2)
                                   (expt (- mid-y (nth 1 center)) 2)))))
        (setq count (1+ count))))
    (/ sum (float count))))

(defun chem-ring-regular-p (drawing ring &optional tolerance)
  "Return non-nil when RING is close to a regular polygon.
TOLERANCE defaults to 1.15, matching SmilesDrawer's radius-ratio check."
  (let* ((tolerance (or tolerance 1.15))
         (radii (chem-ring-vertex-radii drawing ring))
         (min-r (apply #'min radii))
         (max-r (apply #'max radii)))
    (and (> min-r 1e-9)
         (< (/ max-r min-r) tolerance))))

(defun chem-ring-symmetric-p (drawing ring &optional tolerance)
  "Return non-nil when RING's radii and bond lengths are uniform.
TOLERANCE defaults to 0.05, matching the current chemfig symmetry check."
  (let* ((tolerance (or tolerance 0.05))
         (radii (chem-ring-vertex-radii drawing ring))
         (lengths (mapcar (lambda (edge-id)
                            (chem-edge-length drawing (chem-edge-at drawing edge-id)))
                          (chem-ring-bonds ring)))
         (r-max (apply #'max radii))
         (l-max (apply #'max lengths)))
    (and (> r-max 1e-9)
         (> l-max 1e-9)
         (<= (/ (- r-max (apply #'min radii)) r-max) tolerance)
         (<= (/ (- l-max (apply #'min lengths)) l-max) tolerance))))

(defun chem-rings-for-atom (drawing idx)
  "Return the rings containing atom IDX."
  (cl-remove-if-not (lambda (ring) (memq idx (chem-ring-atoms ring)))
                    (chem-drawing-rings drawing)))

(defun chem-rings-for-edge (drawing edge-id)
  "Return the rings containing the canonical EDGE-ID."
  (cl-remove-if-not (lambda (ring) (memq edge-id (chem-ring-bonds ring)))
                    (chem-drawing-rings drawing)))

(defun chem-rings-for-bond (drawing bond)
  "Return the rings containing directed BOND."
  (chem-rings-for-edge drawing (chem-bond-edge-id bond)))

(defun chem-atom-in-ring-p (drawing idx)
  "Return non-nil when atom IDX belongs to a ring."
  (not (null (chem-rings-for-atom drawing idx))))

(defun chem-bond-in-ring-p (drawing edge-id)
  "Return non-nil when canonical EDGE-ID belongs to a ring."
  (not (null (chem-rings-for-edge drawing edge-id))))

(defun chem-ring-side (drawing ring from to)
  "Return `:left' or `:right' for RING's center relative to FROM -> TO.
Return nil when the three points are collinear."
  (let* ((atoms (chem-drawing-atoms drawing))
         (a (gethash from atoms))
         (b (gethash to atoms))
         (center (chem-ring-center drawing ring))
         (dx (- (chem-atom-x b) (chem-atom-x a)))
         (dy (- (chem-atom-y b) (chem-atom-y a)))
         (cx (- (nth 0 center) (chem-atom-x a)))
         (cy (- (nth 1 center) (chem-atom-y a)))
         (cross (- (* dx cy) (* dy cx))))
    (cond ((> cross 1e-9) :left)
          ((< cross -1e-9) :right)
          (t nil))))

(defun chem-apply-drawing-options (drawing options)
  "Apply common drawing OPTIONS to DRAWING.
Set atom labels and numbers, and bond/edge decorations in the IR.
Exporters render these attributes; they must not re-read the options."
  (let ((show-carbons (plist-get options :show-carbons))
        (show-methyls (plist-get options :show-methyls))
        (atom-numbers (plist-get options :atom-numbers))
        (fancy-bonds (plist-get options :fancy-bonds)))
    (maphash
     (lambda (_ atom)
       (let* ((element (chem-atom-element atom))
              (hydrogens (or (chem-atom-hydrogens atom) 0))
              (charged (or (/= (or (chem-atom-charge atom) 0) 0)
                           (/= (or (chem-atom-radical atom) 0) 0)))
              (label (cond
                      ((not (equal element "C")) element)
                      (charged element)
                      (show-carbons element)
                      ((and show-methyls (>= hydrogens 3)) element)
                      (t nil))))
         (setf (chem-atom-label atom) label)
         (setf (chem-atom-number atom)
               (when atom-numbers (1+ (chem-atom-idx atom))))))
     (chem-drawing-atoms drawing))
    (maphash
     (lambda (_ bond)
       (setf (chem-bond-decoration bond)
             (if (and fancy-bonds
                      (memq (chem-bond-bond-type bond) '(double triple)))
                 'fancy
               'plain)))
     (chem-drawing-bonds drawing))
    ;; Mirror the resolved directed decoration onto the canonical edge.
    ;; The canonical direction is always present in the transitional cache.
    (maphash
     (lambda (_ edge)
       (let ((bond (gethash (cons (chem-edge-a edge) (chem-edge-b edge))
                            (chem-drawing-bonds drawing))))
         (setf (chem-edge-decoration edge)
               (if bond (chem-bond-decoration bond) 'plain))))
     (chem-drawing-edges drawing)))
  drawing)

(provide 'chem-ir)
;;; chem-ir.el ends here
