;;; chem-svg-parity-test.el --- SmilesDrawer SVG parity diagnostics -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Giovanni Crisalfi

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Development diagnostics for the SmilesDrawer reference fixtures.  The
;; fixtures contain SmilesDrawer's post-layout coordinates and SVG primitives.
;; We build a test-only `chem-drawing' from those coordinates, render it with
;; the native SVG exporter, and compare edge geometry and ring circles.
;;
;; The fast shared-coordinate checks are part of `eldev-test-fileset'.  The
;; full report remains a development command for investigating known gaps.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'chem-mode)
(require 'chem-indigo)
(require 'chem-exp-svg)

(defconst chem-svg-parity-test--root
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the parity fixtures.")

(defconst chem-svg-parity-test--fixture-dir
  (expand-file-name "fixtures/smilesdrawer" chem-svg-parity-test--root)
  "Directory containing generated SmilesDrawer fixtures.")

(defun chem-svg-parity-test--manifest ()
  "Return the fixture manifest."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "manifest.json"
                                            chem-svg-parity-test--fixture-dir))
    (json-parse-string (buffer-string) :object-type 'alist)))

(defun chem-svg-parity-test--load (name)
  "Load fixture NAME."
  (with-temp-buffer
    (insert-file-contents (expand-file-name (concat name ".json")
                                            chem-svg-parity-test--fixture-dir))
    (json-parse-string (buffer-string) :object-type 'alist)))

(defun chem-svg-parity-test--get (alist key)
  "Return KEY from ALIST, accepting string or symbol keys."
  (or (alist-get (intern key) alist nil nil #'eq)
      (alist-get key alist nil nil #'equal)))

(defun chem-svg-parity-test--list (value)
  "Return VALUE as a list."
  (append value nil))

(defun chem-svg-parity-test--number (value)
  "Return VALUE as a number."
  (cond ((numberp value) value)
        ((stringp value) (string-to-number value))
        (t 0.0)))

(defun chem-svg-parity-test--atom-angle (atoms from to)
  "Return the IR angle from atom FROM to atom TO in ATOMS."
  (let* ((a (gethash from atoms))
         (b (gethash to atoms))
         (radians (atan (- (chem-atom-y b) (chem-atom-y a))
                        (- (chem-atom-x b) (chem-atom-x a)))))
    (mod (* radians (/ 180.0 float-pi)) 360.0)))

(defun chem-svg-parity-test--bond-order (order)
  "Map SmilesDrawer ORDER to an IR bond order."
  (pcase order
    ("single" :single)
    ("double" :double)
    ("triple" :triple)
    ("aromatic" :aromatic)
    (_ :single)))

(defun chem-svg-parity-test--bond-stereo (stereo)
  "Map SmilesDrawer STEREO to an IR bond stereo."
  (pcase stereo
    ("up" :up)
    ("down" :down)
    (_ :none)))

(defun chem-svg-parity-test--build-drawing (fixture)
  "Build a test-only `chem-drawing' from SmilesDrawer FIXTURE.
SmilesDrawer uses SVG-style y-down coordinates; the IR uses y-up."
  (let* ((atoms-json (chem-svg-parity-test--list
                      (chem-svg-parity-test--get fixture "atoms")))
         (edges-json (chem-svg-parity-test--list
                      (chem-svg-parity-test--get fixture "edges")))
         (rings-json (chem-svg-parity-test--list
                      (chem-svg-parity-test--get fixture "rings")))
         (atoms (make-hash-table :test 'eql))
         (edges (make-hash-table :test 'eql))
         (edge-lookup (make-hash-table :test 'equal))
         (adjacency (make-hash-table :test 'eql)))
    (dolist (json atoms-json)
      (let* ((idx (chem-svg-parity-test--number
                   (chem-svg-parity-test--get json "id")))
             (element (chem-svg-parity-test--get json "element"))
             (hydrogens (chem-svg-parity-test--number
                         (chem-svg-parity-test--get json "hydrogens")))
             (charge (chem-svg-parity-test--number
                      (chem-svg-parity-test--get json "charge")))
             (neighbours (chem-svg-parity-test--list
                          (chem-svg-parity-test--get json "neighbours")))
             (atom (chem-atom-create
                    :idx idx
                    :x (chem-svg-parity-test--number
                        (chem-svg-parity-test--get json "x"))
                    :y (- (chem-svg-parity-test--number
                           (chem-svg-parity-test--get json "y")))
                    :element element
                    :hydrogens hydrogens
                    :charge charge
                    :radical 0
                    :neighbors (mapcar #'chem-svg-parity-test--number neighbours)
                    :bond-angles nil
                    :label (if (equal element "C") nil element)
                    :number nil
                    :marker nil
                    :stereocenter nil
                    :allene nil)))
        (puthash idx atom atoms)))
    (maphash
     (lambda (_ atom)
       (setf (chem-atom-bond-angles atom)
             (mapcar (lambda (neighbor)
                       (chem-svg-parity-test--atom-angle atoms
                                                         (chem-atom-idx atom)
                                                         neighbor))
                     (chem-atom-neighbors atom))))
     atoms)
    (dolist (json edges-json)
      (let* ((id (chem-svg-parity-test--number
                  (chem-svg-parity-test--get json "id")))
             (source (chem-svg-parity-test--number
                      (chem-svg-parity-test--get json "a")))
             (target (chem-svg-parity-test--number
                      (chem-svg-parity-test--get json "b")))
             (order (chem-svg-parity-test--bond-order
                     (chem-svg-parity-test--get json "order")))
             (stereo (chem-svg-parity-test--bond-stereo
                      (chem-svg-parity-test--get json "stereo")))
             (edge (chem-edge-create
                    :id id :a source :b target
                    :order order
                    :stereo stereo
                    :stereo-center (when (memq stereo '(:up :down)) source)
                    :link-p nil
                    :cross-p nil
                    :marker ""
                    :decoration 'plain)))
        (puthash id edge edges)
        (puthash (cons (min source target) (max source target))
                 edge edge-lookup)))
    (maphash
     (lambda (idx atom)
       (puthash idx
                (mapcar (lambda (neighbor)
                          (cons (chem-edge-id
                                 (gethash (cons (min idx neighbor)
                                                (max idx neighbor))
                                          edge-lookup))
                                neighbor))
                        (chem-atom-neighbors atom))
                adjacency))
     atoms)
    (let ((rings nil))
      (dolist (json rings-json)
        (let* ((id (chem-svg-parity-test--number
                    (chem-svg-parity-test--get json "id")))
               (members (mapcar #'chem-svg-parity-test--number
                                (chem-svg-parity-test--list
                                 (chem-svg-parity-test--get json "atoms"))))
               (bonds
                (cl-loop for tail on members
                         for from = (car tail)
                         for to = (or (cadr tail) (car members))
                         collect (chem-edge-id
                                  (gethash (cons (min from to) (max from to))
                                           edge-lookup)))))
          (push (chem-ring-create
                 :id id
                 :atoms members
                 :bonds bonds
                 :anchor (car bonds)
                 :aromatic-p (eq t (chem-svg-parity-test--get json "aromatic"))
                 :sequence id)
                rings)))
      (chem-drawing-create
       :atoms atoms
       :edges edges
       :edge-lookup edge-lookup
       :adjacency adjacency
       :rings (nreverse rings)
       :entry-atom (when (> (hash-table-count atoms) 0)
                     (gethash 0 atoms))
       :exit-atom nil
       :exit-node nil
       :root nil))))

(defun chem-svg-parity-test--options ()
  "Return renderer options matching SmilesDrawer's coordinate units."
  (let ((chem-default-exporter 'svg))
    (chem--build-options
     '(("svg-scale" . 1.0)
       ("svg-padding" . 0.0)
       ("svg-width" . nil)
       ("svg-height" . nil)
       ("svg-font-size" . 11.0)
       ("svg-bond-spacing" . 5.1)
       ("svg-bond-thickness" . 1.0)
       ("svg-short-bond-length" . 0.8)
       ("svg-theme" . light)
       ("svg-aromatic" . circle)
       ("svg-show-stereo" . t)))))

(defun chem-svg-parity-test--line-elements (svg)
  "Return line endpoint vectors from SVG."
  (let ((start 0)
        (lines nil))
    (while (string-match
            "<line x1=\"\\([^\"]+\\)\" y1=\"\\([^\"]+\\)\" x2=\"\\([^\"]+\\)\" y2=\"\\([^\"]+\\)\""
            svg start)
      (let ((x1 (string-to-number (match-string 1 svg)))
            (y1 (string-to-number (match-string 2 svg)))
            (x2 (string-to-number (match-string 3 svg)))
            (y2 (string-to-number (match-string 4 svg))))
        (push (list (abs (- x2 x1)) (abs (- y2 y1))) lines))
      (setq start (match-end 0)))
    (nreverse lines)))

(defun chem-svg-parity-test--line-endpoints (svg)
  "Return absolute endpoint lists from the line elements in SVG."
  (let ((start 0)
        (lines nil))
    (while (string-match
            "<line x1=\"\\([^\"]+\\)\" y1=\"\\([^\"]+\\)\" x2=\"\\([^\"]+\\)\" y2=\"\\([^\"]+\\)\""
            svg start)
      (push (list (list (string-to-number (match-string 1 svg))
                        (string-to-number (match-string 2 svg)))
                  (list (string-to-number (match-string 3 svg))
                        (string-to-number (match-string 4 svg))))
            lines)
      (setq start (match-end 0)))
    (nreverse lines)))

(defun chem-svg-parity-test--reference-endpoints (fixture edge)
  "Return absolute reference endpoints for EDGE in FIXTURE."
  (let ((primitives (chem-svg-parity-test--list
                     (chem-svg-parity-test--get fixture "primitives"))))
    (delq nil
          (mapcar
           (lambda (primitive)
             (when (and (equal (chem-svg-parity-test--get primitive "kind") "line")
                        (equal (chem-svg-parity-test--number
                                (chem-svg-parity-test--get primitive "edge"))
                               edge))
               (list
                (list (chem-svg-parity-test--number
                       (chem-svg-parity-test--get primitive "x1"))
                      (chem-svg-parity-test--number
                       (chem-svg-parity-test--get primitive "y1")))
                (list (chem-svg-parity-test--number
                       (chem-svg-parity-test--get primitive "x2"))
                      (chem-svg-parity-test--number
                       (chem-svg-parity-test--get primitive "y2"))))))
           primitives))))

(defun chem-svg-parity-test--relative-lines (lines origin)
  "Return LINES translated relative to ORIGIN, in canonical order."
  (let ((relative
         (mapcar
          (lambda (line)
            (sort
             (mapcar
              (lambda (point)
                (list (- (car point) (car origin))
                      (- (cadr point) (cdr origin))))
              line)
             (lambda (a b)
               (if (= (car a) (car b))
                   (< (cadr a) (cadr b))
                 (< (car a) (car b))))))
          lines)))
    (sort relative
          (lambda (a b)
            (if (= (caar a) (caar b))
                (< (cadar a) (cadar b))
              (< (caar a) (caar b)))))))

(defun chem-svg-parity-test--reference-lines (fixture edge)
  "Return reference line vectors for EDGE in FIXTURE.
Orient each line along the canonical lower-to-higher atom direction."
  (let* ((edges (chem-svg-parity-test--list
                 (chem-svg-parity-test--get fixture "edges")))
         (edge-json (cl-find-if
                     (lambda (candidate)
                       (equal (chem-svg-parity-test--number
                               (chem-svg-parity-test--get candidate "id"))
                              edge))
                     edges))
         (a (chem-svg-parity-test--number
             (chem-svg-parity-test--get edge-json "a")))
         (b (chem-svg-parity-test--number
             (chem-svg-parity-test--get edge-json "b")))
         (atoms (chem-svg-parity-test--list
                 (chem-svg-parity-test--get fixture "atoms")))
         (atom-a (cl-find-if
                  (lambda (atom)
                    (equal (chem-svg-parity-test--number
                            (chem-svg-parity-test--get atom "id"))
                           a))
                  atoms))
         (atom-b (cl-find-if
                  (lambda (atom)
                    (equal (chem-svg-parity-test--number
                            (chem-svg-parity-test--get atom "id"))
                           b))
                  atoms))
         (canonical-x (- (chem-svg-parity-test--number
                          (chem-svg-parity-test--get atom-b "x"))
                         (chem-svg-parity-test--number
                          (chem-svg-parity-test--get atom-a "x"))))
         (canonical-y (- (chem-svg-parity-test--number
                          (chem-svg-parity-test--get atom-b "y"))
                         (chem-svg-parity-test--number
                          (chem-svg-parity-test--get atom-a "y"))))
         (primitives (chem-svg-parity-test--list
                      (chem-svg-parity-test--get fixture "primitives"))))
    (delq nil
          (mapcar
           (lambda (primitive)
             (when (and (equal (chem-svg-parity-test--get primitive "kind") "line")
                        (equal (chem-svg-parity-test--number
                                (chem-svg-parity-test--get primitive "edge"))
                               edge))
               (let* ((dx (- (chem-svg-parity-test--number
                               (chem-svg-parity-test--get primitive "x2"))
                              (chem-svg-parity-test--number
                               (chem-svg-parity-test--get primitive "x1"))))
                      (dy (- (chem-svg-parity-test--number
                               (chem-svg-parity-test--get primitive "y2"))
                              (chem-svg-parity-test--number
                               (chem-svg-parity-test--get primitive "y1"))))
                      (sign (if (< (+ (* dx canonical-x) (* dy canonical-y)) 0.0)
                                -1
                              1)))
                 (list (abs (* sign dx)) (abs (* sign dy))))))
           primitives))))

(defun chem-svg-parity-test--sort-vectors (vectors)
  "Sort VECTORS deterministically."
  (sort (copy-sequence vectors)
        (lambda (a b)
          (if (= (car a) (car b))
              (< (cadr a) (cadr b))
            (< (car a) (car b))))))

(defun chem-svg-parity-test--vector-distance (a b)
  "Return the distance between vectors A and B."
  (sqrt (+ (expt (- (car a) (car b)) 2)
           (expt (- (cadr a) (cadr b)) 2))))

(defun chem-svg-parity-test--report (&optional verbose)
  "Print a renderer parity report.  With VERBOSE, print every mismatch."
  (interactive "P")
  (let* ((manifest (chem-svg-parity-test--manifest))
         (fixtures (chem-svg-parity-test--list
                    (chem-svg-parity-test--get manifest "fixtures")))
         (total-edges 0)
         (line-count-mismatches 0)
         (line-geometry-mismatches 0)
         (max-line-distance 0.0))
    (dolist (entry fixtures)
      (let* ((name (chem-svg-parity-test--get entry "name"))
             (fixture (chem-svg-parity-test--load name))
             (drawing (chem-svg-parity-test--build-drawing fixture))
             (state (chem-exp-svg--make-state drawing
                                              (chem-svg-parity-test--options))))
        (dolist (edge (chem-edges drawing))
          (let* ((edge-id (chem-edge-id edge))
                 (reference (chem-svg-parity-test--reference-lines
                             fixture edge-id))
                 (native (chem-svg-parity-test--line-elements
                          (chem-exp-svg--edge-svg state edge)))
                 (reference-sorted (chem-svg-parity-test--sort-vectors reference))
                 (native-sorted (chem-svg-parity-test--sort-vectors native)))
            (setq total-edges (1+ total-edges))
            (unless (= (length reference-sorted) (length native-sorted))
              (setq line-count-mismatches (1+ line-count-mismatches))
              (when verbose
                (princ (format "%s edge %d: line count %d -> %d\n"
                               name edge-id
                               (length reference-sorted)
                               (length native-sorted)))))
            (when (= (length reference-sorted) (length native-sorted))
              (let ((distance
                     (if reference-sorted
                         (apply #'max
                                (cl-mapcar #'chem-svg-parity-test--vector-distance
                                           reference-sorted native-sorted))
                       0.0)))
                (setq max-line-distance (max max-line-distance distance))
                (when (> distance 0.01)
                  (setq line-geometry-mismatches
                        (1+ line-geometry-mismatches))
                  (when verbose
                    (princ (format "%s edge %d: max vector delta %.4f ref=%S native=%S\n"
                                   name edge-id distance
                                   reference-sorted native-sorted))))))))
        (let* ((svg (chem-exp-svg--rings state))
               (native-radii
                (let ((start 0) radii)
                  (while (string-match
                          "<circle cx=\"[^\"]+\" cy=\"[^\"]+\" r=\"\\([^\"]+\\)\""
                          svg start)
                    (push (string-to-number (match-string 1 svg)) radii)
                    (setq start (match-end 0)))
                  (nreverse radii)))
               (reference-radii
                (delq nil
                      (mapcar
                       (lambda (primitive)
                         (when (equal (chem-svg-parity-test--get
                                       primitive "kind") "circle")
                           (chem-svg-parity-test--number
                            (chem-svg-parity-test--get primitive "r"))))
                       (chem-svg-parity-test--list
                        (chem-svg-parity-test--get fixture "primitives"))))))
          (when (or verbose
                    (not (equal (length reference-radii)
                                (length native-radii))))
            (princ (format "%s rings: reference %S native %S\n"
                           name reference-radii native-radii)))
          (when (and reference-radii native-radii)
            (let ((delta (abs (- (car reference-radii)
                                 (car native-radii)))))
              (when (or verbose (> delta 0.01))
                (princ (format "%s ring radius: reference %.4f native %.4f delta %.4f\n"
                               name (car reference-radii)
                               (car native-radii) delta))))))))
    (princ (format "edges: %d; line-count mismatches: %d; geometry mismatches: %d; max vector delta: %.4f\n"
                   total-edges line-count-mismatches
                   line-geometry-mismatches max-line-distance))))

(defconst chem-svg-parity-test--known-gaps
  '()
  "Fixtures with known unsupported SmilesDrawer features.")

(defun chem-svg-parity-test--strict-fixtures ()
  "Return fixtures that should match the renderer exactly."
  (let* ((manifest (chem-svg-parity-test--manifest))
         (fixtures (chem-svg-parity-test--list
                    (chem-svg-parity-test--get manifest "fixtures"))))
    (cl-remove-if
     (lambda (entry)
       (member (chem-svg-parity-test--get entry "name")
               chem-svg-parity-test--known-gaps))
     fixtures)))

(defun chem-svg-parity-test--reference-circles (fixture)
  "Return reference circle radii for FIXTURE."
  (delq nil
        (mapcar
         (lambda (primitive)
           (when (equal (chem-svg-parity-test--get primitive "kind") "circle")
             (chem-svg-parity-test--number
              (chem-svg-parity-test--get primitive "r"))))
         (chem-svg-parity-test--list
          (chem-svg-parity-test--get fixture "primitives")))))

(defun chem-svg-parity-test--native-circles (state)
  "Return native circle radii for STATE."
  (let ((start 0)
        (radii nil)
        (svg (chem-exp-svg--rings state)))
    (while (string-match
            "<circle cx=\"[^\"]+\" cy=\"[^\"]+\" r=\"\\([^\"]+\\)\""
            svg start)
      (push (string-to-number (match-string 1 svg)) radii)
      (setq start (match-end 0)))
    (nreverse radii)))

(ert-deftest chem-svg-parity-edge-geometry ()
  "Native edge primitives match SmilesDrawer with shared coordinates."
  (dolist (entry (chem-svg-parity-test--strict-fixtures))
    (let* ((name (chem-svg-parity-test--get entry "name"))
           (fixture (chem-svg-parity-test--load name))
           (drawing (chem-svg-parity-test--build-drawing fixture))
           (state (chem-exp-svg--make-state
                   drawing (chem-svg-parity-test--options))))
      (dolist (edge (chem-edges drawing))
        (when (chem-exp-svg--edge-visible-p state edge)
          (let* ((reference
                  (chem-svg-parity-test--sort-vectors
                   (chem-svg-parity-test--reference-lines
                    fixture (chem-edge-id edge))))
                 (native
                  (chem-svg-parity-test--sort-vectors
                   (chem-svg-parity-test--line-elements
                    (chem-exp-svg--edge-svg state edge)))))
            (should (equal (length reference) (length native)))
            (dolist (pair (cl-mapcar #'cons reference native))
              (should (< (chem-svg-parity-test--vector-distance
                          (car pair) (cdr pair))
                         0.01)))))))))

(defun chem-svg-parity-test--polygon-elements (svg)
  "Return polygon point lists from SVG."
  (let ((start 0)
        (polygons nil))
    (while (string-match "<polygon points=\"\\([^\"]+\\)\"" svg start)
      (push (mapcar (lambda (pair)
                      (mapcar #'string-to-number (split-string pair ",")))
                    (split-string (match-string 1 svg) " " t))
            polygons)
      (setq start (match-end 0)))
    (nreverse polygons)))

(defun chem-svg-parity-test--reference-polygons (fixture edge)
  "Return reference polygon point lists for EDGE in FIXTURE."
  (let ((primitives (chem-svg-parity-test--list
                     (chem-svg-parity-test--get fixture "primitives"))))
    (delq nil
          (mapcar
           (lambda (primitive)
             (when (and (equal (chem-svg-parity-test--get primitive "kind")
                               "polygon")
                        (equal (chem-svg-parity-test--number
                                (chem-svg-parity-test--get primitive "edge"))
                               edge))
               (mapcar (lambda (point)
                         (list (chem-svg-parity-test--number
                                (aref point 0))
                               (chem-svg-parity-test--number
                                (aref point 1))))
                       (chem-svg-parity-test--list
                        (chem-svg-parity-test--get primitive "points")))))
           primitives))))

(defun chem-svg-parity-test--relative-points (points origin)
  "Translate POINTS relative to ORIGIN and sort them."
  (sort
   (mapcar (lambda (point)
             (list (- (car point) (car origin))
                   (- (cadr point) (cdr origin))))
           points)
   (lambda (a b)
     (if (= (car a) (car b))
         (< (cadr a) (cadr b))
       (< (car a) (car b))))))

(ert-deftest chem-svg-parity-wedge-polygon ()
  "Native solid-wedge polygons match SmilesDrawer."
  (dolist (entry (chem-svg-parity-test--strict-fixtures))
    (let* ((name (chem-svg-parity-test--get entry "name"))
           (fixture (chem-svg-parity-test--load name))
           (drawing (chem-svg-parity-test--build-drawing fixture))
           (state (chem-exp-svg--make-state
                   drawing (chem-svg-parity-test--options)))
           (atoms (chem-drawing-atoms drawing))
           (fixture-atoms (chem-svg-parity-test--list
                           (chem-svg-parity-test--get fixture "atoms")))
           (fixture-edges (chem-svg-parity-test--list
                           (chem-svg-parity-test--get fixture "edges"))))
      (dolist (edge (chem-edges drawing))
        (when (chem-exp-svg--edge-visible-p state edge)
          (let* ((edge-id (chem-edge-id edge))
                 (reference (chem-svg-parity-test--reference-polygons
                             fixture edge-id)))
            (when reference
              (let* ((edge-json (cl-find-if
                                 (lambda (candidate)
                                   (equal (chem-svg-parity-test--number
                                           (chem-svg-parity-test--get candidate "id"))
                                          edge-id))
                                 fixture-edges))
                     (a (chem-svg-parity-test--number
                         (chem-svg-parity-test--get edge-json "a")))
                     (atom-a (gethash a atoms))
                     (fixture-atom-a (cl-find-if
                                      (lambda (candidate)
                                        (equal (chem-svg-parity-test--number
                                                (chem-svg-parity-test--get candidate "id"))
                                               a))
                                      fixture-atoms))
                     (reference-origin
                      (cons (chem-svg-parity-test--number
                             (chem-svg-parity-test--get fixture-atom-a "x"))
                            (chem-svg-parity-test--number
                             (chem-svg-parity-test--get fixture-atom-a "y"))))
                     (native-origin (chem-exp-svg--point
                                     state
                                     (chem-atom-x atom-a)
                                     (chem-atom-y atom-a)))
                     (native (chem-svg-parity-test--polygon-elements
                              (chem-exp-svg--edge-svg state edge))))
                (should (= (length reference) (length native)))
                (dolist (polygon-pair (cl-mapcar #'cons reference native))
                  (let ((reference-points
                         (chem-svg-parity-test--relative-points
                          (car polygon-pair) reference-origin))
                        (native-points
                         (chem-svg-parity-test--relative-points
                          (cdr polygon-pair) native-origin)))
                    (should (= (length reference-points)
                               (length native-points)))
                    (dolist (point-pair (cl-mapcar #'cons
                                                   reference-points
                                                   native-points))
                      (should (< (chem-svg-parity-test--point-distance
                                  (car point-pair) (cdr point-pair))
                                 0.01)))))))))))))

(defun chem-svg-parity-test--point-distance (a b)
  "Return the distance between points A and B."
  (sqrt (+ (expt (- (car a) (car b)) 2)
           (expt (- (cadr a) (cadr b)) 2))))

(ert-deftest chem-svg-parity-edge-placement ()
  "Native line placement matches SmilesDrawer relative to each edge."
  (dolist (entry (chem-svg-parity-test--strict-fixtures))
    (let* ((name (chem-svg-parity-test--get entry "name"))
           (fixture (chem-svg-parity-test--load name))
           (drawing (chem-svg-parity-test--build-drawing fixture))
           (state (chem-exp-svg--make-state
                   drawing (chem-svg-parity-test--options)))
           (atoms (chem-drawing-atoms drawing))
           (fixture-atoms (chem-svg-parity-test--list
                           (chem-svg-parity-test--get fixture "atoms")))
           (fixture-edges (chem-svg-parity-test--list
                           (chem-svg-parity-test--get fixture "edges"))))
      (dolist (edge (chem-edges drawing))
        (when (chem-exp-svg--edge-visible-p state edge)
        (let* ((edge-id (chem-edge-id edge))
               (edge-json (cl-find-if
                           (lambda (candidate)
                             (equal (chem-svg-parity-test--number
                                     (chem-svg-parity-test--get candidate "id"))
                                    edge-id))
                           fixture-edges))
               (a (chem-svg-parity-test--number
                   (chem-svg-parity-test--get edge-json "a")))
               (atom-a (gethash a atoms))
               (fixture-atom-a (cl-find-if
                                (lambda (candidate)
                                  (equal (chem-svg-parity-test--number
                                          (chem-svg-parity-test--get candidate "id"))
                                         a))
                                fixture-atoms))
               (reference-origin
                (cons (chem-svg-parity-test--number
                       (chem-svg-parity-test--get fixture-atom-a "x"))
                      (chem-svg-parity-test--number
                       (chem-svg-parity-test--get fixture-atom-a "y"))))
               (native-origin
                (chem-exp-svg--point state
                                     (chem-atom-x atom-a)
                                     (chem-atom-y atom-a)))
               (reference
                (chem-svg-parity-test--relative-lines
                 (chem-svg-parity-test--reference-endpoints fixture edge-id)
                 reference-origin))
               (native
                (chem-svg-parity-test--relative-lines
                 (chem-svg-parity-test--line-endpoints
                  (chem-exp-svg--edge-svg state edge))
                 native-origin)))
          (should (equal (length reference) (length native)))
          (dolist (line-pair (cl-mapcar #'cons reference native))
            (dolist (point-pair (cl-mapcar #'cons (car line-pair)
                                           (cdr line-pair)))
              (should (< (chem-svg-parity-test--point-distance
                          (car point-pair) (cdr point-pair))
                         0.01))))))))))

(defconst chem-svg-parity-test--label-known-gaps
  '("allene")
  "Fixtures whose labels are not compared yet.
SmilesDrawer emits a carbon label for allene that is unrelated to the
heteroatom labels covered here.")

(defun chem-svg-parity-test--label-fixtures ()
  "Return strict fixtures with comparable labels."
  (cl-remove-if
   (lambda (entry)
     (member (chem-svg-parity-test--get entry "name")
             chem-svg-parity-test--label-known-gaps))
   (chem-svg-parity-test--strict-fixtures)))

(ert-deftest chem-svg-parity-label-text ()
  "Native label text matches SmilesDrawer."
  (dolist (entry (chem-svg-parity-test--label-fixtures))
    (let* ((name (chem-svg-parity-test--get entry "name"))
           (fixture (chem-svg-parity-test--load name))
           (drawing (chem-svg-parity-test--build-drawing fixture))
           (state (chem-exp-svg--make-state
                   drawing (chem-svg-parity-test--options)))
           (atoms (chem-drawing-atoms drawing))
           (labels (chem-svg-parity-test--list
                    (chem-svg-parity-test--get fixture "labels"))))
      (should (= (length labels)
                 (cl-count-if
                  (lambda (label)
                    (let* ((idx (chem-svg-parity-test--number
                                 (chem-svg-parity-test--get label "atom")))
                           (atom (gethash idx atoms)))
                      (and (not (chem-exp-svg--hidden-atom-p state idx))
                           (chem-exp-svg--atom-text-for-state state atom))))
                  labels)))
      (dolist (label labels)
        (let* ((idx (chem-svg-parity-test--number
                     (chem-svg-parity-test--get label "atom")))
               (atom (gethash idx atoms))
               (reference (chem-svg-parity-test--get label "text"))
               (native (chem-exp-svg--atom-text-for-state state atom)))
          (should atom)
          (should (equal reference native)))))))

(ert-deftest chem-svg-parity-label-placement ()
  "Native labels are centered on atoms like SmilesDrawer labels."
  (dolist (entry (chem-svg-parity-test--label-fixtures))
    (let* ((name (chem-svg-parity-test--get entry "name"))
           (fixture (chem-svg-parity-test--load name))
           (drawing (chem-svg-parity-test--build-drawing fixture))
           (state (chem-exp-svg--make-state
                   drawing (chem-svg-parity-test--options)))
           (atoms (chem-drawing-atoms drawing))
           (labels (chem-svg-parity-test--list
                    (chem-svg-parity-test--get fixture "labels"))))
      (dolist (label labels)
        (let* ((idx (chem-svg-parity-test--number
                     (chem-svg-parity-test--get label "atom")))
               (atom (gethash idx atoms)))
          (unless (chem-exp-svg--hidden-atom-p state idx)
            (let* ((text (chem-exp-svg--atom-text-for-state state atom))
                   (point (chem-exp-svg--point state
                                                (chem-atom-x atom)
                                                (chem-atom-y atom)))
                   (center (chem-exp-svg--label-center state atom text)))
              (should (< (abs (- (car center) (car point))) 1e-6))
              (should (< (abs (- (cadr center) (cdr point))) 1e-6)))))))))

(defun chem-svg-parity-test--mask-circles (svg)
  "Return (CX CY R) for black mask circles in SVG."
  (let ((start 0)
        (circles nil))
    (while (string-match
            "<circle cx=\"\\([^\"]+\\)\" cy=\"\\([^\"]+\\)\" r=\"\\([^\"]+\\)\" fill=\"black\""
            svg start)
      (push (list (string-to-number (match-string 1 svg))
                  (string-to-number (match-string 2 svg))
                  (string-to-number (match-string 3 svg)))
            circles)
      (setq start (match-end 0)))
    (sort (nreverse circles)
          (lambda (a b)
            (if (= (car a) (car b))
                (< (cadr a) (cadr b))
              (< (car a) (car b)))))))

(defun chem-svg-parity-test--parse-native-gradient (svg)
  "Parse one native gradient SVG string."
  (when (string-match "<\\(radial\\|linear\\)Gradient id=\"\\([^\"]+\\)\"" svg)
    (let* ((kind (intern (match-string 1 svg)))
           (id (match-string 2 svg))
           (attrs nil)
           (stops nil)
           (start 0))
      (if (eq kind 'radial)
          (when (string-match
                 "cx=\"\\([^\"]+\\)\" cy=\"\\([^\"]+\\)\" r=\"\\([^\"]+\\)\""
                 svg)
            (setq attrs (list :cx (string-to-number (match-string 1 svg))
                              :cy (string-to-number (match-string 2 svg))
                              :r (string-to-number (match-string 3 svg)))))
        (when (string-match
               "x1=\"\\([^\"]+\\)\" y1=\"\\([^\"]+\\)\" x2=\"\\([^\"]+\\)\" y2=\"\\([^\"]+\\)\""
               svg)
          (setq attrs (list :x1 (string-to-number (match-string 1 svg))
                            :y1 (string-to-number (match-string 2 svg))
                            :x2 (string-to-number (match-string 3 svg))
                            :y2 (string-to-number (match-string 4 svg))))))
      (while (string-match "offset=\"\\([^\"]+\\)\" stop-color=\"\\([^\"]+\\)\"" svg start)
        (push (cons (match-string 1 svg) (match-string 2 svg)) stops)
        (setq start (match-end 0)))
      (cons id (list :kind kind :attrs attrs :stops (nreverse stops))))))

(defun chem-svg-parity-test--reference-gradients (fixture)
  "Return an alist of reference gradients in FIXTURE."
  (mapcar
   (lambda (gradient)
     (cons (chem-svg-parity-test--get gradient "id")
           (list :kind (intern
                        (string-remove-suffix
                         "gradient"
                         (chem-svg-parity-test--get gradient "kind")))
                 :attrs (list :cx (chem-svg-parity-test--get gradient "cx")
                              :cy (chem-svg-parity-test--get gradient "cy")
                              :r (chem-svg-parity-test--get gradient "r")
                              :x1 (chem-svg-parity-test--get gradient "x1")
                              :y1 (chem-svg-parity-test--get gradient "y1")
                              :x2 (chem-svg-parity-test--get gradient "x2")
                              :y2 (chem-svg-parity-test--get gradient "y2"))
                 :stops (mapcar (lambda (stop)
                                  (cons (chem-svg-parity-test--get stop "offset")
                                        (chem-svg-parity-test--get stop "color")))
                                (chem-svg-parity-test--list
                                 (chem-svg-parity-test--get gradient "stops"))))))
   (chem-svg-parity-test--list
    (chem-svg-parity-test--get fixture "gradients"))))

(ert-deftest chem-svg-parity-gradients ()
  "Native bond strokes and gradients match SmilesDrawer."
  (dolist (entry (chem-svg-parity-test--strict-fixtures))
    (let* ((name (chem-svg-parity-test--get entry "name"))
           (fixture (chem-svg-parity-test--load name))
           (drawing (chem-svg-parity-test--build-drawing fixture))
           (state (chem-exp-svg--make-state
                   drawing (chem-svg-parity-test--viewbox-options)))
           (reference-gradients (chem-svg-parity-test--reference-gradients fixture))
           (native-gradients
            (progn
              (chem-exp-svg--prepare-gradients state)
              (delq nil
                    (mapcar #'chem-svg-parity-test--parse-native-gradient
                            (chem-exp-svg-state-gradients state))))))
      (dolist (edge (chem-edges drawing))
        (when (chem-exp-svg--edge-visible-p state edge)
          (let* ((edge-id (chem-edge-id edge))
                 (reference-stroke
                  (catch 'found
                    (dolist (primitive (chem-svg-parity-test--list
                                        (chem-svg-parity-test--get fixture "primitives")))
                      (when (and (equal (chem-svg-parity-test--get primitive "kind") "line")
                                 (equal (chem-svg-parity-test--number
                                         (chem-svg-parity-test--get primitive "edge"))
                                        edge-id))
                        (throw 'found (chem-svg-parity-test--get primitive "stroke"))))
                    nil))
                 (native-stroke (chem-exp-svg--edge-color state edge)))
            (when reference-stroke
              (if (string-prefix-p "url(#" reference-stroke)
                  (let* ((reference-id (substring reference-stroke 5 -1))
                         (reference (cdr (assoc reference-id reference-gradients)))
                         (native (cdr (assoc (substring native-stroke 5 -1)
                                             native-gradients))))
                    (should reference)
                    (should native)
                    (should (equal (plist-get reference :kind)
                                   (plist-get native :kind)))
                    (dolist (key '(:cx :cy :r :x1 :y1 :x2 :y2))
                      (let ((ref-value (plist-get (plist-get reference :attrs) key))
                            (native-value (plist-get (plist-get native :attrs) key)))
                        (when (and ref-value native-value)
                          (should (< (abs (- (chem-svg-parity-test--number ref-value)
                                             (chem-svg-parity-test--number native-value)))
                                     0.01)))))
                    (should (equal (plist-get reference :stops)
                                   (plist-get native :stops))))
                (should (equal reference-stroke native-stroke))))))))))

(defun chem-svg-parity-test--tspan-parts (svg)
  "Return (TEXT . FILL) pairs from SVG tspans."
  (let ((start 0)
        (parts nil))
    (while (string-match "<tspan fill=\"\\([^\"]+\\)\"[^>]*>\\([^<]*\\)</tspan>"
                         svg start)
      (push (cons (match-string 2 svg) (match-string 1 svg)) parts)
      (setq start (match-end 0)))
    (nreverse parts)))

(ert-deftest chem-svg-parity-text-colors ()
  "Native tspan text and colors match SmilesDrawer."
  (dolist (entry (chem-svg-parity-test--label-fixtures))
    (let* ((name (chem-svg-parity-test--get entry "name"))
           (fixture (chem-svg-parity-test--load name))
           (drawing (chem-svg-parity-test--build-drawing fixture))
           (state (chem-exp-svg--make-state
                   drawing (chem-svg-parity-test--options)))
           (atoms (chem-drawing-atoms drawing)))
      (dolist (label (chem-svg-parity-test--list
                      (chem-svg-parity-test--get fixture "labels")))
        (let* ((idx (chem-svg-parity-test--number
                     (chem-svg-parity-test--get label "atom")))
               (atom (gethash idx atoms)))
          (unless (chem-exp-svg--hidden-atom-p state idx)
            (let* ((reference
                    (mapcar (lambda (part)
                              (cons (chem-svg-parity-test--get part "text")
                                    (chem-svg-parity-test--get part "fill")))
                            (chem-svg-parity-test--list
                             (chem-svg-parity-test--get label "parts"))))
                   (native (chem-svg-parity-test--tspan-parts
                            (chem-exp-svg--atom-svg state atom))))
              (should (equal reference native)))))))))

(ert-deftest chem-svg-parity-masks ()
  "Native label masks match SmilesDrawer's mask circles."
  (dolist (entry (chem-svg-parity-test--label-fixtures))
    (let* ((name (chem-svg-parity-test--get entry "name"))
           (fixture (chem-svg-parity-test--load name))
           (drawing (chem-svg-parity-test--build-drawing fixture))
           (state (chem-exp-svg--make-state
                   drawing (chem-svg-parity-test--options)))
           (reference
            (sort
             (mapcar
              (lambda (mask)
                (list (chem-svg-parity-test--number
                       (chem-svg-parity-test--get mask "cx"))
                      (chem-svg-parity-test--number
                       (chem-svg-parity-test--get mask "cy"))
                      (chem-svg-parity-test--number
                       (chem-svg-parity-test--get mask "r"))))
              (cl-remove-if-not
               (lambda (mask)
                 (> (chem-svg-parity-test--number
                     (chem-svg-parity-test--get mask "r"))
                    5.0))
               (chem-svg-parity-test--list
                (chem-svg-parity-test--get fixture "masks"))))
             (lambda (a b)
               (if (= (car a) (car b))
                   (< (cadr a) (cadr b))
                 (< (car a) (car b))))))
           (native (chem-svg-parity-test--mask-circles
                    (chem-exp-svg--defs state))))
      (should (= (length reference) (length native)))
      (let* ((ref-origin (car reference))
             (native-origin (car native))
             (reference-relative
              (mapcar (lambda (mask)
                        (list (- (car mask) (car ref-origin))
                              (- (cadr mask) (cadr ref-origin))
                              (caddr mask)))
                      reference))
             (native-relative
              (mapcar (lambda (mask)
                        (list (- (car mask) (car native-origin))
                              (- (cadr mask) (cadr native-origin))
                              (caddr mask)))
                      native)))
        (dolist (mask-pair (cl-mapcar #'cons reference-relative native-relative))
          (let ((expected (car mask-pair))
                (actual (cdr mask-pair)))
            (should (< (chem-svg-parity-test--point-distance
                        (list (car expected) (cadr expected))
                        (list (car actual) (cadr actual)))
                       0.01))
            (should (< (abs (- (caddr expected) (caddr actual))) 0.01))))))))

(defun chem-svg-parity-test--viewbox-options ()
  "Return shared-coordinate options matching SmilesDrawer's padding."
  (let ((chem-default-exporter 'svg))
    (chem--build-options
     '(("svg-scale" . 1.0)
       ("svg-padding" . 10.0)
       ("svg-width" . nil)
       ("svg-height" . nil)
       ("svg-font-size" . 11.0)
       ("svg-bond-spacing" . 5.1)
       ("svg-bond-thickness" . 1.0)
       ("svg-short-bond-length" . 0.8)
       ("svg-theme" . light)
       ("svg-aromatic" . circle)
       ("svg-show-stereo" . t)))))

(ert-deftest chem-svg-parity-viewbox ()
  "Native square viewBox matches SmilesDrawer."
  (dolist (entry (chem-svg-parity-test--strict-fixtures))
    (let* ((name (chem-svg-parity-test--get entry "name"))
           (fixture (chem-svg-parity-test--load name))
           (drawing (chem-svg-parity-test--build-drawing fixture))
           (state (chem-exp-svg--make-state
                   drawing (chem-svg-parity-test--viewbox-options)))
           (reference (mapcar #'chem-svg-parity-test--number
                              (chem-svg-parity-test--list
                               (chem-svg-parity-test--get fixture "viewBox"))))
           (native (chem-exp-svg--viewbox state)))
      (should (= (length reference) (length native)))
      (dolist (pair (cl-mapcar #'cons reference native))
        (should (< (abs (- (car pair) (cdr pair))) 0.01))))))

(ert-deftest chem-svg-parity-ring-radius ()
  "Native aromatic circle radii match SmilesDrawer."
  (dolist (entry (chem-svg-parity-test--strict-fixtures))
    (let* ((name (chem-svg-parity-test--get entry "name"))
           (fixture (chem-svg-parity-test--load name))
           (drawing (chem-svg-parity-test--build-drawing fixture))
           (state (chem-exp-svg--make-state
                   drawing (chem-svg-parity-test--options)))
           (reference (chem-svg-parity-test--reference-circles fixture))
           (native (chem-svg-parity-test--native-circles state)))
      (should (equal (length reference) (length native)))
      (dolist (pair (cl-mapcar #'cons reference native))
        (should (< (abs (- (car pair) (cdr pair))) 0.01))))))

(ert-deftest chem-svg-parity-determinism ()
  "Shared-coordinate rendering is deterministic."
  (let* ((fixture (chem-svg-parity-test--load "benzene"))
         (drawing (chem-svg-parity-test--build-drawing fixture))
         (options (chem-svg-parity-test--options))
         (state-a (chem-exp-svg--make-state drawing options))
         (state-b (chem-exp-svg--make-state drawing options)))
    (should (equal (mapconcat
                    (lambda (edge)
                      (chem-exp-svg--edge-svg state-a edge))
                    (chem-edges drawing) "")
                   (mapconcat
                    (lambda (edge)
                      (chem-exp-svg--edge-svg state-b edge))
                    (chem-edges drawing) "")))))

(provide 'chem-svg-parity-test)
;;; chem-svg-parity-test.el ends here
