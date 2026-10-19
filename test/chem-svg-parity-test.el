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
             (a (min source target))
             (b (max source target))
             (order (chem-svg-parity-test--bond-order
                     (chem-svg-parity-test--get json "order")))
             (stereo (chem-svg-parity-test--bond-stereo
                      (chem-svg-parity-test--get json "stereo")))
             (edge (chem-edge-create
                    :id id :a a :b b
                    :order order
                    :stereo (if (and (memq stereo '(:up :down))
                                     (/= source a))
                                (if (eq stereo :up) :down :up)
                              stereo)
                    :stereo-center (when (memq stereo '(:up :down)) source)
                    :link-p nil
                    :cross-p nil
                    :marker ""
                    :decoration 'plain)))
        (puthash id edge edges)
        (puthash (cons a b) edge edge-lookup)))
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
        (push (list (- x2 x1) (- y2 y1)) lines))
      (setq start (match-end 0)))
    (nreverse lines)))

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
                           (min a b)))
                  atoms))
         (atom-b (cl-find-if
                  (lambda (atom)
                    (equal (chem-svg-parity-test--number
                            (chem-svg-parity-test--get atom "id"))
                           (max a b)))
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
                 (list (* sign dx) (* sign dy)))))
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
  '("alanine" "butene_e" "butene_z" "nitrobenzene")
  "Fixtures with known SmilesDrawer features not yet supported.
Alanine and nitrobenzene use attached pseudo-element labels; the butenes
exercise SmilesDrawer's acyclic double-bond side selection and shortening.")

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
                       0.01))))))))

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
