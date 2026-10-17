;;; chem-ir.el --- Drawing intermediate representation -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2025 Giovanni Crisalfi

;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The drawing IR shared by all exporters.  It holds the molecular graph, the
;; drawing tree, and the geometry needed to render a structure.
;;
;; This is a transitional extraction: the atom and bond structs still carry a
;; few slots used by the chemfig renderer.  They will move into the chemfig
;; exporter as the renderer is separated from the core.
;;
;; This file must not depend on Indigo or on any exporter.

;;; Code:

(require 'cl-lib)

(eval-and-compile
  (defun chem-ir-alias-accessor (old new)
    "Alias struct accessor OLD to NEW, copying accessor properties.
This keeps `setf' working for the transitional aliases."
    (defalias old new)
    (dolist (prop '(compiler-macro side-effect-free gv-expander gv-setter
                                   setf-method document-generalized-variable))
      (put old prop (get new prop)))))

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
  kind                           ; `bond', `dummy-first' or `aromatic-ring'
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

(cl-defstruct (chem-drawing (:constructor chem-drawing-create))
  options
  toolkit                        ; Transitional Indigo handle
  atoms                          ; hash: idx -> atom
  bonds                          ; hash: (start . end) -> bond
  atom-pairs                     ; list of (start . end)
  seen-atoms                     ; hash: idx -> t
  seen-bonds                     ; hash: (start . end) -> t
  entry-atom exit-atom exit-bond root
  bond-scale)

(defun chem-tree-bonds (drawing)
  "Return all bonds in DRAWING's tree, excluding the dummy first bond."
  (let ((all-bonds nil))
    (cl-labels ((recurse (bond)
                  (push bond all-bonds)
                  (dolist (child (chem-bond-descendants bond))
                    (recurse child))))
      (when (chem-drawing-root drawing)
        (recurse (chem-drawing-root drawing))))
    (setq all-bonds (nreverse all-bonds))
    (if (and all-bonds
             (eq (chem-bond-kind (car all-bonds)) 'dummy-first))
        (cdr all-bonds)
      all-bonds)))

(defun chem-apply-drawing-options (drawing options)
  "Apply common drawing OPTIONS to DRAWING.
Set atom labels and numbers and bond decorations in the IR.  Exporters
render these attributes; they must not re-read the options."
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
     (chem-drawing-bonds drawing)))
  drawing)

;; Transitional aliases while the drawing struct uses its neutral name.
(eval-and-compile
  (chem-ir-alias-accessor 'tochemfig-molecule-create 'chem-drawing-create)
  (chem-ir-alias-accessor 'tochemfig-molecule-p 'chem-drawing-p)
  (chem-ir-alias-accessor 'copy-tochemfig-molecule 'copy-chem-drawing)
  (dolist (slot '(options toolkit atoms bonds atom-pairs seen-atoms seen-bonds
                         entry-atom exit-atom exit-bond root bond-scale))
    (chem-ir-alias-accessor
     (intern (concat "tochemfig-molecule-" (symbol-name slot)))
     (intern (concat "chem-drawing-" (symbol-name slot))))))

(provide 'chem-ir)
;;; chem-ir.el ends here
