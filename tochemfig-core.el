;;; tochemfig-core.el --- Chemfig code generator using Indigo -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2025 Giovanni Crisalfi

;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module provides the core chemfig code generation functionality
;; using the emacs-indigo library for molecular structure manipulation.
;; It implements the mol2chemfig algorithm in pure Elisp.

;;; Code:

(require 'cl-lib)
(require 'indigo)

;;; Data Structures

(cl-defstruct (tochemfig-atom (:constructor tochemfig-atom-create))
  "Represents an atom in a molecule for chemfig generation."
  idx           ; Integer index in molecule
  symbol        ; Element symbol string ("C", "N", etc.)
  x y           ; 2D coordinates
  charge        ; Integer charge
  hydrogens     ; Implicit hydrogen count
  neighbors     ; List of neighbor atom indices
  options)      ; Plist of rendering options

(cl-defstruct (tochemfig-bond (:constructor tochemfig-bond-create))
  "Represents a bond between atoms for chemfig generation."
  idx           ; Bond index
  start-idx     ; Source atom index
  end-idx       ; Destination atom index
  order         ; Bond order (:single, :double, :triple, :aromatic)
  stereo        ; Stereochemistry (:none, :up, :down, :either)
  angle         ; Computed angle in degrees
  length        ; Computed length
  is-trunk      ; Boolean: part of the main chain?
  is-ring-bond  ; Boolean: closes a ring?
  ring-id)      ; Ring identifier for ring closure

;;; Constants

(defconst tochemfig--default-bond-length 1.0
  "Default bond length for chemfig output.")

(defconst tochemfig--angle-tolerance 0.5
  "Tolerance for angle comparison (degrees).")

;;; Utility Functions

(defun tochemfig--rad-to-deg (radians)
  "Convert RADIANS to degrees."
  (* radians (/ 180.0 float-pi)))

(defun tochemfig--deg-to-rad (degrees)
  "Convert DEGREES to radians."
  (* degrees (/ float-pi 180.0)))

(defun tochemfig--compute-angle (x1 y1 x2 y2)
  "Compute angle in degrees from point (X1,Y1) to point (X2,Y2)."
  (let ((dx (- x2 x1))
        (dy (- y2 y1)))
    (tochemfig--rad-to-deg (atan dy dx))))

(defun tochemfig--compute-distance (x1 y1 x2 y2)
  "Compute distance between point (X1,Y1) and point (X2,Y2)."
  (let ((dx (- x2 x1))
        (dy (- y2 y1)))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun tochemfig--round-angle (angle)
  "Round ANGLE to nearest integer if close, otherwise one decimal."
  (let ((rounded (round angle)))
    (if (< (abs (- angle rounded)) tochemfig--angle-tolerance)
        rounded
      (/ (round (* angle 10)) 10.0))))

(defun tochemfig--format-number (num)
  "Format NUM for chemfig output, omitting trailing zeros."
  (if (integerp num)
      (number-to-string num)
    (let ((str (format "%.2f" num)))
      ;; Remove trailing zeros and decimal point if not needed
      (replace-regexp-in-string "\\.?0+$" "" str))))

;;; Atom Parsing

(defun tochemfig--parse-atoms (mol)
  "Extract atoms from Indigo molecule MOL.
Returns a hash table mapping atom index to tochemfig-atom struct."
  (let ((atoms (make-hash-table :test 'equal)))
    (indigo-layout mol)
    (indigo-with-atoms-iterator (iter mol)
      (let ((atom (indigo-next iter)))
        (while atom
          (let* ((idx (indigo-index atom))
                 (symbol (indigo-symbol atom))
                 (coords (indigo-xyz atom))
                 (x (car coords))
                 (y (cadr coords))
                 (charge (indigo-charge atom))
                 (hydrogens (indigo-count-implicit-hydrogens atom)))
            (puthash idx
                     (tochemfig-atom-create
                      :idx idx
                      :symbol symbol
                      :x x
                      :y y
                      :charge charge
                      :hydrogens hydrogens
                      :neighbors nil
                      :options nil)
                     atoms))
          (setq atom (indigo-next iter)))))
    atoms))

;;; Bond Parsing

(defun tochemfig--parse-bonds (mol atoms)
  "Extract bonds from Indigo molecule MOL using ATOMS hash table.
Returns a hash table mapping bond index to tochemfig-bond struct,
and updates neighbor lists in ATOMS."
  (let ((bonds (make-hash-table :test 'equal)))
    (indigo-with-bonds-iterator (iter mol)
      (let ((bond (indigo-next iter)))
        (while bond
          (let* ((bond-idx (indigo-index bond))
                 (src-atom (indigo-source bond))
                 (dst-atom (indigo-destination bond))
                 (src-idx (indigo-index src-atom))
                 (dst-idx (indigo-index dst-atom))
                 (order (indigo-bond-order bond))
                 (stereo (indigo-bond-stereo bond))
                 ;; Get coordinates for angle/length calculation
                 (src-struct (gethash src-idx atoms))
                 (dst-struct (gethash dst-idx atoms))
                 (angle (tochemfig--compute-angle
                         (tochemfig-atom-x src-struct)
                         (tochemfig-atom-y src-struct)
                         (tochemfig-atom-x dst-struct)
                         (tochemfig-atom-y dst-struct)))
                 (length (tochemfig--compute-distance
                          (tochemfig-atom-x src-struct)
                          (tochemfig-atom-y src-struct)
                          (tochemfig-atom-x dst-struct)
                          (tochemfig-atom-y dst-struct))))
            ;; Create bond structure
            (puthash bond-idx
                     (tochemfig-bond-create
                      :idx bond-idx
                      :start-idx src-idx
                      :end-idx dst-idx
                      :order order
                      :stereo stereo
                      :angle angle
                      :length length
                      :is-trunk nil
                      :is-ring-bond nil
                      :ring-id nil)
                     bonds)
            ;; Update neighbor lists
            (let ((src-neighbors (tochemfig-atom-neighbors src-struct))
                  (dst-neighbors (tochemfig-atom-neighbors dst-struct)))
              (setf (tochemfig-atom-neighbors src-struct)
                    (cons dst-idx src-neighbors))
              (setf (tochemfig-atom-neighbors dst-struct)
                    (cons src-idx dst-neighbors))))
          (setq bond (indigo-next iter)))))
    bonds))

;;; Bond Lookup

(defun tochemfig--find-bond (bonds start-idx end-idx)
  "Find bond in BONDS hash connecting START-IDX to END-IDX.
Returns the bond struct or nil."
  (catch 'found
    (maphash (lambda (_idx bond)
               (when (or (and (= (tochemfig-bond-start-idx bond) start-idx)
                              (= (tochemfig-bond-end-idx bond) end-idx))
                         (and (= (tochemfig-bond-start-idx bond) end-idx)
                              (= (tochemfig-bond-end-idx bond) start-idx)))
                 (throw 'found bond)))
             bonds)
    nil))

;;; Tree Building (DFS traversal)

(cl-defstruct (tochemfig-tree-node (:constructor tochemfig-tree-node-create))
  "A node in the traversal tree for chemfig generation."
  atom-idx      ; Index of the atom at this node
  bond          ; Bond used to reach this node (nil for root)
  angle         ; Angle of incoming bond
  children      ; List of child nodes
  is-trunk      ; Part of main chain?
  ring-closures) ; List of ring-ids that this atom participates in

(defun tochemfig--build-tree (atoms bonds entry-idx)
  "Build a spanning tree from ATOMS and BONDS starting at ENTRY-IDX.
Returns the root tochemfig-tree-node.
Ring closures are tracked and both ends are marked with the same ring-id."
  (let ((visited (make-hash-table :test 'equal))
        (ring-counter 0)
        (ring-closures-map (make-hash-table :test 'equal)) ; atom-idx -> list of ring-ids
        (node-map (make-hash-table :test 'equal))) ; atom-idx -> node
    ;; First pass: build tree structure and detect rings
    (cl-labels
        ((dfs (atom-idx parent-idx incoming-bond)
           (puthash atom-idx t visited)
           (let* ((atom (gethash atom-idx atoms))
                  (neighbors (tochemfig-atom-neighbors atom))
                  (children nil))
             ;; Process each neighbor
             (dolist (neighbor-idx neighbors)
               (unless (eq neighbor-idx parent-idx)
                 (let ((bond (tochemfig--find-bond bonds atom-idx neighbor-idx)))
                   (if (gethash neighbor-idx visited)
                       ;; Ring closure - already visited
                       (unless (tochemfig-bond-ring-id bond)
                         (cl-incf ring-counter)
                         (setf (tochemfig-bond-is-ring-bond bond) t)
                         (setf (tochemfig-bond-ring-id bond) ring-counter)
                         ;; Mark both ends with this ring closure
                         (push ring-counter (gethash atom-idx ring-closures-map))
                         (push ring-counter (gethash neighbor-idx ring-closures-map)))
                     ;; New node - recurse
                     (let ((child-node (dfs neighbor-idx atom-idx bond)))
                       (push child-node children))))))
             ;; Create node
             (let ((node (tochemfig-tree-node-create
                          :atom-idx atom-idx
                          :bond incoming-bond
                          :angle (when incoming-bond
                                   (if (= (tochemfig-bond-start-idx incoming-bond) parent-idx)
                                       (tochemfig-bond-angle incoming-bond)
                                     (let ((a (tochemfig-bond-angle incoming-bond)))
                                       (if (>= a 0) (- a 180) (+ a 180)))))
                          :children (nreverse children)
                          :is-trunk nil
                          :ring-closures nil)))
               (puthash atom-idx node node-map)
               node))))
      (let ((root (dfs entry-idx nil nil)))
        ;; Second pass: assign ring closures to nodes
        (maphash (lambda (atom-idx ring-ids)
                   (let ((node (gethash atom-idx node-map)))
                     (when node
                       (setf (tochemfig-tree-node-ring-closures node)
                             (nreverse ring-ids)))))
                 ring-closures-map)
        root))))

;;; Trunk Marking

(defun tochemfig--mark-trunk (node exit-idx)
  "Mark trunk path in tree NODE leading to EXIT-IDX.
Returns t if this node or a descendant contains exit-idx."
  (if (= (tochemfig-tree-node-atom-idx node) exit-idx)
      (progn
        (setf (tochemfig-tree-node-is-trunk node) t)
        t)
    (let ((found nil))
      (dolist (child (tochemfig-tree-node-children node))
        (when (tochemfig--mark-trunk child exit-idx)
          (setq found t)))
      (when found
        (setf (tochemfig-tree-node-is-trunk node) t))
      found)))

;;; Entry/Exit Selection

(defun tochemfig--pick-entry-exit (atoms)
  "Choose entry and exit atoms from ATOMS hash table.
Returns (entry-idx . exit-idx) cons cell.
Prefers terminal atoms (degree 1) for natural chain drawing."
  (let ((terminals nil)
        (first-idx nil))
    (maphash (lambda (idx atom)
               (unless first-idx (setq first-idx idx))
               (when (= (length (tochemfig-atom-neighbors atom)) 1)
                 (push idx terminals)))
             atoms)
    (cond
     ;; Two or more terminals: use first two
     ((>= (length terminals) 2)
      (cons (car terminals) (cadr terminals)))
     ;; One terminal: use it as entry, pick any other as exit
     ((= (length terminals) 1)
      (let ((entry (car terminals))
            (exit first-idx))
        (when (= entry exit)
          ;; Find another atom
          (maphash (lambda (idx _atom)
                     (unless (= idx entry)
                       (setq exit idx)))
                   atoms))
        (cons entry exit)))
     ;; No terminals (cyclic): use first two atoms
     (t
      (let ((second-idx first-idx))
        (maphash (lambda (idx _atom)
                   (unless (= idx first-idx)
                     (setq second-idx idx)))
                 atoms)
        (cons first-idx second-idx))))))

;;; Bond Scaling

(defun tochemfig--scale-bonds (bonds scale-mode stretch-factor)
  "Scale bond lengths in BONDS according to SCALE-MODE and STRETCH-FACTOR.
SCALE-MODE is one of: `keep', `scale', `normalize'.
Modifies bonds in place."
  (pcase scale-mode
    ('keep nil) ; Do nothing
    ('scale
     ;; Multiply all lengths by stretch-factor
     (maphash (lambda (_idx bond)
                (setf (tochemfig-bond-length bond)
                      (* (tochemfig-bond-length bond) stretch-factor)))
              bonds))
    ('normalize
     ;; Calculate average length, scale to target
     (let ((total 0.0)
           (count 0))
       (maphash (lambda (_idx bond)
                  (setq total (+ total (tochemfig-bond-length bond)))
                  (setq count (1+ count)))
                bonds)
       (when (> count 0)
         (let* ((avg (/ total count))
                (factor (if (> avg 0) (/ stretch-factor avg) 1.0)))
           (maphash (lambda (_idx bond)
                      (setf (tochemfig-bond-length bond)
                            (* (tochemfig-bond-length bond) factor)))
                    bonds)))))))

;;; Chemfig Rendering

(defun tochemfig--bond-symbol (order stereo fancy)
  "Return chemfig bond symbol for ORDER and STEREO.
FANCY enables fancier double/triple bond rendering (currently unused)."
  (ignore fancy) ; Reserved for future use
  (pcase order
    (:single
     (pcase stereo
       (:up "<")
       (:down "<:")
       (:either "<|")
       (_ "-")))
    (:double "=")
    (:triple "~")
    (:aromatic "-")
    (_ "-")))

(defun tochemfig--render-atom-label (atom options)
  "Render atom label for ATOM according to OPTIONS."
  (let* ((symbol (tochemfig-atom-symbol atom))
         (charge (tochemfig-atom-charge atom))
         (hydrogens (tochemfig-atom-hydrogens atom))
         (show-carbons (plist-get options :show-carbons))
         (show-methyls (plist-get options :show-methyls))
         (atom-numbers (plist-get options :atom-numbers))
         (is-carbon (string= symbol "C"))
         (is-methyl (and is-carbon (= hydrogens 3))))
    (cond
     ;; Show atom numbers mode
     (atom-numbers
      (number-to-string (tochemfig-atom-idx atom)))
     ;; Carbon handling
     ((and is-carbon (not show-carbons) (not (and is-methyl show-methyls)))
      "") ; Hide carbon
     ;; Show the atom
     (t
      (let ((label symbol))
        ;; Add hydrogens
        (when (and (> hydrogens 0)
                   (not (string= symbol "C"))
                   (not show-carbons))
          (setq label
                (concat label "H"
                        (if (> hydrogens 1)
                            (format "_{%d}" hydrogens)
                          ""))))
        ;; Add charge
        (when (and charge (/= charge 0))
          (let ((charge-str (cond
                              ((= charge 1) "+")
                              ((= charge -1) "-")
                              ((> charge 0) (format "%d+" charge))
                              (t (format "%d-" (abs charge))))))
            (setq label (concat label "^{" charge-str "}"))))
        label)))))

(defun tochemfig--render-bond (bond _atoms options relative-angles last-angle)
  "Render bond specification for BOND.
_ATOMS is the atom hash table (unused, kept for API consistency).
OPTIONS is plist of rendering options.
RELATIVE-ANGLES if non-nil uses relative angle notation.
LAST-ANGLE is the previous bond angle for relative calculation.
Returns (bond-spec . new-angle) cons."
  (let* ((angle (tochemfig-bond-angle bond))
         (length (tochemfig-bond-length bond))
         (order (tochemfig-bond-order bond))
         (stereo (tochemfig-bond-stereo bond))
         (fancy (plist-get options :fancy-bonds))
         (bond-sym (tochemfig--bond-symbol order stereo fancy))
         (display-angle (if relative-angles
                            (- angle (or last-angle 0))
                          angle))
         (rounded-angle (tochemfig--round-angle display-angle))
         (rounded-length (tochemfig--round-angle length)))
    ;; Build bond specification
    ;; Note: Ring bonds are handled via ring-closures on nodes, not here
    (cons
     (cond
      ;; Standard bond with angle and length
      ((or (/= rounded-angle 0) (/= rounded-length tochemfig--default-bond-length))
       (if (= rounded-length tochemfig--default-bond-length)
           (format "%s[:%s]" bond-sym (tochemfig--format-number rounded-angle))
         (format "%s[:%s,%s]" bond-sym
                 (tochemfig--format-number rounded-angle)
                 (tochemfig--format-number rounded-length))))
      ;; Simple bond
      (t bond-sym))
     angle)))

(defun tochemfig--render-tree (node atoms bonds options &optional indent-level last-angle)
  "Render tree NODE to chemfig code.
ATOMS and BONDS are the molecule data structures.
OPTIONS is plist of rendering options.
INDENT-LEVEL controls indentation depth.
LAST-ANGLE is the angle of the previous bond.
Returns chemfig code string."
  (let* ((indent-level (or indent-level 0))
         (terse (plist-get options :terse))
         (indent-size (plist-get options :indent))
         (relative-angles (plist-get options :relative-angles))
         (atom (gethash (tochemfig-tree-node-atom-idx node) atoms))
         (atom-label (tochemfig--render-atom-label atom options))
         (children (tochemfig-tree-node-children node))
         (ring-closures (tochemfig-tree-node-ring-closures node))
         (newline (if terse "" "\n"))
         (indent (if terse "" (make-string (* indent-level indent-size) ?\s)))
         (result ""))
    ;; Add atom label
    (setq result atom-label)
    ;; Add ring closures (ring-closures is a list of ring-ids)
    (dolist (ring-id ring-closures)
      (setq result (concat result (format "?[%s]" ring-id))))
    ;; Process children
    (let ((num-children (length children))
          (child-idx 0)
          (current-angle last-angle))
      (dolist (child children)
        (let* ((child-bond (tochemfig-tree-node-bond child))
               (bond-render (tochemfig--render-bond
                             child-bond atoms options
                             relative-angles current-angle))
               (bond-spec (car bond-render))
               (new-angle (cdr bond-render))
               (is-branch (> num-children 1))
               (is-last-child (= child-idx (1- num-children)))
               (child-result (tochemfig--render-tree
                              child atoms bonds options
                              (1+ indent-level) new-angle)))
          (if (and is-branch (not is-last-child))
              ;; Branch: wrap in parentheses
              (setq result
                    (concat result
                            newline indent "("
                            bond-spec
                            child-result
                            ")"))
            ;; Main chain or last branch
            (setq result
                  (concat result
                          bond-spec
                          child-result)))
          (setq current-angle new-angle)
          (cl-incf child-idx))))
    result))

;;; High-level API

(defun tochemfig--apply-transforms (atoms options)
  "Apply coordinate transforms to ATOMS based on OPTIONS.
Handles rotation, flip, and flop."
  (let ((angle (or (plist-get options :angle) 0))
        (flip (plist-get options :flip))
        (flop (plist-get options :flop)))
    ;; Apply rotation
    (when (/= angle 0)
      (let ((rad (tochemfig--deg-to-rad angle)))
        (maphash (lambda (_idx atom)
                   (let* ((x (tochemfig-atom-x atom))
                          (y (tochemfig-atom-y atom))
                          (cos-a (cos rad))
                          (sin-a (sin rad))
                          (new-x (- (* x cos-a) (* y sin-a)))
                          (new-y (+ (* x sin-a) (* y cos-a))))
                     (setf (tochemfig-atom-x atom) new-x)
                     (setf (tochemfig-atom-y atom) new-y)))
                 atoms)))
    ;; Apply flip (horizontal mirror)
    (when flip
      (maphash (lambda (_idx atom)
                 (setf (tochemfig-atom-x atom)
                       (- (tochemfig-atom-x atom))))
               atoms))
    ;; Apply flop (vertical mirror)
    (when flop
      (maphash (lambda (_idx atom)
                 (setf (tochemfig-atom-y atom)
                       (- (tochemfig-atom-y atom))))
               atoms))))

(defun tochemfig--recalculate-bond-angles (atoms bonds)
  "Recalculate bond angles in BONDS after coordinate transforms on ATOMS."
  (maphash (lambda (_idx bond)
             (let* ((src (gethash (tochemfig-bond-start-idx bond) atoms))
                    (dst (gethash (tochemfig-bond-end-idx bond) atoms))
                    (new-angle (tochemfig--compute-angle
                                (tochemfig-atom-x src)
                                (tochemfig-atom-y src)
                                (tochemfig-atom-x dst)
                                (tochemfig-atom-y dst))))
               (setf (tochemfig-bond-angle bond) new-angle)))
           bonds))

(defun tochemfig-generate (mol options)
  "Generate chemfig code for Indigo molecule MOL with OPTIONS plist.

OPTIONS can include:
  :angle           - Rotation angle in degrees (default 0)
  :flip            - Flip horizontally (default nil)
  :flop            - Flip vertically (default nil)
  :show-carbons    - Show C labels (default nil)
  :show-methyls    - Show CH3 labels (default nil)
  :fancy-bonds     - Fancy double/triple bonds (default nil)
  :atom-numbers    - Show atom numbers instead of labels (default nil)
  :relative-angles - Use relative angles (default nil)
  :terse           - Compact output (default nil)
  :indent          - Indentation spaces (default 4)
  :bond-scale      - Scale mode: keep/scale/normalize (default normalize)
  :bond-stretch    - Scale factor or target length (default 1.0)
  :wrap-chemfig    - Wrap in \\chemfig{} (default nil)
  :submol-name     - Wrap as \\definesubmol (default nil)
  :entry-atom      - Override entry atom index
  :exit-atom       - Override exit atom index

Returns chemfig code as a string."
  (let* (;; Parse molecule structure
         (atoms (tochemfig--parse-atoms mol))
         (bonds (tochemfig--parse-bonds mol atoms))
         ;; Apply transforms
         (_ (tochemfig--apply-transforms atoms options))
         (_ (tochemfig--recalculate-bond-angles atoms bonds))
         ;; Scale bonds
         (scale-mode (or (plist-get options :bond-scale) 'normalize))
         (stretch (or (plist-get options :bond-stretch) 1.0))
         (_ (tochemfig--scale-bonds bonds scale-mode stretch))
         ;; Pick entry/exit
         (entry-exit (tochemfig--pick-entry-exit atoms))
         (entry-idx (or (plist-get options :entry-atom) (car entry-exit)))
         (exit-idx (or (plist-get options :exit-atom) (cdr entry-exit)))
         ;; Build tree
         (tree (tochemfig--build-tree atoms bonds entry-idx))
         (_ (tochemfig--mark-trunk tree exit-idx))
         ;; Render
         (render-options (list :show-carbons (plist-get options :show-carbons)
                               :show-methyls (plist-get options :show-methyls)
                               :fancy-bonds (plist-get options :fancy-bonds)
                               :atom-numbers (plist-get options :atom-numbers)
                               :relative-angles (plist-get options :relative-angles)
                               :terse (plist-get options :terse)
                               :indent (or (plist-get options :indent) 4)))
         (chemfig-code (tochemfig--render-tree tree atoms bonds render-options)))
    ;; Wrap output
    (cond
     ((plist-get options :submol-name)
      (format "\\definesubmol{%s}{%s}"
              (plist-get options :submol-name)
              chemfig-code))
     ((plist-get options :wrap-chemfig)
      (format "\\chemfig{%s}" chemfig-code))
     (t chemfig-code))))

(provide 'tochemfig-core)
;;; tochemfig-core.el ends here
