;;; chem-indigo.el --- Indigo ingestion for chem-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2025 Giovanni Crisalfi

;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Reads an Indigo molecule into the drawing IR: parse atoms and bonds,
;; connect fragments, build the spanning tree, mark the trunk, scale bonds,
;; and annotate rings.  The result is a `chem-drawing' ready for an exporter.
;;
;; This is the only core file that depends on Indigo.  The algorithm is the
;; current mol2chemfigPy3-derived pipeline; it has not been rewritten.

;;; Code:

(require 'cl-lib)
(require 'indigo)
;; See the note in chem-indigo.el: the iterator macros live in `indigo-iter',
;; which indigo only loads once its module has loaded.  Require it explicitly so
;; compilation does not depend on the module.
(eval-when-compile (require 'indigo-mol))
(eval-when-compile (require 'indigo-iter))
(require 'chem-ir)
(require 'chem-bond)
(require 'chem-exp-chemfig-mappings)

(unless (macrop 'indigo-with-atoms-iterator)
  (error "chem-mode requires emacs-indigo (>= 0.10.1), but the \
`indigo-with-atoms-iterator' macro is unavailable.  Install or upgrade \
emacs-indigo (e.g. `M-x straight-pull-package RET indigo' or `doom sync -u') \
and rebuild chem-mode"))

;;; Parsing

(defun chem--parse-atoms (options mol)
  "Extract atoms from Indigo molecule MOL into a hash table."
  (let ((atoms (make-hash-table :test 'equal))
        (strict (plist-get options :strict)))
    (indigo-with-atoms-iterator (iter mol)
      (let ((atom (indigo-next iter)))
        (while atom
          (let* ((idx (indigo-index atom))
                 (element (indigo-symbol atom))
                 (hydrogens (condition-case nil
                                (indigo-count-implicit-hydrogens atom)
                              (error -1)))
                 (hydrogens (if (or (null hydrogens) (< hydrogens 0))
                                (if strict
                                    (error "Cannot calculate implicit hydrogens for atom %d" idx)
                                  0)
                              hydrogens))
                 (charge (or (indigo-charge atom) 0))
                 (radical (condition-case nil (indigo-radical-electrons atom) (error 0)))
                 (xyz (indigo-xyz atom))
                 (marker-opt (plist-get options :markers))
                 (neighbors nil))
            (indigo-with-neighbors-iterator (nit atom)
              (let ((n (indigo-next nit)))
                (while n (push (indigo-index n) neighbors) (setq n (indigo-next nit)))))
            (puthash idx
                     (chem-atom-create
                      :idx idx
                      :x (nth 0 xyz) :y (nth 1 xyz)
                      :element element :hydrogens hydrogens :charge charge
                      :radical radical :neighbors (nreverse neighbors)
                      :marker (when marker-opt (format "%s%d" marker-opt (1+ idx)))
                      :bond-angles nil)
                     atoms))
          (setq atom (indigo-next iter)))))
    atoms))

(defun chem--parse-bonds (options mol atoms edges edge-lookup)
  "Extract bonds from MOL into a hash keyed by (START . END).
Populate EDGES and EDGE-LOOKUP with one canonical edge per bond.
Returns (bonds . atom-pairs)."
  (let ((bonds (make-hash-table :test 'equal))
        (pairs nil))
    (indigo-with-bonds-iterator (iter mol)
      (let ((bond (indigo-next iter)))
        (while bond
          (let* ((start (indigo-index (indigo-source bond)))
                 (end (indigo-index (indigo-destination bond)))
                 (start-atom (gethash start atoms))
                 (end-atom (gethash end atoms))
                 (order (or (indigo-bond-order bond) :query))
                 (stereo (or (indigo-bond-stereo bond) :none))
                 (a (min start end))
                 (b-index (max start end))
                 (id (hash-table-count edges))
                 (edge (chem-edge-create
                        :id id :a a :b b-index
                        :order order
                        :stereo (chem--canonical-stereo stereo start end)
                        :link-p nil :cross-p nil
                        :marker nil
                        :decoration 'plain))
                 (b (chem-bond-create-from
                     options start-atom end-atom order stereo id))
                 (inverted (chem-bond-invert b)))
            (setf (chem-edge-marker edge) (chem-bond-marker b))
            (puthash id edge edges)
            (puthash (cons a b-index) edge edge-lookup)
            (puthash (cons start end) b bonds)
            (puthash (cons end start) inverted bonds)
            (push (cons start end) pairs))
          (setq bond (indigo-next iter)))))
    (cons bonds (nreverse pairs))))

;;; Fragment handling

(defun chem--split-pairs (pair-list)
  "Split PAIR-LIST into connected and unconnected pairs."
  (let* ((first (car pair-list))
         (rest0 (cdr pair-list))
         (connected-atoms (list (car first) (cdr first)))
         (connected-pairs (list first)))
    (cl-labels
        ((loop (rest)
           (let ((unconnected nil) (found nil))
             (dolist (r rest)
               (if (or (member (car r) connected-atoms)
                       (member (cdr r) connected-atoms))
                   (progn
                     (setq found t)
                     (unless (member (car r) connected-atoms) (push (car r) connected-atoms))
                     (unless (member (cdr r) connected-atoms) (push (cdr r) connected-atoms))
                     (push r connected-pairs))
                 (push r unconnected)))
             (if (not found)
                 (list (nreverse connected-pairs) (nreverse unconnected))
               (loop (nreverse unconnected))))))
      (loop rest0))))

(cl-defun chem--molecule-fragments (pairs)
  "Identify unconnected fragments in PAIRS."
  (cond
    ((null pairs) nil)
    ((null (cdr pairs)) (list pairs))
    (t (let ((fragments nil) (remaining pairs))
         (while t
           (let* ((split (chem--split-pairs remaining))
                  (connected (nth 0 split))
                  (rest (nth 1 split)))
             (push connected fragments)
             (if (null rest)
                 (cl-return-from chem--molecule-fragments (nreverse fragments))
               (setq remaining rest))))))))

(defun chem--link-atoms (mol options x y)
  "Connect atoms X and Y of MOL with an invisible link edge.
OPTIONS is the common option plist."
  (let* ((start (gethash x (chem-drawing-atoms mol)))
         (end (gethash y (chem-drawing-atoms mol)))
         (a (min x y))
         (b (max x y))
         (id (hash-table-count (chem-drawing-edges mol)))
         (edge (chem-edge-create
                :id id :a a :b b
                :order :single :stereo :none
                :link-p t :cross-p nil
                :marker "" :decoration 'plain))
         (bond (chem-bond-create-from options start end :single :none id)))
    (chem-bond-set-link bond)
    (let ((inverted (chem-bond-invert bond)))
      (puthash (cons x y) bond (chem-drawing-bonds mol))
      (puthash (cons y x) inverted (chem-drawing-bonds mol)))
    (puthash id edge (chem-drawing-edges mol))
    (puthash (cons a b) edge (chem-drawing-edge-lookup mol))
    (setf (chem-atom-neighbors start)
          (append (chem-atom-neighbors start) (list y)))
    (setf (chem-atom-neighbors end)
          (append (chem-atom-neighbors end) (list x)))))

(defun chem--connect-fragments (mol options pairs)
  "Connect fragments, if any, with invisible bonds.
OPTIONS is the common option plist; PAIRS lists the graph edges."
  (let ((fragments (chem--molecule-fragments pairs)))
    (when (> (length fragments) 1)
      (let ((fs fragments))
        (while (cdr fs)
          (let* ((head (car fs)) (tail (cadr fs))
                 (head-last (cdr (car (last head))))
                 (tail-first (car (car tail))))
            (chem--link-atoms mol options head-last tail-first))
          (setq fs (cdr fs)))))
    (let* ((atoms (chem-drawing-atoms mol))
           (bonded nil)
           (un-bonded nil))
      (dolist (pair pairs)
        (unless (member (car pair) bonded) (push (car pair) bonded))
        (unless (member (cdr pair) bonded) (push (cdr pair) bonded)))
      (maphash (lambda (idx _) (unless (member idx bonded) (push idx un-bonded))) atoms)
      (setq un-bonded (sort un-bonded #'<))
      (when un-bonded
        (let ((anchor (if fragments
                          (cdr (car (last (car (last fragments)))))
                        (prog1 (car un-bonded) (setq un-bonded (cdr un-bonded))))))
          (dolist (atom un-bonded)
            (chem--link-atoms mol options anchor atom)))))))

;;; Adjacency

(defun chem--build-adjacency (mol)
  "Populate MOL's adjacency from atom neighbors and canonical edges.
The neighbor order is the existing encounter order, so tree building and
the chemfig exporter remain byte-identical."
  (let ((adjacency (chem-drawing-adjacency mol))
        (lookup (chem-drawing-edge-lookup mol)))
    (clrhash adjacency)
    (maphash
     (lambda (idx atom)
       (let (entries)
         (dolist (neighbor (chem-atom-neighbors atom))
           (let ((edge (gethash (cons (min idx neighbor) (max idx neighbor))
                                lookup)))
             (when edge
               (cl-pushnew (cons (chem-edge-id edge) neighbor)
                           entries :test #'equal))))
         (puthash idx (nreverse entries) adjacency)))
     (chem-drawing-atoms mol))
    mol))

;;; Tree building

(defun chem--treebonds (mol)
  "Return all directed bonds in MOL's traversal, excluding the root."
  (chem-tree-bonds mol))

(defun chem--default-exit-node (mol)
  "Pick the traversal node farthest from the entry atom along the tree."
  (let ((scored nil)
        (entry-idx (chem-atom-idx (chem-drawing-entry-atom mol))))
    (dolist (node (chem-tree-nodes mol))
      (unless (chem-node-closure-p node)
        (let ((distance 0) (the-node node))
          (while (and the-node (not (eq (chem-node-to the-node) entry-idx)))
            (setq distance (1+ distance))
            (setq the-node (chem-node-parent the-node)))
          (push (list distance (length (chem-node-children node)) node) scored))))
    (setq scored (sort (nreverse scored)
                       (lambda (a b) (< (nth 0 a) (nth 0 b)))))
    (nth 2 (car (last scored)))))

(defun chem--pick-first-last-atoms (mol options)
  "Choose entry and exit atoms for MOL using OPTIONS."
  (let* ((atoms (chem-drawing-atoms mol))
         (entry-opt (plist-get options :entry-atom))
         (exit-opt (plist-get options :exit-atom))
         (entry (if entry-opt
                    (or (gethash (1- entry-opt) atoms)
                        (error "Invalid entry atom number"))
                  (car (sort (chem--hash-values atoms)
                             (lambda (a b)
                               (< (length (chem-atom-neighbors a))
                                  (length (chem-atom-neighbors b))))))))
         (exit (when exit-opt
                 (or (gethash (1- exit-opt) atoms)
                     (error "Invalid exit atom number")))))
    (setf (chem-drawing-entry-atom mol) entry)
    (setf (chem-drawing-exit-atom mol) exit)))

(defun chem--hash-values (hash)
  "Return the values of HASH ordered by integer key."
  (let ((pairs nil))
    (maphash (lambda (k v) (push (cons k v) pairs)) hash)
    (mapcar #'cdr (sort pairs (lambda (a b) (< (car a) (car b)))))))

(cl-defun chem--parse-tree (mol start-atom end-atom seen-atoms seen-bonds)
  "Recursively build the traversal tree for MOL.
SEEN-ATOMS and SEEN-BONDS record traversal state.
Returns the node, or nil when the bond was already seen."
  (let* ((end-idx (chem-atom-idx end-atom))
         (start-idx (when start-atom (chem-atom-idx start-atom)))
         (node
          (if (null start-atom)
              (chem-node-create :bond nil :from nil :to end-idx)
            (if (or (gethash (cons start-idx end-idx) seen-bonds)
                    (gethash (cons end-idx start-idx) seen-bonds))
                (cl-return-from chem--parse-tree nil)
              (let* ((bond (gethash (cons start-idx end-idx)
                                    (chem-drawing-bonds mol)))
                     (closure-p (gethash end-idx seen-atoms))
                     (node (chem-node-create
                            :bond bond :from start-idx :to end-idx
                            :closure-p closure-p
                            :phantom-p closure-p)))
                (puthash (cons start-idx end-idx) t seen-bonds)
                (when closure-p
                  (cl-return-from chem--parse-tree node))
                node)))))
    (puthash end-idx t seen-atoms)
    (when (eq end-atom (chem-drawing-exit-atom mol))
      (setf (chem-drawing-exit-node mol) node))
    (dolist (ni (chem-atom-neighbors end-atom))
      (unless (and start-atom (= ni start-idx))
        (let* ((next-atom (gethash ni (chem-drawing-atoms mol)))
               (next-node (chem--parse-tree mol end-atom next-atom
                                            seen-atoms seen-bonds)))
          (when next-node
            (setf (chem-node-parent next-node) node)
            (push next-node (chem-node-children node))))))
    ;; Children were pushed in reverse; restore neighbor order.
    (setf (chem-node-children node)
          (nreverse (chem-node-children node)))
    node))

;;; Cross bonds, scaling, rings

(defun chem--process-cross-bonds (mol options seen-bonds)
  "Turn declared cross bonds into branches off the exit node.
OPTIONS is the common option plist; SEEN-BONDS is the traversal state."
  (let ((cross-bonds (plist-get options :cross-bond))
        (seen seen-bonds)
        (bonds (chem-drawing-bonds mol))
        (exit-node (chem-drawing-exit-node mol)))
    (dolist (pair cross-bonds)
      (let* ((start (1- (car pair)))
             (end (1- (cadr pair)))
             (bond (cond ((gethash (cons start end) seen) (gethash (cons start end) bonds))
                         ((gethash (cons end start) seen) (gethash (cons end start) bonds))
                         (t (error "bond %d-%d doesn't exist" (car pair) (cadr pair)))))
             (node (chem-tree-node-for-bond mol bond)))
        (unless node
          (error "bond %d-%d is not in the traversal tree"
                 (car pair) (cadr pair)))
        (if (and (chem-node-children exit-node)
                 (eq node (car (last (chem-node-children exit-node)))))
            (progn
              (setf (chem-node-cross-p node) t)
              (setf (chem-node-cross-last-p node) t))
          (let ((bond-copy (copy-chem-bond bond)))
            (chem-bond-set-link bond)
            (setf (chem-node-cross-p node) nil)
            (let ((overlay (chem-node-create
                            :bond bond-copy
                            :from (chem-atom-idx (chem-bond-start-atom bond-copy))
                            :to (chem-atom-idx (chem-bond-end-atom bond-copy))
                            :parent exit-node
                            :cross-p t
                            :phantom-p t)))
              (if (not (eq (chem-bond-start-atom bond-copy)
                           (chem-drawing-exit-atom mol)))
                  (let* ((pseudo-bond (chem-bond-create-from
                                       options
                                       (chem-drawing-exit-atom mol)
                                       (chem-bond-start-atom bond-copy)
                                       :single :none))
                         (pseudo-node (chem-node-create
                                       :bond pseudo-bond
                                       :from (chem-atom-idx (chem-drawing-exit-atom mol))
                                       :to (chem-atom-idx (chem-bond-start-atom bond-copy))
                                       :parent exit-node
                                       :phantom-p t)))
                    (chem-bond-set-link pseudo-bond)
                    (setf (chem-node-parent overlay) pseudo-node)
                    (setf (chem-node-children pseudo-node) (list overlay))
                    (setf (chem-node-children exit-node)
                          (append (chem-node-children exit-node)
                                  (list pseudo-node))))
                (setf (chem-node-children exit-node)
                      (append (chem-node-children exit-node)
                              (list overlay)))))))
        (let ((edge (chem-edge-at mol (chem-bond-edge-id bond))))
          (when edge
            (setf (chem-edge-cross-p edge) t)))))))

(defun chem--scale-bonds (mol options)
  "Scale bond lengths according to OPTIONS."
  (let* ((mode (or (plist-get options :bond-scale) 'normalize))
         (stretch (or (plist-get options :bond-stretch) 1.0))
         (scale 1.0))
    (cond
     ((eq mode 'keep) nil)
     ((eq mode 'normalize)
      (let* ((lengths (mapcar (lambda (b)
                                (chem--num-round (chem-bond-length b)
                                                      (or (plist-get options :bond-round) 3)))
                              (chem--treebonds mol)))
             (counter (make-hash-table :test 'equal))
             (most-common nil))
        (dolist (l lengths) (puthash l (1+ (or (gethash l counter) 0)) counter))
        (let ((items nil))
          (maphash (lambda (k v) (push (cons k v) items)) counter)
          ;; Stable sort by count; last entry wins (mirrors Counter.most_common).
          (setq items (sort (nreverse items) (lambda (a b) (< (cdr a) (cdr b)))))
          (setq most-common (car (last items))))
        (setq scale (if (and most-common (/= (car most-common) 0))
                        (/ stretch (float (car most-common)))
                      1.0))))
     ((eq mode 'scale) (setq scale stretch)))
    (unless (= scale 1.0)
      (chem-scale mol scale)
      (chem-bond-refresh-geometry mol))))

(defun chem--ring-record (mol ring aromatic-p sequence)
  "Build an ordered `chem-ring' from Indigo RING.
AROMATIC-P is the Indigo aromatization result; SEQUENCE is the SSSR index."
  (let ((atoms nil)
        (anchor nil)
        (adjacency (make-hash-table :test 'eql)))
    (indigo-with-bonds-iterator (bit ring)
      (let ((tk-bond (indigo-next bit)))
        (while tk-bond
          (let* ((start (indigo-index (indigo-source tk-bond)))
                 (end (indigo-index (indigo-destination tk-bond)))
                 (edge (chem-get-edge mol start end)))
            (when edge
              (let ((id (chem-edge-id edge)))
                (setq anchor id)
                (cl-pushnew start atoms)
                (cl-pushnew end atoms)
                (push (cons end id) (gethash start adjacency))
                (push (cons start id) (gethash end adjacency)))))
          (setq tk-bond (indigo-next bit)))))
    (setq atoms (sort atoms #'<))
    (let* ((start (car atoms))
           (ordered-atoms (list start))
           (ordered-bonds nil)
           (previous nil)
           (current start)
           (guard 0))
      (while (and (< guard (1+ (length atoms)))
                  (not (and previous (eq current start))))
        (let* ((neighbors (gethash current adjacency))
               (candidates (if previous
                               (cl-remove-if (lambda (pair) (= (car pair) previous))
                                             neighbors)
                             neighbors)))
          (if (null candidates)
              (setq guard (1+ (length atoms)))
            (let ((next (car (sort candidates
                                   (lambda (a b) (< (car a) (car b)))))))
              (push (cdr next) ordered-bonds)
              (setq previous current)
              (setq current (car next))
              (unless (= current start)
                (setq ordered-atoms (append ordered-atoms (list current)))))))
        (setq guard (1+ guard)))
      (chem-ring-create :atoms ordered-atoms
                        :bonds (nreverse ordered-bonds)
                        :anchor anchor
                        :aromatic-p aromatic-p
                        :sequence sequence))))

(defun chem--ring-record-less-p (a b)
  "Return non-nil when ring record A sorts before B."
  (let ((a-atoms (chem-ring-atoms a))
        (b-atoms (chem-ring-atoms b)))
    (cond ((/= (length a-atoms) (length b-atoms))
           (< (length a-atoms) (length b-atoms)))
          (t (cl-loop for x in a-atoms
                      for y in b-atoms
                      when (/= x y)
                      return (< x y)
                      finally return nil)))))

(defun chem--record-rings (mol toolkit)
  "Record all SSSR rings of MOL from Indigo TOOLKIT."
  (indigo-aromatize toolkit)
  (let ((records nil)
        (sequence 0))
    (indigo-with-sssr-iterator (it toolkit)
      (let ((ring (indigo-next it)))
        (while ring
          (let ((is-aromatic t))
            (indigo-with-bonds-iterator (bit ring)
              (let ((tk-bond (indigo-next bit)))
                (while tk-bond
                  (unless (eq (indigo-bond-order tk-bond) :aromatic)
                    (setq is-aromatic nil))
                  (setq tk-bond (indigo-next bit)))))
            (push (chem--ring-record mol ring is-aromatic sequence) records)
            (setq sequence (1+ sequence)))
          (setq ring (indigo-next it)))))
    (setq records (nreverse records))
    (let ((ordered (sort (copy-sequence records) #'chem--ring-record-less-p)))
      (let ((id 0))
        (dolist (record ordered)
          (setf (chem-ring-id record) id)
          (setq id (1+ id))))
      (setf (chem-drawing-rings mol) ordered))))

;;; Pipeline

(defun chem-indigo-build (mol options)
  "Build a prepared `chem-drawing' for Indigo molecule MOL.
OPTIONS is the common option plist.  The drawing is ready to be rendered."
  (let* ((atoms (chem--parse-atoms options mol))
         (seen-atoms (make-hash-table :test 'equal))
         (seen-bonds (make-hash-table :test 'equal))
         (state (chem-drawing-create
                 :atoms atoms
                 :bonds (make-hash-table :test 'equal)
                 :edges (make-hash-table :test 'eql)
                 :edge-lookup (make-hash-table :test 'equal)
                 :adjacency (make-hash-table :test 'eql))))
    ;; Apply coordinate transforms before bonds are parsed.
    (when (plist-get options :flip)
      (chem-flip-horizontal state))
    (when (plist-get options :flop)
      (chem-flip-vertical state))
    (let ((rotation (or (plist-get options :angle) 0.0)))
      (unless (zerop rotation)
        (chem-rotate state rotation)))
    (let* ((parsed (chem--parse-bonds options mol atoms
                                      (chem-drawing-edges state)
                                      (chem-drawing-edge-lookup state)))
           (pairs (cdr parsed)))
      (setf (chem-drawing-bonds state) (car parsed))
      ;; Record the angle of every incident bond on each atom.
      (dolist (pair pairs)
        (let* ((start (car pair)) (end (cdr pair))
               (bonds (chem-drawing-bonds state)))
          (setf (chem-atom-bond-angles (gethash start atoms))
                (append (chem-atom-bond-angles (gethash start atoms))
                        (list (chem-bond-angle (gethash (cons start end) bonds)))))
          (setf (chem-atom-bond-angles (gethash end atoms))
                (append (chem-atom-bond-angles (gethash end atoms))
                        (list (chem-bond-angle (gethash (cons end start) bonds)))))))
      (chem--connect-fragments state options pairs)
      (chem--build-adjacency state)
      (chem--pick-first-last-atoms state options)
      (setf (chem-drawing-root state)
            (chem--parse-tree state nil (chem-drawing-entry-atom state)
                              seen-atoms seen-bonds))
      (when (> (hash-table-count atoms) 1)
        (when (null (chem-drawing-exit-atom state))
          (let ((exit-node (chem--default-exit-node state)))
            (when exit-node
              (setf (chem-drawing-exit-node state) exit-node)
              (setf (chem-drawing-exit-atom state)
                    (gethash (chem-node-to exit-node) atoms)))))
        (when (and (chem-drawing-exit-node state)
                   (not (eq (chem-drawing-entry-atom state)
                            (chem-drawing-exit-atom state))))
          (let ((flagged (chem-drawing-exit-node state))
                (entry-idx (chem-atom-idx (chem-drawing-entry-atom state))))
            (while (and flagged
                        (not (eq (chem-node-to flagged) entry-idx)))
              (setf (chem-node-trunk-p flagged) t)
              (setq flagged (chem-node-parent flagged)))))
        (when (plist-get options :cross-bond)
          (chem--process-cross-bonds state options seen-bonds))
        (chem--scale-bonds state options)
        (chem--record-rings state mol))
      (chem-apply-drawing-options state options)
      state)))

;;; Public entry points

(defun chem--prepare-mol (mol options)
  "Apply hydrogen handling and coordinate layout to MOL per OPTIONS."
  (pcase (plist-get options :hydrogens)
    ('add (indigo-unfold-hydrogens mol) (indigo-layout mol))
    ('delete (indigo-fold-hydrogens mol)))
  (when (or (not (indigo-has-coordinates mol))
            (plist-get options :recalculate-coordinates))
    (indigo-layout mol)))

(defun chem-from-molecule (mol options)
  "Prepare Indigo molecule MOL and build a drawing with OPTIONS."
  (chem--prepare-mol mol options)
  (chem-indigo-build mol options))

(defun chem-from-string (smiles options)
  "Build a drawing from SMILES with OPTIONS."
  (when (or (null smiles) (string-empty-p (string-trim smiles)))
    (user-error "Empty SMILES input"))
  (indigo-with-molecule (mol smiles)
    (chem-from-molecule mol options)))

(defun chem-from-file (path options)
  "Build a drawing from the molecule file at PATH with OPTIONS."
  (indigo-with-mol-file (mol path)
    (chem-from-molecule mol options)))

(provide 'chem-indigo)
;;; chem-indigo.el ends here
