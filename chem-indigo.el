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
(require 'chem-geometry)
(require 'chem-exp-chemfig-mappings)

(unless (macrop 'indigo-with-allene-centers-iterator)
  (error "chem-mode requires emacs-indigo (>= 0.13.0), but the \
`indigo-with-allene-centers-iterator' macro is unavailable.  Install or upgrade \
emacs-indigo (e.g. `M-x straight-pull-package RET indigo' or `doom sync -u') \
and rebuild chem-mode"))

;;; Parsing

(defun chem--bond-endpoints (bond)
  "Return the (SOURCE . DESTINATION) atom indices of BOND."
  (let ((source (indigo-source bond))
        (destination (indigo-destination bond)))
    (unwind-protect
        (cons (indigo-index source) (indigo-index destination))
      (when source (indigo-free source))
      (when destination (indigo-free destination)))))

(defun chem--parse-atoms (options mol)
  "Extract atoms from Indigo molecule MOL into a hash table."
  (let ((atoms (make-hash-table :test 'equal))
        (strict (plist-get options :strict)))
    (indigo-with-atoms-iterator (iter mol)
      (indigo-map
       (lambda (atom)
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
                (neighbors
                 (indigo-with-neighbors-iterator (nit atom)
                   (indigo-map #'indigo-index nit))))
           (puthash idx
                    (chem-atom-create
                     :idx idx
                     :x (nth 0 xyz) :y (nth 1 xyz)
                     :element element :hydrogens hydrogens :charge charge
                     :radical radical :neighbors neighbors
                     :marker (when marker-opt (format "%s%d" marker-opt (1+ idx)))
                     :bond-angles nil)
                    atoms)
           nil))
       iter))
    atoms))

(defun chem--parse-bonds (options mol edges edge-lookup)
  "Extract bonds from MOL into canonical EDGES and EDGE-LOOKUP.
OPTIONS supplies marker settings.  Returns atom-index pairs in input order."
  (let ((pairs nil))
    (indigo-with-bonds-iterator (iter mol)
      (indigo-map
       (lambda (bond)
         (let* ((endpoints (chem--bond-endpoints bond))
                (start (car endpoints))
                (end (cdr endpoints))
                (order (or (indigo-bond-order bond) :query))
                (stereo (or (indigo-bond-stereo bond) :none))
                (a (min start end))
                (b-index (max start end))
                (id (hash-table-count edges))
                (marker-opt (plist-get options :markers))
                (edge (chem-edge-create
                       :id id :a a :b b-index
                       :order order
                       :stereo (chem--canonical-stereo stereo start end)
                       :stereo-center (when (memq stereo '(:up :down)) start)
                       :link-p nil :cross-p nil
                       :marker (if marker-opt
                                   (format "%s%d-%d" marker-opt
                                           (1+ a) (1+ b-index))
                                 "")
                       :decoration 'plain)))
           (puthash id edge edges)
           (puthash (cons a b-index) edge edge-lookup)
           (push (cons start end) pairs)
           nil))
       iter))
    (nreverse pairs)))

;;; Stereocenters

(defun chem--collect-stereocenters (mol atoms edges)
  "Collect stereocenter descriptors from MOL into ATOMS.
EDGES maps canonical edge ids to `chem-edge' records.  The collected
data contains atom indices, keywords, integers, and lists, never
Indigo handles.  CIP computation is best-effort: Indigo rejects some
complex molecules, leaving their descriptors at `:none'."
  (ignore-errors (indigo-add-cip-stereo-descriptors mol))
  (let ((wedges (make-hash-table :test 'eql)))
    (maphash
     (lambda (_ edge)
       (let ((center (chem-edge-stereo-center edge)))
         (when (and center (memq (chem-edge-stereo edge) '(:up :down)))
           (let ((current (gethash center wedges)))
             (when (or (null current)
                       (< (chem-edge-id edge) (chem-edge-id current)))
               (puthash center edge wedges))))))
     edges)
    (indigo-with-stereocenters-iterator (iter mol)
      (indigo-map
       (lambda (atom)
         (let* ((idx (indigo-index atom))
                (center (gethash idx atoms))
                (wedge (gethash idx wedges)))
           (when center
             (let ((record
                    (chem-stereocenter-create
                     :type (indigo-stereocenter-type atom)
                     :group (indigo-stereocenter-group atom)
                     :pyramid (indigo-stereocenter-pyramid atom)
                     :cip (indigo-stereocenter-cip-descriptor atom)
                     :wedge-edge (when wedge (chem-edge-id wedge))
                     :configuration nil)))
               (setf (chem-atom-stereocenter center) record)
               (when wedge
                 (setf (chem-stereocenter-configuration record)
                       (chem-stereocenter-sign atoms idx wedge)))))
           nil))
       iter))
    atoms))

;;; Allene centers

(defun chem--allene-substituents (atoms center terminal)
  "Return TERMINAL's substituents, excluding CENTER.
Return two atom indices, using -1 for an implicit hydrogen.  Return
nil when TERMINAL does not have one or two explicit substituents."
  (let ((others (cl-remove center
                           (chem-atom-neighbors (gethash terminal atoms))
                           :test #'=)))
    (pcase (length others)
      (2 others)
      (1 (append others (list -1)))
      (_ nil))))

(defun chem--collect-allenes (mol state)
  "Collect allene centers from MOL into STATE's atoms.
The recorded data contains atom indices and canonical edge ids, never
Indigo handles."
  (let ((atoms (chem-drawing-atoms state)))
    (indigo-with-allene-centers-iterator (iter mol)
      (indigo-map
       (lambda (atom)
         (let* ((idx (indigo-index atom))
                (center (gethash idx atoms)))
           (when center
             (let* ((terminals
                     (cl-remove-if-not
                      (lambda (neighbor)
                        (let ((edge (chem-get-edge state idx neighbor)))
                          (and edge (eq :double (chem-edge-order edge)))))
                      (chem-atom-neighbors center)))
                    (substituents
                     (mapcar (lambda (terminal)
                               (chem--allene-substituents atoms idx terminal))
                             terminals)))
               (when (and (= 2 (length terminals))
                          (cl-every #'identity substituents))
                 (let ((wedges nil))
                   (cl-loop for terminal in terminals
                            for subs in substituents
                            do (dolist (sub subs)
                                 (unless (= -1 sub)
                                   (let ((edge (chem-get-edge
                                                state terminal sub)))
                                     (when (and edge
                                                (memq (chem-edge-stereo edge)
                                                      '(:up :down)))
                                       (push (chem-edge-id edge) wedges))))))
                   (setf (chem-atom-allene center)
                         (chem-allene-create
                          :center idx
                          :terminals terminals
                          :substituents substituents
                          :wedge-edges (sort (delete-dups wedges) #'<)
                          :configuration nil))))))
           nil))
       iter))
    atoms))

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

(defun chem--link-atoms (mol x y)
  "Connect atoms X and Y of MOL with an invisible link edge."
  (let* ((a (min x y))
         (b (max x y))
         (id (hash-table-count (chem-drawing-edges mol)))
         (edge (chem-edge-create
                :id id :a a :b b
                :order :single :stereo :none
                :link-p t :cross-p nil
                :marker "" :decoration 'plain)))
    (puthash id edge (chem-drawing-edges mol))
    (puthash (cons a b) edge (chem-drawing-edge-lookup mol))
    (let ((start (gethash x (chem-drawing-atoms mol)))
          (end (gethash y (chem-drawing-atoms mol))))
      (setf (chem-atom-neighbors start)
            (append (chem-atom-neighbors start) (list y)))
      (setf (chem-atom-neighbors end)
            (append (chem-atom-neighbors end) (list x))))))

(defun chem--connect-fragments (mol pairs)
  "Connect fragments, if any, with invisible bonds.
PAIRS lists the graph edges."
  (let ((fragments (chem--molecule-fragments pairs)))
    (when (> (length fragments) 1)
      (let ((fs fragments))
        (while (cdr fs)
          (let* ((head (car fs)) (tail (cadr fs))
                 (head-last (cdr (car (last head))))
                 (tail-first (car (car tail))))
            (chem--link-atoms mol head-last tail-first))
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
            (chem--link-atoms mol anchor atom)))))))

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
              (chem-node-create :edge-id nil :from nil :to end-idx)
            (if (or (gethash (cons start-idx end-idx) seen-bonds)
                    (gethash (cons end-idx start-idx) seen-bonds))
                (cl-return-from chem--parse-tree nil)
              (let* ((edge (chem-get-edge mol start-idx end-idx))
                     (closure-p (gethash end-idx seen-atoms))
                     (node (chem-node-create
                            :edge-id (chem-edge-id edge)
                            :from start-idx :to end-idx
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

(defun chem--tree-node-for-pair (mol from to)
  "Return the traversal node directed FROM -> TO, or nil."
  (cl-find-if (lambda (node)
                (and (= (chem-node-from node) from)
                     (= (chem-node-to node) to)))
              (chem-tree-nodes mol)))

(defun chem--process-cross-bonds (mol options seen-bonds)
  "Turn declared cross bonds into branches off the exit node.
OPTIONS supplies the cross-bond list; SEEN-BONDS records traversal direction."
  (let ((cross-bonds (plist-get options :cross-bond))
        (exit-node (chem-drawing-exit-node mol))
        (exit-idx (chem-atom-idx (chem-drawing-exit-atom mol))))
    (dolist (pair cross-bonds)
      (let* ((start (1- (car pair)))
             (end (1- (cadr pair)))
             (node (cond ((gethash (cons start end) seen-bonds)
                          (chem--tree-node-for-pair mol start end))
                         ((gethash (cons end start) seen-bonds)
                          (chem--tree-node-for-pair mol end start))
                         (t (error "bond %d-%d doesn't exist"
                                   (car pair) (cadr pair))))))
        (unless node
          (error "bond %d-%d is not in the traversal tree"
                 (car pair) (cadr pair)))
        (let ((edge-id (chem-node-edge-id node)))
          (if (and (chem-node-children exit-node)
                   (eq node (car (last (chem-node-children exit-node)))))
              (progn
                (setf (chem-node-cross-p node) t)
                (setf (chem-node-cross-last-p node) t))
            (setf (chem-node-link-p node) t
                  (chem-node-cross-p node) nil)
            (let ((overlay (chem-node-create
                            :edge-id edge-id
                            :from (chem-node-from node)
                            :to (chem-node-to node)
                            :parent exit-node
                            :cross-p t
                            :phantom-p t)))
              (if (not (= (chem-node-from node) exit-idx))
                  (let ((connector (chem-node-create
                                    :edge-id nil
                                    :from exit-idx
                                    :to (chem-node-from node)
                                    :parent exit-node
                                    :link-p t
                                    :phantom-p t)))
                    (setf (chem-node-parent overlay) connector)
                    (setf (chem-node-children connector) (list overlay))
                    (setf (chem-node-children exit-node)
                          (append (chem-node-children exit-node)
                                  (list connector))))
                (setf (chem-node-children exit-node)
                      (append (chem-node-children exit-node)
                              (list overlay))))))
          (let ((edge (chem-edge-at mol edge-id)))
            (when edge
              (setf (chem-edge-cross-p edge) t))))))))

(defun chem--scale-bonds (mol options)
  "Scale bond lengths according to OPTIONS."
  (let* ((mode (or (plist-get options :bond-scale) 'normalize))
         (stretch (or (plist-get options :bond-stretch) 1.0))
         (scale 1.0))
    (cond
     ((eq mode 'keep) nil)
     ((eq mode 'normalize)
      (let* ((lengths (mapcar (lambda (length)
                                (chem--num-round length
                                                 (or (plist-get options :bond-round) 3)))
                              (chem-tree-edge-lengths mol)))
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
      (chem-scale mol scale))))

(defun chem--ring-record (mol ring aromatic-p sequence)
  "Build an ordered `chem-ring' from Indigo RING.
AROMATIC-P is the Indigo aromatization result; SEQUENCE is the SSSR index."
  (let ((atoms nil)
        (anchor nil)
        (adjacency (make-hash-table :test 'eql)))
    (indigo-with-bonds-iterator (bit ring)
      (indigo-map
       (lambda (tk-bond)
         (let* ((endpoints (chem--bond-endpoints tk-bond))
                (start (car endpoints))
                (end (cdr endpoints))
                (edge (chem-get-edge mol start end)))
           (when edge
             (let ((id (chem-edge-id edge)))
               (setq anchor id)
               (cl-pushnew start atoms)
               (cl-pushnew end atoms)
               (push (cons end id) (gethash start adjacency))
               (push (cons start id) (gethash end adjacency)))))
         nil)
       bit))
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
      (indigo-map
       (lambda (ring)
         (let ((is-aromatic t))
           (indigo-with-bonds-iterator (bit ring)
             (indigo-map
              (lambda (tk-bond)
                (unless (eq (indigo-bond-order tk-bond) :aromatic)
                  (setq is-aromatic nil))
                nil)
              bit))
           (push (chem--ring-record mol ring is-aromatic sequence) records)
           (setq sequence (1+ sequence))
           nil))
       it))
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
                 :edges (make-hash-table :test 'eql)
                 :edge-lookup (make-hash-table :test 'equal)
                 :adjacency (make-hash-table :test 'eql))))
    (let* ((pairs (chem--parse-bonds options mol
                                     (chem-drawing-edges state)
                                     (chem-drawing-edge-lookup state)))
           (rotation (or (plist-get options :angle) 0.0)))
      (chem--collect-stereocenters mol atoms (chem-drawing-edges state))
      (chem--collect-allenes mol state)
      ;; Configuration signs are computed from the original coordinates.
      ;; Apply depiction transforms before deriving label angles.
      (when (plist-get options :flip)
        (chem-flip-horizontal state))
      (when (plist-get options :flop)
        (chem-flip-vertical state))
      ;; Record label-placement angles using the legacy rotation offset.
      (dolist (pair pairs)
        (let* ((start (car pair))
               (end (cdr pair))
               (a (gethash start atoms))
               (b (gethash end atoms))
               (ab (chem--compare-positions
                    (chem-atom-x a) (chem-atom-y a)
                    (chem-atom-x b) (chem-atom-y b)))
               (ba (chem--compare-positions
                    (chem-atom-x b) (chem-atom-y b)
                    (chem-atom-x a) (chem-atom-y a))))
          (setf (chem-atom-bond-angles a)
                (append (chem-atom-bond-angles a)
                        (list (+ (nth 1 ab) rotation))))
          (setf (chem-atom-bond-angles b)
                (append (chem-atom-bond-angles b)
                        (list (+ (nth 1 ba) rotation))))))
      (unless (zerop rotation)
        (chem-rotate state rotation))
      (chem--connect-fragments state pairs)
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
