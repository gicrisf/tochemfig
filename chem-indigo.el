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
                      :options options :idx idx
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

(cl-defun chem--molecule-fragments (mol)
  "Identify unconnected fragments in MOL."
  (let ((pairs (chem-drawing-atom-pairs mol)))
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
                (setq remaining rest)))))))))

(defun chem--link-atoms (mol x y)
  "Connect atoms X and Y of MOL with an invisible link edge."
  (let* ((options (chem-drawing-options mol))
         (start (gethash x (chem-drawing-atoms mol)))
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

(defun chem--connect-fragments (mol)
  "Connect fragments, if any, with invisible bonds."
  (let ((fragments (chem--molecule-fragments mol)))
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
      (dolist (pair (chem-drawing-atom-pairs mol))
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

(defun chem--treebonds (mol)
  "Return all bonds in MOL's tree, excluding the dummy first bond."
  (chem-tree-bonds mol))

(defun chem--get-bond (mol tk-bond)
  "Return the tree bond of MOL corresponding to TK-BOND."
  (let* ((start (indigo-index (indigo-source tk-bond)))
         (end (indigo-index (indigo-destination tk-bond))))
    (if (gethash (cons start end) (chem-drawing-seen-bonds mol))
        (gethash (cons start end) (chem-drawing-bonds mol))
      (gethash (cons end start) (chem-drawing-bonds mol)))))

(defun chem--default-exit-bond (mol)
  "Pick the bond farthest from the entry atom along the tree."
  (let ((scored nil)
        (entry (chem-drawing-entry-atom mol)))
    (dolist (bond (chem--treebonds mol))
      (unless (chem-bond-to-phantom bond)
        (let ((distance 0) (the-bond bond))
          (while (and the-bond (not (eq (chem-bond-end-atom the-bond) entry)))
            (setq distance (1+ distance))
            (setq the-bond (chem-bond-parent the-bond)))
          (push (list distance (length (chem-bond-descendants bond)) bond) scored))))
    (setq scored (sort (nreverse scored)
                       (lambda (a b) (< (nth 0 a) (nth 0 b)))))
    (nth 2 (car (last scored)))))

(defun chem--pick-first-last-atoms (mol)
  "Choose entry and exit atoms for MOL."
  (let* ((options (chem-drawing-options mol))
         (atoms (chem-drawing-atoms mol))
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

(cl-defun chem--parse-tree (mol start-atom end-atom)
  "Recursively build the spanning tree for MOL.
Returns the bond (or nil when the bond was already seen)."
  (let* ((options (chem-drawing-options mol))
         (end-idx (chem-atom-idx end-atom))
         (start-idx (when start-atom (chem-atom-idx start-atom)))
         (bond
          (if (null start-atom)
              (chem-bond-create
               :options options :kind 'dummy-first :start-atom nil :end-atom end-atom
               :bond-type nil :descendants nil
               :length nil :angle nil :marker "" :to-phantom nil
               :is-trunk nil :parent nil :clockwise 0)
            (if (or (gethash (cons start-idx end-idx) (chem-drawing-seen-bonds mol))
                    (gethash (cons end-idx start-idx) (chem-drawing-seen-bonds mol)))
                (cl-return-from chem--parse-tree nil)
              (let ((b (gethash (cons start-idx end-idx) (chem-drawing-bonds mol))))
                (puthash (cons start-idx end-idx) t (chem-drawing-seen-bonds mol))
                (when (gethash end-idx (chem-drawing-seen-atoms mol))
                  (setf (chem-bond-to-phantom b) t)
                  (cl-return-from chem--parse-tree b))
                b)))))
    (puthash end-idx t (chem-drawing-seen-atoms mol))
    (when (eq end-atom (chem-drawing-exit-atom mol))
      (setf (chem-drawing-exit-bond mol) bond))
    (dolist (ni (chem-atom-neighbors end-atom))
      (unless (and start-atom (= ni start-idx))
        (let* ((next-atom (gethash ni (chem-drawing-atoms mol)))
               (next-bond (chem--parse-tree mol end-atom next-atom)))
          (when next-bond
            (setf (chem-bond-parent next-bond) bond)
            (push next-bond (chem-bond-descendants bond))))))
    ;; Children were pushed in reverse; restore neighbor order.
    (setf (chem-bond-descendants bond)
          (nreverse (chem-bond-descendants bond)))
    bond))

;;; Cross bonds, scaling, rings

(defun chem--process-cross-bonds (mol)
  "Turn declared cross bonds into branches off the exit bond."
  (let ((cross-bonds (plist-get (chem-drawing-options mol) :cross-bond))
        (seen (chem-drawing-seen-bonds mol))
        (bonds (chem-drawing-bonds mol))
        (exit-bond (chem-drawing-exit-bond mol)))
    (dolist (pair cross-bonds)
      (let* ((start (1- (car pair)))
             (end (1- (cadr pair)))
             (bond (cond ((gethash (cons start end) seen) (gethash (cons start end) bonds))
                         ((gethash (cons end start) seen) (gethash (cons end start) bonds))
                         (t (error "bond %d-%d doesn't exist" (car pair) (cadr pair))))))
        (if (and (chem-bond-descendants exit-bond)
                 (eq bond (car (last (chem-bond-descendants exit-bond)))))
            (progn
              (chem-bond-set-cross bond t))
          (let ((bond-copy (copy-chem-bond bond)))
            (chem-bond-set-link bond)
            (chem-bond-set-cross bond-copy nil)
            (setf (chem-bond-to-phantom bond-copy) t)
            (setf (chem-bond-descendants bond-copy) nil)
            (if (not (eq (chem-bond-start-atom bond-copy)
                         (chem-drawing-exit-atom mol)))
                (let ((pseudo (chem-bond-create-from
                               (chem-drawing-options mol)
                               (chem-drawing-exit-atom mol)
                               (chem-bond-start-atom bond-copy) :single :none)))
                  (chem-bond-set-link pseudo)
                  (setf (chem-bond-to-phantom pseudo) t)
                  (setf (chem-bond-parent bond-copy) pseudo)
                  (setf (chem-bond-descendants pseudo)
                        (append (chem-bond-descendants pseudo) (list bond-copy)))
                  (setf (chem-bond-parent pseudo) exit-bond)
                  (setf (chem-bond-descendants exit-bond)
                        (append (chem-bond-descendants exit-bond) (list pseudo))))
              (setf (chem-bond-descendants exit-bond)
                    (append (chem-bond-descendants exit-bond) (list bond-copy))))))
        (let ((edge (chem-edge-at mol (chem-bond-edge-id bond))))
          (when edge
            (setf (chem-edge-cross-p edge) t)))))))

(defun chem-bond-set-cross (bond &optional last)
  "Mark BOND as a cross bond, with LAST as the departure flag."
  (setf (chem-bond-cross-p bond) t)
  (setf (chem-bond-cross-last-p bond) last))

(defun chem--scale-bonds (mol)
  "Scale bond lengths according to the options."
  (let* ((options (chem-drawing-options mol))
         (mode (or (plist-get options :bond-scale) 'normalize))
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
    (setf (chem-drawing-bond-scale mol) scale)
    (dolist (bond (chem--treebonds mol))
      (setf (chem-bond-length bond) (* scale (chem-bond-length bond))))))

(defun chem--aromatize-ring (mol ring center-x center-y)
  "Render RING as an aromatic circle."
  (let ((bond nil) (n 0))
    (indigo-with-bonds-iterator (bit ring)
      (let ((tk-bond (indigo-next bit)))
        (while tk-bond
          (setq n (1+ n))
          (setq bond (chem--get-bond mol tk-bond))
          (setf (chem-bond-bond-type bond) 'aromatic)
          (setq tk-bond (indigo-next bit)))))
    (let* ((atom (chem-bond-end-atom bond))
           (dims (chem--compare-positions
                  (chem-atom-x atom) (chem-atom-y atom) center-x center-y))
           (outer-r (nth 0 dims))
           (angle (+ (nth 1 dims) (or (plist-get (chem-drawing-options mol) :angle) 0.0)))
           (outer-r (* outer-r (chem-drawing-bond-scale mol)))
           (alpha (- (/ float-pi 2) (/ float-pi n)))
           (inner-r (* (sin alpha) outer-r))
           (arb (chem-bond-create
                 :options (chem-drawing-options mol) :kind 'aromatic-ring
                 :parent bond
                 :angle (mod (chem--num-round angle 1) 360)
                 :parent-angle (chem-bond-angle bond)
                 :length (chem--num-round outer-r 2)
                 :radius (chem--num-round (* chem--aromatic-ring-scale inner-r) 2)
                 :bond-type 'aromatic :descendants nil
                 :marker "" :to-phantom nil :is-trunk nil :clockwise 0)))
      (setf (chem-bond-descendants bond)
            (append (chem-bond-descendants bond) (list arb))))))

(defun chem--annotate-ring (mol ring is-aromatic)
  "Annotate RING with an aromatic circle or orient its double bonds."
  (let ((atoms nil) (bond-lengths nil) (bonds nil))
    (indigo-with-bonds-iterator (bit ring)
      (let ((tk-bond (indigo-next bit)))
        (while tk-bond
          (let ((bond (chem--get-bond mol tk-bond)))
            (push bond bonds)
            (cl-pushnew (chem-bond-start-atom bond) atoms)
            (cl-pushnew (chem-bond-end-atom bond) atoms)
            (push (chem-bond-length bond) bond-lengths))
          (setq tk-bond (indigo-next bit)))))
    (when (<= (length bonds) 8)
      (let* ((bl-max (apply #'max bond-lengths))
             (bl-spread (/ (- bl-max (apply #'min bond-lengths)) bl-max))
             (center-x (/ (apply #'+ (mapcar #'chem-atom-x atoms)) (float (length atoms))))
             (center-y (/ (apply #'+ (mapcar #'chem-atom-y atoms)) (float (length atoms))))
             (atom-angles nil) (center-distances nil))
        (dolist (atom atoms)
          (let ((dims (chem--compare-positions
                       (chem-atom-x atom) (chem-atom-y atom) center-x center-y)))
            (push (nth 0 dims) center-distances)
            (push (cons atom (nth 1 dims)) atom-angles)))
        (let* ((cd-max (apply #'max center-distances))
               (cd-spread (/ (- cd-max (apply #'min center-distances)) cd-max))
               (is-symmetric (and (<= cd-spread 0.05) (<= bl-spread 0.05))))
          (if (and is-aromatic is-symmetric
                   (plist-get (chem-drawing-options mol) :aromatic-circles))
              (progn
                (chem--aromatize-ring mol ring center-x center-y)
                (dolist (pair atom-angles)
                  (setf (chem-atom-bond-angles (car pair))
                        (append (chem-atom-bond-angles (car pair))
                                (list (cdr pair))))))
            (dolist (bond bonds)
              (chem-bond-is-clockwise bond center-x center-y))))))))

(defun chem--annotate-rings (mol)
  "Annotate all SSSR rings of MOL."
  (indigo-aromatize (chem-drawing-toolkit mol))
  (let ((rings nil))
    (indigo-with-sssr-iterator (it (chem-drawing-toolkit mol))
      (let ((ring (indigo-next it)))
        (while ring
          (let ((is-aromatic t))
            (indigo-with-bonds-iterator (bit ring)
              (let ((tk-bond (indigo-next bit)))
                (while tk-bond
                  (unless (eq (indigo-bond-order tk-bond) :aromatic)
                    (setq is-aromatic nil))
                  (setq tk-bond (indigo-next bit)))))
            (push (cons is-aromatic ring) rings))
          (setq ring (indigo-next it)))))
    ;; Stable sort by aromaticity (nil/false first), then process reversed.
    (setq rings (sort (nreverse rings)
                      (lambda (a b) (and (not (car a)) (car b)))))
    (dolist (entry (reverse rings))
      (chem--annotate-ring mol (cdr entry) (car entry)))))

;;; Pipeline

(defun chem-indigo-build (mol options)
  "Build a prepared `chem-drawing' for Indigo molecule MOL.
OPTIONS is the common option plist.  The drawing is ready to be rendered."
  (let ((atoms (chem--parse-atoms options mol)))
    ;; Flip / flop coordinates before bonds are parsed.
    (when (plist-get options :flip)
      (maphash (lambda (_ atom) (setf (chem-atom-x atom) (- (chem-atom-x atom)))) atoms))
    (when (plist-get options :flop)
      (maphash (lambda (_ atom) (setf (chem-atom-y atom) (- (chem-atom-y atom)))) atoms))
    (let* ((edges (make-hash-table :test 'eql))
           (edge-lookup (make-hash-table :test 'equal))
           (parsed (chem--parse-bonds options mol atoms edges edge-lookup))
           (state (chem-drawing-create
                   :options options :toolkit mol :atoms atoms
                   :bonds (car parsed)
                   :edges edges
                   :edge-lookup edge-lookup
                   :adjacency (make-hash-table :test 'eql)
                   :atom-pairs (cdr parsed)
                   :seen-atoms (make-hash-table :test 'equal)
                   :seen-bonds (make-hash-table :test 'equal)
                   :bond-scale 1.0)))
      ;; Record the angle of every incident bond on each atom.
      (dolist (pair (chem-drawing-atom-pairs state))
        (let* ((start (car pair)) (end (cdr pair))
               (bonds (chem-drawing-bonds state)))
          (setf (chem-atom-bond-angles (gethash start atoms))
                (append (chem-atom-bond-angles (gethash start atoms))
                        (list (chem-bond-angle (gethash (cons start end) bonds)))))
          (setf (chem-atom-bond-angles (gethash end atoms))
                (append (chem-atom-bond-angles (gethash end atoms))
                        (list (chem-bond-angle (gethash (cons end start) bonds)))))))
      (chem--connect-fragments state)
      (chem--build-adjacency state)
      (chem--pick-first-last-atoms state)
      (setf (chem-drawing-root state)
            (chem--parse-tree state nil (chem-drawing-entry-atom state)))
      (when (> (hash-table-count atoms) 1)
        (when (null (chem-drawing-exit-atom state))
          (let ((exit-bond (chem--default-exit-bond state)))
            (setf (chem-drawing-exit-bond state) exit-bond)
            (setf (chem-drawing-exit-atom state) (chem-bond-end-atom exit-bond))))
        (when (not (eq (chem-drawing-entry-atom state)
                       (chem-drawing-exit-atom state)))
          (let ((flagged (chem-drawing-exit-bond state)))
            (while (not (eq (chem-bond-end-atom flagged)
                            (chem-drawing-entry-atom state)))
              (setf (chem-bond-is-trunk flagged) t)
              (setq flagged (chem-bond-parent flagged)))))
        (when (plist-get options :cross-bond)
          (chem--process-cross-bonds state))
        (chem--scale-bonds state)
        (chem--annotate-rings state))
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
