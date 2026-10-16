;;; tochemfig-molecule.el --- Molecule to chemfig pipeline -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2025 Giovanni Crisalfi

;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Translation of mol2chemfigPy3's `molecule.py'.  This is the pipeline that
;; turns an Indigo molecule into chemfig code: parse atoms and bonds, connect
;; fragments, build the spanning tree, mark the trunk, scale bonds, annotate
;; rings, and finally render.

;;; Code:

(require 'cl-lib)
(require 'indigo)
(require 'tochemfig-atom)
(require 'tochemfig-bond)
(require 'tochemfig-mappings)

(cl-defstruct (tochemfig-molecule (:constructor tochemfig-molecule-create))
  options
  toolkit          ; The Indigo molecule handle
  atoms            ; hash: idx -> atom
  bonds            ; hash: (start . end) -> bond
  atom-pairs       ; list of (start . end)
  seen-atoms       ; hash: idx -> t
  seen-bonds       ; hash: (start . end) -> t
  entry-atom exit-atom exit-bond root
  bond-scale)

;;; Parsing

(defun tochemfig--parse-atoms (options mol)
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
                     (tochemfig-atom-create
                      :options options :idx idx
                      :x (nth 0 xyz) :y (nth 1 xyz)
                      :element element :hydrogens hydrogens :charge charge
                      :radical radical :neighbors (nreverse neighbors)
                      :marker (when marker-opt (format "%s%d" marker-opt (1+ idx)))
                      :bond-angles nil)
                     atoms))
          (setq atom (indigo-next iter)))))
    atoms))

(defun tochemfig--parse-bonds (options mol atoms)
  "Extract bonds from MOL into a hash keyed by (START . END).
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
                 (b (tochemfig-bond-create-from
                     options start-atom end-atom
                     (indigo-bond-order bond) (indigo-bond-stereo bond))))
            (puthash (cons start end) b bonds)
            (puthash (cons end start) (tochemfig-bond-invert b) bonds)
            (push (cons start end) pairs))
          (setq bond (indigo-next iter)))))
    (cons bonds (nreverse pairs))))

;;; Fragment handling

(defun tochemfig--split-pairs (pair-list)
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

(cl-defun tochemfig--molecule-fragments (mol)
  "Identify unconnected fragments in MOL."
  (let ((pairs (tochemfig-molecule-atom-pairs mol)))
    (cond
     ((null pairs) nil)
     ((null (cdr pairs)) (list pairs))
     (t (let ((fragments nil) (remaining pairs))
          (while t
            (let* ((split (tochemfig--split-pairs remaining))
                   (connected (nth 0 split))
                   (rest (nth 1 split)))
              (push connected fragments)
              (if (null rest)
                  (cl-return-from tochemfig--molecule-fragments (nreverse fragments))
                (setq remaining rest)))))))))

(defun tochemfig--link-atoms (mol x y)
  "Connect atoms X and Y of MOL with a pseudo (link) bond."
  (let* ((options (tochemfig-molecule-options mol))
         (start (gethash x (tochemfig-molecule-atoms mol)))
         (end (gethash y (tochemfig-molecule-atoms mol)))
         (bond (tochemfig-bond-create-from options start end :single :none)))
    (tochemfig-bond-set-link bond)
    (puthash (cons x y) bond (tochemfig-molecule-bonds mol))
    (puthash (cons y x) (tochemfig-bond-invert bond) (tochemfig-molecule-bonds mol))
    (setf (tochemfig-atom-neighbors start)
          (append (tochemfig-atom-neighbors start) (list y)))
    (setf (tochemfig-atom-neighbors end)
          (append (tochemfig-atom-neighbors end) (list x)))))

(defun tochemfig--connect-fragments (mol)
  "Connect fragments, if any, with invisible bonds."
  (let ((fragments (tochemfig--molecule-fragments mol)))
    (when (> (length fragments) 1)
      (let ((fs fragments))
        (while (cdr fs)
          (let* ((head (car fs)) (tail (cadr fs))
                 (head-last (cdr (car (last head))))
                 (tail-first (car (car tail))))
            (tochemfig--link-atoms mol head-last tail-first))
          (setq fs (cdr fs)))))
    (let* ((atoms (tochemfig-molecule-atoms mol))
           (bonded nil)
           (un-bonded nil))
      (dolist (pair (tochemfig-molecule-atom-pairs mol))
        (unless (member (car pair) bonded) (push (car pair) bonded))
        (unless (member (cdr pair) bonded) (push (cdr pair) bonded)))
      (maphash (lambda (idx _) (unless (member idx bonded) (push idx un-bonded))) atoms)
      (setq un-bonded (sort un-bonded #'<))
      (when un-bonded
        (let ((anchor (if fragments
                          (cdr (car (last (car (last fragments)))))
                        (prog1 (car un-bonded) (setq un-bonded (cdr un-bonded))))))
          (dolist (atom un-bonded)
            (tochemfig--link-atoms mol anchor atom)))))))

;;; Tree building

(defun tochemfig--treebonds (mol)
  "Return all bonds in MOL's tree, excluding the dummy first bond."
  (let ((all-bonds nil))
    (cl-labels ((recurse (bond)
                  (push bond all-bonds)
                  (dolist (d (tochemfig-bond-descendants bond)) (recurse d))))
      (recurse (tochemfig-molecule-root mol)))
    (setq all-bonds (nreverse all-bonds))
    (if (eq (tochemfig-bond-kind (car all-bonds)) 'dummy-first)
        (cdr all-bonds)
      all-bonds)))

(defun tochemfig--get-bond (mol tk-bond)
  "Return the tree bond of MOL corresponding to TK-BOND."
  (let* ((start (indigo-index (indigo-source tk-bond)))
         (end (indigo-index (indigo-destination tk-bond))))
    (if (gethash (cons start end) (tochemfig-molecule-seen-bonds mol))
        (gethash (cons start end) (tochemfig-molecule-bonds mol))
      (gethash (cons end start) (tochemfig-molecule-bonds mol)))))

(defun tochemfig--default-exit-bond (mol)
  "Pick the bond farthest from the entry atom along the tree."
  (let ((scored nil)
        (entry (tochemfig-molecule-entry-atom mol)))
    (dolist (bond (tochemfig--treebonds mol))
      (unless (tochemfig-bond-to-phantom bond)
        (let ((distance 0) (the-bond bond))
          (while (and the-bond (not (eq (tochemfig-bond-end-atom the-bond) entry)))
            (setq distance (1+ distance))
            (setq the-bond (tochemfig-bond-parent the-bond)))
          (push (list distance (length (tochemfig-bond-descendants bond)) bond) scored))))
    (setq scored (sort (nreverse scored)
                       (lambda (a b) (< (nth 0 a) (nth 0 b)))))
    (nth 2 (car (last scored)))))

(defun tochemfig--pick-first-last-atoms (mol)
  "Choose entry and exit atoms for MOL."
  (let* ((options (tochemfig-molecule-options mol))
         (atoms (tochemfig-molecule-atoms mol))
         (entry-opt (plist-get options :entry-atom))
         (exit-opt (plist-get options :exit-atom))
         (entry (if entry-opt
                    (or (gethash (1- entry-opt) atoms)
                        (error "Invalid entry atom number"))
                  (car (sort (tochemfig--hash-values atoms)
                             (lambda (a b)
                               (< (length (tochemfig-atom-neighbors a))
                                  (length (tochemfig-atom-neighbors b))))))))
         (exit (when exit-opt
                 (or (gethash (1- exit-opt) atoms)
                     (error "Invalid exit atom number")))))
    (setf (tochemfig-molecule-entry-atom mol) entry)
    (setf (tochemfig-molecule-exit-atom mol) exit)))

(defun tochemfig--hash-values (hash)
  "Return the values of HASH ordered by integer key."
  (let ((pairs nil))
    (maphash (lambda (k v) (push (cons k v) pairs)) hash)
    (mapcar #'cdr (sort pairs (lambda (a b) (< (car a) (car b)))))))

(cl-defun tochemfig--parse-tree (mol start-atom end-atom)
  "Recursively build the spanning tree for MOL.
Returns the bond (or nil when the bond was already seen)."
  (let* ((options (tochemfig-molecule-options mol))
         (end-idx (tochemfig-atom-idx end-atom))
         (start-idx (when start-atom (tochemfig-atom-idx start-atom)))
         (bond
          (if (null start-atom)
              (tochemfig-bond-create
               :options options :kind 'dummy-first :start-atom nil :end-atom end-atom
               :tikz-styles nil :tikz-values nil :bond-type nil :descendants nil
               :length nil :angle nil :marker "" :is-last nil :to-phantom nil
               :is-trunk nil :parent nil :clockwise 0)
            (if (or (gethash (cons start-idx end-idx) (tochemfig-molecule-seen-bonds mol))
                    (gethash (cons end-idx start-idx) (tochemfig-molecule-seen-bonds mol)))
                (cl-return-from tochemfig--parse-tree nil)
              (let ((b (gethash (cons start-idx end-idx) (tochemfig-molecule-bonds mol))))
                (puthash (cons start-idx end-idx) t (tochemfig-molecule-seen-bonds mol))
                (when (gethash end-idx (tochemfig-molecule-seen-atoms mol))
                  (setf (tochemfig-bond-to-phantom b) t)
                  (cl-return-from tochemfig--parse-tree b))
                b)))))
    (puthash end-idx t (tochemfig-molecule-seen-atoms mol))
    (when (eq end-atom (tochemfig-molecule-exit-atom mol))
      (setf (tochemfig-molecule-exit-bond mol) bond))
    (dolist (ni (tochemfig-atom-neighbors end-atom))
      (unless (and start-atom (= ni start-idx))
        (let* ((next-atom (gethash ni (tochemfig-molecule-atoms mol)))
               (next-bond (tochemfig--parse-tree mol end-atom next-atom)))
          (when next-bond
            (setf (tochemfig-bond-parent next-bond) bond)
            (push next-bond (tochemfig-bond-descendants bond))))))
    ;; Children were pushed in reverse; restore neighbor order.
    (setf (tochemfig-bond-descendants bond)
          (nreverse (tochemfig-bond-descendants bond)))
    bond))

;;; Cross bonds, scaling, rings

(defun tochemfig--process-cross-bonds (mol)
  "Turn declared cross bonds into branches off the exit bond."
  (let ((cross-bonds (plist-get (tochemfig-molecule-options mol) :cross-bond))
        (seen (tochemfig-molecule-seen-bonds mol))
        (bonds (tochemfig-molecule-bonds mol))
        (exit-bond (tochemfig-molecule-exit-bond mol)))
    (dolist (pair cross-bonds)
      (let* ((start (1- (car pair)))
             (end (1- (cadr pair)))
             (bond (cond ((gethash (cons start end) seen) (gethash (cons start end) bonds))
                         ((gethash (cons end start) seen) (gethash (cons end start) bonds))
                         (t (error "bond %d-%d doesn't exist" (car pair) (cadr pair))))))
        (if (and (tochemfig-bond-descendants exit-bond)
                 (eq bond (car (last (tochemfig-bond-descendants exit-bond)))))
            (progn
              (tochemfig-bond-set-cross bond t))
          (let ((bond-copy (copy-tochemfig-bond bond)))
            (tochemfig-bond-set-link bond)
            (tochemfig-bond-set-cross bond-copy nil)
            (setf (tochemfig-bond-to-phantom bond-copy) t)
            (setf (tochemfig-bond-descendants bond-copy) nil)
            (if (not (eq (tochemfig-bond-start-atom bond-copy)
                         (tochemfig-molecule-exit-atom mol)))
                (let ((pseudo (tochemfig-bond-create-from
                               (tochemfig-molecule-options mol)
                               (tochemfig-molecule-exit-atom mol)
                               (tochemfig-bond-start-atom bond-copy) :single :none)))
                  (tochemfig-bond-set-link pseudo)
                  (setf (tochemfig-bond-to-phantom pseudo) t)
                  (setf (tochemfig-bond-parent bond-copy) pseudo)
                  (setf (tochemfig-bond-descendants pseudo)
                        (append (tochemfig-bond-descendants pseudo) (list bond-copy)))
                  (setf (tochemfig-bond-parent pseudo) exit-bond)
                  (setf (tochemfig-bond-descendants exit-bond)
                        (append (tochemfig-bond-descendants exit-bond) (list pseudo))))
              (setf (tochemfig-bond-descendants exit-bond)
                    (append (tochemfig-bond-descendants exit-bond) (list bond-copy))))))))))

(defun tochemfig-bond-set-cross (bond &optional last)
  "Configure BOND to be drawn crossing over another."
  (let* ((start-angles (mapcar #'cdr (tochemfig-bond-upstream-angles bond)))
         (end-angles (mapcar #'cdr (tochemfig-bond-downstream-angles bond)))
         (start-angle (apply #'min (delq nil start-angles)))
         (end-angle (apply #'min (delq nil end-angles)))
         (start (max 10 (tochemfig--cotan100 start-angle)))
         (end (max 10 (tochemfig--cotan100 end-angle))))
    (cl-pushnew "cross" (tochemfig-bond-tikz-styles bond) :test #'equal)
    (setf (tochemfig-bond-tikz-values bond)
          (append (list (cons 'bgstart start) (cons 'bgend end))
                  (tochemfig-bond-tikz-values bond)))
    (setf (tochemfig-bond-is-last bond) last)))

(defun tochemfig--scale-bonds (mol)
  "Scale bond lengths according to the options."
  (let* ((options (tochemfig-molecule-options mol))
         (mode (or (plist-get options :bond-scale) 'normalize))
         (stretch (or (plist-get options :bond-stretch) 1.0))
         (scale 1.0))
    (cond
     ((eq mode 'keep) nil)
     ((eq mode 'normalize)
      (let* ((lengths (mapcar (lambda (b)
                                (tochemfig--num-round (tochemfig-bond-length b)
                                                      (or (plist-get options :bond-round) 3)))
                              (tochemfig--treebonds mol)))
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
    (setf (tochemfig-molecule-bond-scale mol) scale)
    (dolist (bond (tochemfig--treebonds mol))
      (setf (tochemfig-bond-length bond) (* scale (tochemfig-bond-length bond))))))

(defun tochemfig--aromatize-ring (mol ring center-x center-y)
  "Render RING as an aromatic circle."
  (let ((bond nil) (n 0))
    (indigo-with-bonds-iterator (bit ring)
      (let ((tk-bond (indigo-next bit)))
        (while tk-bond
          (setq n (1+ n))
          (setq bond (tochemfig--get-bond mol tk-bond))
          (setf (tochemfig-bond-bond-type bond) 'aromatic)
          (setq tk-bond (indigo-next bit)))))
    (let* ((atom (tochemfig-bond-end-atom bond))
           (dims (tochemfig--compare-positions
                  (tochemfig-atom-x atom) (tochemfig-atom-y atom) center-x center-y))
           (outer-r (nth 0 dims))
           (angle (+ (nth 1 dims) (or (plist-get (tochemfig-molecule-options mol) :angle) 0.0)))
           (outer-r (* outer-r (tochemfig-molecule-bond-scale mol)))
           (alpha (- (/ float-pi 2) (/ float-pi n)))
           (inner-r (* (sin alpha) outer-r))
           (arb (tochemfig-bond-create
                 :options (tochemfig-molecule-options mol) :kind 'aromatic-ring
                 :parent bond
                 :angle (mod (tochemfig--num-round angle 1) 360)
                 :parent-angle (tochemfig-bond-angle bond)
                 :length (tochemfig--num-round outer-r 2)
                 :radius (tochemfig--num-round (* tochemfig--aromatic-ring-scale inner-r) 2)
                 :tikz-styles nil :tikz-values nil :bond-type 'aromatic :descendants nil
                 :marker "" :is-last nil :to-phantom nil :is-trunk nil :clockwise 0)))
      (setf (tochemfig-bond-descendants bond)
            (append (tochemfig-bond-descendants bond) (list arb))))))

(defun tochemfig--annotate-ring (mol ring is-aromatic)
  "Annotate RING with an aromatic circle or orient its double bonds."
  (let ((atoms nil) (bond-lengths nil) (bonds nil))
    (indigo-with-bonds-iterator (bit ring)
      (let ((tk-bond (indigo-next bit)))
        (while tk-bond
          (let ((bond (tochemfig--get-bond mol tk-bond)))
            (push bond bonds)
            (cl-pushnew (tochemfig-bond-start-atom bond) atoms)
            (cl-pushnew (tochemfig-bond-end-atom bond) atoms)
            (push (tochemfig-bond-length bond) bond-lengths))
          (setq tk-bond (indigo-next bit)))))
    (when (<= (length bonds) 8)
      (let* ((bl-max (apply #'max bond-lengths))
             (bl-spread (/ (- bl-max (apply #'min bond-lengths)) bl-max))
             (center-x (/ (apply #'+ (mapcar #'tochemfig-atom-x atoms)) (float (length atoms))))
             (center-y (/ (apply #'+ (mapcar #'tochemfig-atom-y atoms)) (float (length atoms))))
             (atom-angles nil) (center-distances nil))
        (dolist (atom atoms)
          (let ((dims (tochemfig--compare-positions
                       (tochemfig-atom-x atom) (tochemfig-atom-y atom) center-x center-y)))
            (push (nth 0 dims) center-distances)
            (push (cons atom (nth 1 dims)) atom-angles)))
        (let* ((cd-max (apply #'max center-distances))
               (cd-spread (/ (- cd-max (apply #'min center-distances)) cd-max))
               (is-symmetric (and (<= cd-spread 0.05) (<= bl-spread 0.05))))
          (if (and is-aromatic is-symmetric
                   (plist-get (tochemfig-molecule-options mol) :aromatic-circles))
              (progn
                (tochemfig--aromatize-ring mol ring center-x center-y)
                (dolist (pair atom-angles)
                  (setf (tochemfig-atom-bond-angles (car pair))
                        (append (tochemfig-atom-bond-angles (car pair))
                                (list (cdr pair))))))
            (dolist (bond bonds)
              (tochemfig-bond-is-clockwise bond center-x center-y))))))))

(defun tochemfig--annotate-rings (mol)
  "Annotate all SSSR rings of MOL."
  (indigo-aromatize (tochemfig-molecule-toolkit mol))
  (let ((rings nil))
    (indigo-with-sssr-iterator (it (tochemfig-molecule-toolkit mol))
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
      (tochemfig--annotate-ring mol (cdr entry) (car entry)))))

;;; Rendering

(defun tochemfig--render-branches (mol output level bonds)
  "Append BONDS rendered as indented branches to OUTPUT.
OUTPUT is the reversed line accumulator; returns the updated accumulator."
  (let ((indent (or (plist-get (tochemfig-molecule-options mol) :indent) 4)))
    (dolist (bond bonds output)
      (let ((width (+ (* level indent) tochemfig--bond-code-width)))
        (setq output (cons (concat (make-string (1- width) ?\s) "(") output))
        (setq output (tochemfig--render mol output bond level))
        (setq output (cons (concat (make-string (1- width) ?\s) ")") output))))))

(defun tochemfig--render (mol output bond level)
  "Recursively render BOND into OUTPUT at LEVEL; returns the accumulator."
  (setq output (cons (tochemfig-bond-render bond level) output))
  (let ((branches (copy-sequence (tochemfig-bond-descendants bond))))
    (cond
     ((eq bond (tochemfig-molecule-exit-bond mol))
      (tochemfig--render-branches mol output (1+ level) branches))
     ((null branches) output)
     (t
      (let ((trunk-idx (cl-position-if #'tochemfig-bond-is-trunk branches))
            (first nil))
        (if trunk-idx
            (setq first (nth trunk-idx branches)
                  branches (append (cl-subseq branches 0 trunk-idx)
                                   (cl-subseq branches (1+ trunk-idx))))
          (setq first (car branches)
                branches (cdr branches)))
        (setq output (tochemfig--render-branches mol output (1+ level) branches))
        (tochemfig--render mol output first level))))))

(defun tochemfig--render-molecule (mol)
  "Render MOL to a list of lines."
  (nreverse (tochemfig--render mol nil (tochemfig-molecule-root mol) 0)))

;;; Public entry point

(defun tochemfig-generate (mol options)
  "Generate chemfig code for molecule MOL with OPTIONS plist."
  (let ((atoms (tochemfig--parse-atoms options mol)))
    ;; Flip / flop coordinates before bonds are parsed.
    (when (plist-get options :flip)
      (maphash (lambda (_ atom) (setf (tochemfig-atom-x atom) (- (tochemfig-atom-x atom)))) atoms))
    (when (plist-get options :flop)
      (maphash (lambda (_ atom) (setf (tochemfig-atom-y atom) (- (tochemfig-atom-y atom)))) atoms))
    (let* ((parsed (tochemfig--parse-bonds options mol atoms))
           (state (tochemfig-molecule-create
                   :options options :toolkit mol :atoms atoms
                   :bonds (car parsed) :atom-pairs (cdr parsed)
                   :seen-atoms (make-hash-table :test 'equal)
                   :seen-bonds (make-hash-table :test 'equal)
                   :bond-scale 1.0)))
      ;; Record the angle of every incident bond on each atom.
      (dolist (pair (tochemfig-molecule-atom-pairs state))
        (let* ((start (car pair)) (end (cdr pair))
               (bonds (tochemfig-molecule-bonds state)))
          (setf (tochemfig-atom-bond-angles (gethash start atoms))
                (append (tochemfig-atom-bond-angles (gethash start atoms))
                        (list (tochemfig-bond-angle (gethash (cons start end) bonds)))))
          (setf (tochemfig-atom-bond-angles (gethash end atoms))
                (append (tochemfig-atom-bond-angles (gethash end atoms))
                        (list (tochemfig-bond-angle (gethash (cons end start) bonds)))))))
      (tochemfig--connect-fragments state)
      (tochemfig--pick-first-last-atoms state)
      (setf (tochemfig-molecule-root state)
            (tochemfig--parse-tree state nil (tochemfig-molecule-entry-atom state)))
      (when (> (hash-table-count atoms) 1)
        (when (null (tochemfig-molecule-exit-atom state))
          (let ((exit-bond (tochemfig--default-exit-bond state)))
            (setf (tochemfig-molecule-exit-bond state) exit-bond)
            (setf (tochemfig-molecule-exit-atom state) (tochemfig-bond-end-atom exit-bond))))
        (when (not (eq (tochemfig-molecule-entry-atom state)
                       (tochemfig-molecule-exit-atom state)))
          (let ((flagged (tochemfig-molecule-exit-bond state)))
            (while (not (eq (tochemfig-bond-end-atom flagged)
                            (tochemfig-molecule-entry-atom state)))
              (setf (tochemfig-bond-is-trunk flagged) t)
              (setq flagged (tochemfig-bond-parent flagged)))))
        (when (plist-get options :cross-bond)
          (tochemfig--process-cross-bonds state))
        (tochemfig--scale-bonds state)
        (tochemfig--annotate-rings state))
      (maphash (lambda (_ atom) (tochemfig-atom-score-angles atom)) atoms)
      (tochemfig--format-output options (tochemfig--render-molecule state)))))

(provide 'tochemfig-molecule)
;;; tochemfig-molecule.el ends here
