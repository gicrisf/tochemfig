;;; chem-ir-test.el --- IR/export boundary tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Giovanni Crisalfi

;; This file is not part of GNU Emacs.

;;; Commentary:

;; These tests assert that common drawing decisions are stored in the IR and
;; that the chemfig exporter renders those attributes instead of re-reading
;; the common options.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((here (file-name-directory (or load-file-name buffer-file-name))))
  (when here (add-to-list 'load-path here)))

(let ((sibling (expand-file-name "emacs-indigo"
                                 (file-name-directory
                                  (directory-file-name
                                   (file-name-directory (or load-file-name buffer-file-name)))))))
  (when (file-directory-p sibling)
    (add-to-list 'load-path sibling)))

(require 'chem-mode)
(require 'chem-indigo)
(require 'chem-options)
(require 'chem-export)
(require 'chem-exp-chemfig)

(defconst chem-ir-test--root
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the test files.")

(defun chem-ir-test--path (relative)
  "Resolve RELATIVE against the test directory."
  (expand-file-name relative chem-ir-test--root))

(defun chem-ir-test--drawing (smiles overrides)
  "Build a prepared drawing from SMILES with OVERRIDES."
  (let ((options (chem--build-options overrides)))
    (indigo-with-molecule (mol smiles)
      (chem--prepare-mol mol options)
      (chem-indigo-build mol options))))

(defun chem-ir-test--drawing-from-file (relative overrides)
  "Build a prepared drawing from a corpus MOL file with OVERRIDES."
  (let ((options (chem--build-options overrides)))
    (indigo-with-mol-file (mol (chem-ir-test--path relative))
      (chem--prepare-mol mol options)
      (chem-indigo-build mol options))))

(defun chem-ir-test--render (drawing &optional overrides)
  "Render DRAWING as chemfig with OVERRIDES."
  (chem-render drawing 'chemfig (chem--build-options overrides)))

(ert-deftest chem-option-registry ()
  "Common and exporter options are registered."
  (let ((common (mapcar (lambda (spec) (plist-get spec :key))
                        chem-common-option-specs)))
    (should (memq :angle common))
    (should (memq :hydrogens common))
    (should (memq :show-carbons common)))
  (let ((chemfig (chem-exporter-option-specs 'chemfig)))
    (should (cl-find :terse chemfig :key (lambda (spec) (plist-get spec :key))))
    (should (cl-find :wrap-chemfig chemfig :key (lambda (spec) (plist-get spec :key))))))

(ert-deftest chem-ir-fancy-decoration ()
  "Fancy bond choice comes from the IR, not from exporter options."
  (let* ((fancy (chem-ir-test--drawing-from-file "corpus/benzene.mol" '(("fancy-bonds" . t))))
         (plain (chem-ir-test--drawing-from-file "corpus/benzene.mol" '(("fancy-bonds" . nil))))
         (fancy-without-option (chem-ir-test--render fancy '(("fancy-bonds" . nil))))
         (plain-with-option (chem-ir-test--render plain '(("fancy-bonds" . t)))))
    (should (string-match-p "db[lr]\\|d[lr][hn]" fancy-without-option))
    (should-not (string-match-p "db[lr]\\|d[lr][hn]" plain-with-option))
    (should (equal plain-with-option (chem-ir-test--render plain nil)))))

(ert-deftest chem-ir-atom-labels ()
  "Carbon label visibility comes from the IR, not from exporter options."
  (let* ((shown (chem-ir-test--drawing-from-file "corpus/ethanol.mol" '(("show-carbons" . t))))
         (hidden (chem-ir-test--drawing-from-file "corpus/ethanol.mol" '(("show-carbons" . nil))))
         (shown-without-option (chem-ir-test--render shown '(("show-carbons" . nil))))
         (hidden-with-option (chem-ir-test--render hidden '(("show-carbons" . t)))))
    (should (string-match-p "C" shown-without-option))
    (should-not (string-match-p "C" hidden-with-option))
    (should (equal hidden-with-option (chem-ir-test--render hidden nil)))))

(ert-deftest chem-ir-atom-numbers ()
  "Atom numbering comes from the IR, not from exporter options."
  (let* ((numbered (chem-ir-test--drawing-from-file "corpus/ethanol.mol" '(("atom-numbers" . t))))
         (plain (chem-ir-test--drawing-from-file "corpus/ethanol.mol" '(("atom-numbers" . nil))))
         (numbered-without-option (chem-ir-test--render numbered '(("atom-numbers" . nil))))
         (plain-with-option (chem-ir-test--render plain '(("atom-numbers" . t)))))
    (should (string-match-p "\\\\mcfatomno" numbered-without-option))
    (should-not (string-match-p "\\\\mcfatomno" plain-with-option))
    (should (equal plain-with-option (chem-ir-test--render plain nil)))))

(ert-deftest chem-ir-aromatic-circles ()
  "Aromatic ring display is exporter policy, not stored in the IR."
  (let* ((circles (chem-ir-test--drawing-from-file "corpus/benzene.mol" '(("aromatic-circles" . t))))
         (plain (chem-ir-test--drawing-from-file "corpus/benzene.mol" '(("aromatic-circles" . nil))))
         (circles-without-option (chem-ir-test--render circles '(("aromatic-circles" . nil))))
         (plain-with-option (chem-ir-test--render plain '(("aromatic-circles" . t)))))
    (should-not (string-match-p "mcfcringle" circles-without-option))
    (should (string-match-p "mcfcringle" plain-with-option))))

;;; Canonical edge graph

(ert-deftest chem-ir-canonical-edges ()
  "Each chemical bond has one canonical edge and a symmetric lookup."
  (let* ((drawing (chem-ir-test--drawing-from-file "corpus/benzene.mol" nil))
         (edges (chem-edges drawing)))
    (should (= 6 (length edges)))
    (should (= 6 (length (chem-visible-edges drawing))))
    (should (equal (number-sequence 0 5)
                   (mapcar #'chem-edge-id edges)))
    (dolist (edge edges)
      (should (< (chem-edge-a edge) (chem-edge-b edge)))
      (should (eq edge (chem-get-edge drawing
                                      (chem-edge-a edge)
                                      (chem-edge-b edge))))
      (should (eq edge (chem-get-edge drawing
                                      (chem-edge-b edge)
                                      (chem-edge-a edge))))
      (should (eq edge (chem-edge-at drawing (chem-edge-id edge)))))))

(ert-deftest chem-ir-edge-adjacency ()
  "Adjacency is symmetric and references canonical edges."
  (dolist (file '("corpus/benzene.mol" "corpus/salt_nacl.mol"))
    (let ((drawing (chem-ir-test--drawing-from-file file nil)))
      (maphash
       (lambda (idx _atom)
         (dolist (entry (chem-atom-adjacency drawing idx))
           (let* ((edge-id (car entry))
                  (neighbor (cdr entry))
                  (edge (chem-edge-at drawing edge-id)))
             (should edge)
             (should (= neighbor (chem-edge-other-atom edge idx)))
             (should (assoc edge-id (chem-atom-adjacency drawing neighbor))))))
       (chem-drawing-atoms drawing)))))

(ert-deftest chem-ir-edge-order ()
  "Canonical edges preserve the chemical bond order."
  (let ((kekule (chem-ir-test--drawing-from-file "corpus/benzene.mol" nil))
        (aromatic (chem-ir-test--drawing "c1ccccc1" nil))
        (butene (chem-ir-test--drawing-from-file "corpus/butene_e.mol" nil)))
    (let ((orders (mapcar #'chem-edge-order (chem-edges kekule))))
      (should (= 3 (cl-count :single orders)))
      (should (= 3 (cl-count :double orders))))
    (should (= 6 (cl-count :aromatic
                           (mapcar #'chem-edge-order (chem-edges aromatic)))))
    (let ((orders (mapcar #'chem-edge-order (chem-edges butene))))
      (should (= 2 (cl-count :single orders)))
      (should (= 1 (cl-count :double orders))))))

(ert-deftest chem-ir-edge-stereo ()
  "Canonical stereo is relative to a -> b and flips for directed views."
  (let* ((drawing (chem-ir-test--drawing-from-file
                   "corpus/bromochloromethane_stereo.mol" nil))
         (edge (cl-find-if (lambda (e) (memq (chem-edge-stereo e) '(:up :down)))
                           (chem-edges drawing))))
    (should edge)
    (let ((from-a (chem-edge-stereo-directed edge (chem-edge-a edge)))
          (from-b (chem-edge-stereo-directed edge (chem-edge-b edge))))
      (should (memq from-a '(:up :down)))
      (should (eq from-b (if (eq from-a :up) :down :up)))))
  (let* ((e (chem-ir-test--drawing-from-file "corpus/butene_e.mol" nil))
         (edge (cl-find-if (lambda (edge) (eq :double (chem-edge-order edge)))
                           (chem-edges e))))
    (should (eq :trans (chem-edge-stereo edge))))
  (let* ((z (chem-ir-test--drawing-from-file "corpus/butene_z.mol" nil))
         (edge (cl-find-if (lambda (edge) (eq :double (chem-edge-order edge)))
                           (chem-edges z))))
    (should (eq :cis (chem-edge-stereo edge)))))

(ert-deftest chem-ir-link-edge ()
  "Fragment links are canonical edges with link-p set."
  (let* ((drawing (chem-ir-test--drawing-from-file "corpus/salt_nacl.mol" nil))
         (edges (chem-edges drawing)))
    (should (= 1 (length edges)))
    (let ((edge (car edges)))
      (should (chem-edge-link-p edge))
      (should (eq :single (chem-edge-order edge)))
      (should (eq :none (chem-edge-stereo edge)))
      (should-not (chem-visible-edges drawing)))))

(ert-deftest chem-ir-edge-decoration ()
  "Canonical edge decoration mirrors the resolved directed decoration."
  (let* ((fancy (chem-ir-test--drawing-from-file "corpus/benzene.mol" '(("fancy-bonds" . t))))
         (plain (chem-ir-test--drawing-from-file "corpus/benzene.mol" nil))
         (circles (chem-ir-test--drawing-from-file "corpus/benzene.mol" '(("aromatic-circles" . t)))))
    (dolist (edge (chem-edges fancy))
      (should (eq (if (eq :double (chem-edge-order edge)) 'fancy 'plain)
                  (chem-edge-decoration edge))))
    (dolist (edge (chem-edges plain))
      (should (eq 'plain (chem-edge-decoration edge))))
    (dolist (edge (chem-edges circles))
      (should (eq 'plain (chem-edge-decoration edge))))))

(ert-deftest chem-ir-edge-markers ()
  "Canonical edges carry markers in sorted atom order."
  (let ((drawing (chem-ir-test--drawing-from-file
                  "corpus/ethanol.mol" '(("markers" . "a")))))
    (dolist (edge (chem-edges drawing))
      (should (string-match-p
               (format "\\`a%d-%d\\'" (1+ (chem-edge-a edge))
                       (1+ (chem-edge-b edge)))
               (chem-edge-marker edge))))))

(ert-deftest chem-ir-edge-cross ()
  "Cross-bond selection is recorded on the canonical edge."
  (let* ((drawing (chem-ir-test--drawing-from-file
                   "corpus/benzene.mol" '(("cross-bond" . ((1 2))))))
         (crossed (cl-remove-if-not #'chem-edge-cross-p (chem-edges drawing))))
    (should (= 1 (length crossed)))
    (should (equal '(0 . 1)
                   (cons (chem-edge-a (car crossed))
                         (chem-edge-b (car crossed)))))))

;;; Geometry transforms

(defun chem-ir-test--close-p (a b &optional tolerance)
  "Return non-nil when A and B are within TOLERANCE."
  (< (abs (- a b)) (or tolerance 1e-6)))

(defun chem-ir-test--angle-close-p (a b &optional tolerance)
  "Return non-nil when angles A and B match modulo 360."
  (let* ((tolerance (or tolerance 1e-6))
         (diff (abs (mod (- a b) 360))))
    (or (< diff tolerance)
        (> diff (- 360 tolerance)))))

(ert-deftest chem-ir-coordinate-transforms ()
  "Coordinate transforms update atom positions."
  (let* ((atom (chem-atom-create :idx 0 :x 1.0 :y 0.0))
         (atoms (make-hash-table :test 'equal))
         (drawing (chem-drawing-create :atoms atoms)))
    (puthash 0 atom atoms)
    (chem-rotate drawing 90.0)
    (should (chem-ir-test--close-p 0.0 (chem-atom-x atom)))
    (should (chem-ir-test--close-p 1.0 (chem-atom-y atom)))
    (chem-flip-horizontal drawing)
    (should (chem-ir-test--close-p 0.0 (chem-atom-x atom)))
    (chem-flip-vertical drawing)
    (should (chem-ir-test--close-p -1.0 (chem-atom-y atom)))
    (chem-scale drawing 2.0)
    (should (chem-ir-test--close-p 0.0 (chem-atom-x atom)))
    (should (chem-ir-test--close-p -2.0 (chem-atom-y atom)))))

(ert-deftest chem-ir-derived-geometry ()
  "Traversal nodes derive finite geometry from coordinates."
  (dolist (case '(("corpus/ethanol.mol" nil)
                  ("corpus/ethanol.mol" (("angle" . 30.0) ("flip" . t)))
                  ("corpus/benzene.mol" (("bond-scale" . scale)
                                         ("bond-stretch" . 2.0)))
                  ("corpus/benzene.mol" (("aromatic-circles" . t)))))
    (let ((drawing (chem-ir-test--drawing-from-file (car case) (cadr case))))
      (dolist (node (chem-tree-nodes drawing))
        (let ((dims (chem-exp-chemfig--node-geometry drawing node)))
          (should (> (nth 0 dims) 0))
          (should (numberp (nth 1 dims))))))))

(ert-deftest chem-ir-rotation-coordinates ()
  "Rotation is applied to coordinates before bond geometry is derived."
  (let ((plain (chem-ir-test--drawing-from-file "corpus/ethanol.mol" nil))
        (rotated (chem-ir-test--drawing-from-file
                  "corpus/ethanol.mol" '(("angle" . 90.0)))))
    (maphash
     (lambda (idx atom)
       (let ((other (gethash idx (chem-drawing-atoms rotated))))
         (should (chem-ir-test--close-p (- (chem-atom-y atom))
                                        (chem-atom-x other)))
         (should (chem-ir-test--close-p (chem-atom-x atom)
                                        (chem-atom-y other)))))
     (chem-drawing-atoms plain))))

(ert-deftest chem-ir-scale-coordinates ()
  "Scaling is applied to coordinates, not only to bond lengths."
  (let ((keep (chem-ir-test--drawing-from-file
               "corpus/ethanol.mol" '(("bond-scale" . keep))))
        (scaled (chem-ir-test--drawing-from-file
                 "corpus/ethanol.mol" '(("bond-scale" . scale)
                                        ("bond-stretch" . 2.0)))))
    (maphash
     (lambda (idx atom)
       (let ((other (gethash idx (chem-drawing-atoms scaled))))
         (should (chem-ir-test--close-p (* 2.0 (chem-atom-x atom))
                                        (chem-atom-x other)))
         (should (chem-ir-test--close-p (* 2.0 (chem-atom-y atom))
                                        (chem-atom-y other)))))
     (chem-drawing-atoms keep))
    (dolist (edge (chem-edges keep))
      (let ((after (chem-get-edge scaled (chem-edge-a edge) (chem-edge-b edge))))
        (should (chem-ir-test--close-p (* 2.0 (chem-edge-length keep edge))
                                       (chem-edge-length scaled after)))))))

(ert-deftest chem-ir-flip-coordinates ()
  "Flip and flop are applied to coordinates before bond geometry is derived."
  (let ((plain (chem-ir-test--drawing-from-file "corpus/ethanol.mol" nil))
        (flipped (chem-ir-test--drawing-from-file
                  "corpus/ethanol.mol" '(("flip" . t))))
        (flopped (chem-ir-test--drawing-from-file
                  "corpus/ethanol.mol" '(("flop" . t)))))
    (maphash
     (lambda (idx atom)
       (let ((f (gethash idx (chem-drawing-atoms flipped)))
             (v (gethash idx (chem-drawing-atoms flopped))))
         (should (chem-ir-test--close-p (- (chem-atom-x atom))
                                        (chem-atom-x f)))
         (should (chem-ir-test--close-p (chem-atom-y atom)
                                        (chem-atom-y f)))
         (should (chem-ir-test--close-p (chem-atom-x atom)
                                        (chem-atom-x v)))
         (should (chem-ir-test--close-p (- (chem-atom-y atom))
                                        (chem-atom-y v)))))
     (chem-drawing-atoms plain))))

;;; Traversal

(ert-deftest chem-ir-traversal-nodes ()
  "Traversal nodes form a consistent tree over the canonical graph."
  (let* ((drawing (chem-ir-test--drawing-from-file "corpus/benzene.mol" nil))
         (root (chem-drawing-root drawing))
         (nodes (chem-tree-nodes drawing)))
    (should (chem-node-p root))
    (should-not (chem-node-edge-id root))
    (should (= 6 (length nodes)))
    (should (eq root (chem-node-parent (car nodes))))
    (dolist (node nodes)
      (should (chem-node-p node))
      (should (chem-node-edge-id node))
      (should (chem-edge-at drawing (chem-node-edge-id node)))
      (should (memq node (chem-node-children (chem-node-parent node))))
      (should (eq node (chem-tree-node-for-edge
                        drawing (chem-node-edge-id node)))))
    ;; Every node reaches the root through parent links.
    (dolist (node nodes)
      (let ((cursor node))
        (while (chem-node-parent cursor)
          (setq cursor (chem-node-parent cursor)))
        (should (eq cursor root)))))
  (let* ((drawing (chem-ir-test--drawing-from-file "corpus/naphthalene.mol" nil))
         (closures (cl-remove-if-not #'chem-node-closure-p
                                     (chem-tree-nodes drawing))))
    (should (= 2 (length closures)))
    (dolist (node closures)
      (should (chem-node-phantom-p node)))))

(ert-deftest chem-ir-traversal-trunk ()
  "Trunk flags mark the path from entry to exit."
  (let* ((drawing (chem-ir-test--drawing-from-file "corpus/butene_e.mol" nil))
         (root (chem-drawing-root drawing))
         (exit (chem-drawing-exit-node drawing)))
    (should exit)
    (should (chem-node-trunk-p exit))
    (let ((cursor exit))
      (while (chem-node-parent cursor)
        (unless (eq cursor root)
          (should (chem-node-trunk-p cursor)))
        (setq cursor (chem-node-parent cursor))))
    (should-not (chem-node-trunk-p root)))
  (let* ((drawing (chem-ir-test--drawing-from-file
                   "corpus/ethanol.mol" '(("entry-atom" . 1)
                                          ("exit-atom" . 1)))))
    (should (eq (chem-drawing-root drawing)
                (chem-drawing-exit-node drawing)))
    (should-not (cl-find-if #'chem-node-trunk-p (chem-tree-nodes drawing)))))

(ert-deftest chem-ir-ring-display-policy ()
  "Circle display is exporter policy; the traversal is unchanged."
  (let* ((plain (chem-ir-test--drawing-from-file "corpus/benzene.mol" nil))
         (circles (chem-ir-test--drawing-from-file
                   "corpus/benzene.mol" '(("aromatic-circles" . t)))))
    (should (= (length (chem-tree-nodes plain))
               (length (chem-tree-nodes circles))))
    (should (string-match-p "mcfcringle"
                            (chem-ir-test--render
                             circles '(("aromatic-circles" . t)))))))

(ert-deftest chem-ir-traversal-cross ()
  "Cross bonds are represented on traversal nodes, not chemical edges."
  (let* ((drawing (chem-ir-test--drawing-from-file
                   "corpus/benzene.mol" '(("cross-bond" . ((1 2))))))
         (nodes (chem-tree-nodes drawing))
         (crossed (cl-remove-if-not #'chem-node-cross-p nodes))
         (edge (chem-get-edge drawing 0 1))
         (link-nodes (cl-remove-if-not
                      (lambda (node)
                        (and (chem-node-link-p node)
                             (equal (chem-node-edge-id node)
                                    (chem-edge-id edge))))
                      nodes)))
    (should (= 1 (length crossed)))
    (should (chem-edge-cross-p edge))
    (should (chem-node-phantom-p (car crossed)))
    (should (equal (chem-edge-id edge)
                   (chem-node-edge-id (car crossed))))
    (should (eq :double (chem-edge-order edge)))
    (should (= 1 (length link-nodes)))
    (should-not (chem-node-cross-p (car link-nodes)))))

;;; Contract boundaries

(ert-deftest chem-ir-no-ingestion-state ()
  "The finished drawing carries no ingestion scratch or option snapshots."
  (let ((drawing-slots (mapcar #'car (cl-struct-slot-info 'chem-drawing)))
        (atom-slots (mapcar #'car (cl-struct-slot-info 'chem-atom)))
        (node-slots (mapcar #'car (cl-struct-slot-info 'chem-node))))
    (dolist (slot '(bonds options toolkit seen-atoms seen-bonds
                    bond-scale exit-bond))
      (should-not (memq slot drawing-slots)))
    (should (memq 'parity drawing-slots))
    (should-not (memq 'options atom-slots))
    (dolist (slot '(edge-id from to link-p))
      (should (memq slot node-slots)))
    (should-not (memq 'bond node-slots))))

;;; Rings

(defun chem-ir-test--ring-cycle-consistent-p (drawing ring)
  "Return non-nil when RING's atoms and bonds form an ordered cycle."
  (let* ((atoms (chem-ring-atoms ring))
         (bonds (chem-ring-bonds ring))
         (n (length atoms)))
    (and (= n (length bonds))
         (cl-loop for i from 0 below n
                  for from = (nth i atoms)
                  for to = (nth (mod (1+ i) n) atoms)
                  for edge = (chem-edge-at drawing (nth i bonds))
                  always (or (and (= from (chem-edge-a edge))
                                  (= to (chem-edge-b edge)))
                             (and (= from (chem-edge-b edge))
                                  (= to (chem-edge-a edge))))))))

(ert-deftest chem-ir-rings ()
  "Ring records are ordered, consistent, and derive geometry."
  (let* ((drawing (chem-ir-test--drawing-from-file "corpus/benzene.mol" nil))
         (rings (chem-drawing-rings drawing)))
    (should (= 1 (length rings)))
    (let ((ring (car rings)))
      (should (= 0 (chem-ring-id ring)))
      (should (eq ring (chem-ring-at drawing 0)))
      (should (= 6 (length (chem-ring-atoms ring))))
      (should (= 6 (length (chem-ring-bonds ring))))
      (should (chem-ir-test--ring-cycle-consistent-p drawing ring))
      (should (chem-ring-aromatic-p ring))
      (should (chem-ring-anchor ring))
      (should (memq (chem-ring-anchor ring) (chem-ring-bonds ring)))
      (should (chem-ring-regular-p drawing ring))
      (should (chem-ring-symmetric-p drawing ring))
      (should (> (chem-ring-circumradius drawing ring) 0))
      (should (> (chem-ring-apothem drawing ring) 0))
      (dolist (idx (chem-ring-atoms ring))
        (should (chem-atom-in-ring-p drawing idx))
        (should (= 1 (length (chem-rings-for-atom drawing idx)))))
      (dolist (edge-id (chem-ring-bonds ring))
        (should (chem-edge-in-ring-p drawing edge-id))
        (should (= 1 (length (chem-rings-for-edge drawing edge-id)))))))
  (let* ((drawing (chem-ir-test--drawing-from-file "corpus/naphthalene.mol" nil))
         (rings (chem-drawing-rings drawing)))
    (should (= 2 (length rings)))
    (dolist (ring rings)
      (should (chem-ir-test--ring-cycle-consistent-p drawing ring)))
    (let ((shared (cl-find-if (lambda (edge-id)
                                (> (length (chem-rings-for-edge drawing edge-id)) 1))
                              (chem-ring-bonds (car rings)))))
      (should shared)
      (should (= 2 (length (chem-rings-for-edge drawing shared))))))
  (let ((drawing (chem-ir-test--drawing-from-file "corpus/caffeine.mol" nil)))
    (should (= 2 (length (chem-drawing-rings drawing))))
    (dolist (ring (chem-drawing-rings drawing))
      (should (chem-ir-test--ring-cycle-consistent-p drawing ring))))
  (let ((drawing (chem-ir-test--drawing-from-file "corpus/cyclohexane.mol" nil)))
    (should-not (chem-ring-aromatic-p (car (chem-drawing-rings drawing)))))
  (let ((drawing (chem-ir-test--drawing "c1ccccc1" nil)))
    (should (chem-ring-aromatic-p (car (chem-drawing-rings drawing))))))

(ert-deftest chem-ir-ring-side ()
  "Ring sides are consistent per cycle and drive the exporter clockwise cache."
  (dolist (file '("corpus/benzene.mol" "corpus/naphthalene.mol"
                  "corpus/indole.mol" "corpus/caffeine.mol"))
    (let* ((options (chem--build-options nil))
           (drawing (chem-ir-test--drawing-from-file file nil))
           (state (chem-exp-chemfig--make-state options)))
      (chem-exp-chemfig--prepare-rings state drawing options)
      (dolist (ring (chem-drawing-rings drawing))
        (let* ((atoms (chem-ring-atoms ring))
               (n (length atoms))
               (sides (cl-loop for i from 0 below n
                               collect (chem-ring-side
                                        drawing ring
                                        (nth i atoms)
                                        (nth (mod (1+ i) n) atoms)))))
          (should (cl-every (lambda (side) (eq side (car sides))) sides))))
      (let ((ring-bonds (cl-remove-duplicates
                         (apply #'append
                                (mapcar #'chem-ring-bonds
                                        (chem-drawing-rings drawing)))
                         :test #'=)))
        (dolist (edge-id ring-bonds)
          (let ((node (chem-tree-node-for-edge drawing edge-id)))
            (when node
              (let* ((from (chem-node-from node))
                     (to (chem-node-to node))
                     (clockwise (chem-exp-chemfig--node-clockwise state node)))
                (unless (zerop clockwise)
                  (let ((expected (if (= clockwise -1) :left :right)))
                    (should (cl-find expected
                                     (mapcar (lambda (ring)
                                               (chem-ring-side drawing ring from to))
                                             (chem-rings-for-edge drawing edge-id))))))))))))))

(ert-deftest chem-ir-ring-circle-geometry ()
  "Derived ring geometry matches the exporter's synthetic circle."
  (let* ((options (chem--build-options '(("aromatic-circles" . t))))
         (drawing (chem-ir-test--drawing-from-file
                   "corpus/benzene.mol" '(("aromatic-circles" . t))))
         (state (chem-exp-chemfig--make-state options)))
    (chem-exp-chemfig--prepare-rings state drawing options)
    (let (circles)
      (maphash (lambda (_ items) (setq circles (append circles items)))
               (chem-exp-chemfig-state-circle-children state))
      (should (= 1 (length circles)))
      (let* ((circle (car circles))
             (ring (car (chem-drawing-rings drawing)))
             (anchor-node (chem-tree-node-for-edge
                           drawing (chem-ring-anchor ring)))
             (center (chem-ring-center drawing ring))
             (atom (gethash (chem-node-to anchor-node)
                            (chem-drawing-atoms drawing)))
             (outer-r (sqrt (+ (expt (- (chem-atom-x atom) (nth 0 center)) 2)
                               (expt (- (chem-atom-y atom) (nth 1 center)) 2))))
             (n (length (chem-ring-atoms ring)))
             (alpha (- (/ float-pi 2) (/ float-pi n)))
             (inner-r (* (sin alpha) outer-r)))
        (should (chem-ring-aromatic-p ring))
        (should (chem-ir-test--close-p
                 outer-r (chem-exp-chemfig-circle-length circle) 0.01))
        (should (chem-ir-test--close-p
                 inner-r (/ (chem-exp-chemfig-circle-radius circle) 1.5) 0.01))
        (should (chem-ir-test--close-p
                 (chem-exp-chemfig--node-angle drawing anchor-node)
                 (chem-exp-chemfig-circle-parent-angle circle)))))))

(provide 'chem-ir-test)
;;; chem-ir-test.el ends here
