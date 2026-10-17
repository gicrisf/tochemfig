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
  "Aromatic ring display comes from the IR, not from exporter options."
  (let* ((circles (chem-ir-test--drawing-from-file "corpus/benzene.mol" '(("aromatic-circles" . t))))
         (plain (chem-ir-test--drawing-from-file "corpus/benzene.mol" '(("aromatic-circles" . nil))))
         (circles-without-option (chem-ir-test--render circles '(("aromatic-circles" . nil))))
         (plain-with-option (chem-ir-test--render plain '(("aromatic-circles" . t)))))
    (should (string-match-p "mcfcringle" circles-without-option))
    (should-not (string-match-p "mcfcringle" plain-with-option))))

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
  "Canonical edges carry the same marker as their directed bonds."
  (let* ((drawing (chem-ir-test--drawing-from-file "corpus/ethanol.mol" '(("markers" . "a"))))
         (bonds (chem-drawing-bonds drawing)))
    (dolist (edge (chem-edges drawing))
      (let ((bond (gethash (cons (chem-edge-a edge) (chem-edge-b edge)) bonds)))
        (should (equal (chem-edge-marker edge) (chem-bond-marker bond)))))))

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
  "Stored bond geometry matches coordinates after transforms."
  (dolist (case '(("corpus/ethanol.mol" nil)
                  ("corpus/ethanol.mol" (("angle" . 30.0) ("flip" . t)))
                  ("corpus/benzene.mol" (("bond-scale" . scale)
                                         ("bond-stretch" . 2.0)))
                  ("corpus/benzene.mol" (("aromatic-circles" . t)))))
    (let ((drawing (chem-ir-test--drawing-from-file (car case) (cadr case))))
      (maphash
       (lambda (_ bond)
         (let ((start (chem-bond-start-atom bond))
               (end (chem-bond-end-atom bond)))
           (when (and start end)
             (let ((dims (chem--compare-positions
                          (chem-atom-x start) (chem-atom-y start)
                          (chem-atom-x end) (chem-atom-y end))))
               (should (chem-ir-test--close-p (nth 0 dims)
                                              (chem-bond-length bond)))
               (should (chem-ir-test--angle-close-p (nth 1 dims)
                                                    (chem-bond-angle bond)))))))
       (chem-drawing-bonds drawing)))))

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
      (let* ((pair (cons (chem-edge-a edge) (chem-edge-b edge)))
             (before (gethash pair (chem-drawing-bonds keep)))
             (after (gethash pair (chem-drawing-bonds scaled))))
        (should (chem-ir-test--close-p (* 2.0 (chem-bond-length before))
                                       (chem-bond-length after)))))))

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

(provide 'chem-ir-test)
;;; chem-ir-test.el ends here
