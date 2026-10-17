;;; chem-graph-test.el --- Structural (graph) tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Giovanni Crisalfi

;; This file is not part of GNU Emacs.

;;; Commentary:

;; These tests close the loop between the generator and the molecule it claims
;; to draw.  The generated chemfig is parsed back into a molecular graph and
;; compared to the graph read directly from Indigo (captured *before*
;; `chem-indigo-build' aromatizes the toolkit).  Unlike the byte-parity suite,
;; this does not depend on the reference implementation: it proves the output
;; encodes the same atoms and bonds.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Ensure the sibling support file resolves when the test is loaded directly.
(let ((here (file-name-directory (or load-file-name buffer-file-name))))
  (when here (add-to-list 'load-path here)))

;; Prefer a sibling emacs-indigo checkout, as in `chem-parity-test.el'.
(let ((sibling (expand-file-name "emacs-indigo"
                                 (file-name-directory
                                  (directory-file-name
                                   (file-name-directory (or load-file-name buffer-file-name)))))))
  (when (file-directory-p sibling)
    (add-to-list 'load-path sibling)))

(require 'chem-mode)
(require 'chem-exp-chemfig)
(require 'chem-exp-chemfig-parser)

;;; Graph representation
;;
;; A graph is (LABELS . ADJ).  LABELS is a vector of (ELEMENT CHARGE RADICAL).
;; ADJ is a vector of alists (NEIGHBOR . ORDER).

(defun chem-graph-test--keyword-order (order)
  "Map an Indigo bond ORDER keyword to a structural integer."
  (pcase order
    (:single 1) (:double 2) (:triple 3) (:aromatic 4) (_ 1)))

(defun chem-graph-test--indigo-graph (mol)
  "Return the graph of MOL as read directly from Indigo."
  (let* ((n (indigo-count-atoms mol))
         (labels (make-vector n nil))
         (adj (make-vector n nil)))
    (indigo-with-atoms-iterator (it mol)
      (let ((atom (indigo-next it)))
        (while atom
          (aset labels (indigo-index atom)
                (list (indigo-symbol atom)
                      (or (indigo-charge atom) 0)
                      (condition-case nil (indigo-radical-electrons atom) (error 0))))
          (setq atom (indigo-next it)))))
    (indigo-with-bonds-iterator (it mol)
      (let ((bond (indigo-next it)))
        (while bond
          (let* ((a (indigo-index (indigo-source bond)))
                 (b (indigo-index (indigo-destination bond)))
                 (order (chem-graph-test--keyword-order
                         (indigo-bond-order bond))))
            (aset adj a (cons (cons b order) (aref adj a)))
            (aset adj b (cons (cons a order) (aref adj b))))
          (setq bond (indigo-next it)))))
    (cons labels adj)))

(defun chem-graph-test--parse-graph (code)
  "Parse chemfig CODE into the same graph representation as Indigo."
  (let* ((parsed (chem-exp-chemfig-parse code))
         (atoms (car parsed))
         (bonds (cdr parsed))
         (n (length atoms))
         (labels (make-vector n nil))
         (adj (make-vector n nil)))
    (dolist (atom atoms)
      (aset labels (plist-get atom :id)
            (list (plist-get atom :element)
                  (plist-get atom :charge)
                  (plist-get atom :radical))))
    (dolist (bond bonds)
      (let ((x (nth 0 bond)) (y (nth 1 bond)) (order (nth 2 bond)))
        (aset adj x (cons (cons y order) (aref adj x)))
        (aset adj y (cons (cons x order) (aref adj y)))))
    (cons labels adj)))

;;; Graph comparison
;;
;; Rather than reimplementing graph isomorphism, both graphs are serialized to
;; a V2000 molfile and compared through Indigo's canonical SMILES.  This is
;; authoritative and scales to the large, highly symmetric corpus molecules
;; where a hand-rolled backtracking search explodes.

(defun chem-graph-test--insert-m-line (tag pairs)
  "Insert `M  TAG' lines for PAIRS of (INDEX . VALUE), eight per line."
  (while pairs
    (let ((chunk (cl-subseq pairs 0 (min 8 (length pairs)))))
      (setq pairs (cl-subseq pairs (length chunk)))
      (insert (format "M  %s%3d" tag (length chunk)))
      (dolist (pair chunk) (insert (format " %3d %3d" (car pair) (cdr pair))))
      (insert "\n"))))

(defun chem-graph-test--molfile (g)
  "Serialize graph G to a V2000 molfile string."
  (let* ((labels (car g))
         (adj (cdr g))
         (n (length labels))
         (edges nil))
    (dotimes (i n)
      (dolist (e (aref adj i))
        (when (< i (car e)) (push (list i (car e) (cdr e)) edges))))
    (setq edges (nreverse edges))
    (with-temp-buffer
      (insert "chem-mode\n  chem-mode graph test\n\n")
      (insert (format "%3d%3d  0  0  0  0  0  0  0  0999 V2000\n" n (length edges)))
      (dotimes (i n)
        (insert (format "%10.4f%10.4f%10.4f %-3s 0  0  0  0  0  0  0  0  0  0  0  0\n"
                        0.0 0.0 0.0 (nth 0 (aref labels i)))))
      (dolist (edge edges)
        (insert (format "%3d%3d%3d  0  0  0  0\n"
                        (1+ (nth 0 edge)) (1+ (nth 1 edge)) (nth 2 edge))))
      (let ((charges nil) (radicals nil))
        (dotimes (i n)
          (let ((ch (nth 1 (aref labels i))) (rad (nth 2 (aref labels i))))
            (when (/= ch 0) (push (cons (1+ i) ch) charges))
            ;; molfile RAD 2 = doublet (one electron), 3 = triplet (two).
            (when (and rad (/= rad 0))
              (push (cons (1+ i) (if (= rad 1) 2 3)) radicals))))
        (chem-graph-test--insert-m-line "CHG" (nreverse charges))
        (chem-graph-test--insert-m-line "RAD" (nreverse radicals)))
      (insert "M  END\n")
      (buffer-string))))

(defun chem-graph-test--canonical (g)
  "Return Indigo's canonical SMILES for graph G."
  (indigo-with-molecule (mol (chem-graph-test--molfile g))
    (indigo-canonical-smiles mol)))

(defun chem-graph-test--equal (g1 g2)
  "Return non-nil when G1 and G2 describe the same molecule."
  (equal (chem-graph-test--canonical g1)
         (chem-graph-test--canonical g2)))

;;; Helpers

(defun chem-graph-test--check (mol &optional overrides)
  "Generate from MOL with OVERRIDES and compare graphs.
Return (EXPECTED ACTUAL CODE)."
  (let* ((options (chem--build-options overrides)))
    (chem--prepare-mol mol options)
    (let* ((expected (chem-graph-test--indigo-graph mol))
           (code (chem-render (chem-indigo-build mol options) 'chemfig options))
           (actual (chem-graph-test--parse-graph code)))
      (list expected actual code))))

(defun chem-graph-test--assert-mol (mol &optional overrides)
  "Assert that generating from MOL with OVERRIDES round-trips its graph."
  (pcase-let ((`(,expected ,actual ,code) (chem-graph-test--check mol overrides)))
    (should (chem-graph-test--syntax-ok-p code))
    (should (chem-graph-test--equal expected actual))
    (unless (chem-graph-test--equal expected actual)
      (ert-info ((format "code:\n%s" code))
        (should (equal (car expected) (car actual)))
        (should (equal (cdr expected) (cdr actual)))))))

(defun chem-graph-test--syntax-ok-p (code)
  "Return non-nil when CODE is non-empty and balanced."
  (and (not (string-empty-p code))
       (let ((stack nil) (ok t) (i 0))
         (while (< i (length code))
           (let ((c (aref code i)))
             (cond ((memq c '(?\( ?\[ ?{)) (push c stack))
                   ((memq c '(?\) ?\] ?}))
                    (let ((open (cdr (assq c '((?\) . ?\() (?\] . ?\[) (?} . ?{))))))
                      (if (eq (car stack) open) (pop stack) (setq ok nil))))))
           (setq i (1+ i)))
         (and ok (null stack)))))

;;; Tests

(defconst chem-graph-test--smiles
  '("CCO" "CC(=O)O" "CC(C)C" "CC(C)(C)C" "CC=O" "CC#N" "C1CCCCC1"
    "c1ccccc1" "c1ccc2ccccc2c1" "c1ccncc1" "c1cc[nH]c1" "c1ccoc1" "c1ccc2[nH]ccc2c1"
    "Oc1ccccc1" "NCC(=O)O" "C[N+](C)(C)C" "CC(=O)[O-]" "O=[N+]([O-])c1ccccc1" "OP(=O)(O)O"
    "CN1C=NC2=C1C(=O)N(C(=O)N2C)C" "N[C@@H](C)C(=O)O" "C/C=C/C" "C/C=C\\C"
    "[Na+].[Cl-]" "[NH4+].[Cl-]" "CCO.CCO" "[CH3]" "[O]" "FC(F)(F)F" "c1ccc(cc1)C(=O)O")
  "SMILES strings whose generated chemfig must round-trip.")

(ert-deftest chem-graph-smiles ()
  "Every curated SMILES must encode the same graph as Indigo."
  :tags '(graph)
  ;; `strict' is turned off because emacs-indigo cannot compute the implicit
  ;; hydrogens of an aromatic nitrogen loaded from SMILES (e.g. pyridine); the
  ;; rendered graph is still correct, so the error policy is irrelevant here.
  (dolist (smiles chem-graph-test--smiles)
    (ert-info ((format "smiles: %s" smiles))
      (indigo-with-molecule (mol smiles)
        (chem-graph-test--assert-mol mol '(("strict" . nil)))))))

(defconst chem-graph-test--corpus
  (let ((dir (expand-file-name "corpus"
                               (file-name-directory (or load-file-name buffer-file-name)))))
    (when (file-directory-p dir)
      (directory-files dir t "\\.mol\\'")))
  "MOL files that must round-trip under the default options.")

(ert-deftest chem-graph-corpus ()
  "Every corpus MOL must encode the same graph as Indigo (default options).
Also reports total generation time (loose upper bound only)."
  :tags '(graph)
  (should chem-graph-test--corpus)
  (let ((total-time 0.0))
    (dolist (path chem-graph-test--corpus)
      (ert-info ((format "mol: %s" (file-name-nondirectory path)))
        (let ((start (float-time)))
          (indigo-with-mol-file (mol path)
            (chem-graph-test--assert-mol mol))
          (setq total-time (+ total-time (- (float-time) start))))))
    (message "chem-mode: corpus generated in %.3fs" total-time)
    (should (< total-time 60.0))))

(ert-deftest chem-graph-hydrogens ()
  "Hydrogen add/delete round-trip against Indigo."
  :tags '(graph)
  (dolist (smiles '("CCO" "c1ccccc1" "NCC(=O)O"))
    (dolist (override '(("hydrogens" . add) ("hydrogens" . delete)))
      (ert-info ((format "smiles: %s override: %S" smiles override))
        (indigo-with-molecule (mol smiles)
          (chem-graph-test--assert-mol mol (list override '("strict" . nil))))))))

;;; Option matrix (metamorphic testing)

(defconst chem-graph-test--profiles
  '(("default" . nil)
    ("relative-angles" . (("relative-angles" . t)))
    ("rotate" . (("angle" . 30.0) ("flip" . t)))
    ("flip" . (("flip" . t)))
    ("flop" . (("flop" . t)))
    ("show-carbons" . (("show-carbons" . t)))
    ("show-methyls" . (("show-methyls" . t)))
    ("atom-numbers" . (("atom-numbers" . t)))
    ("bond-scale-keep" . (("bond-scale" . keep)))
    ("bond-scale-scale" . (("bond-scale" . scale) ("bond-stretch" . 2.0)))
    ("bond-scale-normalize" . (("bond-scale" . normalize) ("bond-stretch" . 1.5)))
    ("wrap-chemfig" . (("wrap-chemfig" . t)))
    ("submol-name" . (("submol-name" . "mol")))
    ("fancy-bonds" . (("fancy-bonds" . t)))
    ("markers" . (("markers" . "a"))))
  "Option profiles that must not change the encoded molecule.
`terse' is excluded because it strips the ring-closure comments the parser
needs; `aromatic-circles' because the circle replaces explicit ring bond
orders; and `hydrogens' because it is covered by `chem-graph-hydrogens'.
`atom-numbers' drops charges and radicals, so it is only exercised on neutral,
closed-shell molecules.")

(defun chem-graph-test--charged-p (graph)
  "Return non-nil when GRAPH contains a charged or radical atom."
  (cl-some (lambda (atom) (or (/= (nth 1 atom) 0) (/= (nth 2 atom) 0)))
           (car graph)))

(defun chem-graph-test--matrix-check (mol base)
  "Run every option profile on MOL (with base overrides BASE).
Return a list of (PROFILE . CODE) failures."
  (let (failures)
    (dolist (profile chem-graph-test--profiles)
      (condition-case err
          (pcase-let ((`(,expected ,actual ,code)
                       (chem-graph-test--check
                        mol (append (cdr profile) base))))
            (unless (and (equal (car profile) "atom-numbers")
                         (chem-graph-test--charged-p expected))
              (unless (chem-graph-test--equal expected actual)
                (push (cons (car profile) code) failures))))
        (error (push (cons (car profile) (format "error: %S" err)) failures))))
    (nreverse failures)))

(ert-deftest chem-graph-option-matrix ()
  "No option profile may change the molecule encoded by the chemfig output."
  :tags '(graph)
  (dolist (smiles chem-graph-test--smiles)
    (ert-info ((format "smiles: %s" smiles))
      (indigo-with-molecule (mol smiles)
        (dolist (failure (chem-graph-test--matrix-check
                          mol '(("strict" . nil))))
          (ert-fail (format "smiles %s profile %s:\n%s"
                            smiles (car failure) (cdr failure)))))))
  (dolist (path chem-graph-test--corpus)
    (ert-info ((format "mol: %s" (file-name-nondirectory path)))
      (indigo-with-mol-file (mol path)
        (dolist (failure (chem-graph-test--matrix-check mol nil))
          (ert-fail (format "mol %s profile %s:\n%s"
                            (file-name-nondirectory path) (car failure)
                            (cdr failure))))))))

;;; Fuzz smoke + performance

(defconst chem-graph-test--fuzz-atoms
  '("C" "C" "C" "N" "O" "S" "F" "Cl")
  "Atoms used by the random tree generator.")

(defconst chem-graph-test--fuzz-maxdeg
  '(("C" . 4) ("N" . 3) ("O" . 2) ("S" . 2) ("F" . 1) ("Cl" . 1))
  "Maximum heavy-atom degree per element.")

(defun chem-graph-test--fuzz-smiles (state counter)
  "Return a random, valid, single-bonded SMILES tree, or nil when out of budget.
STATE is a random state; COUNTER is a (REMAINING . nil) cell mutated in place,
so the total atom count stays bounded."
  (when (> (car counter) 0)
    (setcar counter (1- (car counter)))
    (let* ((atoms chem-graph-test--fuzz-atoms)
           (element (nth (cl-random (length atoms) state) atoms))
           (max (min (cdr (assoc element chem-graph-test--fuzz-maxdeg))
                     (car counter)))
           (children nil))
      (dotimes (_ (cl-random (1+ max) state))
        (let ((child (chem-graph-test--fuzz-smiles state counter)))
          (when child (push child children))))
      (setq children (nreverse children))
      (concat element
              (mapconcat (lambda (child) (concat "(" child ")")) (cdr children) "")
              (or (car children) "")))))

(ert-deftest chem-graph-fuzz ()
  "Random valid SMILES must generate syntax-valid, graph-faithful output."
  :tags '(graph)
  (let ((state (cl-make-random-state 20251016))
        (count 0))
    (dotimes (_ 60)
      (let ((smiles (chem-graph-test--fuzz-smiles state (cons 8 nil))))
        (setq count (1+ count))
        (ert-info ((format "fuzz #%d: %s" count smiles))
          (indigo-with-molecule (mol smiles)
            (let ((options (chem--build-options '(("strict" . nil)))))
              (chem--prepare-mol mol options)
              (let* ((expected (chem-graph-test--indigo-graph mol))
                     (code (chem-render (chem-indigo-build mol options) 'chemfig options)))
                (should (chem-graph-test--syntax-ok-p code))
                (should (chem-graph-test--equal
                         expected (chem-graph-test--parse-graph code)))))))))
    (should (> count 0))))

(provide 'chem-graph-test)
;;; chem-graph-test.el ends here
