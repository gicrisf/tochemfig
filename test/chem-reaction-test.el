;;; chem-reaction-test.el --- Reaction IR and layout tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Giovanni Crisalfi

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the reaction IR, Indigo ingestion, and renderer-neutral layout.
;; Exporters are exercised in later phases.

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
(require 'chem-reaction)
(require 'chem-exp-chemfig)
(require 'chem-exp-svg)

(defconst chem-reaction-test--root
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the test files.")

(defun chem-reaction-test--path (relative)
  "Resolve RELATIVE against the test directory."
  (expand-file-name relative chem-reaction-test--root))

(defun chem-reaction-test--options (&optional overrides)
  "Return reaction options from OVERRIDES."
  (chem--build-reaction-options overrides))

(defun chem-reaction-test--reaction (smiles &optional overrides)
  "Build a reaction from SMILES with OVERRIDES."
  (chem-from-reaction-string smiles (chem-reaction-test--options overrides)))

(defun chem-reaction-test--file (relative &optional overrides)
  "Build a reaction from RELATIVE with OVERRIDES."
  (chem-from-reaction-file (chem-reaction-test--path relative)
                           (chem-reaction-test--options overrides)))

(defun chem-reaction-test--first-atom (drawing)
  "Return DRAWING's atom with index zero."
  (gethash 0 (chem-drawing-atoms drawing)))

(defun chem-reaction-test--coordinates (drawing)
  "Return DRAWING's atom coordinates in a stable order."
  (let (coordinates)
    (maphash (lambda (_ atom)
               (push (list (chem-atom-x atom) (chem-atom-y atom))
                     coordinates))
             (chem-drawing-atoms drawing))
    (sort coordinates
          (lambda (a b)
            (if (= (car a) (car b))
                (< (cadr a) (cadr b))
              (< (car a) (car b)))))))

(defun chem-reaction-test--layout-signature (reaction)
  "Return a comparable signature for REACTION's layout."
  (mapcar (lambda (part)
            (list (chem-reaction-part-kind part)
                  (chem-reaction-part-x part)
                  (chem-reaction-part-y part)
                  (chem-reaction-part-width part)
                  (chem-reaction-part-height part)))
          (chem-reaction-layout reaction)))

(defun chem-reaction-test--kinds (reaction)
  "Return the kinds in REACTION's layout."
  (mapcar #'chem-reaction-part-kind (chem-reaction-layout reaction)))

(defun chem-reaction-test--parts-of-kind (reaction kind)
  "Return REACTION's layout parts of KIND."
  (cl-remove-if-not (lambda (part)
                      (eq kind (chem-reaction-part-kind part)))
                    (chem-reaction-layout reaction)))

(defun chem-reaction-test--flow-kinds (reaction)
  "Return REACTION's molecule-flow part kinds."
  (cl-remove-if (lambda (kind)
                  (eq kind :text))
                (chem-reaction-test--kinds reaction)))

(defun chem-reaction-test--text-parts (reaction role)
  "Return REACTION's :text parts with ROLE."
  (cl-remove-if-not (lambda (part)
                      (and (eq :text (chem-reaction-part-kind part))
                           (eq role (chem-reaction-part-role part))))
                    (chem-reaction-layout reaction)))

(ert-deftest chem-reaction-one-to-one ()
  "One reactant and one product build the expected reaction IR."
  (let* ((reaction (chem-reaction-test--reaction "CCO>>CC=O"))
         (reactant (car (chem-reaction-reactants reaction)))
         (product (car (chem-reaction-products reaction))))
    (should (= 1 (length (chem-reaction-reactants reaction))))
    (should (= 1 (length (chem-reaction-products reaction))))
    (should (= 0 (length (chem-reaction-agents reaction))))
    (should (= 3 (hash-table-count (chem-drawing-atoms reactant))))
    (should (= 2 (hash-table-count (chem-drawing-edges reactant))))
    (should (= 3 (hash-table-count (chem-drawing-atoms product))))
    (should (= 2 (hash-table-count (chem-drawing-edges product))))
    (should (equal '(:above "" :below "")
                   (chem-reaction-conditions reaction)))
    (should (equal '(:style :forward)
                   (chem-reaction-arrow reaction)))
    (should (equal '(:reactant :arrow :product)
                   (chem-reaction-test--kinds reaction)))))

(ert-deftest chem-reaction-multiple-components ()
  "Multiple reactants and products preserve order and add plus signs."
  (let ((two-to-one (chem-reaction-test--reaction "C=C.C=C>>C1CCC1"))
        (one-to-two (chem-reaction-test--reaction "CCO>>C=C.O"))
        (two-to-two (chem-reaction-test--reaction
                     "CC(=O)O.OCC>>CC(=O)OCC.O")))
    (should (= 2 (length (chem-reaction-reactants two-to-one))))
    (should (= 1 (length (chem-reaction-products two-to-one))))
    (should (equal '(:reactant :plus :reactant :arrow :product)
                   (chem-reaction-test--kinds two-to-one)))
    (should (= 1 (length (chem-reaction-reactants one-to-two))))
    (should (= 2 (length (chem-reaction-products one-to-two))))
    (should (equal '(:reactant :arrow :product :plus :product)
                   (chem-reaction-test--kinds one-to-two)))
    (should (equal '(:reactant :plus :reactant :arrow
                     :product :plus :product)
                   (chem-reaction-test--kinds two-to-two)))))

(ert-deftest chem-reaction-agents ()
  "Agents are parsed, labeled, and kept out of the horizontal flow."
  (let ((reaction (chem-reaction-test--reaction "CC>O.CCO>CC")))
    (should (= 1 (length (chem-reaction-reactants reaction))))
    (should (= 2 (length (chem-reaction-agents reaction))))
    (should (= 1 (length (chem-reaction-products reaction))))
    (should (equal '("H2O" "C2H6O")
                   (mapcar #'chem-reaction-agent-label
                           (chem-reaction-agents reaction))))
    (should (equal '(:reactant :arrow :product)
                   (chem-reaction-test--flow-kinds reaction)))
    (should (= 1 (length (chem-reaction-test--text-parts
                          reaction :agent-label)))))
  (let ((charged (chem-reaction-test--reaction "CC>[H+]>CC")))
    (should (equal '("H+")
                   (mapcar #'chem-reaction-agent-label
                           (chem-reaction-agents charged))))))

(ert-deftest chem-reaction-conditions-and-arrow ()
  "Conditions and arrow options are stored in the reaction."
  (let ((reaction (chem-reaction-test--reaction
                   "CCO>>CC=O"
                   '(("reaction-above" . "heat")
                     ("reaction-below" . "cat.")
                     ("reaction-arrow" . "equilibrium")))))
    (should (equal '(:above "heat" :below "cat.")
                   (chem-reaction-conditions reaction)))
    (should (equal '(:style :equilibrium)
                   (chem-reaction-arrow reaction))))
  (should-error
   (chem-reaction-test--reaction
    "CCO>>CC=O" '(("reaction-arrow" . "sideways")))
   :type 'user-error))

(ert-deftest chem-reaction-text-layout ()
  "Agents and conditions are centered above and below the arrow."
  (let* ((reaction (chem-reaction-test--reaction
                    "CC>O>CC"
                    '(("reaction-above" . "heat")
                      ("reaction-below" . "cat."))))
         (arrow (car (chem-reaction-test--parts-of-kind reaction :arrow)))
         (above (car (chem-reaction-test--text-parts reaction :above)))
         (below (car (chem-reaction-test--text-parts reaction :below)))
         (center-x (+ (chem-reaction-part-x arrow)
                      (/ (chem-reaction-part-width arrow) 2.0)))
         (center-y (+ (chem-reaction-part-y arrow)
                      (/ (chem-reaction-part-height arrow) 2.0))))
    (should above)
    (should below)
    (should (equal "H2O, heat" (chem-reaction-part-text above)))
    (should (equal "cat." (chem-reaction-part-text below)))
    (dolist (part (list above below))
      (should (< (abs (- (+ (chem-reaction-part-x part)
                           (/ (chem-reaction-part-width part) 2.0))
                        center-x))
                 1e-9)))
    (should (> (chem-reaction-part-y above) center-y))
    (should (< (chem-reaction-part-y below) center-y))))

(ert-deftest chem-reaction-normalized-bond-scale ()
  "Reaction components share a normalized bond scale."
  (dolist (override '(("bond-scale" . keep)
                      ("bond-scale" . scale)))
    (let* ((reaction (chem-reaction-test--reaction
                      "CCO>>CC=O" (list override)))
           (drawings (append (chem-reaction-reactants reaction)
                             (chem-reaction-products reaction))))
      (dolist (drawing drawings)
        (dolist (edge (chem-visible-edges drawing))
          (should (< (abs (- 1.0 (chem-edge-length drawing edge)))
                     1e-6)))))))

(ert-deftest chem-reaction-options-for-exporters ()
  "Reaction options are available for SVG and chemfig."
  (dolist (exporter '(svg chemfig))
    (let ((options (chem--build-reaction-options nil exporter)))
      (dolist (key '(:reaction-arrow
                     :reaction-above
                     :reaction-below
                     :reaction-spacing
                     :reaction-arrow-length
                     :reaction-plus-size
                     :reaction-condition-width))
        (should (plist-member options key))))))

(ert-deftest chem-reaction-geometry-options ()
  "Geometry options apply to reaction components."
  (let* ((plain (chem-reaction-test--reaction "CCO>>CC=O"))
         (rotated (chem-reaction-test--reaction
                   "CCO>>CC=O" '(("angle" . 90))))
         (plain-reactant (car (chem-reaction-reactants plain)))
         (rotated-reactant (car (chem-reaction-reactants rotated))))
    (should-not (equal (chem-reaction-test--coordinates plain-reactant)
                       (chem-reaction-test--coordinates rotated-reactant)))))

(ert-deftest chem-reaction-component-options ()
  "Molecule options apply independently to every component."
  (let ((reaction (chem-reaction-test--reaction
                   "CC=O>>CC=O"
                   '(("show-carbons" . t)
                     ("fancy-bonds" . t)))))
    (dolist (drawing (append (chem-reaction-reactants reaction)
                             (chem-reaction-products reaction)))
      (let ((carbon (cl-find-if (lambda (idx)
                                  (equal "C" (chem-atom-element
                                              (gethash idx
                                                       (chem-drawing-atoms
                                                        drawing)))))
                                (number-sequence
                                 0
                                 (1- (hash-table-count
                                      (chem-drawing-atoms drawing)))))))
        (should carbon)
        (should (equal "C" (chem-atom-label
                            (gethash carbon (chem-drawing-atoms drawing))))))
      (let ((double (cl-find-if (lambda (edge)
                                  (eq :double (chem-edge-order edge)))
                                (chem-edges drawing))))
        (should double)
        (should (eq 'fancy (chem-edge-decoration double)))))))

(ert-deftest chem-reaction-rxn-file ()
  "RXN files build the same reaction shape as reaction SMILES."
  (let ((reaction (chem-reaction-test--file "corpus/reactions/simple.rxn")))
    (should (= 1 (length (chem-reaction-reactants reaction))))
    (should (= 1 (length (chem-reaction-products reaction))))
    (should (= 0 (length (chem-reaction-agents reaction))))
    (should (equal '(:reactant :arrow :product)
                   (chem-reaction-test--kinds reaction))))
  (let ((reaction (chem-reaction-test--file
                   "corpus/reactions/esterification.rxn")))
    (should (= 2 (length (chem-reaction-reactants reaction))))
    (should (= 2 (length (chem-reaction-products reaction))))))

(ert-deftest chem-reaction-validation ()
  "Invalid reactions signal user errors."
  (dolist (smiles '("" "CC>>" ">>CC" "CCO>CC=O" "CC>>CC>>CC"))
    (should-error (chem-reaction-test--reaction smiles) :type 'user-error))
  (should-error
   (chem-reaction-test--file "corpus/reactions/missing.rxn")
   :type 'user-error)
  (let ((empty (make-temp-file "chem-reaction-empty-" nil ".rxn")))
    (unwind-protect
        (progn
          (with-temp-file empty)
          (should-error
           (chem-from-reaction-file empty (chem-reaction-test--options))
           :type 'user-error))
      (delete-file empty)))
  (let ((multiple (make-temp-file "chem-reaction-multiple-" nil ".rxn")))
    (unwind-protect
        (progn
          (with-temp-file multiple
            (insert "$RXN\n$RXN\n"))
          (should-error
           (chem-from-reaction-file multiple (chem-reaction-test--options))
           :type 'user-error))
      (delete-file multiple))))

(ert-deftest chem-reaction-determinism ()
  "Building the same reaction twice produces the same layout."
  (let* ((first (chem-reaction-test--reaction
                 "CC(=O)O.OCC>>CC(=O)OCC.O"))
         (second (chem-reaction-test--reaction
                  "CC(=O)O.OCC>>CC(=O)OCC.O")))
    (should (= (length (chem-reaction-reactants first))
               (length (chem-reaction-reactants second))))
    (should (= (length (chem-reaction-products first))
               (length (chem-reaction-products second))))
    (should (equal (chem-reaction-test--layout-signature first)
                   (chem-reaction-test--layout-signature second)))
    (let ((a (chem-reaction-test--first-atom
              (car (chem-reaction-reactants first))))
          (b (chem-reaction-test--first-atom
              (car (chem-reaction-reactants second)))))
      (should (= (chem-atom-x a) (chem-atom-x b)))
      (should (= (chem-atom-y a) (chem-atom-y b))))))

(ert-deftest chem-reaction-layout-shape ()
  "Layout centers molecules on the arrow and avoids overlap."
  (let* ((reaction (chem-reaction-test--reaction
                    "CC(=O)O.OCC>>CC(=O)OCC.O"))
         (parts (cl-remove-if
                 (lambda (part)
                   (eq (chem-reaction-part-kind part) :text))
                 (chem-reaction-layout reaction))))
    (should (= 1 (length (chem-reaction-test--parts-of-kind reaction :arrow))))
    (should (= 2 (length (chem-reaction-test--parts-of-kind reaction :plus))))
    (should (= 2 (length (chem-reaction-test--parts-of-kind reaction :reactant))))
    (should (= 2 (length (chem-reaction-test--parts-of-kind reaction :product))))
    (dolist (part parts)
      (when (memq (chem-reaction-part-kind part)
                  '(:reactant :product))
        (should (< (abs (+ (chem-reaction-part-y part)
                           (/ (chem-reaction-part-height part) 2.0)))
                   1e-9))))
    (cl-loop for part in parts
             for next in (cdr parts)
             do (should (<= (+ (chem-reaction-part-x part)
                               (chem-reaction-part-width part))
                            (chem-reaction-part-x next))))))

(ert-deftest chem-reaction-layout-options ()
  "Layout honors spacing and arrow-length options."
  (let* ((options '(("reaction-spacing" . 2)
                    ("reaction-arrow-length" . 6)))
         (reaction (chem-reaction-test--reaction
                    "CCO>>CC=O" options))
         (parts (chem-reaction-layout reaction))
         (reactant (car (chem-reaction-test--parts-of-kind
                         reaction :reactant)))
         (arrow (car (chem-reaction-test--parts-of-kind
                      reaction :arrow)))
         (product (car (chem-reaction-test--parts-of-kind
                        reaction :product))))
    (should (= 6.0 (chem-reaction-part-width arrow)))
    (should (= (+ (chem-reaction-part-x reactant)
                  (chem-reaction-part-width reactant)
                  2.0)
               (chem-reaction-part-x arrow)))
    (should (= (+ (chem-reaction-part-x arrow)
                  (chem-reaction-part-width arrow)
                  2.0)
               (chem-reaction-part-x product))))
  (let* ((reaction (chem-reaction-test--reaction
                    "C=C.C=C>>C1CCC1"
                    '(("reaction-plus-size" . 3))))
         (plus (car (chem-reaction-test--parts-of-kind reaction :plus))))
    (should (= 3.0 (chem-reaction-part-width plus)))
    (should (= 3.0 (chem-reaction-part-height plus)))))

(ert-deftest chem-reaction-layout-wraps-long-text ()
  "Long condition text wraps at the arrow length."
  (let* ((text "alpha beta gamma delta epsilon zeta eta theta iota kappa")
         (reaction (chem-reaction-test--reaction
                    "CCO>>CC=O" (list (cons "reaction-above" text))))
         (arrow (car (chem-reaction-test--parts-of-kind reaction :arrow)))
         (parts (cl-remove-if-not
                 (lambda (part)
                   (eq :text (chem-reaction-part-kind part)))
                 (chem-reaction-layout reaction))))
    (should (= chem-reaction--default-arrow-length
               (chem-reaction-part-width arrow)))
    (should (> (length parts) 1))
    (should (equal text
                   (mapconcat #'chem-reaction-part-text parts " ")))
    (dolist (part parts)
      (should (<= (chem-reaction-part-width part)
                  (+ chem-reaction--default-arrow-length 1e-9))))))

(ert-deftest chem-reaction-layout-smilesdrawer-wrap-points ()
  "Wrapping matches SmilesDrawer's line breaks at the arrow length."
  (let* ((text "alpha beta gamma delta epsilon zeta eta theta iota kappa")
         (reaction (chem-reaction-test--reaction
                    "CCO>>CC=O" (list (cons "reaction-above" text))))
         (parts (cl-remove-if-not
                 (lambda (part)
                   (eq :above-line (chem-reaction-part-role part)))
                 (chem-reaction-layout reaction))))
    (should (equal '("alpha beta gamma delta"
                     "epsilon zeta eta theta"
                     "iota kappa")
                   (mapcar #'chem-reaction-part-text parts)))))

(ert-deftest chem-reaction-layout-condition-width ()
  "A custom condition width changes the wrap points."
  (let* ((text "alpha beta gamma delta epsilon zeta")
         (default (chem-reaction-test--reaction
                   "CCO>>CC=O" (list (cons "reaction-above" text))))
         (narrow (chem-reaction-test--reaction
                  "CCO>>CC=O"
                  (list (cons "reaction-above" text)
                        (cons "reaction-condition-width" 2.0))))
         (default-lines (cl-remove-if-not
                         (lambda (part)
                           (eq :above-line (chem-reaction-part-role part)))
                         (chem-reaction-layout default)))
         (narrow-lines (cl-remove-if-not
                        (lambda (part)
                          (eq :above-line (chem-reaction-part-role part)))
                        (chem-reaction-layout narrow))))
    (should (> (length narrow-lines) (length default-lines)))))

(ert-deftest chem-reaction-layout-multiline-conditions ()
  "Explicit newlines create one text part per line."
  (let* ((reaction (chem-reaction-test--reaction
                    "CCO>>CC=O"
                    '(("reaction-above" . "first line\nsecond line"))))
         (parts (cl-remove-if-not
                 (lambda (part)
                   (eq :text (chem-reaction-part-kind part)))
                 (chem-reaction-layout reaction))))
    (should (= 2 (length parts)))
    (should (equal '("first line" "second line")
                   (mapcar #'chem-reaction-part-text parts)))))

(ert-deftest chem-reaction-layout-bounds ()
  "Reaction bounds contain every layout part."
  (let* ((reaction (chem-reaction-test--reaction
                    "CC(=O)O.OCC>>CC(=O)OCC.O"))
         (bounds (chem-reaction-bounds reaction))
         (parts (chem-reaction-layout reaction))
         (min-x (apply #'min (mapcar #'chem-reaction-part-x parts)))
         (max-x (apply #'max (mapcar (lambda (part)
                                       (+ (chem-reaction-part-x part)
                                          (chem-reaction-part-width part)))
                                     parts))))
    (should (= min-x (nth 0 bounds)))
    (should (= max-x (nth 2 bounds)))
    (should (<= (nth 0 bounds) (nth 2 bounds)))
    (should (<= (nth 1 bounds) (nth 3 bounds)))))

(ert-deftest chem-reaction-repeated-builds ()
  "Repeated reaction builds do not leak or corrupt handles."
  (dotimes (_ 5)
    (let ((reaction (chem-reaction-test--reaction "CCO>>CC=O")))
      (should (= 1 (length (chem-reaction-reactants reaction))))
      (should (= 1 (length (chem-reaction-products reaction)))))))

(provide 'chem-reaction-test)
;;; chem-reaction-test.el ends here
