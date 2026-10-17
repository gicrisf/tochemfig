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

(require 'tochemfig)
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
  (let ((options (tochemfig--build-options overrides)))
    (indigo-with-molecule (mol smiles)
      (tochemfig--prepare-mol mol options)
      (chem-indigo-build mol options))))

(defun chem-ir-test--drawing-from-file (relative overrides)
  "Build a prepared drawing from a corpus MOL file with OVERRIDES."
  (let ((options (tochemfig--build-options overrides)))
    (indigo-with-mol-file (mol (chem-ir-test--path relative))
      (tochemfig--prepare-mol mol options)
      (chem-indigo-build mol options))))

(defun chem-ir-test--render (drawing &optional overrides)
  "Render DRAWING as chemfig with OVERRIDES."
  (chem-render drawing 'chemfig (tochemfig--build-options overrides)))

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

(provide 'chem-ir-test)
;;; chem-ir-test.el ends here
