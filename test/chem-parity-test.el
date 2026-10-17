;;; chem-parity-test.el --- Tests for chem-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Giovanni Crisalfi

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Two kinds of test:
;;
;; - Parity tests compare `chem-render' output against golden files produced by
;;   the reference mol2chemfigPy3 implementation (see tools/gen-goldens.py).
;;   The corpus uses MOL files with explicit coordinates so the comparison is
;;   independent of the Indigo layout implementation.
;;
;; - Syntax tests verify that generated chemfig code is well formed: balanced
;;   delimiters and properly paired ring-closure markers.  Rendering is not
;;   checked (that would require a full LaTeX/chemfig toolchain).

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst chem-test--root
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the test files.")

(defconst chem-test--project
  (file-name-directory (directory-file-name chem-test--root))
  "Project root directory.")

(defun chem-test--path (relative)
  "Resolve RELATIVE against the project root."
  (expand-file-name relative chem-test--project))

;; Prefer a sibling emacs-indigo checkout during development.  It already has
;; the dynamic module built, so tests do not need to download and compile it.
(let ((sibling (chem-test--path "emacs-indigo")))
  (when (file-directory-p sibling)
    (add-to-list 'load-path sibling)))

(require 'chem-mode)
(require 'chem-exp-chemfig)

(defun chem-test--manifest ()
  "Return the list of golden test cases."
  (with-temp-buffer
    (insert-file-contents (chem-test--path "test/golden/manifest.el"))
    (goto-char (point-min))
    (read (current-buffer))))

(defun chem-test--overrides (plist)
  "Convert option PLIST into the alist form accepted by `chem--build-options'."
  (let (out)
    (cl-loop for (key value) on plist by #'cddr
             do (push (cons (substring (symbol-name key) 1) value) out))
    out))

(defun chem-test--generate-string (smiles &optional overrides)
  "Generate chemfig output for SMILES with OVERRIDES."
  (let ((options (chem--build-options overrides)))
    (chem-render (chem-from-string smiles options) 'chemfig options)))

(defun chem-test--generate-file (path &optional overrides)
  "Generate chemfig output for the MOL file at PATH with OVERRIDES."
  (let ((options (chem--build-options overrides)))
    (chem-render (chem-from-file path options) 'chemfig options)))

(defun chem-test--generate (case)
  "Generate chemfig output for a manifest CASE."
  (chem-test--generate-file
   (chem-test--path (plist-get case :input))
   (chem-test--overrides (plist-get case :options))))

(defun chem-test--expected (case)
  "Return the golden output for a manifest CASE."
  (with-temp-buffer
    (insert-file-contents (chem-test--path (plist-get case :golden)))
    (buffer-string)))

;;; Syntax validation

(defun chem-test--strip-comments (code)
  "Remove chemfig comments (text after a percent sign) from CODE."
  (mapconcat (lambda (line) (car (split-string line "%")))
             (split-string code "\n") "\n"))

(defun chem-test--balanced-p (code)
  "Return non-nil when all delimiters in CODE are balanced."
  (let ((code (chem-test--strip-comments code))
        (stack nil)
        (ok t)
        (i 0))
    (while (< i (length code))
      (let ((c (aref code i)))
        (cond
         ((memq c '(?\( ?\[ ?{)) (push c stack))
         ((memq c '(?\) ?\] ?}))
          (let ((open (cdr (assq c '((?\) . ?\() (?\] . ?\[) (?} . ?{))))))
            (if (eq (car stack) open) (pop stack) (setq ok nil))))))
      (setq i (1+ i)))
    (and ok (null stack))))

(defun chem-test--closures-paired-p (code)
  "Return non-nil when every ring-closure marker in CODE appears twice."
  (let ((code (chem-test--strip-comments code))
        (counts (make-hash-table :test 'equal))
        (start 0))
    (while (string-match "?\\[\\([^]]+\\)\\]" code start)
      (let ((name (match-string 1 code)))
        (puthash name (1+ (gethash name counts 0)) counts))
      (setq start (match-end 0)))
    (let ((ok t))
      (maphash (lambda (_ n) (unless (= n 2) (setq ok nil))) counts)
      ok)))

(defun chem-test--syntax-ok-p (code)
  "Return non-nil when CODE is syntactically valid chemfig."
  (and (not (string-empty-p code))
       (chem-test--balanced-p code)
       (chem-test--closures-paired-p code)))

;;; Parity tests (one per golden case)

(defmacro chem-test--define-parity (name)
  "Define a parity test for the case NAME."
  `(ert-deftest ,(intern (concat "chem-parity-" name)) ()
     :tags '(parity)
     (let* ((case (chem-test--case ,name))
            (got (chem-test--generate case))
            (expected (chem-test--expected case)))
       (should (chem-test--syntax-ok-p got))
       (should (equal got expected)))))

(defun chem-test--case (name)
  "Return the manifest case called NAME."
  (cl-find name (chem-test--manifest)
           :key (lambda (case) (plist-get case :name))
           :test #'equal))

(dolist (case (chem-test--manifest))
  (eval `(chem-test--define-parity ,(plist-get case :name))))

;;; Syntax tests over a SMILES smoke set (exercises the layout path)

(defconst chem-test--smiles
  '("CCO" "c1ccccc1" "CC(=O)O" "CN1C=NC2=C1C(=O)N(C(=O)N2C)C"
    "N[C@@H](C)C(=O)O" "[Na+].[Cl-]" "O=[N+]([O-])c1ccccc1")
  "SMILES strings used for syntax smoke tests.")

(ert-deftest chem-syntax-smiles ()
  "Generated output for a set of SMILES must be syntactically valid."
  (dolist (smiles chem-test--smiles)
    (dolist (overrides '(nil (("terse" . t)) (("fancy-bonds" . t))
                         (("aromatic-circles" . t)) (("wrap-chemfig" . t))))
      (let ((code (chem-test--generate-string smiles overrides)))
        (should (chem-test--syntax-ok-p code))))))

(ert-deftest chem-syntax-goldens ()
  "Every golden file must be syntactically valid chemfig."
  (dolist (case (chem-test--manifest))
    (should (chem-test--syntax-ok-p (chem-test--expected case)))))

;;; Error paths and degenerate input

(ert-deftest chem-error-invalid-smiles ()
  "An unparseable SMILES signals an error."
  (should-error (chem-test--generate-string "not a smiles")))

(ert-deftest chem-error-empty-smiles ()
  "Blank input signals a user-error."
  (should-error (chem-test--generate-string "") :type 'user-error)
  (should-error (chem-test--generate-string "   ") :type 'user-error))

(ert-deftest chem-error-invalid-entry-exit ()
  "Out-of-range entry/exit atoms signal an error."
  (should-error (chem-test--generate-string "CCO" '(("entry-atom" . 99))))
  (should-error (chem-test--generate-string "CCO" '(("exit-atom" . 99)))))

(ert-deftest chem-error-invalid-cross-bond ()
  "A cross-bond naming a non-existent bond signals an error."
  (should-error (chem-test--generate-string "CCO" '(("cross-bond" . ((1 9)))))))

(ert-deftest chem-error-strict-implicit-hydrogens ()
  "Under `strict' t an uncomputable hydrogen signals; nil degrades gracefully."
  (should-error (chem-test--generate-string "c1ccncc1" '(("strict" . t))))
  (should (stringp (chem-test--generate-string "c1ccncc1" '(("strict" . nil))))))

(ert-deftest chem-degenerate-single-atom ()
  "Single-atom molecules (no bonds) generate valid, non-empty syntax."
  (dolist (smiles '("C" "[Na+]" "[O]"))
    (should (chem-test--syntax-ok-p (chem-test--generate-string smiles)))))

(provide 'chem-parity-test)
;;; chem-parity-test.el ends here
