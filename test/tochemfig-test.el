;;; tochemfig-test.el --- Tests for tochemfig -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Giovanni Crisalfi

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Two kinds of test:
;;
;; - Parity tests compare `tochemfig' output against golden files produced by
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

(defconst tochemfig-test--root
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the test files.")

(defconst tochemfig-test--project
  (file-name-directory (directory-file-name tochemfig-test--root))
  "Project root directory.")

(defun tochemfig-test--path (relative)
  "Resolve RELATIVE against the project root."
  (expand-file-name relative tochemfig-test--project))

;; Prefer a sibling emacs-indigo checkout during development.  It already has
;; the dynamic module built, so tests do not need to download and compile it.
(let ((sibling (tochemfig-test--path "emacs-indigo")))
  (when (file-directory-p sibling)
    (add-to-list 'load-path sibling)))

(require 'tochemfig)

(defun tochemfig-test--manifest ()
  "Return the list of golden test cases."
  (with-temp-buffer
    (insert-file-contents (tochemfig-test--path "test/golden/manifest.el"))
    (goto-char (point-min))
    (read (current-buffer))))

(defun tochemfig-test--overrides (plist)
  "Convert option PLIST into the alist form accepted by `tochemfig'."
  (let (out)
    (cl-loop for (key value) on plist by #'cddr
             do (push (cons (substring (symbol-name key) 1) value) out))
    out))

(defun tochemfig-test--generate (case)
  "Generate tochemfig output for a manifest CASE."
  (tochemfig--generate-from-file
   (tochemfig-test--path (plist-get case :input))
   (tochemfig-test--overrides (plist-get case :options))))

(defun tochemfig-test--expected (case)
  "Return the golden output for a manifest CASE."
  (with-temp-buffer
    (insert-file-contents (tochemfig-test--path (plist-get case :golden)))
    (buffer-string)))

;;; Syntax validation

(defun tochemfig-test--strip-comments (code)
  "Remove chemfig comments (text after a percent sign) from CODE."
  (mapconcat (lambda (line) (car (split-string line "%")))
             (split-string code "\n") "\n"))

(defun tochemfig-test--balanced-p (code)
  "Return non-nil when all delimiters in CODE are balanced."
  (let ((code (tochemfig-test--strip-comments code))
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

(defun tochemfig-test--closures-paired-p (code)
  "Return non-nil when every ring-closure marker in CODE appears twice."
  (let ((code (tochemfig-test--strip-comments code))
        (counts (make-hash-table :test 'equal))
        (start 0))
    (while (string-match "?\\[\\([^]]+\\)\\]" code start)
      (let ((name (match-string 1 code)))
        (puthash name (1+ (gethash name counts 0)) counts))
      (setq start (match-end 0)))
    (let ((ok t))
      (maphash (lambda (_ n) (unless (= n 2) (setq ok nil))) counts)
      ok)))

(defun tochemfig-test--syntax-ok-p (code)
  "Return non-nil when CODE is syntactically valid chemfig."
  (and (not (string-empty-p code))
       (tochemfig-test--balanced-p code)
       (tochemfig-test--closures-paired-p code)))

;;; Parity tests (one per golden case)

(defmacro tochemfig-test--define-parity (name)
  "Define a parity test for the case NAME."
  `(ert-deftest ,(intern (concat "tochemfig-parity-" name)) ()
     :tags '(parity)
     (let* ((case (tochemfig-test--case ,name))
            (got (tochemfig-test--generate case))
            (expected (tochemfig-test--expected case)))
       (should (tochemfig-test--syntax-ok-p got))
       (should (equal got expected)))))

(defun tochemfig-test--case (name)
  "Return the manifest case called NAME."
  (cl-find name (tochemfig-test--manifest)
           :key (lambda (case) (plist-get case :name))
           :test #'equal))

(dolist (case (tochemfig-test--manifest))
  (eval `(tochemfig-test--define-parity ,(plist-get case :name))))

;;; Syntax tests over a SMILES smoke set (exercises the layout path)

(defconst tochemfig-test--smiles
  '("CCO" "c1ccccc1" "CC(=O)O" "CN1C=NC2=C1C(=O)N(C(=O)N2C)C"
    "N[C@@H](C)C(=O)O" "[Na+].[Cl-]" "O=[N+]([O-])c1ccccc1")
  "SMILES strings used for syntax smoke tests.")

(ert-deftest tochemfig-syntax-smiles ()
  "Generated output for a set of SMILES must be syntactically valid."
  (dolist (smiles tochemfig-test--smiles)
    (dolist (overrides '(nil (("terse" . t)) (("fancy-bonds" . t))
                         (("aromatic-circles" . t)) (("wrap-chemfig" . t))))
      (let ((code (tochemfig--generate-from-string smiles overrides)))
        (should (tochemfig-test--syntax-ok-p code))))))

(ert-deftest tochemfig-syntax-goldens ()
  "Every golden file must be syntactically valid chemfig."
  (dolist (case (tochemfig-test--manifest))
    (should (tochemfig-test--syntax-ok-p (tochemfig-test--expected case)))))

(provide 'tochemfig-test)
;;; tochemfig-test.el ends here
