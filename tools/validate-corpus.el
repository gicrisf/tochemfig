;;; validate-corpus.el --- Validate chemfig generation over a MOL corpus -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Giovanni Crisalfi

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Manual validation helper for the external MOL corpora under `studies/'.
;; The corpus is not committed.  These functions are not part of `eldev test'.
;;
;; Usage:
;;
;;   emacs -Q --batch -L . -L emacs-indigo -L test \
;;     -l tools/validate-corpus.el \
;;     --eval '(chem-validate-corpus-write "studies/external-corpus" "/tmp/chem-mode-baseline")'
;;
;;   emacs -Q --batch -L . -L emacs-indigo -L test \
;;     -l tools/validate-corpus.el \
;;     --eval '(chem-validate-corpus-compare "studies/external-corpus" "/tmp/chem-mode-baseline")'
;;
;;   emacs -Q --batch -L . -L emacs-indigo -L test \
;;     -l tools/validate-corpus.el \
;;     --eval '(chem-validate-corpus-graph "studies/external-corpus")'

;;; Code:

(require 'cl-lib)
(require 'chem-mode)
(require 'chem-exp-chemfig-parser)

(defun chem-validate-corpus--mol-files (root)
  "Return all MOL files under ROOT in a stable order."
  (let ((files (directory-files-recursively root "\\.mol\\'")))
    (sort files #'string<)))

(defun chem-validate-corpus--output-path (root output-dir mol-file)
  "Return the output path for MOL-FILE under OUTPUT-DIR.
ROOT is the corpus root.  The relative directory layout is preserved."
  (let* ((relative (file-relative-name mol-file root))
         (tex (concat (file-name-sans-extension relative) ".tex")))
    (expand-file-name tex output-dir)))

(defun chem-validate-corpus--ensure-parent (path)
  "Create the parent directory of PATH when needed."
  (make-directory (file-name-directory path) t))

(defun chem-validate-corpus--generate (mol-file)
  "Generate chemfig output for MOL-FILE with the validation options."
  (let ((options (chem--build-options '(("strict" . nil)))))
    (chem-render (chem-from-file mol-file options) 'chemfig options)))

(defun chem-validate-corpus-write (root output-dir)
  "Generate default chemfig output for every MOL file under ROOT.
Write the results under OUTPUT-DIR, preserving the directory layout."
  (let ((files (chem-validate-corpus--mol-files root))
        (count 0))
    (make-directory output-dir t)
    (dolist (mol-file files)
      (let* ((out (chem-validate-corpus--output-path root output-dir mol-file))
             (code (chem-validate-corpus--generate mol-file)))
        (chem-validate-corpus--ensure-parent out)
        (with-temp-file out (insert code))
        (setq count (1+ count))
        (when (zerop (% count 100))
          (message "validated %d/%d" count (length files)))))
    (message "wrote %d outputs to %s" count output-dir)
    count))

(defun chem-validate-corpus-compare (root baseline-dir)
  "Regenerate every MOL file under ROOT and compare with BASELINE-DIR.
Signal an error when any output differs.  Return the number of files checked."
  (let ((files (chem-validate-corpus--mol-files root))
        (count 0)
        (failures 0))
    (dolist (mol-file files)
      (let* ((baseline (chem-validate-corpus--output-path root baseline-dir mol-file))
             (code (chem-validate-corpus--generate mol-file))
             (expected (when (file-readable-p baseline)
                         (with-temp-buffer
                           (insert-file-contents baseline)
                           (buffer-string)))))
        (setq count (1+ count))
        (unless (equal code expected)
          (setq failures (1+ failures))
          (message "MISMATCH %s" (file-relative-name mol-file root)))
        (when (zerop (% count 100))
          (message "compared %d/%d" count (length files)))))
    (if (zerop failures)
        (progn
          (message "compared %d files: no differences" count)
          count)
      (error "corpus comparison failed: %d/%d files differ" failures count))))

(defun chem-validate-corpus-graph (root)
  "Check graph round-trip for every MOL file under ROOT.
Return the number of files checked.  Signal an error on the first failure."
  (require 'chem-graph-test)
  (let ((files (chem-validate-corpus--mol-files root))
        (count 0))
    (dolist (mol-file files)
      (condition-case err
          (indigo-with-mol-file (mol mol-file)
            (chem-graph-test--assert-mol mol '(("strict" . nil))))
        (error
         (error "graph validation failed for %s: %s"
                (file-relative-name mol-file root)
                (error-message-string err))))
      (setq count (1+ count))
      (when (zerop (% count 100))
        (message "graph-checked %d/%d" count (length files))))
    (message "graph-checked %d files" count)
    count))

(provide 'validate-corpus)
;;; validate-corpus.el ends here
