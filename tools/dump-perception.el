;;; dump-perception.el --- Dump emacs-indigo perception for MOL files -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Giovanni Crisalfi

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Emits one line per MOL file: "<basename>\t<canonical SMILES>", computed with
;; emacs-indigo.  `tools/check-perception.py' compares this against the
;; reference epam.indigo view so that golden files are only regenerated when
;; both Indigo builds agree on what the molecule is.

;;; Code:

(require 'cl-lib)

(let* ((here (file-name-directory (or load-file-name buffer-file-name)))
       (root (file-name-directory (directory-file-name here))))
  (add-to-list 'load-path root)
  (let ((sibling (expand-file-name "emacs-indigo" root)))
    (when (file-directory-p sibling)
      (add-to-list 'load-path sibling))))

(require 'indigo)

(dolist (path command-line-args-left)
  (condition-case err
      (indigo-with-mol-file (mol path)
        (princ (format "%s\t%s\n" (file-name-nondirectory path)
                       (indigo-canonical-smiles mol))))
    (error
     (princ (format "error: %s\t%s\n" (file-name-nondirectory path)
                    (error-message-string err))))))

;;; dump-perception.el ends here
