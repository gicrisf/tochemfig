;;; chem-mode-test.el --- Tests for the generic chem-mode surface -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Giovanni Crisalfi

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Smoke tests for the generic commands, keymap, and dispatch.  The old
;; tochemfig command tests remain intact until the compatibility layer is
;; removed.

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
(require 'chem-dispatch)

(ert-deftest chem-mode-keymap ()
  "The generic mode exposes the expected key bindings."
  (should (eq (lookup-key chem-mode-map (kbd "C-c m g")) #'chem-export))
  (should (eq (lookup-key chem-mode-map (kbd "C-c m o")) #'chem-dispatch))
  (should (eq (lookup-key chem-mode-map (kbd "C-c m c")) #'chem-copy))
  (should (eq (lookup-key chem-mode-map (kbd "C-c m d")) #'chem-describe-options)))

(ert-deftest chem-mode-export-inserts ()
  "`chem-export' replaces the SMILES at point with chemfig output."
  (with-temp-buffer
    (insert "CCO")
    (goto-char (point-max))
    (chem-export)
    (should-not (equal (buffer-string) "CCO"))
    (should (string-match-p "O" (buffer-string)))))

(ert-deftest chem-mode-copy ()
  "`chem-copy' puts generated output on the kill ring."
  (with-temp-buffer
    (insert "CCO")
    (goto-char (point-max))
    (chem-copy)
    (should (stringp (current-kill 0)))))

(ert-deftest chem-mode-dispatch-defined ()
  "The generic dispatch menu is defined."
  (should (fboundp 'chem-dispatch-menu))
  (should (fboundp 'chem-dispatch-option-exporter)))

(provide 'chem-mode-test)
;;; chem-mode-test.el ends here
