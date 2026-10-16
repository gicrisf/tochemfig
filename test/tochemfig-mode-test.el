;;; tochemfig-mode-test.el --- Tests for tochemfig-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Giovanni Crisalfi

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the context-aware minor mode: context detection, region and
;; thing-at-point conversion, buffer-local options, the transient menu and
;; the keymap.

;;; Code:

(require 'ert)
(require 'tex-mode)
(require 'tochemfig-mode)

(ert-deftest tochemfig-mode-context-p ()
  "LaTeX and Org buffers are recognised as wrapping contexts."
  (with-temp-buffer (latex-mode) (should (tochemfig-mode--wrap-context-p)))
  (with-temp-buffer (org-mode) (should (tochemfig-mode--wrap-context-p)))
  (with-temp-buffer (emacs-lisp-mode)
    (should-not (tochemfig-mode--wrap-context-p))))

(ert-deftest tochemfig-mode-wrap-modes-configurable ()
  "`tochemfig-mode-wrap-modes' selects the wrapping contexts."
  (let ((tochemfig-mode-wrap-modes '(emacs-lisp-mode)))
    (with-temp-buffer (emacs-lisp-mode)
      (should (tochemfig-mode--wrap-context-p)))
    (with-temp-buffer (latex-mode)
      (should-not (tochemfig-mode--wrap-context-p)))))

(ert-deftest tochemfig-mode-org-src-block ()
  "A LaTeX source block in Org is a wrapping context."
  (require 'org)
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_src latex\nCCO\n#+end_src\n")
    (goto-char (point-min))
    (search-forward "CCO")
    (should (tochemfig-mode--org-latex-block-p))))

(ert-deftest tochemfig-mode-wrap-per-command ()
  "Wrapping is decided per command, not when the mode is enabled."
  (with-temp-buffer
    (latex-mode)
    (tochemfig-mode 1)
    (insert "CCO")
    (tochemfig-convert-region (point-min) (point-max))
    (should (string-prefix-p "\\chemfig{" (buffer-string))))
  (with-temp-buffer
    (emacs-lisp-mode)
    (tochemfig-mode 1)
    (insert "CCO")
    (tochemfig-convert-region (point-min) (point-max))
    (should-not (string-prefix-p "\\chemfig{" (buffer-string)))))

(ert-deftest tochemfig-mode-options-localize-lazily ()
  "Enabling the mode does not freeze global options; editing localizes."
  (with-temp-buffer
    (emacs-lisp-mode)
    (set-default 'tochemfig-default-terse nil)
    (tochemfig-mode 1)
    (setq tochemfig-mode--edit-buffer (current-buffer))
    (should-not (local-variable-p 'tochemfig-default-terse))
    ;; a global change still reaches the buffer
    (set-default 'tochemfig-default-terse t)
    (should (tochemfig-mode--value 'tochemfig-default-terse))
    ;; editing the option localizes it from now on
    (call-interactively #'tochemfig-toggle-terse)
    (should (local-variable-p 'tochemfig-default-terse))
    (should-not (tochemfig-mode--value 'tochemfig-default-terse))
    ;; global changes no longer leak in
    (set-default 'tochemfig-default-terse t)
    (should-not (tochemfig-mode--value 'tochemfig-default-terse)))
  (set-default 'tochemfig-default-terse nil))

(ert-deftest tochemfig-mode-region-conversion ()
  "Converting a region wraps the output in a LaTeX buffer."
  (with-temp-buffer
    (latex-mode)
    (tochemfig-mode 1)
    (insert "CCO")
    (tochemfig-convert-region (point-min) (point-max))
    (should (string-prefix-p "\\chemfig{" (buffer-string)))
    (should (equal (buffer-string)
                   (tochemfig--generate-from-string
                    "CCO" '(("wrap-chemfig" . t)))))))

(ert-deftest tochemfig-mode-thing-at-point ()
  "The SMILES string at point is recognised."
  (with-temp-buffer
    (insert "use CCO here")
    (goto-char 6)
    (should (equal (tochemfig-smiles-at-point) "CCO"))))

(ert-deftest tochemfig-mode-dwim-replaces-thing ()
  "The dispatch command replaces the SMILES string at point."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "see CCO now")
    (goto-char 6)
    (tochemfig-mode 1)
    (tochemfig)
    (should (equal (buffer-string)
                   (concat "see " (tochemfig--generate-from-string "CCO") " now")))))

(ert-deftest tochemfig-mode-copy ()
  "The copy command puts chemfig code on the kill ring."
  (with-temp-buffer
    (insert "CCO")
    (goto-char 2)
    (tochemfig-copy)
    (should (equal (current-kill 0) (tochemfig--generate-from-string "CCO")))))

(ert-deftest tochemfig-mode-toggle-suffix ()
  "Toggling an option suffix updates the buffer-local value."
  (with-temp-buffer
    (emacs-lisp-mode)
    (tochemfig-mode 1)
    (setq tochemfig-mode--edit-buffer (current-buffer))
    (should-not tochemfig-default-terse)
    (call-interactively #'tochemfig-toggle-terse)
    (should tochemfig-default-terse)
    (should (local-variable-p 'tochemfig-default-terse))))

(ert-deftest tochemfig-mode-transient-defined ()
  "The transient prefix and its alias exist, and keys are bound."
  (should (get 'tochemfig-dispatch 'transient--prefix))
  (should (fboundp 'tochemfig-custom))
  (should (eq (lookup-key tochemfig-mode-map (kbd "C-c m d")) #'tochemfig))
  (should (eq (lookup-key tochemfig-mode-map (kbd "C-c m c")) #'tochemfig-dispatch)))

(ert-deftest tochemfig-mode-global-eligibility ()
  "The global mode enables only in eligible buffers by default."
  (let ((tochemfig-global-mode-all-buffers nil))
    (unwind-protect
        (progn
          (tochemfig-global-mode 1)
          (with-temp-buffer
            (rename-buffer "tochemfig-test.tex" t)
            (latex-mode)
            (should tochemfig-mode))
          (with-temp-buffer
            (rename-buffer "tochemfig-test.org" t)
            (org-mode)
            (should tochemfig-mode))
          (with-temp-buffer
            (rename-buffer "tochemfig-test.el" t)
            (emacs-lisp-mode)
            (should-not tochemfig-mode)))
      (tochemfig-global-mode -1))))

(ert-deftest tochemfig-mode-global-all-buffers ()
  "With the opt-in, the global mode enables everywhere."
  (let ((tochemfig-global-mode-all-buffers t))
    (unwind-protect
        (progn
          (tochemfig-global-mode 1)
          (with-temp-buffer
            (rename-buffer "tochemfig-test-lisp.el" t)
            (emacs-lisp-mode)
            (should tochemfig-mode)))
      (tochemfig-global-mode -1))))

(ert-deftest tochemfig-mode-convert-empty-region ()
  "Converting an empty region signals a user-error."
  (with-temp-buffer
    (should-error (tochemfig-convert-region (point-min) (point-max))
                  :type 'user-error)))

(ert-deftest tochemfig-mode-invalid-input-is-user-error ()
  "A generation failure surfaces as a user-error, not a raw Indigo error."
  (with-temp-buffer
    (insert "not a smiles")
    (goto-char 6)
    (should-error (tochemfig-convert-region (point-min) (point-max))
                  :type 'user-error)))

(provide 'tochemfig-mode-test)
;;; tochemfig-mode-test.el ends here
