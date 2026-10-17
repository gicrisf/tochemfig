;;; tochemfig-mode-test.el --- Tests for tochemfig-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Giovanni Crisalfi

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the context-aware minor mode: context detection, region and
;; thing-at-point conversion, buffer-local options, the transient menu and
;; the keymap.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'tex-mode)
(require 'tochemfig-mode)

(defun tochemfig-mode-test--generate-region ()
  "Generate from the whole buffer as an active region."
  (let ((transient-mark-mode t)
        (mark-even-if-inactive nil))
    (goto-char (point-min))
    (push-mark (point-max) nil t)
    (activate-mark)
    (tochemfig)))

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
    (tochemfig-mode-test--generate-region)
    (should (string-prefix-p "\\chemfig{" (buffer-string))))
  (with-temp-buffer
    (emacs-lisp-mode)
    (tochemfig-mode 1)
    (insert "CCO")
    (tochemfig-mode-test--generate-region)
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
    (tochemfig-mode-test--generate-region)
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
  "The transient prefix and its alias exist."
  (should (get 'tochemfig-dispatch 'transient--prefix))
  (should (fboundp 'tochemfig-custom)))

(ert-deftest tochemfig-mode-keymap ()
  "The mode keymap binds the mode-aware commands under `C-c m'."
  (should (eq (lookup-key tochemfig-mode-map (kbd "C-c m g")) #'tochemfig))
  (should (eq (lookup-key tochemfig-mode-map (kbd "C-c m o")) #'tochemfig-dispatch))
  (should (eq (lookup-key tochemfig-mode-map (kbd "C-c m c")) #'tochemfig-copy))
  (should (eq (lookup-key tochemfig-mode-map (kbd "C-c m d"))
              #'tochemfig-describe-options))
  ;; The old one-shot bindings are gone (clean break).
  (should-not (lookup-key tochemfig-mode-map (kbd "C-c m f")))
  (should-not (lookup-key tochemfig-mode-map (kbd "C-c m t")))
  (should-not (lookup-key tochemfig-mode-map (kbd "C-c m w")))
  (should-not (lookup-key tochemfig-mode-map (kbd "C-c m r")))
  (should-not (lookup-key tochemfig-mode-map (kbd "C-c m p"))))

(ert-deftest tochemfig-mode-one-shot-commands-removed ()
  "The duplicated one-shot command surface is gone."
  (dolist (command '(tochemfig-default
                     tochemfig-input-file
                     tochemfig-input-direct
                     tochemfig-input-direct-output-file
                     tochemfig-input-file-output-file
                     tochemfig-terse
                     tochemfig-verbose
                     tochemfig-indent
                     tochemfig-rotate
                     tochemfig-show-carbons
                     tochemfig-show-methyls
                     tochemfig-add-hydrogens
                     tochemfig-delete-hydrogens
                     tochemfig-fancy-bonds
                     tochemfig-vanilla-bonds
                     tochemfig-aromatic-circles
                     tochemfig-no-aromatic-circles
                     tochemfig-show-atom-numbers
                     tochemfig-hide-atom-numbers
                     tochemfig-bond-scale
                     tochemfig-bond-normalize
                     tochemfig-markers
                     tochemfig-cross-bond
                     tochemfig-wrap-chemfig
                     tochemfig-wrap-submol
                     tochemfig-unwrap
                     tochemfig-partial-submol
                     tochemfig-convert-region
                     tochemfig-mode-toggle-terse
                     tochemfig-mode-toggle-wrap-chemfig
                     tochemfig-mode-rotate))
    (should-not (fboundp command))))

(ert-deftest tochemfig-mode-toggle-terse-buffer-local ()
  "The transient terse toggle is buffer-local."
  (with-temp-buffer
    (emacs-lisp-mode)
    (tochemfig-mode 1)
    (setq tochemfig-mode--edit-buffer (current-buffer))
    (setq tochemfig-default-terse nil)
    (tochemfig-toggle-terse)
    (should (local-variable-p 'tochemfig-default-terse))
    (should tochemfig-default-terse)
    (tochemfig-toggle-terse)
    (should-not tochemfig-default-terse))
  (setq tochemfig-default-terse nil))

(ert-deftest tochemfig-mode-toggle-wrap-buffer-local ()
  "The transient wrap toggle is buffer-local."
  (with-temp-buffer
    (emacs-lisp-mode)
    (tochemfig-mode 1)
    (setq tochemfig-mode--edit-buffer (current-buffer))
    (setq tochemfig-default-wrap-chemfig nil)
    (tochemfig-toggle-wrap-chemfig)
    (should (local-variable-p 'tochemfig-default-wrap-chemfig))
    (should tochemfig-default-wrap-chemfig)
    (tochemfig-toggle-wrap-chemfig)
    (should-not tochemfig-default-wrap-chemfig))
  (setq tochemfig-default-wrap-chemfig nil))

(ert-deftest tochemfig-mode-transient-rotate ()
  "The transient rotates one generation without changing the default."
  (with-temp-buffer
    (emacs-lisp-mode)
    (tochemfig-mode 1)
    (setq tochemfig-mode--edit-buffer (current-buffer))
    (setq tochemfig-mode--pending-source nil)
    (setq tochemfig-mode--pending-overrides nil)
    (setq tochemfig-default-angle 0.0)
    (insert "CCO")
    (goto-char 2)
    (cl-letf (((symbol-function 'read-number) (lambda (&rest _) 30)))
      (tochemfig-transient-rotate))
    (should (equal (cdr (assoc "angle" tochemfig-mode--pending-overrides))
                   30))
    (should (= tochemfig-default-angle 0.0))
    (tochemfig-transient-generate)
    (should (equal (buffer-string)
                   (tochemfig--generate-from-string
                    "CCO" '(("angle" . 30)))))
    (should-not tochemfig-mode--pending-overrides)))

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

(ert-deftest tochemfig-mode-empty-input-is-user-error ()
  "Empty molecule input signals a user-error."
  (should-error (tochemfig-mode--generate 'direct "") :type 'user-error))

(ert-deftest tochemfig-mode-invalid-input-is-user-error ()
  "A generation failure surfaces as a user-error, not a raw Indigo error."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "not a smiles")
    (should-error (tochemfig-mode-test--generate-region)
                  :type 'user-error)))

;;; Thing-at-point and heuristic

(ert-deftest tochemfig-mode-smiles-heuristic ()
  "The SMILES heuristic accepts molecules and rejects prose."
  (should (tochemfig-mode--smiles-looking-p "CCO"))
  (should (tochemfig-mode--smiles-looking-p "c1ccccc1"))
  (should (tochemfig-mode--smiles-looking-p "N[C@@H](C)C(=O)O"))
  (should-not (tochemfig-mode--smiles-looking-p "Hello"))
  (should-not (tochemfig-mode--smiles-looking-p "well-known"))
  (should-not (tochemfig-mode--smiles-looking-p "")))

(ert-deftest tochemfig-mode-smiles-thing ()
  "SMILES is registered as a `thing'."
  (with-temp-buffer
    (insert "use CCO here")
    (goto-char (point-min))
    (search-forward "CCO")
    (goto-char (match-beginning 0))
    (should (equal (tochemfig-smiles-at-point) "CCO"))
    (should (equal (thing-at-point 'smiles) "CCO"))
    (let ((bounds (bounds-of-thing-at-point 'smiles)))
      (should bounds)
      (should (equal (buffer-substring-no-properties (car bounds) (cdr bounds))
                     "CCO")))))

;;; Options introspection and persistence

(ert-deftest tochemfig-mode-describe-options ()
  "`tochemfig-describe-options' lists effective values."
  (with-temp-buffer
    (emacs-lisp-mode)
    (tochemfig-mode 1)
    (set (make-local-variable 'tochemfig-default-indent) 7)
    (tochemfig-describe-options)
    (with-current-buffer "*tochemfig options*"
      (goto-char (point-min))
      (should (search-forward "tochemfig-default-indent" nil t))
      (should (search-forward "buffer-local" nil t)))))

(ert-deftest tochemfig-mode-save-reload-options ()
  "Saving and reloading global options round-trips."
  (let ((tochemfig-options-file (make-temp-file "tochemfig-options" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-buffer
            (emacs-lisp-mode)
            (tochemfig-mode 1)
            (set (make-local-variable 'tochemfig-default-flip) t)
            (tochemfig-save-options))
          (should (default-value 'tochemfig-default-flip))
          (set-default 'tochemfig-default-flip nil)
          (tochemfig-reload-options)
          (should tochemfig-default-flip))
      (set-default 'tochemfig-default-flip nil)
      (delete-file tochemfig-options-file))))

(ert-deftest tochemfig-mode-reset-options ()
  "Resetting removes buffer-local values."
  (with-temp-buffer
    (emacs-lisp-mode)
    (tochemfig-mode 1)
    (set (make-local-variable 'tochemfig-default-terse) t)
    (should (local-variable-p 'tochemfig-default-terse))
    (tochemfig-reset-options)
    (should-not (local-variable-p 'tochemfig-default-terse))))

;;; Transient guards and killed-buffer safety

(ert-deftest tochemfig-mode-entry-exit-guard ()
  "Entry/exit suffixes are hidden unless a submol name is set."
  (let* ((object (get 'tochemfig-option-entry-atom 'transient--suffix))
         (pred (and object (eieio-oref object 'if))))
    (should object)
    (should pred)
    (with-temp-buffer
      (emacs-lisp-mode)
      (tochemfig-mode 1)
      (setq tochemfig-mode--edit-buffer (current-buffer))
      (set (make-local-variable 'tochemfig-default-submol-name) nil)
      (should-not (funcall pred))
      (set (make-local-variable 'tochemfig-default-submol-name) "mol")
      (should (funcall pred)))))

(ert-deftest tochemfig-mode-killed-source-inserts ()
  "Generating from a killed buffer inserts without deleting."
  (let ((edit (generate-new-buffer "tochemfig-edit")))
    (with-current-buffer edit
      (emacs-lisp-mode)
      (tochemfig-mode 1)
      (insert "CCO")
      (goto-char (point-min)))
    (setq tochemfig-mode--edit-buffer edit)
    (setq tochemfig-mode--pending-source
          (with-current-buffer edit (tochemfig-mode--source-at-point)))
    (kill-buffer edit)
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "PREFIX")
      (tochemfig-transient-generate)
      (should (string-prefix-p "PREFIX" (buffer-string)))
      (should (string-match-p "OH" (buffer-string))))))

(provide 'tochemfig-mode-test)
;;; tochemfig-mode-test.el ends here
