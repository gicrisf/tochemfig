;;; chem-mode-test.el --- Tests for chem-mode -*- lexical-binding: t; -*-

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
(require 'chem-mode)
(require 'chem-dispatch)

(defun chem-mode-ui-test--generate-string (smiles &optional overrides)
  "Generate chemfig output for SMILES with OVERRIDES."
  (chem-mode--generate 'direct smiles overrides))

(defun chem-mode-test--generate-region ()
  "Generate from the whole buffer as an active region."
  (let ((transient-mark-mode t)
        (mark-even-if-inactive nil))
    (goto-char (point-min))
    (push-mark (point-max) nil t)
    (activate-mark)
    (chem-export)))

(ert-deftest chem-mode-ui-context-p ()
  "LaTeX and Org buffers are recognised as wrapping contexts."
  (with-temp-buffer (latex-mode) (should (chem-mode--wrap-context-p)))
  (with-temp-buffer (org-mode) (should (chem-mode--wrap-context-p)))
  (with-temp-buffer (emacs-lisp-mode)
    (should-not (chem-mode--wrap-context-p))))

(ert-deftest chem-mode-ui-wrap-modes-configurable ()
  "`chem-mode-wrap-modes' selects the wrapping contexts."
  (let ((chem-mode-wrap-modes '(emacs-lisp-mode)))
    (with-temp-buffer (emacs-lisp-mode)
      (should (chem-mode--wrap-context-p)))
    (with-temp-buffer (latex-mode)
      (should-not (chem-mode--wrap-context-p)))))

(ert-deftest chem-mode-ui-org-src-block ()
  "A LaTeX source block in Org is a wrapping context."
  (require 'org)
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_src latex\nCCO\n#+end_src\n")
    (goto-char (point-min))
    (search-forward "CCO")
    (should (chem-mode--org-latex-block-p))))

(ert-deftest chem-mode-ui-wrap-per-command ()
  "Wrapping is decided per command, not when the mode is enabled."
  (with-temp-buffer
    (latex-mode)
    (chem-mode 1)
    (insert "CCO")
    (chem-mode-test--generate-region)
    (should (string-prefix-p "\\chemfig{" (buffer-string))))
  (with-temp-buffer
    (emacs-lisp-mode)
    (chem-mode 1)
    (insert "CCO")
    (chem-mode-test--generate-region)
    (should-not (string-prefix-p "\\chemfig{" (buffer-string)))))

(ert-deftest chem-mode-ui-options-localize-lazily ()
  "Enabling the mode does not freeze global options; editing localizes."
  (with-temp-buffer
    (emacs-lisp-mode)
    (set-default 'chem-exp-chemfig-default-terse nil)
    (chem-mode 1)
    (setq chem-dispatch--edit-buffer (current-buffer))
    (should-not (local-variable-p 'chem-exp-chemfig-default-terse))
    ;; a global change still reaches the buffer
    (set-default 'chem-exp-chemfig-default-terse t)
    (should (chem-dispatch--value 'chem-exp-chemfig-default-terse))
    ;; editing the option localizes it from now on
    (call-interactively #'chem-dispatch-toggle-terse)
    (should (local-variable-p 'chem-exp-chemfig-default-terse))
    (should-not (chem-dispatch--value 'chem-exp-chemfig-default-terse))
    ;; global changes no longer leak in
    (set-default 'chem-exp-chemfig-default-terse t)
    (should-not (chem-dispatch--value 'chem-exp-chemfig-default-terse)))
  (set-default 'chem-exp-chemfig-default-terse nil))

(ert-deftest chem-mode-ui-region-conversion ()
  "Converting a region wraps the output in a LaTeX buffer."
  (with-temp-buffer
    (latex-mode)
    (chem-mode 1)
    (insert "CCO")
    (chem-mode-test--generate-region)
    (should (string-prefix-p "\\chemfig{" (buffer-string)))
    (should (equal (buffer-string)
                   (chem-mode-ui-test--generate-string
                    "CCO" '(("wrap-chemfig" . t)))))))

(ert-deftest chem-mode-ui-thing-at-point ()
  "The SMILES string at point is recognised."
  (with-temp-buffer
    (insert "use CCO here")
    (goto-char 6)
    (should (equal (chem-smiles-at-point) "CCO"))))

(ert-deftest chem-mode-ui-dwim-replaces-thing ()
  "The dispatch command replaces the SMILES string at point."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "see CCO now")
    (goto-char 6)
    (chem-mode 1)
    (chem-export)
    (should (equal (buffer-string)
                   (concat "see " (chem-mode-ui-test--generate-string "CCO") " now")))))

(ert-deftest chem-mode-ui-copy ()
  "The copy command puts chemfig code on the kill ring."
  (with-temp-buffer
    (insert "CCO")
    (goto-char 2)
    (chem-copy)
    (should (equal (current-kill 0) (chem-mode-ui-test--generate-string "CCO")))))

(ert-deftest chem-mode-ui-toggle-suffix ()
  "Toggling an option suffix updates the buffer-local value."
  (with-temp-buffer
    (emacs-lisp-mode)
    (chem-mode 1)
    (setq chem-dispatch--edit-buffer (current-buffer))
    (should-not chem-exp-chemfig-default-terse)
    (call-interactively #'chem-dispatch-toggle-terse)
    (should chem-exp-chemfig-default-terse)
    (should (local-variable-p 'chem-exp-chemfig-default-terse))))

(ert-deftest chem-mode-ui-transient-defined ()
  "The transient prefix and its alias exist."
  (should (get 'chem-dispatch-menu 'transient--prefix))
  (should (fboundp 'chem-dispatch)))

(ert-deftest chem-mode-ui-keymap ()
  "The mode keymap binds the mode-aware commands under `C-c m'."
  (should (eq (lookup-key chem-mode-map (kbd "C-c m g")) #'chem-export))
  (should (eq (lookup-key chem-mode-map (kbd "C-c m o")) #'chem-dispatch))
  (should (eq (lookup-key chem-mode-map (kbd "C-c m c")) #'chem-copy))
  (should (eq (lookup-key chem-mode-map (kbd "C-c m d"))
              #'chem-describe-options))
  ;; The old one-shot bindings are gone (clean break).
  (should-not (lookup-key chem-mode-map (kbd "C-c m f")))
  (should-not (lookup-key chem-mode-map (kbd "C-c m t")))
  (should-not (lookup-key chem-mode-map (kbd "C-c m w")))
  (should-not (lookup-key chem-mode-map (kbd "C-c m r")))
  (should-not (lookup-key chem-mode-map (kbd "C-c m p"))))

(ert-deftest chem-mode-ui-one-shot-commands-removed ()
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
                     tochemfig-convert-region))
    (should-not (fboundp command))))

(ert-deftest chem-dispatch-toggle-terse-buffer-local ()
  "The transient terse toggle is buffer-local."
  (with-temp-buffer
    (emacs-lisp-mode)
    (chem-mode 1)
    (setq chem-dispatch--edit-buffer (current-buffer))
    (setq chem-exp-chemfig-default-terse nil)
    (chem-dispatch-toggle-terse)
    (should (local-variable-p 'chem-exp-chemfig-default-terse))
    (should chem-exp-chemfig-default-terse)
    (chem-dispatch-toggle-terse)
    (should-not chem-exp-chemfig-default-terse))
  (setq chem-exp-chemfig-default-terse nil))

(ert-deftest chem-mode-ui-toggle-wrap-buffer-local ()
  "The transient wrap toggle is buffer-local."
  (with-temp-buffer
    (emacs-lisp-mode)
    (chem-mode 1)
    (setq chem-dispatch--edit-buffer (current-buffer))
    (setq chem-exp-chemfig-default-wrap-chemfig nil)
    (chem-dispatch-toggle-wrap-chemfig)
    (should (local-variable-p 'chem-exp-chemfig-default-wrap-chemfig))
    (should chem-exp-chemfig-default-wrap-chemfig)
    (chem-dispatch-toggle-wrap-chemfig)
    (should-not chem-exp-chemfig-default-wrap-chemfig))
  (setq chem-exp-chemfig-default-wrap-chemfig nil))

(ert-deftest chem-mode-ui-transient-rotate ()
  "The transient rotates one generation without changing the default."
  (with-temp-buffer
    (emacs-lisp-mode)
    (chem-mode 1)
    (setq chem-dispatch--edit-buffer (current-buffer))
    (setq chem-dispatch--pending-source nil)
    (setq chem-dispatch--pending-overrides nil)
    (setq chem-default-angle 0.0)
    (insert "CCO")
    (goto-char 2)
    (cl-letf (((symbol-function 'read-number) (lambda (&rest _) 30)))
      (chem-dispatch-rotate))
    (should (equal (cdr (assoc "angle" chem-dispatch--pending-overrides))
                   30))
    (should (= chem-default-angle 0.0))
    (chem-dispatch-generate)
    (should (equal (buffer-string)
                   (chem-mode-ui-test--generate-string
                    "CCO" '(("angle" . 30)))))
    (should-not chem-dispatch--pending-overrides)))

(ert-deftest chem-mode-ui-global-eligibility ()
  "The global mode enables only in eligible buffers by default."
  (let ((chem-global-mode-all-buffers nil))
    (unwind-protect
        (progn
          (chem-global-mode 1)
          (with-temp-buffer
            (rename-buffer "chem-test.tex" t)
            (latex-mode)
            (should chem-mode))
          (with-temp-buffer
            (rename-buffer "chem-test.org" t)
            (org-mode)
            (should chem-mode))
          (with-temp-buffer
            (rename-buffer "chem-test.el" t)
            (emacs-lisp-mode)
            (should-not chem-mode)))
      (chem-global-mode -1))))

(ert-deftest chem-mode-ui-global-all-buffers ()
  "With the opt-in, the global mode enables everywhere."
  (let ((chem-global-mode-all-buffers t))
    (unwind-protect
        (progn
          (chem-global-mode 1)
          (with-temp-buffer
            (rename-buffer "tochemfig-test-lisp.el" t)
            (emacs-lisp-mode)
            (should chem-mode)))
      (chem-global-mode -1))))

(ert-deftest chem-mode-ui-empty-input-is-user-error ()
  "Empty molecule input signals a user-error."
  (should-error (chem-mode--generate 'direct "") :type 'user-error))

(ert-deftest chem-mode-ui-invalid-input-is-user-error ()
  "A generation failure surfaces as a user-error, not a raw Indigo error."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "not a smiles")
    (should-error (chem-mode-test--generate-region)
                  :type 'user-error)))

;;; Thing-at-point and heuristic

(ert-deftest chem-mode-ui-smiles-heuristic ()
  "The SMILES heuristic accepts molecules and rejects prose."
  (should (chem-mode--smiles-looking-p "CCO"))
  (should (chem-mode--smiles-looking-p "c1ccccc1"))
  (should (chem-mode--smiles-looking-p "N[C@@H](C)C(=O)O"))
  (should-not (chem-mode--smiles-looking-p "Hello"))
  (should-not (chem-mode--smiles-looking-p "well-known"))
  (should-not (chem-mode--smiles-looking-p "")))

(ert-deftest chem-mode-ui-smiles-thing ()
  "SMILES is registered as a `thing'."
  (with-temp-buffer
    (insert "use CCO here")
    (goto-char (point-min))
    (search-forward "CCO")
    (goto-char (match-beginning 0))
    (should (equal (chem-smiles-at-point) "CCO"))
    (should (equal (thing-at-point 'smiles) "CCO"))
    (let ((bounds (bounds-of-thing-at-point 'smiles)))
      (should bounds)
      (should (equal (buffer-substring-no-properties (car bounds) (cdr bounds))
                     "CCO")))))

;;; Options introspection and persistence

(ert-deftest chem-mode-ui-describe-options ()
  "`chem-describe-options' lists effective values."
  (with-temp-buffer
    (emacs-lisp-mode)
    (chem-mode 1)
    (set (make-local-variable 'chem-exp-chemfig-default-indent) 7)
    (chem-describe-options)
    (with-current-buffer "*chem options*"
      (goto-char (point-min))
      (should (search-forward "chem-exp-chemfig-default-indent" nil t))
      (should (search-forward "buffer-local" nil t)))))

(ert-deftest chem-mode-ui-save-reload-options ()
  "Saving and reloading global options round-trips."
  (let ((chem-options-file (make-temp-file "tochemfig-options" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-buffer
            (emacs-lisp-mode)
            (chem-mode 1)
            (set (make-local-variable 'chem-default-flip) t)
            (chem-save-options))
          (should (default-value 'chem-default-flip))
          (set-default 'chem-default-flip nil)
          (chem-reload-options)
          (should chem-default-flip))
      (set-default 'chem-default-flip nil)
      (delete-file chem-options-file))))

(ert-deftest chem-mode-ui-reset-options ()
  "Resetting removes buffer-local values."
  (with-temp-buffer
    (emacs-lisp-mode)
    (chem-mode 1)
    (set (make-local-variable 'chem-exp-chemfig-default-terse) t)
    (should (local-variable-p 'chem-exp-chemfig-default-terse))
    (chem-reset-options)
    (should-not (local-variable-p 'chem-exp-chemfig-default-terse))))

;;; Transient guards and killed-buffer safety

(ert-deftest chem-mode-ui-entry-exit-guard ()
  "Entry/exit suffixes are hidden unless a submol name is set."
  (let* ((object (get 'chem-dispatch-option-entry-atom 'transient--suffix))
         (pred (and object (eieio-oref object 'if))))
    (should object)
    (should pred)
    (with-temp-buffer
      (emacs-lisp-mode)
      (chem-mode 1)
      (setq chem-dispatch--edit-buffer (current-buffer))
      (set (make-local-variable 'chem-exp-chemfig-default-submol-name) nil)
      (should-not (funcall pred))
      (set (make-local-variable 'chem-exp-chemfig-default-submol-name) "mol")
      (should (funcall pred)))))

(ert-deftest chem-mode-ui-killed-source-inserts ()
  "Generating from a killed buffer inserts without deleting."
  (let ((edit (generate-new-buffer "tochemfig-edit")))
    (with-current-buffer edit
      (emacs-lisp-mode)
      (chem-mode 1)
      (insert "CCO")
      (goto-char (point-min)))
    (setq chem-dispatch--edit-buffer edit)
    (setq chem-dispatch--pending-source
          (with-current-buffer edit (chem-mode--source-at-point)))
    (kill-buffer edit)
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "PREFIX")
      (chem-dispatch-generate)
      (should (string-prefix-p "PREFIX" (buffer-string)))
      (should (string-match-p "OH" (buffer-string))))))

(provide 'chem-mode-test)
;;; chem-mode-test.el ends here
