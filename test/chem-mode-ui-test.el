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

(defconst chem-mode-ui-test--root
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the test files.")

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
  (should (eq (lookup-key chem-mode-map (kbd "C-c m r"))
              #'chem-export-reaction))
  (should (eq (lookup-key chem-mode-map (kbd "C-c m i")) #'chem-insert-image))
  (should (eq (lookup-key chem-mode-map (kbd "C-c m d"))
              #'chem-describe-options))
  ;; The old one-shot bindings are gone (clean break).
  (should-not (lookup-key chem-mode-map (kbd "C-c m f")))
  (should-not (lookup-key chem-mode-map (kbd "C-c m t")))
  (should-not (lookup-key chem-mode-map (kbd "C-c m w")))
  (should-not (lookup-key chem-mode-map (kbd "C-c m p"))))

(ert-deftest chem-mode-ui-one-shot-commands-removed ()
  "The duplicated one-shot command surface is gone."
  (dolist (command '(tochemfig
                     tochemfig-default
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

;;; Reactions

(ert-deftest chem-mode-ui-reaction-export-insert ()
  "The reaction export command inserts a scheme."
  (let ((chem-default-exporter 'chemfig))
    (with-temp-buffer
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "CCO>>CC=O")))
        (chem-export-reaction))
      (should (string-prefix-p "\\schemestart" (buffer-string)))
      (should (string-match-p (regexp-quote "\\arrow") (buffer-string)))
      (should (string-match-p (regexp-quote "\\chemfig{") (buffer-string))))))

(ert-deftest chem-mode-ui-reaction-export-svg ()
  "The reaction export command works with the SVG exporter."
  (let ((chem-default-exporter 'svg))
    (with-temp-buffer
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "CCO>>CC=O")))
        (chem-export-reaction))
      (should (string-match-p "data-reaction-part=\"arrow\""
                              (buffer-string)))
      (should-not (string-match-p "schemestart" (buffer-string))))))

(ert-deftest chem-mode-ui-reaction-copy ()
  "The reaction copy command puts the scheme on the kill ring."
  (let ((chem-default-exporter 'chemfig))
    (with-temp-buffer
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "CCO>>CC=O")))
        (chem-copy-reaction))
      (should (string-prefix-p "\\schemestart" (current-kill 0))))))

(ert-deftest chem-mode-ui-reaction-file-input ()
  "A prefix argument reads an RXN file."
  (let* ((chem-default-exporter 'chemfig)
         (path (expand-file-name "corpus/reactions/simple.rxn"
                                 chem-mode-ui-test--root)))
    (with-temp-buffer
      (cl-letf (((symbol-function 'read-file-name)
                 (lambda (&rest _) path)))
        (chem-export-reaction t))
      (should (string-prefix-p "\\schemestart" (buffer-string))))))

(ert-deftest chem-mode-ui-reaction-copy-file-input ()
  "The copy command accepts an RXN file with a prefix argument."
  (let* ((chem-default-exporter 'chemfig)
         (path (expand-file-name "corpus/reactions/simple.rxn"
                                 chem-mode-ui-test--root)))
    (with-temp-buffer
      (cl-letf (((symbol-function 'read-file-name)
                 (lambda (&rest _) path)))
        (chem-copy-reaction t))
      (should (string-prefix-p "\\schemestart" (current-kill 0))))))

(ert-deftest chem-mode-ui-reaction-ignores-auto-wrap ()
  "Reaction commands use the scheme wrapper, not the molecule wrapper."
  (let ((chem-default-exporter 'chemfig)
        (chem-mode-auto-wrap t))
    (with-temp-buffer
      (latex-mode)
      (chem-mode 1)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "CCO>>CC=O")))
        (chem-export-reaction))
      (should (string-prefix-p "\\schemestart" (buffer-string)))
      (should-not (string-prefix-p "\\chemfig{" (buffer-string))))))

(ert-deftest chem-mode-ui-reaction-empty-input ()
  "Empty reaction input signals a user-error."
  (let ((chem-default-exporter 'chemfig))
    (with-temp-buffer
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "")))
        (should-error (chem-export-reaction) :type 'user-error)))))

(ert-deftest chem-mode-ui-reaction-transient ()
  "Reaction transient options and actions generate a reaction."
  (let ((chem-default-exporter 'chemfig)
        (chem-default-reaction-above "heat"))
    (with-temp-buffer
      (emacs-lisp-mode)
      (chem-mode 1)
      (setq chem-dispatch--edit-buffer (current-buffer))
      (setq chem-dispatch--pending-overrides nil)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "CCO>>CC=O")))
        (chem-dispatch-reaction-generate))
      (should (string-prefix-p "\\schemestart" (buffer-string)))
      (should (string-match-p "heat" (buffer-string))))))

(ert-deftest chem-mode-ui-reaction-multiline-command ()
  "Reaction commands honor literal newlines in conditions."
  (let ((chem-default-exporter 'svg)
        (chem-default-reaction-above "first line\nsecond line"))
    (with-temp-buffer
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "CCO>>CC=O")))
        (chem-export-reaction))
      (should (= 1 (let ((count 0)
                         (start 0))
                     (while (string-match
                             "data-reaction-part=\"text\"" (buffer-string)
                             start)
                       (setq count (1+ count)
                             start (match-end 0)))
                     count)))
      (let* ((svg (buffer-string))
             (start (string-match
                     "<text data-reaction-part=\"text\"" svg))
             (end (string-match "</text>" svg start))
             (block (substring svg start end)))
        (should (= 2 (let ((count 0)
                           (position 0))
                       (while (string-match "<tspan " block position)
                         (setq count (1+ count)
                               position (match-end 0)))
                       count))))
      (should (string-match-p (regexp-quote "first line")
                              (buffer-string)))
      (should (string-match-p (regexp-quote "second line")
                              (buffer-string))))))

(ert-deftest chem-mode-ui-reaction-transient-guards ()
  "Reaction transient options follow the selected exporter."
  (with-temp-buffer
    (setq chem-dispatch--edit-buffer (current-buffer))
    (setq-local chem-default-exporter 'svg)
    (let* ((spacing (get 'chem-dispatch-option-reaction-spacing
                         'transient--suffix))
           (wrap (get 'chem-dispatch-toggle-reaction-wrap
                      'transient--suffix)))
      (should (funcall (eieio-oref spacing 'if)))
      (should-not (funcall (eieio-oref wrap 'if))))
    (setq-local chem-default-exporter 'chemfig)
    (let* ((spacing (get 'chem-dispatch-option-reaction-spacing
                         'transient--suffix))
           (wrap (get 'chem-dispatch-toggle-reaction-wrap
                      'transient--suffix)))
      (should-not (funcall (eieio-oref spacing 'if)))
      (should (funcall (eieio-oref wrap 'if))))))

(ert-deftest chem-mode-ui-reaction-describe-options ()
  "Reaction options appear in the effective options buffer."
  (with-temp-buffer
    (emacs-lisp-mode)
    (chem-mode 1)
    (set (make-local-variable 'chem-default-reaction-above) "heat")
    (chem-describe-options)
    (with-current-buffer "*chem options*"
      (goto-char (point-min))
      (should (search-forward "chem-default-reaction-above" nil t))
      (should (search-forward "heat" nil t)))))

(ert-deftest chem-mode-ui-reaction-save-reload-options ()
  "Reaction options round-trip through save and reload."
  (let ((chem-options-file (make-temp-file "tochemfig-reaction-options"
                                           nil ".el")))
    (unwind-protect
        (progn
          (with-temp-buffer
            (emacs-lisp-mode)
            (chem-mode 1)
            (set (make-local-variable 'chem-default-reaction-arrow)
                 'reverse)
            (chem-save-options))
          (should (eq (default-value 'chem-default-reaction-arrow)
                      'reverse))
          (set-default 'chem-default-reaction-arrow 'forward)
          (chem-reload-options)
          (should (eq chem-default-reaction-arrow 'reverse)))
      (set-default 'chem-default-reaction-arrow 'forward)
      (delete-file chem-options-file))))

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

(ert-deftest chem-mode-ui-svg-options-visible ()
  "Exporter-specific menu predicates follow the selected exporter."
  (with-temp-buffer
    (setq chem-dispatch--edit-buffer (current-buffer))
    (setq-local chem-default-exporter 'chemfig)
    (should (chem-dispatch--chemfig-exporter-p))
    (should-not (chem-dispatch--svg-exporter-p))
    (setq-local chem-default-exporter 'svg)
    (should (chem-dispatch--svg-exporter-p))
    (should-not (chem-dispatch--chemfig-exporter-p))))

(ert-deftest chem-mode-ui-toggle-svg-option ()
  "SVG options are buffer-local through the transient."
  (with-temp-buffer
    (emacs-lisp-mode)
    (chem-mode 1)
    (setq chem-dispatch--edit-buffer (current-buffer))
    (setq-local chem-default-exporter 'svg)
    (setq-local chem-exp-svg-default-show-cip-labels nil)
    (call-interactively #'chem-dispatch-toggle-svg-show-cip-labels)
    (should chem-exp-svg-default-show-cip-labels)
    (should (local-variable-p 'chem-exp-svg-default-show-cip-labels))))

(ert-deftest chem-mode-ui-svg-theme-option ()
  "The SVG theme selector sets a buffer-local theme."
  (with-temp-buffer
    (emacs-lisp-mode)
    (chem-mode 1)
    (setq chem-dispatch--edit-buffer (current-buffer))
    (setq-local chem-default-exporter 'svg)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "dark")))
      (call-interactively #'chem-dispatch-option-svg-theme))
    (should (eq (chem-dispatch--value 'chem-exp-svg-default-theme) 'dark))
    (should (local-variable-p 'chem-exp-svg-default-theme))))

(ert-deftest chem-mode-ui-svg-option-vars ()
  "SVG variables are part of the generic option surface."
  (with-temp-buffer
    (setq-local chem-default-exporter 'svg)
    (should (memq 'chem-exp-svg-default-width
                  (chem-mode--option-vars)))))

(ert-deftest chem-mode-ui-reaction-option-vars ()
  "Reaction variables are part of the generic option surface."
  (with-temp-buffer
    (setq-local chem-default-exporter 'svg)
    (should (memq 'chem-default-reaction-arrow
                  (chem-mode--option-vars)))
    (should (memq 'chem-default-reaction-spacing
                  (chem-mode--option-vars)))
    (should (memq 'chem-default-reaction-condition-width
                  (chem-mode--option-vars)))))

(ert-deftest chem-mode-ui-insert-image-org ()
  "`chem-insert-image' writes SVG and inserts an Org link."
  (require 'org)
  (let* ((dir (make-temp-file "chem-images-" t))
         (default-directory (file-name-as-directory dir)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "CCO")
          (goto-char (point-max))
          (let ((chem-image-directory dir))
            (chem-insert-image))
          (let ((link (buffer-string)))
            (should (string-match "\\`\\[\\[file:\\(.*\\)\\]\\]\\'" link))
            (let* ((relative (match-string 1 link))
                   (path (expand-file-name relative default-directory)))
              (should (file-exists-p path))
              (with-temp-buffer
                (insert-file-contents path)
                (should (string-match-p "<svg " (buffer-string)))))))
      (delete-directory dir t))))

(ert-deftest chem-mode-ui-insert-image-deterministic ()
  "Image names are derived from SVG content."
  (let* ((dir (make-temp-file "chem-images-" t))
         (default-directory (file-name-as-directory dir)))
    (unwind-protect
        (let* ((chem-image-directory dir)
               (link-a (with-temp-buffer
                         (insert "CCO")
                         (goto-char (point-max))
                         (chem-insert-image)
                         (buffer-string)))
               (link-b (with-temp-buffer
                         (insert "CCO")
                         (goto-char (point-max))
                         (chem-insert-image)
                         (buffer-string))))
          (should (equal link-a link-b))
          (should (= 1 (length (directory-files dir nil "\\.svg\\'")))))
      (delete-directory dir t))))

(ert-deftest chem-mode-ui-image-link-markdown ()
  "Markdown image links escape spaces."
  (let ((default-directory "/tmp/")
        (buffer-file-name nil))
    (cl-letf (((symbol-function 'derived-mode-p)
               (lambda (&rest modes) (memq 'markdown-mode modes))))
      (should (equal "![](foo%20bar.svg)"
                     (chem-mode--image-link "/tmp/foo bar.svg"))))))

(provide 'chem-mode-test)
;;; chem-mode-test.el ends here
