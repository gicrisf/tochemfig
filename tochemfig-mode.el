;;; tochemfig-mode.el --- Context-aware minor mode for tochemfig -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2025 Giovanni Crisalfi

;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Maintainer: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `tochemfig-mode' turns a buffer into a surface for generating chemfig
;; code.  It makes the `tochemfig-default-*' options buffer-local, so every
;; command in that buffer shares the same settings, and (in LaTeX and Org
;; buffers) wraps generated code in \chemfig{...} automatically.
;;
;; The key bindings live under the `C-c m' prefix.

;;; Code:

(require 'tochemfig)

;;; Customization

(defcustom tochemfig-mode-auto-wrap t
  "When non-nil, wrap output in \\chemfig{...} in LaTeX and Org buffers.
`tochemfig-mode' applies this when it is enabled, so that all commands in
the buffer produce code ready to be pasted into a LaTeX document."
  :group 'tochemfig
  :type 'boolean)

(defconst tochemfig-mode--option-vars
  '(tochemfig-default-input
    tochemfig-default-terse
    tochemfig-default-strict
    tochemfig-default-indent
    tochemfig-default-recalculate-coordinates
    tochemfig-default-angle
    tochemfig-default-relative-angles
    tochemfig-default-flip
    tochemfig-default-flop
    tochemfig-default-show-carbons
    tochemfig-default-show-methyls
    tochemfig-default-hydrogens
    tochemfig-default-aromatic-circles
    tochemfig-default-fancy-bonds
    tochemfig-default-atom-numbers
    tochemfig-default-bond-scale
    tochemfig-default-bond-stretch
    tochemfig-default-markers
    tochemfig-default-cross-bond
    tochemfig-default-legacy-lewis
    tochemfig-default-wrap-chemfig)
  "Option variables that `tochemfig-mode' makes buffer-local.")

;;; Minor Mode

(defun tochemfig-mode--latex-or-org-p ()
  "Return non-nil in a LaTeX or Org buffer."
  (derived-mode-p 'tex-mode 'latex-mode 'LaTeX-mode 'org-mode))

(defun tochemfig-mode--on ()
  "Set up buffer-local state for `tochemfig-mode'."
  (dolist (var tochemfig-mode--option-vars)
    (make-local-variable var))
  (when (and tochemfig-mode-auto-wrap (tochemfig-mode--latex-or-org-p))
    (setq tochemfig-default-wrap-chemfig t)))

(defun tochemfig-mode--off ()
  "Remove buffer-local state installed by `tochemfig-mode'."
  (dolist (var tochemfig-mode--option-vars)
    (kill-local-variable var)))

;;;###autoload
(define-minor-mode tochemfig-mode
  "Minor mode for generating chemfig code from molecules.
Makes the `tochemfig-default-*' options buffer-local and, in LaTeX and
Org buffers, wraps generated code in \\chemfig{...}.  The commands are
available under the `C-c m' prefix."
  :lighter " tochemfig"
  :group 'tochemfig
  (if tochemfig-mode
      (tochemfig-mode--on)
    (tochemfig-mode--off)))

(provide 'tochemfig-mode)
;;; tochemfig-mode.el ends here
