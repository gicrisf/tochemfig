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
(require 'thingatpt)

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

;;; Input helpers

(defconst tochemfig-mode--smiles-charset "A-Za-z0-9@+[]()=#$:/\\.%*-"
  "Characters that may appear in a SMILES string.")

(defun tochemfig-mode--bounds-of-smiles-at-point ()
  "Return the (START . END) bounds of the SMILES string at point, or nil."
  (let ((start (save-excursion
                 (skip-chars-backward tochemfig-mode--smiles-charset)
                 (point)))
        (end (save-excursion
               (skip-chars-forward tochemfig-mode--smiles-charset)
               (point))))
    (when (< start end)
      (cons start end))))

(defun tochemfig-smiles-at-point ()
  "Return the SMILES string at point, or nil."
  (let ((bounds (tochemfig-mode--bounds-of-smiles-at-point)))
    (when bounds
      (string-trim (buffer-substring-no-properties (car bounds) (cdr bounds))))))

;; Expose SMILES as a `thing' when the running Emacs supports it.
(when (boundp 'thing-at-point-provider-alist)
  (add-to-list 'thing-at-point-provider-alist
               (cons 'smiles #'tochemfig-smiles-at-point)))

(defun tochemfig-mode--source-at-point ()
  "Return (TEXT BOUNDS) for the molecule in the region or at point, or nil."
  (let ((bounds (cond ((use-region-p)
                       (cons (region-beginning) (region-end)))
                      ((tochemfig-mode--bounds-of-smiles-at-point)))))
    (when bounds
      (let ((text (string-trim (buffer-substring-no-properties
                                (car bounds) (cdr bounds)))))
        (unless (string-empty-p text)
          (list text bounds))))))

(defun tochemfig-mode--read-input ()
  "Read a molecule from the minibuffer.
Return (KIND . STRING), where KIND is either `direct' or `file'."
  (if (eq tochemfig-default-input 'file)
      (cons 'file (read-file-name "Molecule file: "))
    (cons 'direct (read-string "Enter SMILES: "))))

(defun tochemfig-mode--generate (kind text)
  "Generate chemfig code for TEXT, read according to KIND."
  (if (eq kind 'file)
      (tochemfig--generate-from-file text)
    (tochemfig--generate-from-string text)))

(defun tochemfig-mode--insert (code bounds)
  "Insert CODE, replacing BOUNDS when non-nil."
  (atomic-change-group
    (when bounds
      (goto-char (car bounds))
      (delete-region (car bounds) (cdr bounds)))
    (insert code)))

;;; Commands

;;;###autoload
(defun tochemfig ()
  "Generate chemfig code for the molecule at point, in the region, or prompted.
The region or the SMILES string at point is replaced by the generated code;
otherwise a molecule is read from the minibuffer and inserted at point.
When `tochemfig-mode' is active in a LaTeX or Org buffer the output is
wrapped in \\chemfig{...}."
  (interactive)
  (let ((source (tochemfig-mode--source-at-point)))
    (if source
        (tochemfig-mode--insert (tochemfig-mode--generate 'direct (car source))
                                (cadr source))
      (let ((input (tochemfig-mode--read-input)))
        (insert (tochemfig-mode--generate (car input) (cdr input)))))))

;;;###autoload
(defun tochemfig-convert-region (start end)
  "Replace the region from START to END with chemfig code.
The region is interpreted as a SMILES string."
  (interactive "r")
  (let ((text (string-trim (buffer-substring-no-properties start end))))
    (when (string-empty-p text)
      (user-error "Empty region"))
    (tochemfig-mode--insert (tochemfig-mode--generate 'direct text)
                            (cons start end))))

;;;###autoload
(defun tochemfig-copy ()
  "Put chemfig code for the molecule at point or in the region on the kill ring."
  (interactive)
  (let ((source (tochemfig-mode--source-at-point)))
    (kill-new (if source
                  (tochemfig-mode--generate 'direct (car source))
                (let ((input (tochemfig-mode--read-input)))
                  (tochemfig-mode--generate (car input) (cdr input)))))))

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
