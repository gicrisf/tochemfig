;;; tochemfig-mode.el --- Context-aware minor mode for tochemfig -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2025 Giovanni Crisalfi

;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Maintainer: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `tochemfig-mode' turns a buffer into a surface for generating chemfig
;; code.  The `tochemfig-default-*' options are made buffer-local on first
;; edit, so every command in that buffer shares the same settings, and (in
;; LaTeX and Org buffers) generated code is wrapped in \chemfig{...}
;; automatically.
;;
;; The commands are available under the `C-c m' prefix.  The transient menu
;; `tochemfig-dispatch' (also `tochemfig-custom') is the primary interface:
;; it edits the options interactively and generates from the current region,
;; the SMILES at point, or a prompted molecule.

;;; Code:

(require 'tochemfig)
(require 'tochemfig-transient)

(defvar tochemfig-mode)                ; defined below by `define-minor-mode'

;;; Customization

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
    tochemfig-default-wrap-chemfig
    tochemfig-default-submol-name
    tochemfig-default-entry-atom
    tochemfig-default-exit-atom)
  "Option variables that `tochemfig-mode' makes buffer-local.")

;;; Option introspection

;;;###autoload
(defun tochemfig-describe-options ()
  "Display the effective tochemfig options for the current buffer."
  (interactive)
  (let ((subject (current-buffer))
        (out (get-buffer-create "*tochemfig options*")))
    (with-current-buffer out
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "tochemfig options for buffer `%s'\n\n"
                        (buffer-name subject)))
        (insert (format "%-32s %-22s %s\n" "OPTION" "EFFECTIVE" "SCOPE"))
        (insert (make-string 72 ?-) "\n")
        (dolist (var tochemfig-mode--option-vars)
          (let* ((effective (buffer-local-value var subject))
                 (local (local-variable-p var subject))
                 (differs (and local (not (equal effective (default-value var)))))
                 (scope (cond (differs "buffer-local")
                              (local "buffer-local (= global)")
                              (t "global"))))
            (insert (format "%-32s %-22s %s\n"
                            (symbol-name var)
                            (tochemfig-mode--display effective)
                            scope))))
        (goto-char (point-min)))
      (special-mode)
      ;; `special-mode' is not one of Evil's Emacs-state modes, so without this
      ;; the buffer would open in normal state where `q' records a macro
      ;; instead of quitting.  Guarded so vanilla Emacs is unaffected.
      (when (and (bound-and-true-p evil-mode) (fboundp 'evil-emacs-state))
        (evil-emacs-state)))
    (display-buffer out)))

;;;###autoload
(defun tochemfig-reset-options (&optional global)
  "Reset tochemfig options to their defaults.
Without a prefix argument, remove the buffer-local option values in the
current buffer.  With a prefix argument GLOBAL, reset the global defaults
to their `defcustom' values everywhere."
  (interactive "P")
  (if global
      (dolist (var tochemfig-mode--option-vars)
        (set-default var (eval (car (get var 'standard-value)) t)))
    (dolist (var tochemfig-mode--option-vars)
      (kill-local-variable var)))
  (message "tochemfig options reset%s" (if global " globally" "")))

;;; Persistence

(defcustom tochemfig-options-file
  (locate-user-emacs-file "tochemfig-options.el")
  "File where `tochemfig-save-options' writes the global option values.
Set it to nil to rely on other persistence (for example `savehist') instead."
  :group 'tochemfig
  :type '(choice (const :tag "Disabled" nil) file))

(defun tochemfig-mode--effective-options ()
  "Return an alist of (VARIABLE . VALUE) for the current buffer."
  (mapcar (lambda (var) (cons var (buffer-local-value var (current-buffer))))
          tochemfig-mode--option-vars))

;;;###autoload
(defun tochemfig-save-options ()
  "Save the current buffer's effective options as the global defaults.
Also writes them to `tochemfig-options-file' when that is non-nil, so
`tochemfig-reload-options' (or a later session) can restore them."
  (interactive)
  (let ((values (tochemfig-mode--effective-options)))
    (dolist (pair values)
      (set-default (car pair) (cdr pair)))
    (when tochemfig-options-file
      (with-temp-file tochemfig-options-file
        (insert ";;; tochemfig-options.el --- saved tochemfig options "
                "-*- lexical-binding: t; -*-\n")
        (insert ";;; Generated by `tochemfig-save-options'; do not edit.\n\n")
        (prin1 values (current-buffer))
        (insert "\n"))
      (message "tochemfig options saved to %s" (abbreviate-file-name
                                                tochemfig-options-file)))))

;;;###autoload
(defun tochemfig-reload-options ()
  "Load the global option defaults from `tochemfig-options-file'."
  (interactive)
  (cond
   ((null tochemfig-options-file)
    (user-error "`tochemfig-options-file' is nil"))
   ((not (file-readable-p tochemfig-options-file))
    (user-error "No tochemfig options file at %s"
                (abbreviate-file-name tochemfig-options-file)))
   (t
    (with-temp-buffer
      (insert-file-contents tochemfig-options-file)
      (goto-char (point-min))
      (dolist (pair (read (current-buffer)))
        (set-default (car pair) (cdr pair))))
    (message "tochemfig options loaded from %s"
             (abbreviate-file-name tochemfig-options-file)))))

;;; Keymap

(defvar tochemfig-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c m g") #'tochemfig)
    (define-key map (kbd "C-c m o") #'tochemfig-dispatch)
    (define-key map (kbd "C-c m c") #'tochemfig-copy)
    (define-key map (kbd "C-c m d") #'tochemfig-describe-options)
    map)
  "Keymap for `tochemfig-mode'.
The commands are bound under the `C-c m' prefix.")

(with-eval-after-load 'which-key
  (when (fboundp 'which-key-add-keymap-based-replacements)
    (which-key-add-keymap-based-replacements tochemfig-mode-map
      "C-c m" "tochemfig"
      "C-c m g" "Generate (region/point/prompt)"
      "C-c m o" "Options menu"
      "C-c m c" "Copy to kill ring"
      "C-c m d" "Describe options")))

;;; Menu bar

(defcustom tochemfig-mode-menu t
  "Show a tochemfig item in the menu bar while `tochemfig-mode' is active."
  :group 'tochemfig
  :type 'boolean)

(defvar tochemfig-mode-menu-map
  (easy-menu-create-menu
   "tochemfig"
   '(["Generate (region/point/prompt)" tochemfig t]
     ["Options menu" tochemfig-dispatch t]
     ["Copy to kill ring" tochemfig-copy t]
     "--"
     ["Describe options" tochemfig-describe-options t]))
  "Menu-bar entry shown while `tochemfig-mode' is active.")

(when tochemfig-mode-menu
  (define-key tochemfig-mode-map [menu-bar tochemfig]
    (cons "tochemfig" tochemfig-mode-menu-map)))

;;; Minor Mode

(defun tochemfig-mode--off ()
  "Remove buffer-local state installed by `tochemfig-mode'."
  (dolist (var tochemfig-mode--option-vars)
    (kill-local-variable var)))

;;;###autoload
(define-minor-mode tochemfig-mode
  "Minor mode for generating chemfig code from molecules.
Makes the `tochemfig-default-*' options buffer-local and, in LaTeX and
Org buffers, wraps generated code in \\chemfig{...}.  The commands are
available under the `C-c m' prefix; the full option surface lives in the
`tochemfig-dispatch' transient menu."
  :lighter " tochemfig"
  :keymap tochemfig-mode-map
  :group 'tochemfig
  (unless tochemfig-mode
    (tochemfig-mode--off)))

;;; Global mode

(defcustom tochemfig-global-mode-all-buffers nil
  "Enable `tochemfig-global-mode' in every buffer when non-nil.
By default the global mode only turns on `tochemfig-mode' in wrapping
contexts (LaTeX and Org); set this to t to enable it everywhere."
  :group 'tochemfig
  :type 'boolean)

(defun tochemfig-mode--eligible-p ()
  "Return non-nil when `tochemfig-global-mode' should enable the mode here."
  (and (not (minibufferp))
       (not (string-prefix-p " " (buffer-name)))
       (not (derived-mode-p 'special-mode))
       (not buffer-read-only)
       (or tochemfig-global-mode-all-buffers
           (tochemfig-mode--wrap-context-p))))

(defun tochemfig-mode--turn-on ()
  "Enable `tochemfig-mode' in eligible buffers."
  (when (tochemfig-mode--eligible-p)
    (tochemfig-mode 1)))

;;;###autoload
(define-globalized-minor-mode tochemfig-global-mode
  tochemfig-mode tochemfig-mode--turn-on
  :group 'tochemfig)

;;;###autoload
(defun tochemfig-global-mode-refresh ()
  "Enable `tochemfig-mode' in buffers that are now eligible.
Useful after changing `tochemfig-mode-wrap-modes' or
`tochemfig-global-mode-all-buffers'.  Never disables the mode in buffers
where it is already on."
  (interactive)
  (when tochemfig-global-mode
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (not tochemfig-mode) (tochemfig-mode--eligible-p))
          (tochemfig-mode 1))))))

(provide 'tochemfig-mode)
;;; tochemfig-mode.el ends here
