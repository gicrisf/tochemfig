;;; chem-mode.el --- Generate chemical drawings from molecules -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2025 Giovanni Crisalfi

;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Maintainer: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Version: 0.4.0
;; Keywords: chemistry mol smiles convenience languages
;; Homepage: https://github.com/gicrisf/tochemfig
;; Package-Requires: ((emacs "29.1") (indigo "0.10.1") (transient "0.7.0"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Generic commands and minor mode for chem-mode.  Molecules are read into the
;; drawing IR and rendered by the selected exporter.  The chemfig exporter is
;; the first registered exporter; `tochemfig' remains as its direct command.

;;; Code:

(require 'thingatpt)
(require 'chem-indigo)
(require 'chem-options)
(require 'chem-export)

(declare-function chem-dispatch-menu "chem-dispatch" ())

(defcustom chem-default-exporter 'chemfig
  "Exporter used by `chem-export' and `chem-copy'."
  :group 'chem-mode
  :type 'symbol)

(defcustom chem-mode-auto-wrap t
  "When non-nil, wrap output in the exporter's natural wrapper.
For chemfig this is \\chemfig{...} in LaTeX and Org buffers."
  :group 'chem-mode
  :type 'boolean)

(defcustom chem-mode-wrap-modes
  '(tex-mode latex-mode LaTeX-mode org-mode markdown-mode)
  "Major modes where output is wrapped automatically."
  :group 'chem-mode
  :type '(repeat symbol))

(defcustom chem-options-file
  (locate-user-emacs-file "chem-options.el")
  "File where `chem-save-options' writes the global option values.
Set it to nil to rely on other persistence."
  :group 'chem-mode
  :type '(choice (const :tag "Disabled" nil) file))

;;; Input helpers

(defconst chem-mode--smiles-charset "A-Za-z0-9@+[]()=#$:/\\.%*-"
  "Characters that may appear in a SMILES string.")

(defun chem-mode--smiles-syntax-p (s)
  "Return non-nil when S contains SMILES-specific punctuation."
  (catch 'yes
    (dolist (char '("(" ")" "[" "]" "=" "#" "@" "+" "/" "\\"))
      (when (string-search char s) (throw 'yes t)))
    nil))

(defun chem-mode--smiles-looking-p (s)
  "Return non-nil when S looks like SMILES rather than ordinary prose."
  (and (not (string-empty-p s))
       (or (string-match-p "[0-9]" s)
           (chem-mode--smiles-syntax-p s)
           (string-match-p "\\`[BCNOPSFI][BCNOPSFI]*\\'" s))))

(defcustom chem-smiles-looking-predicate #'chem-mode--smiles-looking-p
  "Function of one string deciding whether it looks like SMILES."
  :group 'chem-mode
  :type 'function)

(defun chem-mode--bounds-of-smiles-at-point ()
  "Return the raw (START . END) bounds of the token at point, or nil."
  (let ((start (save-excursion
                 (skip-chars-backward chem-mode--smiles-charset)
                 (point)))
        (end (save-excursion
               (skip-chars-forward chem-mode--smiles-charset)
               (point))))
    (when (< start end)
      (cons start end))))

(defun chem-smiles-bounds-at-point ()
  "Return the (START . END) bounds of the SMILES string at point, or nil."
  (let ((bounds (chem-mode--bounds-of-smiles-at-point)))
    (when bounds
      (let ((text (string-trim
                   (buffer-substring-no-properties (car bounds) (cdr bounds)))))
        (when (funcall chem-smiles-looking-predicate text)
          bounds)))))

(defun chem-smiles-at-point ()
  "Return the SMILES string at point, or nil."
  (let ((bounds (chem-smiles-bounds-at-point)))
    (when bounds
      (string-trim (buffer-substring-no-properties (car bounds) (cdr bounds))))))

(defun forward-chem-smiles (&optional arg)
  "Move forward across a SMILES string (backward with negative ARG)."
  (interactive "^p")
  (let ((n (or arg 1)))
    (dotimes (_ (abs n))
      (if (> n 0)
          (progn
            (skip-chars-forward (concat "^" chem-mode--smiles-charset))
            (skip-chars-forward chem-mode--smiles-charset))
        (skip-chars-backward chem-mode--smiles-charset)
        (skip-chars-backward (concat "^" chem-mode--smiles-charset))))))

(put 'smiles 'forward-op #'forward-chem-smiles)
(put 'smiles 'bounds-of-thing-at-point #'chem-smiles-bounds-at-point)
(when (boundp 'thing-at-point-provider-alist)
  (add-to-list 'thing-at-point-provider-alist
               (cons 'smiles #'chem-smiles-at-point)))
(when (boundp 'bounds-of-thing-at-point-provider-alist)
  (add-to-list 'bounds-of-thing-at-point-provider-alist
               (cons 'smiles #'chem-smiles-bounds-at-point)))

;;; Source and insertion helpers

(defun chem-mode--source-at-point ()
  "Return (TEXT BOUNDS) for the molecule in the region or at point, or nil."
  (let ((bounds (cond ((use-region-p)
                       (cons (region-beginning) (region-end)))
                      ((chem-smiles-bounds-at-point)))))
    (when bounds
      (let ((text (string-trim (buffer-substring-no-properties
                                (car bounds) (cdr bounds)))))
        (unless (string-empty-p text)
          (list text (cons (copy-marker (car bounds))
                           (copy-marker (cdr bounds)))))))))

(defun chem-mode--read-input ()
  "Read a molecule from the minibuffer.
Return (KIND . STRING), where KIND is either `direct' or `file'."
  (if (eq chem-default-input 'file)
      (cons 'file (read-file-name "Molecule file: "))
    (cons 'direct (read-string "Enter SMILES: "))))

(defun chem-mode--generate (kind text &optional overrides)
  "Generate output for TEXT, read according to KIND.
OVERRIDES is an alist of option overrides."
  (condition-case err
      (let* ((options (chem--build-options overrides))
             (drawing (if (eq kind 'file)
                          (chem-from-file text options)
                        (chem-from-string text options))))
        (chem-render drawing chem-default-exporter options))
    (error (user-error "chem-mode: %s" (error-message-string err)))))

(defun chem-mode--insert (code bounds)
  "Insert CODE, replacing BOUNDS when non-nil and still valid."
  (atomic-change-group
    (when (and bounds
               (or (not (markerp (car bounds)))
                   (marker-buffer (car bounds))))
      (goto-char (car bounds))
      (delete-region (car bounds) (cdr bounds)))
    (insert code))
  (when bounds (deactivate-mark t)))

(defun chem-mode--org-latex-block-p ()
  "Return non-nil when point is inside an Org LaTeX source block."
  (and (derived-mode-p 'org-mode)
       (fboundp 'org-babel-get-src-block-info)
       (let ((info (ignore-errors (org-babel-get-src-block-info 'light))))
         (and info (member (car info) '("latex" "tex"))))))

(defun chem-mode--wrap-context-p ()
  "Return non-nil when the buffer (or point) is a wrapping context."
  (or (apply #'derived-mode-p chem-mode-wrap-modes)
      (chem-mode--org-latex-block-p)))

(defun chem-mode--wrap-p ()
  "Return non-nil when generated code should be wrapped."
  (or chem-exp-chemfig-default-wrap-chemfig
      (and (bound-and-true-p chem-mode)
           chem-mode-auto-wrap
           (chem-mode--wrap-context-p))))

(defun chem-mode--ensure-exporter ()
  "Load the selected exporter when needed."
  (unless (chem-exporter chem-default-exporter)
    (require (intern (concat "chem-exp-" (symbol-name chem-default-exporter))) nil t)
    (unless (chem-exporter chem-default-exporter)
      (user-error "Exporter not available: %s" chem-default-exporter))))

(defun chem-mode--produce (copy &optional source overrides)
  "Generate output and either insert it or COPY it to the kill ring."
  (chem-mode--ensure-exporter)
  (let ((src (or source (chem-mode--source-at-point)))
        (options (append overrides
                         (and (chem-mode--wrap-p)
                              '(("wrap-chemfig" . t))))))
    (if src
        (let ((code (chem-mode--generate 'direct (car src) options)))
          (if copy
              (progn (kill-new code) (deactivate-mark t))
            (chem-mode--insert code (cadr src))))
      (let* ((input (chem-mode--read-input))
             (code (chem-mode--generate (car input) (cdr input) options)))
        (if copy (kill-new code) (insert code))))))

;;; Commands

;;;###autoload
(defun chem-export ()
  "Generate a chemical drawing for the molecule at point or in the region.
The selected exporter is `chem-default-exporter'."
  (interactive)
  (chem-mode--produce nil))

;;;###autoload
(defun chem-copy ()
  "Copy a chemical drawing for the molecule at point or in the region."
  (interactive)
  (chem-mode--produce t))

;;;###autoload
(defun chem-dispatch ()
  "Customize options and generate a chemical drawing."
  (interactive)
  (require 'chem-dispatch)
  (chem-dispatch-menu))

;;;###autoload
(defun tochemfig ()
  "Generate chemfig code for the molecule at point or in the region."
  (interactive)
  (let ((chem-default-exporter 'chemfig))
    (chem-export)))

;;; Option introspection

(defun chem-mode--option-vars ()
  "Return all option variables used by the generic UI."
  (chem-mode--ensure-exporter)
  (delete-dups
   (append (list 'chem-default-input)
           (mapcar (lambda (spec) (plist-get spec :variable))
                   chem-common-option-specs)
           (mapcar (lambda (spec) (plist-get spec :variable))
                   (chem-exporter-option-specs chem-default-exporter)))))

(defun chem-mode--display (value)
  "Format VALUE for display in the options buffer."
  (cond ((null value) "-")
        ((eq value t) "on")
        ((stringp value) (if (string-empty-p value) "-" value))
        (t (format "%s" value))))

;;;###autoload
(defun chem-describe-options ()
  "Display the effective chem-mode options for the current buffer."
  (interactive)
  (let ((subject (current-buffer))
        (out (get-buffer-create "*chem options*")))
    (with-current-buffer out
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "chem-mode options for buffer `%s'\n\n"
                        (buffer-name subject)))
        (insert (format "%-32s %-22s %s\n" "OPTION" "EFFECTIVE" "SCOPE"))
        (insert (make-string 72 ?-) "\n")
        (dolist (var (chem-mode--option-vars))
          (let* ((effective (buffer-local-value var subject))
                 (local (local-variable-p var subject))
                 (differs (and local (not (equal effective (default-value var)))))
                 (scope (cond (differs "buffer-local")
                              (local "buffer-local (= global)")
                              (t "global"))))
            (insert (format "%-32s %-22s %s\n"
                            (symbol-name var)
                            (chem-mode--display effective)
                            scope))))
        (goto-char (point-min)))
      (special-mode)
      (when (and (bound-and-true-p evil-mode) (fboundp 'evil-emacs-state))
        (evil-emacs-state)))
    (display-buffer out)))

;;;###autoload
(defun chem-reset-options (&optional global)
  "Reset chem-mode options to their defaults."
  (interactive "P")
  (if global
      (dolist (var (chem-mode--option-vars))
        (set-default var (eval (car (get var 'standard-value)) t)))
    (dolist (var (chem-mode--option-vars))
      (kill-local-variable var)))
  (message "chem-mode options reset%s" (if global " globally" "")))

;;;###autoload
(defun chem-save-options ()
  "Save the current buffer's effective options as global defaults."
  (interactive)
  (let ((values (mapcar (lambda (var)
                          (cons var (buffer-local-value var (current-buffer))))
                        (chem-mode--option-vars))))
    (dolist (pair values)
      (set-default (car pair) (cdr pair)))
    (when chem-options-file
      (with-temp-file chem-options-file
        (insert ";;; chem-options.el --- saved chem-mode options "
                "-*- lexical-binding: t; -*-\n")
        (insert ";;; Generated by `chem-save-options'; do not edit.\n\n")
        (prin1 values (current-buffer))
        (insert "\n"))
      (message "chem-mode options saved to %s"
               (abbreviate-file-name chem-options-file)))))

;;;###autoload
(defun chem-reload-options ()
  "Load global option defaults from `chem-options-file'."
  (interactive)
  (cond
   ((null chem-options-file)
    (user-error "`chem-options-file' is nil"))
   ((not (file-readable-p chem-options-file))
    (user-error "No chem-mode options file at %s"
                (abbreviate-file-name chem-options-file)))
   (t
    (with-temp-buffer
      (insert-file-contents chem-options-file)
      (goto-char (point-min))
      (dolist (pair (read (current-buffer)))
        (set-default (car pair) (cdr pair))))
    (message "chem-mode options loaded from %s"
             (abbreviate-file-name chem-options-file)))))

;;; Minor mode

(defvar chem-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c m g") #'chem-export)
    (define-key map (kbd "C-c m o") #'chem-dispatch)
    (define-key map (kbd "C-c m c") #'chem-copy)
    (define-key map (kbd "C-c m d") #'chem-describe-options)
    map)
  "Keymap for `chem-mode'.")

(defun chem-mode--off ()
  "Remove buffer-local state installed by `chem-mode'."
  (dolist (var (chem-mode--option-vars))
    (kill-local-variable var)))

;;;###autoload
(define-minor-mode chem-mode
  "Minor mode for generating chemical drawings from molecules."
  :lighter " chem"
  :keymap chem-mode-map
  :group 'chem-mode
  (unless chem-mode
    (chem-mode--off)))

;;; Global mode

(defcustom chem-global-mode-all-buffers nil
  "Enable `chem-global-mode' in every buffer when non-nil."
  :group 'chem-mode
  :type 'boolean)

(defun chem-mode--eligible-p ()
  "Return non-nil when `chem-global-mode' should enable the mode here."
  (and (not (minibufferp))
       (not (string-prefix-p " " (buffer-name)))
       (not (derived-mode-p 'special-mode))
       (not buffer-read-only)
       (or chem-global-mode-all-buffers
           (chem-mode--wrap-context-p))))

(defun chem-mode--turn-on ()
  "Enable `chem-mode' in eligible buffers."
  (when (chem-mode--eligible-p)
    (chem-mode 1)))

;;;###autoload
(define-globalized-minor-mode chem-global-mode
  chem-mode chem-mode--turn-on
  :group 'chem-mode)

;;;###autoload
(defun chem-global-mode-refresh ()
  "Enable `chem-mode' in buffers that are now eligible."
  (interactive)
  (when chem-global-mode
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (not chem-mode) (chem-mode--eligible-p))
          (chem-mode 1))))))

(provide 'chem-mode)
;;; chem-mode.el ends here
