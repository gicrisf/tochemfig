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
;; The commands are available under the `C-c m' prefix, and the transient
;; menu `tochemfig-dispatch' (also `tochemfig-custom') edits the options
;; interactively.

;;; Code:

(require 'tochemfig)
(require 'thingatpt)
(require 'transient)

(defvar tochemfig-mode)                ; defined below by `define-minor-mode'

;;; Customization

(defcustom tochemfig-mode-auto-wrap t
  "When non-nil, wrap output in \\chemfig{...} in LaTeX and Org buffers.
`tochemfig-mode' applies this when it is enabled, so that all commands in
the buffer produce code ready to be pasted into a LaTeX document."
  :group 'tochemfig
  :type 'boolean)

(defcustom tochemfig-mode-wrap-modes
  '(tex-mode latex-mode LaTeX-mode org-mode markdown-mode)
  "Major modes (and their derivatives) where output is wrapped.
When `tochemfig-mode-auto-wrap' is non-nil and the current command runs in
one of these modes (or inside an Org LaTeX source block), the generated
code is wrapped in \\chemfig{...}."
  :group 'tochemfig
  :type '(repeat symbol))

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

(defun tochemfig-mode--generate (kind text &optional overrides)
  "Generate chemfig code for TEXT, read according to KIND.
OVERRIDES, when non-nil, is an alist of option overrides.  Signals a
`user-error' when generation fails, so interactive commands do not surface
a raw Indigo error."
  (condition-case err
      (if (eq kind 'file)
          (tochemfig--generate-from-file text overrides)
        (tochemfig--generate-from-string text overrides))
    (error (user-error "tochemfig: %s" (error-message-string err)))))

(defun tochemfig-mode--insert (code bounds)
  "Insert CODE, replacing BOUNDS when non-nil."
  (atomic-change-group
    (when bounds
      (goto-char (car bounds))
      (delete-region (car bounds) (cdr bounds)))
    (insert code)))

;;; Commands

(defun tochemfig-mode--produce (copy &optional source)
  "Generate chemfig code and either insert it or COPY it to the kill ring.
SOURCE, when non-nil, is a (TEXT BOUNDS) list to use instead of the
molecule at point or in the region."
  (let ((src (or source (tochemfig-mode--source-at-point)))
        (overrides (and (tochemfig-mode--wrap-p) '(("wrap-chemfig" . t)))))
    (if src
        (let ((code (tochemfig-mode--generate 'direct (car src) overrides)))
          (if copy (kill-new code) (tochemfig-mode--insert code (cadr src))))
      (let* ((input (tochemfig-mode--read-input))
             (code (tochemfig-mode--generate (car input) (cdr input) overrides)))
        (if copy (kill-new code) (insert code))))))

;;;###autoload
(defun tochemfig ()
  "Generate chemfig code for the molecule at point, in the region, or prompted.
The region or the SMILES string at point is replaced by the generated code;
otherwise a molecule is read from the minibuffer and inserted at point.
When `tochemfig-mode' is active in a LaTeX or Org buffer the output is
wrapped in \\chemfig{...}."
  (interactive)
  (tochemfig-mode--produce nil))

;;;###autoload
(defun tochemfig-convert-region (start end)
  "Replace the region from START to END with chemfig code.
The region is interpreted as a SMILES string."
  (interactive "r")
  (let ((text (string-trim (buffer-substring-no-properties start end))))
    (when (string-empty-p text)
      (user-error "Empty region"))
    (tochemfig-mode--insert
     (tochemfig-mode--generate 'direct text
                               (and (tochemfig-mode--wrap-p)
                                    '(("wrap-chemfig" . t))))
     (cons start end))))

;;;###autoload
(defun tochemfig-copy ()
  "Put chemfig code for the molecule at point or in the region on the kill ring."
  (interactive)
  (tochemfig-mode--produce t))

;;; Transient menu

(defvar tochemfig-mode--edit-buffer nil
  "Buffer the transient menu was invoked from.")

(defvar tochemfig-mode--pending-source nil
  "Molecule source captured when the transient menu was opened.
A (TEXT BOUNDS) list, or nil.")

(defun tochemfig-mode--value (variable)
  "Return VARIABLE's value in the buffer the transient was invoked from."
  (if (buffer-live-p tochemfig-mode--edit-buffer)
      (buffer-local-value variable tochemfig-mode--edit-buffer)
    (symbol-value variable)))

(defun tochemfig-mode--set (variable value)
  "Set VARIABLE buffer-locally in the buffer the transient was invoked from."
  (with-current-buffer (if (buffer-live-p tochemfig-mode--edit-buffer)
                           tochemfig-mode--edit-buffer
                         (current-buffer))
    (set (make-local-variable variable) value)))

(defun tochemfig-mode--display (value)
  "Format VALUE for display in the transient menu."
  (cond ((null value) "-")
        ((eq value t) "on")
        ((stringp value) (if (string-empty-p value) "-" value))
        (t (format "%s" value))))

(defun tochemfig-mode--string-reader (prompt initial &rest _)
  "Reader for an optional string option."
  (let ((value (read-string prompt (and initial (format "%s" initial)))))
    (unless (string-empty-p value) value)))

(defun tochemfig-mode--number-reader (prompt initial &rest _)
  "Reader for a number option."
  (read-number prompt initial))

(defun tochemfig-mode--optional-number-reader (prompt initial &rest _)
  "Reader for an optional number option."
  (let ((value (read-string prompt (if initial (number-to-string initial) ""))))
    (unless (string-empty-p value) (string-to-number value))))

(defun tochemfig-mode--choice-reader (choices)
  "Return a reader function choosing among CHOICES."
  (lambda (prompt initial &rest _)
    (intern (completing-read prompt choices nil t nil nil
                             (and initial (symbol-name initial))))))

(defun tochemfig-mode--cross-bond-reader (prompt initial &rest _)
  "Reader for the cross-bond option, a list of (START END) pairs."
  (let* ((default (and initial
                       (mapconcat (lambda (pair)
                                    (format "%d-%d" (car pair) (cadr pair)))
                                  initial ",")))
         (value (read-string prompt default)))
    (unless (string-empty-p value)
      (mapcar (lambda (fragment)
                (mapcar #'string-to-number (split-string fragment "-")))
              (split-string value "," t)))))

(defmacro tochemfig-mode--define-toggle (name key variable label)
  "Define a transient suffix NAME toggling boolean VARIABLE.
KEY is the transient key and LABEL the menu entry description."
  `(transient-define-suffix ,name ()
     :transient t
     :key ,key
     :description (lambda ()
                    (format "%s [%s]" ,label
                            (if (tochemfig-mode--value ',variable) "on" "off")))
     (interactive)
     (tochemfig-mode--set ',variable (not (tochemfig-mode--value ',variable)))))

(defmacro tochemfig-mode--define-option (name key variable label reader)
  "Define a transient suffix NAME editing VARIABLE.
KEY is the transient key, LABEL the menu entry description and READER the
function used to read a new value."
  `(transient-define-suffix ,name ()
     :transient t
     :key ,key
     :description (lambda ()
                    (format "%s [%s]" ,label
                            (tochemfig-mode--display
                             (tochemfig-mode--value ',variable))))
     (interactive)
     (tochemfig-mode--set
      ',variable
      (funcall ,reader (format "%s: " ,label)
               (tochemfig-mode--value ',variable)))))

(tochemfig-mode--define-option tochemfig-option-input
  "i" tochemfig-default-input "Input"
  (tochemfig-mode--choice-reader '("direct" "file")))
(tochemfig-mode--define-option tochemfig-option-hydrogens
  "y" tochemfig-default-hydrogens "Hydrogens"
  (tochemfig-mode--choice-reader '("keep" "add" "delete")))

(tochemfig-mode--define-option tochemfig-option-angle
  "a" tochemfig-default-angle "Rotation angle" #'tochemfig-mode--number-reader)
(tochemfig-mode--define-toggle tochemfig-toggle-flip
  "p" tochemfig-default-flip "Flip horizontally")
(tochemfig-mode--define-toggle tochemfig-toggle-flop
  "q" tochemfig-default-flop "Flip vertically")
(tochemfig-mode--define-toggle tochemfig-toggle-relative-angles
  "v" tochemfig-default-relative-angles "Relative angles")

(tochemfig-mode--define-toggle tochemfig-toggle-show-carbons
  "c" tochemfig-default-show-carbons "Show carbons")
(tochemfig-mode--define-toggle tochemfig-toggle-show-methyls
  "m" tochemfig-default-show-methyls "Show methyls")
(tochemfig-mode--define-toggle tochemfig-toggle-atom-numbers
  "n" tochemfig-default-atom-numbers "Atom numbers")
(tochemfig-mode--define-toggle tochemfig-toggle-aromatic-circles
  "o" tochemfig-default-aromatic-circles "Aromatic circles")

(tochemfig-mode--define-toggle tochemfig-toggle-fancy-bonds
  "f" tochemfig-default-fancy-bonds "Fancy bonds")
(tochemfig-mode--define-option tochemfig-option-bond-scale
  "s" tochemfig-default-bond-scale "Bond scaling"
  (tochemfig-mode--choice-reader '("normalize" "keep" "scale")))
(tochemfig-mode--define-option tochemfig-option-bond-stretch
  "t" tochemfig-default-bond-stretch "Bond stretch" #'tochemfig-mode--number-reader)

(tochemfig-mode--define-toggle tochemfig-toggle-terse
  "z" tochemfig-default-terse "Terse output")
(tochemfig-mode--define-option tochemfig-option-indent
  "d" tochemfig-default-indent "Indentation" #'tochemfig-mode--number-reader)
(tochemfig-mode--define-toggle tochemfig-toggle-wrap-chemfig
  "w" tochemfig-default-wrap-chemfig "Wrap in \\chemfig{}")
(tochemfig-mode--define-option tochemfig-option-markers
  "g" tochemfig-default-markers "Marker prefix" #'tochemfig-mode--string-reader)

(tochemfig-mode--define-option tochemfig-option-submol-name
  "l" tochemfig-default-submol-name "Submol name" #'tochemfig-mode--string-reader)
(tochemfig-mode--define-option tochemfig-option-entry-atom
  "e" tochemfig-default-entry-atom "Entry atom"
  #'tochemfig-mode--optional-number-reader)
(tochemfig-mode--define-option tochemfig-option-exit-atom
  "x" tochemfig-default-exit-atom "Exit atom"
  #'tochemfig-mode--optional-number-reader)

(tochemfig-mode--define-toggle tochemfig-toggle-strict
  "r" tochemfig-default-strict "Strict validation")
(tochemfig-mode--define-toggle tochemfig-toggle-recalculate-coordinates
  "u" tochemfig-default-recalculate-coordinates "Recalculate coordinates")
(tochemfig-mode--define-toggle tochemfig-toggle-legacy-lewis
  "j" tochemfig-default-legacy-lewis "Legacy \\lewis (ChemFig < 1.6)")
(tochemfig-mode--define-option tochemfig-option-cross-bond
  "k" tochemfig-default-cross-bond "Cross bonds"
  #'tochemfig-mode--cross-bond-reader)

(transient-define-suffix tochemfig-transient-generate ()
  "Generate chemfig code using the current options."
  :key "RET"
  :description "Generate"
  (interactive)
  (let ((buffer tochemfig-mode--edit-buffer)
        (source tochemfig-mode--pending-source))
    (setq tochemfig-mode--pending-source nil)
    (with-current-buffer (or (and (buffer-live-p buffer) buffer)
                             (current-buffer))
      (tochemfig-mode--produce nil source))))

(transient-define-suffix tochemfig-transient-copy ()
  "Copy chemfig code using the current options."
  :key "C"
  :description "Copy"
  (interactive)
  (let ((buffer tochemfig-mode--edit-buffer)
        (source tochemfig-mode--pending-source))
    (setq tochemfig-mode--pending-source nil)
    (with-current-buffer (or (and (buffer-live-p buffer) buffer)
                             (current-buffer))
      (tochemfig-mode--produce t source))))

(transient-define-prefix tochemfig-dispatch ()
  "Customize `tochemfig' options and generate chemfig code."
  ["Input"
   (tochemfig-option-input)
   (tochemfig-option-hydrogens)]
  ["Geometry"
   (tochemfig-option-angle)
   (tochemfig-toggle-flip)
   (tochemfig-toggle-flop)
   (tochemfig-toggle-relative-angles)]
  ["Atoms"
   (tochemfig-toggle-show-carbons)
   (tochemfig-toggle-show-methyls)
   (tochemfig-toggle-atom-numbers)
   (tochemfig-toggle-aromatic-circles)]
  ["Bonds"
   (tochemfig-toggle-fancy-bonds)
   (tochemfig-option-bond-scale)
   (tochemfig-option-bond-stretch)]
  ["Output"
   (tochemfig-toggle-terse)
   (tochemfig-option-indent)
   (tochemfig-toggle-wrap-chemfig)
   (tochemfig-option-markers)]
  ["Sub-molecule"
   (tochemfig-option-submol-name)
   (tochemfig-option-entry-atom)
   (tochemfig-option-exit-atom)]
  ["Advanced"
   (tochemfig-toggle-strict)
   (tochemfig-toggle-recalculate-coordinates)
   (tochemfig-toggle-legacy-lewis)
   (tochemfig-option-cross-bond)]
  ["Actions"
   (tochemfig-transient-generate)
   (tochemfig-transient-copy)]
  (interactive)
  (unless tochemfig-mode
    (tochemfig-mode 1))
  (setq tochemfig-mode--edit-buffer (current-buffer))
  (setq tochemfig-mode--pending-source (tochemfig-mode--source-at-point))
  (transient-setup 'tochemfig-dispatch))

;;;###autoload
(defun tochemfig-custom ()
  "Customize all options interactively and generate chemfig code."
  (interactive)
  (tochemfig-dispatch))

;;; Keymap

(defvar tochemfig-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c m d") #'tochemfig)
    (define-key map (kbd "C-c m c") #'tochemfig-dispatch)
    (define-key map (kbd "C-c m f") #'tochemfig-input-file)
    (define-key map (kbd "C-c m w") #'tochemfig-wrap-chemfig)
    (define-key map (kbd "C-c m t") #'tochemfig-terse)
    (define-key map (kbd "C-c m r") #'tochemfig-rotate)
    (define-key map (kbd "C-c m p") #'tochemfig-copy)
    map)
  "Keymap for `tochemfig-mode'.
The commands are bound under the `C-c m' prefix.")

(with-eval-after-load 'which-key
  (when (fboundp 'which-key-add-keymap-based-replacements)
    (which-key-add-keymap-based-replacements tochemfig-mode-map
      "C-c m" "tochemfig"
      "C-c m d" "Generate (region/point/prompt)"
      "C-c m c" "Options menu"
      "C-c m f" "Generate from file"
      "C-c m w" "Wrap in \\chemfig{}"
      "C-c m t" "Terse output"
      "C-c m r" "Rotate"
      "C-c m p" "Copy to kill ring")))

;;; Minor Mode

(defun tochemfig-mode--org-latex-block-p ()
  "Return non-nil when point is inside an Org LaTeX source block."
  (and (derived-mode-p 'org-mode)
       (fboundp 'org-babel-get-src-block-info)
       (let ((info (ignore-errors (org-babel-get-src-block-info 'light))))
         (and info (member (car info) '("latex" "tex"))))))

(defun tochemfig-mode--wrap-context-p ()
  "Return non-nil when the buffer (or point) is a wrapping context."
  (or (apply #'derived-mode-p tochemfig-mode-wrap-modes)
      (tochemfig-mode--org-latex-block-p)))

(defun tochemfig-mode--wrap-p ()
  "Return non-nil when generated code should be wrapped in \\chemfig{}."
  (or tochemfig-default-wrap-chemfig
      (and tochemfig-mode
           tochemfig-mode-auto-wrap
           (tochemfig-mode--wrap-context-p))))

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
