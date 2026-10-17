;;; tochemfig-transient.el --- Transient interface for tochemfig -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2025 Giovanni Crisalfi

;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Maintainer: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The interactive surface of tochemfig: the SMILES-at-point helpers, the
;; generate/copy workflow, and the `tochemfig-dispatch' transient menu.
;;
;; This file is loaded on demand from `tochemfig.el' and required explicitly
;; by `tochemfig-mode.el'.  It does not require the mode at load time, so the
;; transient remains usable from any buffer; the mode is loaded when the menu
;; is opened.

;;; Code:

(require 'tochemfig)
(require 'thingatpt)
(require 'transient)

(defvar tochemfig-mode)                 ; defined by `define-minor-mode'

(declare-function tochemfig-mode "tochemfig-mode" (&optional arg))

;;; Customization

(defcustom tochemfig-mode-auto-wrap t
  "When non-nil, wrap output in \\chemfig{...} in LaTeX and Org buffers.
This applies while `tochemfig-mode' is active, so that commands in the
buffer produce code ready to be pasted into a LaTeX document."
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

;;; Input helpers

(defconst tochemfig-mode--smiles-charset "A-Za-z0-9@+[]()=#$:/\\.%*-"
  "Characters that may appear in a SMILES string.")

(defun tochemfig-mode--smiles-syntax-p (s)
  "Return non-nil when S contains SMILES-specific punctuation."
  (catch 'yes
    (dolist (char '("(" ")" "[" "]" "=" "#" "@" "+" "/" "\\"))
      (when (string-search char s) (throw 'yes t)))
    nil))

(defun tochemfig-mode--smiles-looking-p (s)
  "Return non-nil when S looks like SMILES rather than ordinary prose."
  (and (not (string-empty-p s))
       (or (string-match-p "[0-9]" s)              ; ring closures, counts
           (tochemfig-mode--smiles-syntax-p s)     ; SMILES-only punctuation
           (string-match-p "\\`[BCNOPSFI][BCNOPSFI]*\\'" s)))) ; bare chain

(defcustom tochemfig-smiles-looking-predicate #'tochemfig-mode--smiles-looking-p
  "Function of one string deciding whether it looks like SMILES.
Used to keep `tochemfig-smiles-at-point' and the SMILES `thing' from
matching ordinary prose.  Set it to a function that always returns t to
restore the previous permissive behaviour."
  :group 'tochemfig
  :type 'function)

(defun tochemfig-mode--bounds-of-smiles-at-point ()
  "Return the raw (START . END) bounds of the token at point, or nil."
  (let ((start (save-excursion
                 (skip-chars-backward tochemfig-mode--smiles-charset)
                 (point)))
        (end (save-excursion
               (skip-chars-forward tochemfig-mode--smiles-charset)
               (point))))
    (when (< start end)
      (cons start end))))

(defun tochemfig-smiles-bounds-at-point ()
  "Return the (START . END) bounds of the SMILES string at point, or nil.
Returns nil when the token at point does not look like SMILES."
  (let ((bounds (tochemfig-mode--bounds-of-smiles-at-point)))
    (when bounds
      (let ((text (string-trim
                   (buffer-substring-no-properties (car bounds) (cdr bounds)))))
        (when (funcall tochemfig-smiles-looking-predicate text)
          bounds)))))

(defun tochemfig-smiles-at-point ()
  "Return the SMILES string at point, or nil."
  (let ((bounds (tochemfig-smiles-bounds-at-point)))
    (when bounds
      (string-trim (buffer-substring-no-properties (car bounds) (cdr bounds))))))

(defun forward-tochemfig-smiles (&optional arg)
  "Move forward across a SMILES string (backward with negative ARG)."
  (interactive "^p")
  (let ((n (or arg 1)))
    (dotimes (_ (abs n))
      (if (> n 0)
          (progn
            (skip-chars-forward (concat "^" tochemfig-mode--smiles-charset))
            (skip-chars-forward tochemfig-mode--smiles-charset))
        (skip-chars-backward tochemfig-mode--smiles-charset)
        (skip-chars-backward (concat "^" tochemfig-mode--smiles-charset))))))

;; Expose SMILES as a `thing' when the running Emacs supports it.
(put 'smiles 'forward-op #'forward-tochemfig-smiles)
(put 'smiles 'bounds-of-thing-at-point #'tochemfig-smiles-bounds-at-point)
(when (boundp 'thing-at-point-provider-alist)
  (add-to-list 'thing-at-point-provider-alist
               (cons 'smiles #'tochemfig-smiles-at-point)))
(when (boundp 'bounds-of-thing-at-point-provider-alist)
  (add-to-list 'bounds-of-thing-at-point-provider-alist
               (cons 'smiles #'tochemfig-smiles-bounds-at-point)))

(defun tochemfig-mode--source-at-point ()
  "Return (TEXT BOUNDS) for the molecule in the region or at point, or nil.
BOUNDS is a pair of markers, so the source follows edits in its buffer and
can be detected as dead once that buffer is killed."
  (let ((bounds (cond ((use-region-p)
                       (cons (region-beginning) (region-end)))
                      ((tochemfig-smiles-bounds-at-point)))))
    (when bounds
      (let ((text (string-trim (buffer-substring-no-properties
                                (car bounds) (cdr bounds)))))
        (unless (string-empty-p text)
          (list text (cons (copy-marker (car bounds))
                           (copy-marker (cdr bounds)))))))))

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
  "Insert CODE, replacing BOUNDS when non-nil and still valid.
BOUNDS may be buffer positions or markers.  When BOUNDS are markers whose
buffer has been killed, CODE is inserted without deleting anything."
  (atomic-change-group
    (when (and bounds
               (or (not (markerp (car bounds)))
                   (marker-buffer (car bounds))))
      (goto-char (car bounds))
      (delete-region (car bounds) (cdr bounds)))
    (insert code))
  ;; Deactivate the mark so Evil leaves visual state after the region was
  ;; consumed (its `deactivate-mark-hook' does this).  Harmless in vanilla
  ;; Emacs.
  (when bounds (deactivate-mark t)))

;;; Commands

(defun tochemfig-mode--produce (copy &optional source overrides)
  "Generate chemfig code and either insert it or COPY it to the kill ring.
SOURCE, when non-nil, is a (TEXT BOUNDS) list to use instead of the
molecule at point or in the region.  OVERRIDES, when non-nil, is an alist
of option overrides for this generation only."
  (let ((src (or source (tochemfig-mode--source-at-point)))
        (options (append overrides
                         (and (tochemfig-mode--wrap-p)
                              '(("wrap-chemfig" . t))))))
    (if src
        (let ((code (tochemfig-mode--generate 'direct (car src) options)))
          (if copy
              (progn (kill-new code) (deactivate-mark t))
            (tochemfig-mode--insert code (cadr src))))
      (let* ((input (tochemfig-mode--read-input))
             (code (tochemfig-mode--generate (car input) (cdr input) options)))
        (if copy (kill-new code) (insert code))))))

(defun tochemfig ()
  "Generate chemfig code for the molecule at point, in the region, or prompted.
The region or the SMILES string at point is replaced by the generated code;
otherwise a molecule is read from the minibuffer and inserted at point.
When `tochemfig-mode' is active in a LaTeX or Org buffer the output is
wrapped in \\chemfig{...}."
  (interactive)
  (tochemfig-mode--produce nil))

(defun tochemfig-copy ()
  "Put chemfig code for the molecule at point or in the region on the kill ring."
  (interactive)
  (tochemfig-mode--produce t))

;;; Wrap context

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
      (and (bound-and-true-p tochemfig-mode)
           tochemfig-mode-auto-wrap
           (tochemfig-mode--wrap-context-p))))

;;; Transient menu

(defvar tochemfig-mode--edit-buffer nil
  "Buffer the transient menu was invoked from.")

(defvar tochemfig-mode--pending-source nil
  "Molecule source captured when the transient menu was opened.
A (TEXT BOUNDS) list, or nil.")

(defvar tochemfig-mode--pending-overrides nil
  "Option overrides for the next transient Generate or Copy action.")

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

(defmacro tochemfig-mode--define-option (name key variable label reader &optional guard)
  "Define a transient suffix NAME editing VARIABLE.
KEY is the transient key, LABEL the menu entry description, READER the
function used to read a new value, and GUARD an optional form; when it
evaluates to nil the suffix is hidden."
  `(transient-define-suffix ,name ()
     :transient t
     :key ,key
     ,@(when guard (list :if guard))
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
  #'tochemfig-mode--optional-number-reader
  (lambda () (tochemfig-mode--value 'tochemfig-default-submol-name)))
(tochemfig-mode--define-option tochemfig-option-exit-atom
  "x" tochemfig-default-exit-atom "Exit atom"
  #'tochemfig-mode--optional-number-reader
  (lambda () (tochemfig-mode--value 'tochemfig-default-submol-name)))

(tochemfig-mode--define-toggle tochemfig-toggle-strict
  "r" tochemfig-default-strict "Strict validation")
(tochemfig-mode--define-toggle tochemfig-toggle-recalculate-coordinates
  "u" tochemfig-default-recalculate-coordinates "Recalculate coordinates")
(tochemfig-mode--define-toggle tochemfig-toggle-legacy-lewis
  "j" tochemfig-default-legacy-lewis "Legacy \\lewis (ChemFig < 1.6)")
(tochemfig-mode--define-option tochemfig-option-cross-bond
  "k" tochemfig-default-cross-bond "Cross bonds"
  #'tochemfig-mode--cross-bond-reader)

(defun tochemfig-transient--consume-overrides ()
  "Return and clear the pending one-off option overrides."
  (prog1 tochemfig-mode--pending-overrides
    (setq tochemfig-mode--pending-overrides nil)))

(transient-define-suffix tochemfig-transient-generate ()
  "Generate chemfig code using the current options."
  :key "RET"
  :description "Generate"
  (interactive)
  (let ((buffer tochemfig-mode--edit-buffer)
        (source tochemfig-mode--pending-source)
        (overrides (tochemfig-transient--consume-overrides)))
    (setq tochemfig-mode--pending-source nil)
    (with-current-buffer (or (and (buffer-live-p buffer) buffer)
                             (current-buffer))
      (tochemfig-mode--produce nil source overrides))))

(transient-define-suffix tochemfig-transient-copy ()
  "Copy chemfig code using the current options."
  :key "C"
  :description "Copy"
  (interactive)
  (let ((buffer tochemfig-mode--edit-buffer)
        (source tochemfig-mode--pending-source)
        (overrides (tochemfig-transient--consume-overrides)))
    (setq tochemfig-mode--pending-source nil)
    (with-current-buffer (or (and (buffer-live-p buffer) buffer)
                             (current-buffer))
      (tochemfig-mode--produce t source overrides))))

(transient-define-suffix tochemfig-transient-file ()
  "Generate chemfig code from a MOL file without changing the input option."
  :key "F"
  :description "Generate from file..."
  (interactive)
  (let ((buffer tochemfig-mode--edit-buffer)
        (overrides (tochemfig-transient--consume-overrides))
        (path (read-file-name "Molecule file: ")))
    (setq tochemfig-mode--pending-source nil)
    (with-current-buffer (or (and (buffer-live-p buffer) buffer)
                             (current-buffer))
      (insert (tochemfig-mode--generate
               'file path
               (append overrides
                       (and (tochemfig-mode--wrap-p)
                            '(("wrap-chemfig" . t)))))))))

(transient-define-suffix tochemfig-transient-rotate ()
  "Rotate the next generated structure without changing the default angle."
  :key "A"
  :description (lambda ()
                 (let ((angle (cdr (assoc "angle"
                                          tochemfig-mode--pending-overrides))))
                   (if angle
                       (format "Rotate once [%s]" angle)
                     "Rotate once...")))
  (interactive)
  (let ((angle (read-number "Rotation angle: "
                            (tochemfig-mode--value 'tochemfig-default-angle))))
    (setq tochemfig-mode--pending-overrides
          (cons (cons "angle" angle)
                (assoc-delete-all "angle" tochemfig-mode--pending-overrides)))
    (message "tochemfig: next generation rotated by %s degrees" angle)))

(transient-define-suffix tochemfig-transient-bindings ()
  "Show the `C-c m' key bindings."
  :key "?"
  :description "Key bindings"
  (interactive)
  (if (fboundp 'describe-keymap)
      (describe-keymap 'tochemfig-mode-map)
    (describe-bindings)))

;; Do not put an `;;;###autoload' cookie on `transient-define-prefix': the
;; autoload generator would copy the macro call into the generated autoloads
;; file, which fails before `transient' is loaded.  `tochemfig.el' registers
;; the autoload for `tochemfig-dispatch' explicitly instead.
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
   (tochemfig-transient-copy)
   (tochemfig-transient-file)
   (tochemfig-transient-rotate)
   (tochemfig-transient-bindings)]
  (interactive)
  (require 'tochemfig-mode nil t)
  (unless (bound-and-true-p tochemfig-mode)
    (tochemfig-mode 1))
  (setq tochemfig-mode--edit-buffer (current-buffer))
  (setq tochemfig-mode--pending-source (tochemfig-mode--source-at-point))
  (setq tochemfig-mode--pending-overrides nil)
  (transient-setup 'tochemfig-dispatch))

(defun tochemfig-custom ()
  "Customize all options interactively and generate chemfig code."
  (interactive)
  (tochemfig-dispatch))

(provide 'tochemfig-transient)
;;; tochemfig-transient.el ends here
