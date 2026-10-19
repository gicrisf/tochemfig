;;; chem-dispatch.el --- Transient menu for chem-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2025 Giovanni Crisalfi

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The generic options menu.  The option surface is registry-backed but
;; manually grouped; the exporter selector is at the top.  Chemfig is the only
;; exporter registered in milestone 1.

;;; Code:

(require 'transient)
(require 'chem-mode)
(require 'chem-options)
(require 'chem-export)
(require 'chem-exp-svg)

(defvar chem-dispatch--edit-buffer nil
  "Buffer the transient menu was invoked from.")

(defvar chem-dispatch--pending-source nil
  "Molecule source captured when the transient menu was opened.")

(defvar chem-dispatch--pending-overrides nil
  "Option overrides for the next transient Generate or Copy action.")

(defun chem-dispatch--value (variable)
  "Return VARIABLE's value in the buffer the transient was invoked from."
  (if (buffer-live-p chem-dispatch--edit-buffer)
      (buffer-local-value variable chem-dispatch--edit-buffer)
    (symbol-value variable)))

(defun chem-dispatch--set (variable value)
  "Set VARIABLE buffer-locally in the buffer the transient was invoked from."
  (with-current-buffer (if (buffer-live-p chem-dispatch--edit-buffer)
                           chem-dispatch--edit-buffer
                         (current-buffer))
    (set (make-local-variable variable) value)))

(defun chem-dispatch--display (value)
  "Format VALUE for display in the transient menu."
  (cond ((null value) "-")
        ((eq value t) "on")
        ((stringp value) (if (string-empty-p value) "-" value))
        (t (format "%s" value))))

(defun chem-dispatch--string-reader (prompt initial &rest _)
  "Reader for an optional string option."
  (let ((value (read-string prompt (and initial (format "%s" initial)))))
    (unless (string-empty-p value) value)))

(defun chem-dispatch--number-reader (prompt initial &rest _)
  "Reader for a number option."
  (read-number prompt initial))

(defun chem-dispatch--optional-number-reader (prompt initial &rest _)
  "Reader for an optional number option."
  (let ((value (read-string prompt (if initial (number-to-string initial) ""))))
    (unless (string-empty-p value) (string-to-number value))))

(defun chem-dispatch--choice-reader (choices)
  "Return a reader function choosing among CHOICES."
  (lambda (prompt initial &rest _)
    (intern (completing-read prompt choices nil t nil nil
                             (and initial (symbol-name initial))))))

(defun chem-dispatch--chemfig-exporter-p ()
  "Return non-nil when the selected exporter is chemfig."
  (eq (chem-dispatch--value 'chem-default-exporter) 'chemfig))

(defun chem-dispatch--svg-exporter-p ()
  "Return non-nil when the selected exporter is svg."
  (eq (chem-dispatch--value 'chem-default-exporter) 'svg))

(defun chem-dispatch--svg-theme-reader (prompt initial &rest _)
  "Reader for the SVG theme option."
  (intern
   (completing-read prompt
                    (mapcar (lambda (entry) (symbol-name (car entry)))
                            chem-exp-svg-themes)
                    nil t nil nil
                    (and initial (symbol-name initial)))))

(defun chem-dispatch--cross-bond-reader (prompt initial &rest _)
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

(defmacro chem-dispatch--define-toggle (name key variable label &optional guard)
  "Define a transient suffix NAME toggling boolean VARIABLE.
KEY is the transient key, LABEL the menu entry description, and GUARD an
optional form controlling visibility."
  `(transient-define-suffix ,name ()
     :transient t
     :key ,key
     ,@(when guard (list :if guard))
     :description (lambda ()
                    (format "%s [%s]" ,label
                            (if (chem-dispatch--value ',variable) "on" "off")))
     (interactive)
     (chem-dispatch--set ',variable (not (chem-dispatch--value ',variable)))))

(defmacro chem-dispatch--define-option (name key variable label reader &optional guard)
  "Define a transient suffix NAME editing VARIABLE.
KEY is the transient key, LABEL the menu entry description, READER the
function used to read a new value, and GUARD an optional form."
  `(transient-define-suffix ,name ()
     :transient t
     :key ,key
     ,@(when guard (list :if guard))
     :description (lambda ()
                    (format "%s [%s]" ,label
                            (chem-dispatch--display
                             (chem-dispatch--value ',variable))))
     (interactive)
     (chem-dispatch--set
      ',variable
      (funcall ,reader (format "%s: " ,label)
               (chem-dispatch--value ',variable)))))

(transient-define-suffix chem-dispatch-option-exporter ()
  "Select the exporter."
  :key "E"
  :description (lambda ()
                 (format "Exporter [%s]"
                         (chem-dispatch--display
                          (chem-dispatch--value 'chem-default-exporter))))
  (interactive)
  (let* ((names (chem-registered-exporters))
         (choice (intern (completing-read "Exporter: " names nil t nil nil
                                         (symbol-name
                                          (chem-dispatch--value 'chem-default-exporter))))))
    (chem-dispatch--set 'chem-default-exporter choice)
    (message "chem-mode exporter: %s" choice)))

(chem-dispatch--define-option chem-dispatch-option-input
  "i" chem-default-input "Input"
  (chem-dispatch--choice-reader '("direct" "file")))
(chem-dispatch--define-option chem-dispatch-option-hydrogens
  "y" chem-default-hydrogens "Hydrogens"
  (chem-dispatch--choice-reader '("keep" "add" "delete")))

(chem-dispatch--define-option chem-dispatch-option-angle
  "a" chem-default-angle "Rotation angle" #'chem-dispatch--number-reader)
(chem-dispatch--define-toggle chem-dispatch-toggle-flip
  "p" chem-default-flip "Flip horizontally")
(chem-dispatch--define-toggle chem-dispatch-toggle-flop
  "q" chem-default-flop "Flip vertically")
(chem-dispatch--define-toggle chem-dispatch-toggle-relative-angles
  "v" chem-exp-chemfig-default-relative-angles "Relative angles")

(chem-dispatch--define-toggle chem-dispatch-toggle-show-carbons
  "c" chem-default-show-carbons "Show carbons")
(chem-dispatch--define-toggle chem-dispatch-toggle-show-methyls
  "m" chem-default-show-methyls "Show methyls")
(chem-dispatch--define-toggle chem-dispatch-toggle-atom-numbers
  "n" chem-default-atom-numbers "Atom numbers")
(chem-dispatch--define-toggle chem-dispatch-toggle-aromatic-circles
  "o" chem-default-aromatic-circles "Aromatic circles")

(chem-dispatch--define-toggle chem-dispatch-toggle-fancy-bonds
  "f" chem-default-fancy-bonds "Fancy bonds")
(chem-dispatch--define-option chem-dispatch-option-bond-scale
  "s" chem-default-bond-scale "Bond scaling"
  (chem-dispatch--choice-reader '("normalize" "keep" "scale")))
(chem-dispatch--define-option chem-dispatch-option-bond-stretch
  "t" chem-default-bond-stretch "Bond stretch" #'chem-dispatch--number-reader)

(chem-dispatch--define-toggle chem-dispatch-toggle-terse
  "z" chem-exp-chemfig-default-terse "Terse output")
(chem-dispatch--define-option chem-dispatch-option-indent
  "d" chem-exp-chemfig-default-indent "Indentation" #'chem-dispatch--number-reader)
(chem-dispatch--define-toggle chem-dispatch-toggle-wrap-chemfig
  "w" chem-exp-chemfig-default-wrap-chemfig "Wrap in \\chemfig{}")
(chem-dispatch--define-option chem-dispatch-option-markers
  "g" chem-default-markers "Marker prefix" #'chem-dispatch--string-reader)

(chem-dispatch--define-option chem-dispatch-option-submol-name
  "l" chem-exp-chemfig-default-submol-name "Submol name" #'chem-dispatch--string-reader)
(chem-dispatch--define-option chem-dispatch-option-entry-atom
  "e" chem-default-entry-atom "Entry atom"
  #'chem-dispatch--optional-number-reader
  (lambda () (chem-dispatch--value 'chem-exp-chemfig-default-submol-name)))
(chem-dispatch--define-option chem-dispatch-option-exit-atom
  "x" chem-default-exit-atom "Exit atom"
  #'chem-dispatch--optional-number-reader
  (lambda () (chem-dispatch--value 'chem-exp-chemfig-default-submol-name)))

(chem-dispatch--define-toggle chem-dispatch-toggle-strict
  "r" chem-default-strict "Strict validation")
(chem-dispatch--define-toggle chem-dispatch-toggle-recalculate-coordinates
  "u" chem-default-recalculate-coordinates "Recalculate coordinates")
(chem-dispatch--define-toggle chem-dispatch-toggle-legacy-lewis
  "j" chem-exp-chemfig-default-legacy-lewis "Legacy \\lewis (ChemFig < 1.6)")
(chem-dispatch--define-option chem-dispatch-option-cross-bond
  "k" chem-default-cross-bond "Cross bonds"
  #'chem-dispatch--cross-bond-reader)

(chem-dispatch--define-option chem-dispatch-option-svg-width
  "W" chem-exp-svg-default-width "SVG width"
  #'chem-dispatch--optional-number-reader)
(chem-dispatch--define-option chem-dispatch-option-svg-height
  "H" chem-exp-svg-default-height "SVG height"
  #'chem-dispatch--optional-number-reader)
(chem-dispatch--define-option chem-dispatch-option-svg-padding
  "P" chem-exp-svg-default-padding "SVG padding"
  #'chem-dispatch--number-reader)
(chem-dispatch--define-option chem-dispatch-option-svg-scale
  "S" chem-exp-svg-default-scale "SVG scale"
  #'chem-dispatch--number-reader)
(chem-dispatch--define-option chem-dispatch-option-svg-bond-spacing
  "B" chem-exp-svg-default-bond-spacing "SVG bond spacing"
  #'chem-dispatch--number-reader)
(chem-dispatch--define-option chem-dispatch-option-svg-bond-thickness
  "T" chem-exp-svg-default-bond-thickness "SVG bond thickness"
  #'chem-dispatch--number-reader)
(chem-dispatch--define-option chem-dispatch-option-svg-short-bond-length
  "L" chem-exp-svg-default-short-bond-length "SVG short bond length"
  #'chem-dispatch--number-reader)
(chem-dispatch--define-option chem-dispatch-option-svg-font-family
  "N" chem-exp-svg-default-font-family "SVG font family"
  #'chem-dispatch--string-reader)
(chem-dispatch--define-option chem-dispatch-option-svg-font-size
  "Z" chem-exp-svg-default-font-size "SVG font size"
  #'chem-dispatch--number-reader)
(chem-dispatch--define-option chem-dispatch-option-svg-theme
  "O" chem-exp-svg-default-theme "SVG theme"
  #'chem-dispatch--svg-theme-reader)
(chem-dispatch--define-option chem-dispatch-option-svg-aromatic
  "R" chem-exp-svg-default-aromatic "SVG aromatic display"
  (chem-dispatch--choice-reader '("auto" "circle" "alternating")))
(chem-dispatch--define-toggle chem-dispatch-toggle-svg-show-stereo
  "V" chem-exp-svg-default-show-stereo "SVG show stereo")
(chem-dispatch--define-toggle chem-dispatch-toggle-svg-show-cip-labels
  "I" chem-exp-svg-default-show-cip-labels "SVG CIP labels")
(chem-dispatch--define-toggle chem-dispatch-toggle-svg-show-enhanced-stereo
  "U" chem-exp-svg-default-show-enhanced-stereo "SVG enhanced stereo")

(chem-dispatch--define-option chem-dispatch-option-reaction-arrow
  "1" chem-default-reaction-arrow "Reaction arrow"
  (chem-dispatch--choice-reader '("forward" "reverse" "equilibrium")))
(chem-dispatch--define-option chem-dispatch-option-reaction-above
  "2" chem-default-reaction-above "Text above arrow"
  #'chem-dispatch--string-reader)
(chem-dispatch--define-option chem-dispatch-option-reaction-below
  "3" chem-default-reaction-below "Text below arrow"
  #'chem-dispatch--string-reader)
(chem-dispatch--define-option chem-dispatch-option-reaction-spacing
  "4" chem-default-reaction-spacing "Reaction spacing"
  #'chem-dispatch--number-reader
  (lambda () (chem-dispatch--svg-exporter-p)))
(chem-dispatch--define-option chem-dispatch-option-reaction-arrow-length
  "5" chem-default-reaction-arrow-length "Reaction arrow length"
  #'chem-dispatch--number-reader
  (lambda () (chem-dispatch--svg-exporter-p)))
(chem-dispatch--define-option chem-dispatch-option-reaction-plus-size
  "6" chem-default-reaction-plus-size "Reaction plus size"
  #'chem-dispatch--number-reader
  (lambda () (chem-dispatch--svg-exporter-p)))
(chem-dispatch--define-toggle chem-dispatch-toggle-reaction-wrap
  "7" chem-exp-chemfig-default-reaction-wrap "Wrap reaction in scheme"
  (lambda () (chem-dispatch--chemfig-exporter-p)))
(chem-dispatch--define-option chem-dispatch-option-reaction-arrow-chemfig
  "8" chem-exp-chemfig-default-reaction-arrow-chemfig "Reaction arrow chemfig"
  #'chem-dispatch--string-reader
  (lambda () (chem-dispatch--chemfig-exporter-p)))

(defun chem-dispatch--consume-overrides ()
  "Return and clear the pending one-off option overrides."
  (prog1 chem-dispatch--pending-overrides
    (setq chem-dispatch--pending-overrides nil)))

(transient-define-suffix chem-dispatch-generate ()
  "Generate a drawing using the current options."
  :key "RET"
  :description "Generate"
  (interactive)
  (let ((buffer chem-dispatch--edit-buffer)
        (source chem-dispatch--pending-source)
        (overrides (chem-dispatch--consume-overrides)))
    (setq chem-dispatch--pending-source nil)
    (with-current-buffer (or (and (buffer-live-p buffer) buffer)
                             (current-buffer))
      (chem-mode--produce nil source overrides))))

(transient-define-suffix chem-dispatch-copy ()
  "Copy a drawing using the current options."
  :key "C"
  :description "Copy"
  (interactive)
  (let ((buffer chem-dispatch--edit-buffer)
        (source chem-dispatch--pending-source)
        (overrides (chem-dispatch--consume-overrides)))
    (setq chem-dispatch--pending-source nil)
    (with-current-buffer (or (and (buffer-live-p buffer) buffer)
                             (current-buffer))
      (chem-mode--produce t source overrides))))

(transient-define-suffix chem-dispatch-file ()
  "Generate from a MOL file without changing the input option."
  :key "F"
  :description "Generate from file..."
  (interactive)
  (let ((buffer chem-dispatch--edit-buffer)
        (overrides (chem-dispatch--consume-overrides))
        (path (read-file-name "Molecule file: ")))
    (setq chem-dispatch--pending-source nil)
    (with-current-buffer (or (and (buffer-live-p buffer) buffer)
                             (current-buffer))
      (insert (chem-mode--generate 'file path overrides)))))

(transient-define-suffix chem-dispatch-insert-image ()
  "Insert an SVG image link using the current options."
  :key "M"
  :description "Insert image"
  (interactive)
  (let ((buffer chem-dispatch--edit-buffer)
        (source chem-dispatch--pending-source)
        (overrides (chem-dispatch--consume-overrides)))
    (setq chem-dispatch--pending-source nil)
    (with-current-buffer (or (and (buffer-live-p buffer) buffer)
                             (current-buffer))
      (chem-mode--insert-image source overrides))))

(transient-define-suffix chem-dispatch-reaction-generate ()
  "Generate a reaction using the current options."
  :key "G"
  :description "Generate reaction"
  (interactive)
  (let ((buffer chem-dispatch--edit-buffer)
        (overrides (chem-dispatch--consume-overrides))
        (file current-prefix-arg))
    (with-current-buffer (or (and (buffer-live-p buffer) buffer)
                             (current-buffer))
      (chem-mode--produce-reaction nil file overrides))))

(transient-define-suffix chem-dispatch-reaction-copy ()
  "Copy a reaction using the current options."
  :key "K"
  :description "Copy reaction"
  (interactive)
  (let ((buffer chem-dispatch--edit-buffer)
        (overrides (chem-dispatch--consume-overrides))
        (file current-prefix-arg))
    (with-current-buffer (or (and (buffer-live-p buffer) buffer)
                             (current-buffer))
      (chem-mode--produce-reaction t file overrides))))

(transient-define-suffix chem-dispatch-rotate ()
  "Rotate the next generated structure without changing the default angle."
  :key "A"
  :description (lambda ()
                 (let ((angle (cdr (assoc "angle"
                                          chem-dispatch--pending-overrides))))
                   (if angle
                       (format "Rotate once [%s]" angle)
                     "Rotate once...")))
  (interactive)
  (let ((angle (read-number "Rotation angle: "
                            (chem-dispatch--value 'chem-default-angle))))
    (setq chem-dispatch--pending-overrides
          (cons (cons "angle" angle)
                (assoc-delete-all "angle" chem-dispatch--pending-overrides)))
    (message "chem-mode: next generation rotated by %s degrees" angle)))

(transient-define-suffix chem-dispatch-bindings ()
  "Show the `C-c m' key bindings."
  :key "?"
  :description "Key bindings"
  (interactive)
  (if (fboundp 'describe-keymap)
      (describe-keymap 'chem-mode-map)
    (describe-bindings)))

(transient-define-prefix chem-dispatch-menu ()
  "Customize options and generate a chemical drawing."
  ["Exporter"
   (chem-dispatch-option-exporter)]
  ["Input"
   (chem-dispatch-option-input)
   (chem-dispatch-option-hydrogens)]
  ["Geometry"
   (chem-dispatch-option-angle)
   (chem-dispatch-toggle-flip)
   (chem-dispatch-toggle-flop)]
  ["Atoms"
   (chem-dispatch-toggle-show-carbons)
   (chem-dispatch-toggle-show-methyls)
   (chem-dispatch-toggle-atom-numbers)]
  ["Bonds"
   (chem-dispatch-toggle-fancy-bonds)
   (chem-dispatch-option-bond-scale)
   (chem-dispatch-option-bond-stretch)]
  ["Markers"
   (chem-dispatch-option-markers)]
  ["Advanced"
   (chem-dispatch-toggle-strict)
   (chem-dispatch-toggle-recalculate-coordinates)
   (chem-dispatch-option-cross-bond)]
  ["Chemfig"
   :if (lambda () (chem-dispatch--chemfig-exporter-p))
   (chem-dispatch-toggle-relative-angles)
   (chem-dispatch-toggle-aromatic-circles)
   (chem-dispatch-toggle-terse)
   (chem-dispatch-option-indent)
   (chem-dispatch-toggle-wrap-chemfig)
   (chem-dispatch-option-submol-name)
   (chem-dispatch-option-entry-atom)
   (chem-dispatch-option-exit-atom)
   (chem-dispatch-toggle-legacy-lewis)]
  ["SVG canvas"
   :if (lambda () (chem-dispatch--svg-exporter-p))
   (chem-dispatch-option-svg-width)
   (chem-dispatch-option-svg-height)
   (chem-dispatch-option-svg-padding)
   (chem-dispatch-option-svg-scale)]
  ["SVG bonds"
   :if (lambda () (chem-dispatch--svg-exporter-p))
   (chem-dispatch-option-svg-bond-spacing)
   (chem-dispatch-option-svg-bond-thickness)
   (chem-dispatch-option-svg-short-bond-length)
   (chem-dispatch-option-svg-aromatic)]
  ["SVG atoms"
   :if (lambda () (chem-dispatch--svg-exporter-p))
   (chem-dispatch-option-svg-font-family)
   (chem-dispatch-option-svg-font-size)]
  ["SVG colors"
   :if (lambda () (chem-dispatch--svg-exporter-p))
   (chem-dispatch-option-svg-theme)]
  ["SVG stereo"
   :if (lambda () (chem-dispatch--svg-exporter-p))
   (chem-dispatch-toggle-svg-show-stereo)
   (chem-dispatch-toggle-svg-show-cip-labels)
   (chem-dispatch-toggle-svg-show-enhanced-stereo)]
  ["Reaction"
   (chem-dispatch-option-reaction-arrow)
   (chem-dispatch-option-reaction-above)
   (chem-dispatch-option-reaction-below)
   (chem-dispatch-option-reaction-spacing)
   (chem-dispatch-option-reaction-arrow-length)
   (chem-dispatch-option-reaction-plus-size)
   (chem-dispatch-toggle-reaction-wrap)
   (chem-dispatch-option-reaction-arrow-chemfig)
   (chem-dispatch-reaction-generate)
   (chem-dispatch-reaction-copy)]
  ["Actions"
   (chem-dispatch-generate)
   (chem-dispatch-copy)
   (chem-dispatch-file)
   (chem-dispatch-rotate)
   (chem-dispatch-insert-image)
   (chem-dispatch-bindings)]
  (interactive)
  (unless (bound-and-true-p chem-mode)
    (chem-mode 1))
  (setq chem-dispatch--edit-buffer (current-buffer))
  (setq chem-dispatch--pending-source (chem-mode--source-at-point))
  (setq chem-dispatch--pending-overrides nil)
  (transient-setup 'chem-dispatch-menu))

(provide 'chem-dispatch)
;;; chem-dispatch.el ends here
