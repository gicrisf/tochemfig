;;; chem-exp-svg.el --- SVG exporter for chem-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Giovanni Crisalfi

;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Pure-Elisp SVG exporter for prepared `chem-drawing' values.  The exporter
;; consumes the shared IR and keeps all SVG presentation local: viewBox,
;; colors, fonts, IDs, and XML.  This is the Phase 1 skeleton: options,
;; themes, primitives, and a valid empty document.  Bond and atom drawing are
;; added in later phases.

;;; Code:

(require 'cl-lib)
(require 'chem-ir)
(require 'chem-export)
(require 'chem-options)

;;; Options

(defcustom chem-exp-svg-default-width nil
  "Width of the generated SVG in pixels, or nil for automatic."
  :group 'chem-mode
  :type '(choice (const :tag "Automatic" nil) number))

(defcustom chem-exp-svg-default-height nil
  "Height of the generated SVG in pixels, or nil for automatic."
  :group 'chem-mode
  :type '(choice (const :tag "Automatic" nil) number))

(defcustom chem-exp-svg-default-padding 10.0
  "Padding around the drawing in SVG user units."
  :group 'chem-mode
  :type 'number)

(defcustom chem-exp-svg-default-scale 30.0
  "Scale factor from IR coordinates to SVG user units."
  :group 'chem-mode
  :type 'number)

(defcustom chem-exp-svg-default-bond-spacing 5.1
  "Spacing between parallel bond strokes in SVG user units."
  :group 'chem-mode
  :type 'number)

(defcustom chem-exp-svg-default-bond-thickness 1.0
  "Stroke width for bonds in SVG user units."
  :group 'chem-mode
  :type 'number)

(defcustom chem-exp-svg-default-short-bond-length 0.8
  "Fraction of a bond length used for shortened ring double bonds."
  :group 'chem-mode
  :type 'number)

(defcustom chem-exp-svg-default-font-family "Arial, Helvetica, sans-serif"
  "Font family used for atom labels."
  :group 'chem-mode
  :type 'string)

(defcustom chem-exp-svg-default-font-size 11.0
  "Font size for atom labels in SVG user units."
  :group 'chem-mode
  :type 'number)

(defcustom chem-exp-svg-default-theme 'light
  "Color theme used by the SVG exporter."
  :group 'chem-mode
  :type 'symbol)

(defcustom chem-exp-svg-default-aromatic 'auto
  "Aromatic bond rendering policy.
`auto' uses circles for eligible rings, `circle' always prefers circles,
and `alternating' draws dashed inner lines."
  :group 'chem-mode
  :type '(choice (const :tag "Automatic" auto)
                 (const :tag "Aromatic circles" circle)
                 (const :tag "Alternating bonds" alternating)))

(defcustom chem-exp-svg-default-show-stereo t
  "Draw wedge and hash bonds when non-nil."
  :group 'chem-mode
  :type 'boolean)

(defcustom chem-exp-svg-default-show-cip-labels nil
  "Draw CIP stereocenter labels when non-nil."
  :group 'chem-mode
  :type 'boolean)

(defcustom chem-exp-svg-default-show-enhanced-stereo nil
  "Draw enhanced-stereo group labels when non-nil."
  :group 'chem-mode
  :type 'boolean)

;;; Themes

(defcustom chem-exp-svg-themes
  '((light . (:foreground "#222222"
             :background "#ffffff"
             :default    "#222222"
             :C "#222222" :N "#3498db" :O "#e74c3c"
             :S "#f1c40f" :P "#d35400"
             :F "#27ae60" :CL "#16a085" :BR "#d35400" :I "#8e44ad"
             :B "#e67e22" :SI "#e67e22" :H "#666666"))
    (dark . (:foreground "#ffffff"
            :background "#141414"
            :default    "#ffffff"
            :C "#ffffff" :N "#3498db" :O "#e74c3c"
            :S "#f1c40f" :P "#d35400"
            :F "#27ae60" :CL "#16a085" :BR "#d35400" :I "#8e44ad"
            :B "#e67e22" :SI "#e67e22" :H "#aaaaaa")))
  "Centralized SVG color themes."
  :group 'chem-mode
  :type '(alist :key-type symbol :value-type sexp))

;;; Option metadata

(defconst chem-exp-svg-option-specs
  '((:key :svg-width
     :variable chem-exp-svg-default-width
     :label "Width"
     :type optional-number
     :group canvas)
    (:key :svg-height
     :variable chem-exp-svg-default-height
     :label "Height"
     :type optional-number
     :group canvas)
    (:key :svg-padding
     :variable chem-exp-svg-default-padding
     :label "Padding"
     :type number
     :group canvas)
    (:key :svg-scale
     :variable chem-exp-svg-default-scale
     :label "Scale"
     :type number
     :group canvas)
    (:key :svg-bond-spacing
     :variable chem-exp-svg-default-bond-spacing
     :label "Bond spacing"
     :type number
     :group bonds)
    (:key :svg-bond-thickness
     :variable chem-exp-svg-default-bond-thickness
     :label "Bond thickness"
     :type number
     :group bonds)
    (:key :svg-short-bond-length
     :variable chem-exp-svg-default-short-bond-length
     :label "Short bond length"
     :type number
     :group bonds)
    (:key :svg-font-family
     :variable chem-exp-svg-default-font-family
     :label "Font family"
     :type string
     :group atoms)
    (:key :svg-font-size
     :variable chem-exp-svg-default-font-size
     :label "Font size"
     :type number
     :group atoms)
    (:key :svg-theme
     :variable chem-exp-svg-default-theme
     :label "Theme"
     :type symbol
     :group colors)
    (:key :svg-aromatic
     :variable chem-exp-svg-default-aromatic
     :label "Aromatic display"
     :type choice
     :choices (auto circle alternating)
     :group bonds)
    (:key :svg-show-stereo
     :variable chem-exp-svg-default-show-stereo
     :label "Show stereochemistry"
     :type boolean
     :group stereo)
    (:key :svg-show-cip-labels
     :variable chem-exp-svg-default-show-cip-labels
     :label "Show CIP labels"
     :type boolean
     :group stereo)
    (:key :svg-show-enhanced-stereo
     :variable chem-exp-svg-default-show-enhanced-stereo
     :label "Show enhanced stereo"
     :type boolean
     :group stereo))
  "SVG exporter option metadata.")

;;; Renderer state

(cl-defstruct (chem-exp-svg-state (:constructor chem-exp-svg-state-create))
  drawing
  options
  theme
  bounds)

;;; Option and theme helpers

(defun chem-exp-svg--option-spec (key)
  "Return the SVG option spec for KEY."
  (or (cl-find key chem-exp-svg-option-specs
               :key (lambda (spec) (plist-get spec :key))
               :test #'eq)
      (error "Unknown SVG option: %s" key)))

(defun chem-exp-svg--option (state key)
  "Return STATE's value for SVG option KEY."
  (let ((options (chem-exp-svg-state-options state)))
    (if (plist-member options key)
        (plist-get options key)
      (symbol-value (plist-get (chem-exp-svg--option-spec key) :variable)))))

(defun chem-exp-svg--theme-name (value)
  "Return VALUE as a theme name symbol."
  (if (stringp value) (intern value) value))

(defun chem-exp-svg--theme (state)
  "Return STATE's resolved theme plist."
  (let* ((name (chem-exp-svg--theme-name
                (chem-exp-svg--option state :svg-theme)))
         (theme (alist-get name chem-exp-svg-themes)))
    (unless theme
      (user-error "Unknown SVG theme: %s" name))
    theme))

(defun chem-exp-svg--element-key (element)
  "Return the theme keyword for ELEMENT."
  (if (keywordp element)
      element
    (intern (concat ":" (upcase element)))))

(defun chem-exp-svg--theme-color (theme element)
  "Return THEME's color for ELEMENT with deterministic fallbacks."
  (or (plist-get theme (chem-exp-svg--element-key element))
      (plist-get theme :default)
      (plist-get theme :foreground)
      (plist-get theme :C)))

(defun chem-exp-svg--theme-foreground (theme)
  "Return THEME's foreground color."
  (or (plist-get theme :foreground)
      (plist-get theme :C)))

(defun chem-exp-svg--theme-background (theme)
  "Return THEME's background color, or nil for a transparent background."
  (plist-get theme :background))

;;; Primitive helpers

(defun chem-exp-svg--escape (string)
  "Escape STRING for XML text and attribute values."
  (replace-regexp-in-string
   "[&<>\"']"
   (lambda (match)
     (pcase match
       ("&" "&amp;")
       ("<" "&lt;")
       (">" "&gt;")
       ("\"" "&quot;")
       ("'" "&apos;")))
   (or string "")))

(defun chem-exp-svg--number (value)
  "Format VALUE for SVG output without trailing zeros."
  (let ((formatted (format "%.3f" (float value))))
    (setq formatted (replace-regexp-in-string "\\.?0+\\'" "" formatted))
    (if (equal formatted "-0") "0" formatted)))

(defun chem-exp-svg--id (name)
  "Return a deterministic SVG ID for NAME."
  (format "chem-exp-svg-%s"
          (if (symbolp name) (symbol-name name) name)))

(defun chem-exp-svg--atom-bounds (drawing)
  "Return (MIN-X MIN-Y MAX-X MAX-Y) for DRAWING's atoms."
  (let (min-x min-y max-x max-y)
    (maphash
     (lambda (_ atom)
       (let ((x (chem-atom-x atom))
             (y (chem-atom-y atom)))
         (setq min-x (if min-x (min min-x x) x))
         (setq min-y (if min-y (min min-y y) y))
         (setq max-x (if max-x (max max-x x) x))
         (setq max-y (if max-y (max max-y y) y))))
     (chem-drawing-atoms drawing))
    (if min-x
        (list min-x min-y max-x max-y)
      (list 0.0 0.0 0.0 0.0))))

(defun chem-exp-svg--bounds (state)
  "Return STATE's (MIN-X MIN-Y MAX-X MAX-Y) bounds."
  (chem-exp-svg-state-bounds state))

(defun chem-exp-svg--point (state x y)
  "Map IR coordinates X and Y to SVG user coordinates in STATE."
  (let* ((bounds (chem-exp-svg--bounds state))
         (min-x (nth 0 bounds))
         (max-y (nth 3 bounds))
         (scale (chem-exp-svg--option state :svg-scale))
         (padding (chem-exp-svg--option state :svg-padding)))
    (cons (+ (* (- x min-x) scale) padding)
          (+ (* (- max-y y) scale) padding))))

(defun chem-exp-svg--viewbox (state)
  "Return STATE's viewBox as (MIN-X MIN-Y WIDTH HEIGHT)."
  (let* ((bounds (chem-exp-svg--bounds state))
         (scale (chem-exp-svg--option state :svg-scale))
         (padding (chem-exp-svg--option state :svg-padding))
         (width (+ (* (- (nth 2 bounds) (nth 0 bounds)) scale)
                   (* 2 padding)))
         (height (+ (* (- (nth 3 bounds) (nth 1 bounds)) scale)
                    (* 2 padding))))
    (list 0.0 0.0 (max width 1.0) (max height 1.0))))

(defun chem-exp-svg--size-attributes (state)
  "Return the width and height attributes for STATE."
  (let* ((viewbox (chem-exp-svg--viewbox state))
         (viewbox-width (nth 2 viewbox))
         (viewbox-height (nth 3 viewbox))
         (width (chem-exp-svg--option state :svg-width))
         (height (chem-exp-svg--option state :svg-height)))
    (cond
     ((and width height)
      (format " width=\"%s\" height=\"%s\" preserveAspectRatio=\"xMidYMid meet\""
              (chem-exp-svg--number width)
              (chem-exp-svg--number height)))
     (width
      (format " width=\"%s\" height=\"%s\" preserveAspectRatio=\"xMidYMid meet\""
              (chem-exp-svg--number width)
              (chem-exp-svg--number (* width (/ viewbox-height viewbox-width)))))
     (height
      (format " width=\"%s\" height=\"%s\" preserveAspectRatio=\"xMidYMid meet\""
              (chem-exp-svg--number (* height (/ viewbox-width viewbox-height)))
              (chem-exp-svg--number height)))
     (t ""))))

(defun chem-exp-svg--document (state body)
  "Return an SVG document for STATE with BODY."
  (let ((viewbox (chem-exp-svg--viewbox state)))
    (concat
     "<svg xmlns=\"http://www.w3.org/2000/svg\""
     (format " viewBox=\"%s %s %s %s\""
             (chem-exp-svg--number (nth 0 viewbox))
             (chem-exp-svg--number (nth 1 viewbox))
             (chem-exp-svg--number (nth 2 viewbox))
             (chem-exp-svg--number (nth 3 viewbox)))
     (chem-exp-svg--size-attributes state)
     ">\n"
     body
     "</svg>\n")))

;;; Renderer

(defun chem-exp-svg--background (_state)
  "Return the background SVG."
  "")

(defun chem-exp-svg--bonds (_state)
  "Return the bond SVG."
  "")

(defun chem-exp-svg--rings (_state)
  "Return the ring SVG."
  "")

(defun chem-exp-svg--atoms (_state)
  "Return the atom label SVG."
  "")

(defun chem-exp-svg--stereo-labels (_state)
  "Return the stereochemistry label SVG."
  "")

(defun chem-exp-svg--make-state (drawing options)
  "Create an SVG renderer state for DRAWING and OPTIONS."
  (let ((state (chem-exp-svg-state-create
                :drawing drawing
                :options options
                :bounds (chem-exp-svg--atom-bounds drawing))))
    (setf (chem-exp-svg-state-theme state)
          (chem-exp-svg--theme state))
    state))

(defun chem-exp-svg-render (drawing options)
  "Render DRAWING to an SVG string using OPTIONS."
  (let* ((state (chem-exp-svg--make-state drawing options))
         (body (concat (chem-exp-svg--background state)
                       (chem-exp-svg--bonds state)
                       (chem-exp-svg--rings state)
                       (chem-exp-svg--atoms state)
                       (chem-exp-svg--stereo-labels state))))
    (chem-exp-svg--document state body)))

(chem-register-exporter 'svg #'chem-exp-svg-render
                        (list :label "SVG"
                              :options chem-exp-svg-option-specs
                              :target 'image))

(provide 'chem-exp-svg)
;;; chem-exp-svg.el ends here
