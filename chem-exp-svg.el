;;; chem-exp-svg.el --- SVG exporter for chem-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Giovanni Crisalfi

;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Pure-Elisp SVG exporter for prepared `chem-drawing' values.  It renders
;; atoms, bonds, rings, aromatic displays, stereochemistry, labels, themes, and
;; deterministic SVG documents from the shared IR.  All SVG presentation stays
;; local to this exporter: viewBox, colors, fonts, IDs, masks, and XML.

;;; Code:

(require 'cl-lib)
(require 'chem-ir)
(require 'chem-geometry)
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
  bounds
  hidden
  pseudo)

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

;;; Atom labels

(defconst chem-exp-svg--subscript-digits "₀₁₂₃₄₅₆₇₈₉")
(defconst chem-exp-svg--superscript-digits "⁰¹²³⁴⁵⁶⁷⁸⁹")

(defun chem-exp-svg--digit-string (digits text)
  "Translate each digit in TEXT using DIGITS."
  (mapconcat (lambda (char)
               (let ((index (- char ?0)))
                 (if (and (>= index 0) (< index (length digits)))
                     (substring digits index (1+ index))
                   (char-to-string char))))
             text ""))

(defun chem-exp-svg--subscript (number)
  "Return NUMBER as Unicode subscript digits."
  (chem-exp-svg--digit-string chem-exp-svg--subscript-digits
                              (number-to-string number)))

(defun chem-exp-svg--superscript (number)
  "Return NUMBER as Unicode superscript digits."
  (chem-exp-svg--digit-string chem-exp-svg--superscript-digits
                              (number-to-string number)))

(defun chem-exp-svg--charge-text (charge)
  "Return CHARGE as Unicode superscript text."
  (if (zerop charge)
      ""
    (let ((sign (if (> charge 0) "⁺" "⁻"))
          (magnitude (abs charge)))
      (if (= magnitude 1)
          sign
        (concat (chem-exp-svg--superscript magnitude) sign)))))

(defun chem-exp-svg--radical-text (radical)
  "Return RADICAL as superscript dot text."
  (if (or (null radical) (zerop radical))
      ""
    (make-string radical ?·)))

(defun chem-exp-svg--hydrogen-text (hydrogens)
  "Return HYDROGENS as label text."
  (cond
   ((or (null hydrogens) (zerop hydrogens)) "")
   ((= hydrogens 1) "H")
   (t (concat "H" (chem-exp-svg--subscript hydrogens)))))

(defun chem-exp-svg--atom-text (atom &optional direction)
  "Return ATOM's display text, or nil when it has no label.
For left-directed text, put hydrogens before the element so the label
reads outward from the atom, matching SmilesDrawer."
  (let ((label (chem-atom-label atom))
        (number (chem-atom-number atom))
        (direction (or direction (chem-exp-svg--text-direction atom))))
    (cond
     (number
      (if label
          (concat label (number-to-string number))
        (number-to-string number)))
     (label
      (let ((hydrogens (chem-exp-svg--hydrogen-text
                        (chem-atom-hydrogens atom)))
            (charge (chem-exp-svg--charge-text
                     (or (chem-atom-charge atom) 0)))
            (radical (chem-exp-svg--radical-text
                      (chem-atom-radical atom))))
        (if (eq direction 'left)
            (concat hydrogens label charge radical)
          (concat label hydrogens charge radical))))
     (t nil))))

(defun chem-exp-svg--atom-text-color (state atom)
  "Return STATE's text color for ATOM."
  (if (chem-atom-label atom)
      (chem-exp-svg--theme-color (chem-exp-svg-state-theme state)
                                 (chem-atom-element atom))
    (chem-exp-svg--theme-foreground (chem-exp-svg-state-theme state))))

(defun chem-exp-svg--hidden-atom-p (state idx)
  "Return non-nil when atom IDX is hidden by pseudo-element grouping."
  (and (chem-exp-svg-state-hidden state)
       (gethash idx (chem-exp-svg-state-hidden state))))

(defun chem-exp-svg--edge-visible-p (state edge)
  "Return non-nil when EDGE has no hidden endpoint in STATE."
  (not (or (chem-exp-svg--hidden-atom-p state (chem-edge-a edge))
           (chem-exp-svg--hidden-atom-p state (chem-edge-b edge)))))

(defun chem-exp-svg--attach-pseudo (table center element hydrogens charge)
  "Attach a pseudo ELEMENT to CENTER in TABLE."
  (let* ((key (format "%s%s%s" hydrogens element charge))
         (groups (gethash center table))
         (existing (cl-find key groups
                            :key (lambda (group) (plist-get group :key))
                            :test #'equal)))
    (if existing
        (plist-put existing :count (1+ (plist-get existing :count)))
      (puthash center
               (cons (list :key key
                           :element element
                           :count 1
                           :hydrogens hydrogens
                           :charge charge)
                     groups)
               table))))

(defun chem-exp-svg--pseudo-elements (drawing)
  "Return (HIDDEN . PSEUDO) for DRAWING.
HIDDEN is a hash of atom indices hidden behind pseudo-element labels.
PSEUDO maps a center atom index to a sorted list of pseudo groups."
  (let ((hidden (make-hash-table :test 'eql))
        (pseudo (make-hash-table :test 'eql))
        (atoms (chem-drawing-atoms drawing)))
    (maphash
     (lambda (idx atom)
       (let* ((neighbors (mapcar (lambda (neighbor)
                                   (gethash neighbor atoms))
                                 (chem-atom-neighbors atom)))
              (element (chem-atom-element atom))
              (count (length neighbors)))
         (when (and (>= count 3)
                    (null (chem-rings-for-atom drawing idx))
                    (not (equal element "P"))
                    (not (and (equal element "C")
                              (= count 3)
                              (cl-every (lambda (neighbor)
                                          (equal "N" (chem-atom-element neighbor)))
                                        neighbors))))
           (let* ((terminal
                   (cl-remove-if-not
                    (lambda (neighbor)
                      (and (not (member (chem-atom-element neighbor) '("C" "H")))
                           (= 1 (length (chem-atom-neighbors neighbor)))))
                    neighbors))
                  (ctn (cl-count-if
                        (lambda (neighbor)
                          (> (length (chem-atom-neighbors neighbor)) 1))
                        neighbors)))
             (when (and (>= (length terminal) 2) (<= ctn 1))
               (dolist (neighbor terminal)
                 (puthash (chem-atom-idx neighbor) t hidden)
                 (chem-exp-svg--attach-pseudo
                  pseudo idx
                  (chem-atom-element neighbor)
                  (or (chem-atom-hydrogens neighbor) 0)
                  (or (chem-atom-charge neighbor) 0))))))))
     atoms)
    (maphash
     (lambda (idx groups)
       (puthash idx
                (sort groups
                      (lambda (a b)
                        (string< (plist-get a :key) (plist-get b :key))))
                pseudo))
     pseudo)
    (cons hidden pseudo)))

(defun chem-exp-svg--pseudo-group-text (group)
  "Return GROUP's display text without hydrogens."
  (let ((text (plist-get group :element))
        (count (plist-get group :count))
        (charge (plist-get group :charge)))
    (when (> count 1)
      (setq text (concat text (chem-exp-svg--subscript count))))
    (when (/= charge 0)
      (setq text (concat text (chem-exp-svg--charge-text charge))))
    text))

(defun chem-exp-svg--pseudo-groups-for-atom (atom groups)
  "Return GROUPS normalized for ATOM's display.
This implements SmilesDrawer's nitro rendering: N+ with neutral and
negative terminal oxygens is drawn as N+O2."
  (if (and (equal (chem-atom-element atom) "N")
           (= 1 (or (chem-atom-charge atom) 0)))
      (let ((oxygens (cl-remove-if-not
                      (lambda (group)
                        (equal (plist-get group :element) "O"))
                      groups)))
        (if (and (= 2 (length oxygens))
                 (cl-some (lambda (group)
                            (= -1 (plist-get group :charge)))
                          oxygens)
                 (cl-some (lambda (group)
                            (= 0 (plist-get group :charge)))
                          oxygens)
                 (cl-every (lambda (group)
                             (zerop (plist-get group :hydrogens)))
                           oxygens))
            (list (list :key "nitro"
                        :element "O"
                        :count 2
                        :hydrogens 0
                        :charge 0))
          groups))
    groups))

(defun chem-exp-svg--pseudo-atom-text (atom groups direction)
  "Return ATOM's text with pseudo GROUPS in DIRECTION."
  (let* ((groups (chem-exp-svg--pseudo-groups-for-atom atom groups))
         (label (or (chem-atom-label atom)
                    (chem-atom-element atom)))
         (center (concat label
                         (chem-exp-svg--charge-text
                          (or (chem-atom-charge atom) 0))))
         (parts (list center))
         (hydrogens (chem-exp-svg--hydrogen-text
                     (chem-atom-hydrogens atom))))
    (unless (string-empty-p hydrogens)
      (push hydrogens parts))
    (dolist (group groups)
      (push (chem-exp-svg--pseudo-group-text group) parts)
      (let ((hcount (* (plist-get group :count)
                       (plist-get group :hydrogens))))
        (unless (zerop hcount)
          (push (chem-exp-svg--hydrogen-text hcount) parts))))
    (if (eq direction 'left)
        (mapconcat #'identity parts "")
      (mapconcat #'identity (nreverse parts) ""))))

(defun chem-exp-svg--atom-text-for-state (state atom)
  "Return ATOM's displayed text in STATE, including pseudo groups."
  (let* ((idx (chem-atom-idx atom))
         (groups (and (chem-exp-svg-state-pseudo state)
                      (gethash idx (chem-exp-svg-state-pseudo state))))
         (direction (chem-exp-svg--text-direction-for-state state atom)))
    (if groups
        (chem-exp-svg--pseudo-atom-text atom groups direction)
      (chem-exp-svg--atom-text atom direction))))

(defun chem-exp-svg--direction-from-svg-angle (angle)
  "Map an SVG text ANGLE in degrees to right, down, left, or up."
  (let ((angle (mod angle 360.0)))
    (cond
     ((or (< angle 45.0) (>= angle 315.0)) 'right)
     ((< angle 135.0) 'down)
     ((< angle 225.0) 'left)
     (t 'up))))

(defun chem-exp-svg--text-direction-from-angles (angles &optional only-horizontal)
  "Return a text direction from IR bond ANGLES.
ONLY-HORIZONTAL forces right/left rounding for pseudo-element centers."
  (let ((angles (mapcar (lambda (angle)
                          (mod (- 180.0 angle) 360.0))
                        angles)))
    (cond
     ((null angles) 'right)
     ((or only-horizontal (null (cdr angles)))
      (let* ((angle (if only-horizontal
                        (car angles)
                      (car angles)))
             (angle (if (< (abs (- angle 90.0)) 0.5)
                        (- angle 11.4591559026)
                      angle))
             (rounded (* 180.0 (round (/ angle 180.0)))))
        (if (= 0 (mod (round rounded) 360)) 'right 'left)))
     (t
      (let* ((radians (mapcar (lambda (angle)
                                (* angle (/ float-pi 180.0)))
                              angles))
             (x (apply #'+ (mapcar #'cos radians)))
             (y (apply #'+ (mapcar #'sin radians)))
             (mean (if (< (sqrt (+ (* x x) (* y y))) 1e-9)
                       0.0
                     (* (atan y x) (/ 180.0 float-pi))))
             (rounded (* 90.0 (round (/ mean 90.0)))))
        (chem-exp-svg--direction-from-svg-angle rounded))))))

(defun chem-exp-svg--text-direction (atom)
  "Return ATOM's preferred text direction from its bond angles."
  (chem-exp-svg--text-direction-from-angles
   (chem-atom-bond-angles atom)))

(defun chem-exp-svg--text-direction-for-state (state atom)
  "Return ATOM's text direction using only visible neighbours."
  (let* ((drawing (chem-exp-svg-state-drawing state))
         (atoms (chem-drawing-atoms drawing))
         (visible-neighbors
          (cl-remove-if
           (lambda (idx) (chem-exp-svg--hidden-atom-p state idx))
           (chem-atom-neighbors atom)))
         (angles
          (mapcar
           (lambda (neighbor)
             (let ((other (gethash neighbor atoms)))
               (mod (* (atan (- (chem-atom-y other) (chem-atom-y atom))
                             (- (chem-atom-x other) (chem-atom-x atom)))
                       (/ 180.0 float-pi))
                    360.0)))
           visible-neighbors))
         (pseudo (gethash (chem-atom-idx atom)
                          (chem-exp-svg-state-pseudo state))))
    (chem-exp-svg--text-direction-from-angles angles (and pseudo t))))

(defun chem-exp-svg--text-position (state atom _direction &optional _text)
  "Return (X Y ANCHOR) for ATOM's text, centered on the atom.
The anchor is always `middle'; SmilesDrawer centers each label over its
atom and uses the mask to keep bonds clear."
  (let ((point (chem-exp-svg--point state
                                    (chem-atom-x atom)
                                    (chem-atom-y atom))))
    (list (car point) (cdr point) "middle")))

(defun chem-exp-svg--label-radius (state text)
  "Return an approximate mask radius for TEXT."
  (let* ((font-size (chem-exp-svg--option state :svg-font-size))
         (length (length (or text "")))
         (base (* 0.75 font-size))
         (half-width (* 0.3 font-size length)))
    (+ (max base half-width) (* 0.1 font-size))))

(defun chem-exp-svg--label-center (state atom text)
  "Return the approximate center of ATOM's TEXT in STATE."
  (let* ((direction (chem-exp-svg--text-direction atom))
         (position (chem-exp-svg--text-position state atom direction text))
         (x (nth 0 position))
         (y (nth 1 position))
         (anchor (nth 2 position))
         (font-size (chem-exp-svg--option state :svg-font-size))
         (half-width (* 0.3 font-size (length text))))
    (list (pcase anchor
            ("start" (+ x half-width))
            ("end" (- x half-width))
            (_ x))
          y)))

(defun chem-exp-svg--marker-present-p (marker)
  "Return non-nil when MARKER should be emitted."
  (and marker (not (string-empty-p marker))))

(defun chem-exp-svg--sanitize-id (string)
  "Return STRING as a deterministic SVG ID."
  (let* ((raw (if (stringp string) string (format "%s" string)))
         (sanitized (replace-regexp-in-string "[^A-Za-z0-9_.-]" "-" raw)))
    (if (string-match-p "\\`[A-Za-z_]" sanitized)
        sanitized
      (concat "marker-" sanitized))))

(defun chem-exp-svg--marker-attributes (marker)
  "Return SVG attributes for MARKER, or nil."
  (when (chem-exp-svg--marker-present-p marker)
    (let ((id (chem-exp-svg--sanitize-id marker)))
      (concat
       (format " id=\"%s\"" (chem-exp-svg--escape id))
       (unless (equal id marker)
         (format " data-marker=\"%s\"" (chem-exp-svg--escape marker)))))))

(defun chem-exp-svg--sorted-atoms (drawing)
  "Return DRAWING's atoms sorted by index."
  (let (atoms)
    (maphash (lambda (_ atom) (push atom atoms))
             (chem-drawing-atoms drawing))
    (sort atoms (lambda (a b) (< (chem-atom-idx a) (chem-atom-idx b))))))

(defun chem-exp-svg--atom-svg (state atom)
  "Return ATOM's label SVG for STATE."
  (if (chem-exp-svg--hidden-atom-p state (chem-atom-idx atom))
      ""
    (let* ((text (chem-exp-svg--atom-text-for-state state atom))
           (attributes (chem-exp-svg--marker-attributes
                        (chem-atom-marker atom))))
      (cond
       (text
        (let* ((direction (chem-exp-svg--text-direction-for-state state atom))
               (position (chem-exp-svg--text-position state atom direction text))
             (text-svg
              (format "<text x=\"%s\" y=\"%s\" fill=\"%s\" font-family=\"%s\" font-size=\"%s\" text-anchor=\"%s\" dominant-baseline=\"central\">%s</text>\n"
                      (chem-exp-svg--number (nth 0 position))
                      (chem-exp-svg--number (nth 1 position))
                      (chem-exp-svg--escape
                       (chem-exp-svg--atom-text-color state atom))
                      (chem-exp-svg--escape
                       (chem-exp-svg--option state :svg-font-family))
                      (chem-exp-svg--number
                       (chem-exp-svg--option state :svg-font-size))
                      (nth 2 position)
                      (chem-exp-svg--escape text))))
        (if attributes
            (format "<g%s>%s</g>\n" attributes text-svg)
          text-svg)))
       (attributes
        (let ((point (chem-exp-svg--point state
                                          (chem-atom-x atom)
                                          (chem-atom-y atom))))
          (format "<g%s transform=\"translate(%s %s)\"></g>\n"
                  attributes
                  (chem-exp-svg--number (car point))
                  (chem-exp-svg--number (cdr point)))))
       (t "")))))

(defun chem-exp-svg--atoms (state)
  "Return the atom label SVG for STATE."
  (mapconcat
   (lambda (atom)
     (unless (chem-exp-svg--hidden-atom-p state (chem-atom-idx atom))
       (chem-exp-svg--atom-svg state atom)))
   (chem-exp-svg--sorted-atoms (chem-exp-svg-state-drawing state))
   ""))

(defun chem-exp-svg--label-mask-circle (state atom)
  "Return a label mask circle for ATOM in STATE."
  (unless (chem-exp-svg--hidden-atom-p state (chem-atom-idx atom))
    (let ((text (chem-exp-svg--atom-text-for-state state atom)))
      (when text
        (let ((center (chem-exp-svg--label-center state atom text))
              (radius (chem-exp-svg--label-radius state text)))
          (format "<circle cx=\"%s\" cy=\"%s\" r=\"%s\" fill=\"black\"/>\n"
                  (chem-exp-svg--number (car center))
                  (chem-exp-svg--number (cadr center))
                  (chem-exp-svg--number radius)))))))

(defun chem-exp-svg--mask-id ()
  "Return the deterministic label mask ID."
  (chem-exp-svg--id 'label-mask))

(defun chem-exp-svg--defs (state)
  "Return the SVG defs block with STATE's label mask."
  (let* ((viewbox (chem-exp-svg--viewbox state))
         (width (chem-exp-svg--number (nth 2 viewbox)))
         (height (chem-exp-svg--number (nth 3 viewbox)))
         (circles
          (mapconcat (lambda (atom)
                       (or (chem-exp-svg--label-mask-circle state atom) ""))
                     (chem-exp-svg--sorted-atoms
                      (chem-exp-svg-state-drawing state))
                     "")))
    (format "<defs>\n<mask id=\"%s\" maskUnits=\"userSpaceOnUse\" x=\"0\" y=\"0\" width=\"%s\" height=\"%s\">\n<rect x=\"0\" y=\"0\" width=\"%s\" height=\"%s\" fill=\"white\"/>\n%s</mask>\n</defs>\n"
            (chem-exp-svg--escape (chem-exp-svg--mask-id))
            width height width height circles)))

;;; Bonds

(defun chem-exp-svg--bond-color (state)
  "Return STATE's bond color."
  (chem-exp-svg--theme-foreground (chem-exp-svg-state-theme state)))

(defun chem-exp-svg--line (state x1 y1 x2 y2 &optional dash)
  "Return a line element for STATE, optionally with DASH."
  (format "<line x1=\"%s\" y1=\"%s\" x2=\"%s\" y2=\"%s\" stroke=\"%s\" stroke-width=\"%s\" stroke-linecap=\"round\"%s/>\n"
          (chem-exp-svg--number x1)
          (chem-exp-svg--number y1)
          (chem-exp-svg--number x2)
          (chem-exp-svg--number y2)
          (chem-exp-svg--escape (chem-exp-svg--bond-color state))
          (chem-exp-svg--number
           (chem-exp-svg--option state :svg-bond-thickness))
          (if dash
              (format " stroke-dasharray=\"%s\"" dash)
            "")))

(defun chem-exp-svg--parallel-lines (state point-a point-b offsets)
  "Return parallel lines from POINT-A to POINT-B at OFFSETS."
  (let* ((x1 (car point-a))
         (y1 (cdr point-a))
         (x2 (car point-b))
         (y2 (cdr point-b))
         (dx (- x2 x1))
         (dy (- y2 y1))
         (length (sqrt (+ (* dx dx) (* dy dy)))))
    (if (< length 1e-9)
        (chem-exp-svg--line state x1 y1 x2 y2)
      (let ((normal-x (/ (- dy) length))
            (normal-y (/ dx length)))
        (mapconcat
         (lambda (offset)
           (chem-exp-svg--line state
                               (+ x1 (* normal-x offset))
                               (+ y1 (* normal-y offset))
                               (+ x2 (* normal-x offset))
                               (+ y2 (* normal-y offset))))
         offsets "")))))

(defun chem-exp-svg--edge-normals (point-a point-b)
  "Return the two unit normals for the SVG segment POINT-A to POINT-B."
  (let* ((dx (- (car point-b) (car point-a)))
         (dy (- (cdr point-b) (cdr point-a)))
         (length (sqrt (+ (* dx dx) (* dy dy)))))
    (when (> length 1e-9)
      (let ((nx (/ (- dy) length))
            (ny (/ dx length)))
        (list (cons nx ny) (cons (- nx) (- ny)))))))

(defun chem-exp-svg--which-side (point a b)
  "Return the side of POINT relative to the line from A to B."
  (- (* (- (car point) (car a)) (- (cdr b) (cdr a)))
     (* (- (cdr point) (cdr a)) (- (car b) (car a)))))

(defun chem-exp-svg--allene-edge-p (drawing edge)
  "Return non-nil when EDGE participates in an allene center."
  (let ((atoms (chem-drawing-atoms drawing)))
    (cl-some
     (lambda (idx)
       (let* ((atom (gethash idx atoms))
              (neighbors (chem-atom-neighbors atom)))
         (and (= 2 (length neighbors))
              (cl-every
               (lambda (neighbor)
                 (eq :double
                     (chem-edge-order
                      (chem-get-edge drawing idx neighbor))))
               neighbors))))
     (list (chem-edge-a edge) (chem-edge-b edge)))))

(defun chem-exp-svg--same-side-p (a b)
  "Return non-nil when A and B have the same sign, including both zero."
  (or (and (< a 0.0) (< b 0.0))
      (and (= a 0.0) (= b 0.0))
      (and (> a 0.0) (> b 0.0))))

(defun chem-exp-svg--double-side (state edge)
  "Return the side (0 or 1) for EDGE's second stroke, or nil.
Nil means draw two full parallel lines, matching SmilesDrawer's terminal
and center-double-bond cases."
  (let* ((drawing (chem-exp-svg-state-drawing state))
         (atoms (chem-drawing-atoms drawing))
         (atom-a (gethash (chem-edge-a edge) atoms))
         (atom-b (gethash (chem-edge-b edge) atoms))
         (neighbors-a (remove (chem-edge-b edge)
                              (chem-atom-neighbors atom-a)))
         (neighbors-b (remove (chem-edge-a edge)
                              (chem-atom-neighbors atom-b)))
         (an-count (length neighbors-a))
         (bn-count (length neighbors-b)))
    (unless (or (and (zerop an-count) (zerop bn-count))
                (and (zerop an-count) (> bn-count 1))
                (and (zerop bn-count) (> an-count 1))
                (chem-exp-svg--allene-edge-p drawing edge))
      (let* ((point-a (chem-exp-svg--point state
                                           (chem-atom-x atom-a)
                                           (chem-atom-y atom-a)))
             (point-b (chem-exp-svg--point state
                                           (chem-atom-x atom-b)
                                           (chem-atom-y atom-b)))
             (normal (car (chem-exp-svg--edge-normals point-a point-b)))
             (side-point (cons (+ (car point-a) (* 10.0 (car normal)))
                               (+ (cdr point-a) (* 10.0 (cdr normal)))))
             (side-sign (chem-exp-svg--which-side side-point point-a point-b))
             (side-count (vector 0 0))
             (total-count (vector 0 0)))
        (dolist (idx (append neighbors-a neighbors-b))
          (let* ((atom (gethash idx atoms))
                 (point (chem-exp-svg--point state
                                             (chem-atom-x atom)
                                             (chem-atom-y atom)))
                 (sign (chem-exp-svg--which-side point point-a point-b)))
            (if (chem-exp-svg--same-side-p sign side-sign)
                (aset side-count 0 (1+ (aref side-count 0)))
              (aset side-count 1 (1+ (aref side-count 1))))))
        (maphash
         (lambda (_ atom)
           (let* ((point (chem-exp-svg--point state
                                               (chem-atom-x atom)
                                               (chem-atom-y atom)))
                  (sign (chem-exp-svg--which-side point point-a point-b)))
             (if (chem-exp-svg--same-side-p sign side-sign)
                 (aset total-count 0 (1+ (aref total-count 0)))
               (aset total-count 1 (1+ (aref total-count 1))))))
         atoms)
        (cond ((> (aref side-count 0) (aref side-count 1)) 0)
              ((< (aref side-count 0) (aref side-count 1)) 1)
              ((> (aref total-count 0) (aref total-count 1)) 0)
              (t 1))))))

(defun chem-exp-svg--edge-svg (state edge)
  "Return EDGE's SVG for STATE."
  (let* ((atoms (chem-drawing-atoms (chem-exp-svg-state-drawing state)))
         (atom-a (gethash (chem-edge-a edge) atoms))
         (atom-b (gethash (chem-edge-b edge) atoms))
         (point-a (chem-exp-svg--point state
                                       (chem-atom-x atom-a)
                                       (chem-atom-y atom-a)))
         (point-b (chem-exp-svg--point state
                                       (chem-atom-x atom-b)
                                       (chem-atom-y atom-b)))
         (spacing (chem-exp-svg--option state :svg-bond-spacing))
         (short-fraction
          (chem-exp-svg--option state :svg-short-bond-length))
         (ring (chem-exp-svg--edge-ring state edge))
         (stereo (chem-edge-stereo edge))
         (show-stereo (chem-exp-svg--option state :svg-show-stereo))
         (lines
          (if (and show-stereo (memq stereo '(:up :down :either)))
              (chem-exp-svg--stereo-edge-svg state edge point-a point-b)
            (pcase (chem-edge-order edge)
              (:double
               (let* ((drawing (chem-exp-svg-state-drawing state))
                      (rings (chem-rings-for-edge drawing (chem-edge-id edge)))
                      (circle-ring
                       (cl-find-if
                        (lambda (ring)
                          (eq 'circle
                              (chem-exp-svg--ring-aromatic-display state ring)))
                        rings))
                      (target-ring
                       (if circle-ring
                           (chem-exp-svg--edge-inner-ring state edge)
                         ring))
                      (base (chem-exp-svg--line state
                                                (car point-a) (cdr point-a)
                                                (car point-b) (cdr point-b))))
                 (cond
                  ((and circle-ring (null target-ring)) base)
                  (target-ring
                   (let ((offset
                          (chem-exp-svg--ring-normal-offset
                           state edge target-ring spacing)))
                     (if offset
                         (concat
                          base
                          (chem-exp-svg--ring-inner-line
                           state point-a point-b short-fraction offset nil))
                       base)))
                  (t
                   (let ((side (chem-exp-svg--double-side state edge)))
                     (if side
                         (let* ((normal (nth side
                                             (chem-exp-svg--edge-normals
                                              point-a point-b)))
                                (offset (cons (* spacing (car normal))
                                              (* spacing (cdr normal)))))
                           (concat
                            base
                            (chem-exp-svg--ring-inner-line
                             state point-a point-b short-fraction offset nil)))
                       (let ((half (/ spacing 2.0)))
                         (chem-exp-svg--parallel-lines
                          state point-a point-b (list (- half) half)))))))))
              (:triple
               (let ((offset (/ spacing 1.5)))
                 (chem-exp-svg--parallel-lines
                  state point-a point-b (list (- offset) 0.0 offset))))
              (:aromatic
               (if (eq 'alternating
                       (chem-exp-svg--edge-aromatic-display state edge))
                   (let ((offset
                          (and ring
                               (chem-exp-svg--ring-normal-offset
                                state edge ring spacing))))
                     (if offset
                         (concat
                          (chem-exp-svg--line state
                                              (car point-a) (cdr point-a)
                                              (car point-b) (cdr point-b))
                          (chem-exp-svg--ring-inner-line
                           state point-a point-b short-fraction offset
                           (chem-exp-svg--dash-pattern state)))
                       (chem-exp-svg--line state
                                           (car point-a) (cdr point-a)
                                           (car point-b) (cdr point-b))))
                 (chem-exp-svg--line state
                                     (car point-a) (cdr point-a)
                                     (car point-b) (cdr point-b))))
              (_
               (chem-exp-svg--line state
                                   (car point-a) (cdr point-a)
                                   (car point-b) (cdr point-b))))))
         (attributes (chem-exp-svg--marker-attributes
                      (chem-edge-marker edge))))
    (if attributes
        (format "<g%s>%s</g>\n" attributes lines)
      lines)))

(defun chem-exp-svg--visible-edges (state)
  "Return STATE's visible edges."
  (chem-visible-edges (chem-exp-svg-state-drawing state)))

(defun chem-exp-svg--bonds (state)
  "Return the normal bond SVG for STATE."
  (mapconcat (lambda (edge) (chem-exp-svg--edge-svg state edge))
             (cl-remove-if-not
              (lambda (edge)
                (and (not (chem-edge-cross-p edge))
                     (chem-exp-svg--edge-visible-p state edge)))
              (chem-exp-svg--visible-edges state))
             ""))

(defun chem-exp-svg--cross-bonds (state)
  "Return the cross bond SVG for STATE."
  (mapconcat (lambda (edge) (chem-exp-svg--edge-svg state edge))
             (cl-remove-if-not
              (lambda (edge)
                (and (chem-edge-cross-p edge)
                     (chem-exp-svg--edge-visible-p state edge)))
              (chem-exp-svg--visible-edges state))
             ""))

(defun chem-exp-svg--atom-bounds (drawing &optional hidden)
  "Return (MIN-X MIN-Y MAX-X MAX-Y) for DRAWING's visible atoms."
  (let (min-x min-y max-x max-y)
    (maphash
     (lambda (idx atom)
       (unless (and hidden (gethash idx hidden))
         (let ((x (chem-atom-x atom))
               (y (chem-atom-y atom)))
           (setq min-x (if min-x (min min-x x) x))
           (setq min-y (if min-y (min min-y y) y))
           (setq max-x (if max-x (max max-x x) x))
           (setq max-y (if max-y (max max-y y) y)))))
     (chem-drawing-atoms drawing))
    (if min-x
        (list min-x min-y max-x max-y)
      (list 0.0 0.0 0.0 0.0))))

(defun chem-exp-svg--bounds (state)
  "Return STATE's (MIN-X MIN-Y MAX-X MAX-Y) bounds."
  (chem-exp-svg-state-bounds state))

(defun chem-exp-svg--expanded-bounds (state)
  "Return STATE's bounds expanded for atom labels."
  (let* ((bounds (chem-exp-svg--bounds state))
         (scale (max (chem-exp-svg--option state :svg-scale) 1e-6))
         (font-size (chem-exp-svg--option state :svg-font-size))
         (margin (/ (* 3.0 font-size) scale)))
    (list (- (nth 0 bounds) margin)
          (- (nth 1 bounds) margin)
          (+ (nth 2 bounds) margin)
          (+ (nth 3 bounds) margin))))

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

;;; Stereochemistry

(defun chem-exp-svg--wedge-half-width (state)
  "Return the wide half-width for wedges in STATE."
  (+ 3.0 (/ (chem-exp-svg--option state :svg-font-size) 4.0)))

(defun chem-exp-svg--wedge-points (state point-narrow point-wide)
  "Return the four polygon points for a wedge."
  (let* ((dx (- (car point-wide) (car point-narrow)))
         (dy (- (cdr point-wide) (cdr point-narrow)))
         (length (sqrt (+ (* dx dx) (* dy dy)))))
    (when (> length 1e-9)
      (let* ((ux (/ dx length))
             (uy (/ dy length))
             (nx (- uy))
             (ny ux)
             (narrow-half (/ (chem-exp-svg--option
                              state :svg-bond-thickness)
                             2.0))
             (wide-half (chem-exp-svg--wedge-half-width state)))
        (list
         (cons (+ (car point-narrow) (* nx narrow-half))
               (+ (cdr point-narrow) (* ny narrow-half)))
         (cons (- (car point-narrow) (* nx narrow-half))
               (- (cdr point-narrow) (* ny narrow-half)))
         (cons (- (car point-wide) (* nx wide-half))
               (- (cdr point-wide) (* ny wide-half)))
         (cons (+ (car point-wide) (* nx wide-half))
               (+ (cdr point-wide) (* ny wide-half))))))))

(defun chem-exp-svg--solid-wedge-svg (state point-narrow point-wide)
  "Return a filled wedge from POINT-NARROW to POINT-WIDE."
  (let ((points (chem-exp-svg--wedge-points state point-narrow point-wide)))
    (if points
        (format "<polygon points=\"%s\" fill=\"%s\"/>\n"
                (mapconcat
                 (lambda (point)
                   (format "%s,%s"
                           (chem-exp-svg--number (car point))
                           (chem-exp-svg--number (cdr point))))
                 points " ")
                (chem-exp-svg--escape (chem-exp-svg--bond-color state)))
      (chem-exp-svg--line state
                          (car point-narrow) (cdr point-narrow)
                          (car point-wide) (cdr point-wide)))))

(defun chem-exp-svg--hashed-wedge-svg (state point-narrow point-wide)
  "Return a hashed wedge from POINT-NARROW to POINT-WIDE."
  (let* ((dx (- (car point-wide) (car point-narrow)))
         (dy (- (cdr point-wide) (cdr point-narrow)))
         (length (sqrt (+ (* dx dx) (* dy dy)))))
    (if (< length 1e-9)
        (chem-exp-svg--line state
                            (car point-narrow) (cdr point-narrow)
                            (car point-wide) (cdr point-wide))
      (let* ((ux (/ dx length))
             (uy (/ dy length))
             (nx (- uy))
             (ny ux)
             (step (/ 3.75 length))
             (font-size (chem-exp-svg--option state :svg-font-size))
             (progress 0.0)
             (lines nil))
        (while (< progress 1.0)
          (let* ((width (* 0.5 font-size progress))
                 (base-x (+ (car point-narrow) (* dx progress)))
                 (base-y (+ (cdr point-narrow) (* dy progress))))
            (push (chem-exp-svg--line
                   state
                   (- base-x (* nx width))
                   (- base-y (* ny width))
                   (+ base-x (* nx width))
                   (+ base-y (* ny width)))
                  lines))
          (setq progress (+ progress step)))
        (let* ((width (* 0.5 font-size))
               (base-x (car point-wide))
               (base-y (cdr point-wide)))
          (push (chem-exp-svg--line
                 state
                 (- base-x (* nx width))
                 (- base-y (* ny width))
                 (+ base-x (* nx width))
                 (+ base-y (* ny width)))
                lines))
        (mapconcat #'identity (nreverse lines) "")))))

(defun chem-exp-svg--stereo-wedge-up-p (state edge center)
  "Return non-nil when EDGE is solid at CENTER in STATE."
  (let* ((drawing (chem-exp-svg-state-drawing state))
         (atoms (chem-drawing-atoms drawing))
         (atom (gethash center atoms))
         (stereocenter (and atom (chem-atom-stereocenter atom)))
         (configuration (and stereocenter
                             (chem-stereocenter-configuration stereocenter))))
    (if configuration
        (chem-stereocenter-wedge-up-p atoms center edge configuration)
      (eq :up (chem-edge-stereo-directed edge center)))))

(defun chem-exp-svg--stereo-edge-svg (state edge point-a point-b)
  "Return EDGE's stereochemical SVG."
  (pcase (chem-edge-stereo edge)
    (:either
     (chem-exp-svg--line state
                         (car point-a) (cdr point-a)
                         (car point-b) (cdr point-b)
                         (chem-exp-svg--dash-pattern state)))
    ((or :up :down)
     (let ((center (chem-edge-stereo-center edge)))
       (if (null center)
           (chem-exp-svg--line state
                               (car point-a) (cdr point-a)
                               (car point-b) (cdr point-b))
         (let* ((wide (if (= center (chem-edge-a edge)) point-a point-b))
                (narrow (if (= center (chem-edge-a edge)) point-b point-a)))
           (if (chem-exp-svg--stereo-wedge-up-p state edge center)
               (chem-exp-svg--solid-wedge-svg state narrow wide)
             (chem-exp-svg--hashed-wedge-svg state narrow wide))))))
    (_
     (chem-exp-svg--line state
                         (car point-a) (cdr point-a)
                         (car point-b) (cdr point-b)))))

(defun chem-exp-svg--stereo-label-text (state center)
  "Return the stereochemistry label text for CENTER."
  (let* ((cip (and (chem-exp-svg--option state :svg-show-cip-labels)
                   (chem-stereocenter-cip center)))
         (type (chem-stereocenter-type center))
         (group (or (chem-stereocenter-group center) 0))
         (cip-text (and (memq cip '(:R :S :r :s))
                        (substring (symbol-name cip) 1)))
         (enhanced-text
          (and (chem-exp-svg--option state :svg-show-enhanced-stereo)
               (pcase type
                 (:and (format "&%d" group))
                 (:or (format "or%d" group))
                 (:either (format "e%d" group))
                 (_ nil)))))
    (cond
     ((and cip-text enhanced-text)
      (concat cip-text ", " enhanced-text))
     (cip-text cip-text)
     (enhanced-text enhanced-text)
     (t nil))))

(defun chem-exp-svg--stereo-label-svg (state atom)
  "Return ATOM's stereochemistry label SVG."
  (let ((center (chem-atom-stereocenter atom)))
    (when center
      (let ((text (chem-exp-svg--stereo-label-text state center)))
        (when text
          (let* ((point (chem-exp-svg--point state
                                            (chem-atom-x atom)
                                            (chem-atom-y atom)))
                 (font-size (chem-exp-svg--option state :svg-font-size))
                 (offset (* 0.75 font-size))
                 (x (+ (car point) offset))
                 (y (- (cdr point) offset)))
            (format "<text class=\"stereo-label\" x=\"%s\" y=\"%s\" fill=\"%s\" font-family=\"%s\" font-size=\"%s\" text-anchor=\"start\" dominant-baseline=\"central\">%s</text>\n"
                    (chem-exp-svg--number x)
                    (chem-exp-svg--number y)
                    (chem-exp-svg--escape
                     (chem-exp-svg--theme-foreground
                      (chem-exp-svg-state-theme state)))
                    (chem-exp-svg--escape
                     (chem-exp-svg--option state :svg-font-family))
                    (chem-exp-svg--number font-size)
                    (chem-exp-svg--escape text))))))))

(defun chem-exp-svg--stereo-labels (state)
  "Return the stereochemistry label SVG for STATE."
  (mapconcat (lambda (atom)
               (or (chem-exp-svg--stereo-label-svg state atom) ""))
             (chem-exp-svg--sorted-atoms (chem-exp-svg-state-drawing state))
             ""))

;;; Rings

(defun chem-exp-svg--ring-center-svg (state ring)
  "Return RING's center in SVG coordinates."
  (let ((center (chem-ring-center (chem-exp-svg-state-drawing state) ring)))
    (chem-exp-svg--point state (nth 0 center) (nth 1 center))))

(defun chem-exp-svg--ring-bridged-p (state ring)
  "Return non-nil when RING appears bridged."
  (let ((drawing (chem-exp-svg-state-drawing state)))
    (cl-some (lambda (edge-id)
               (> (length (chem-rings-for-edge drawing edge-id)) 2))
             (chem-ring-bonds ring))))

(defun chem-exp-svg--ring-circle-eligible-p (state ring)
  "Return non-nil when RING can use an aromatic circle."
  (let ((drawing (chem-exp-svg-state-drawing state)))
    (and (chem-ring-aromatic-p ring)
         (>= (length (chem-ring-atoms ring)) 5)
         (or (chem-ring-regular-p drawing ring)
             (chem-ring-symmetric-p drawing ring))
         (not (chem-exp-svg--ring-bridged-p state ring))
         (> (chem-ring-apothem drawing ring) 0.0))))

(defun chem-exp-svg--ring-aromatic-display (state ring)
  "Return RING's aromatic display policy result."
  (when (chem-ring-aromatic-p ring)
    (if (and (memq (chem-exp-svg--option state :svg-aromatic)
                   '(auto circle))
             (chem-exp-svg--ring-circle-eligible-p state ring))
        'circle
      'alternating)))

(defun chem-exp-svg--edge-aromatic-display (state edge)
  "Return EDGE's aromatic display, or nil."
  (let ((rings (cl-remove-if-not
                (lambda (ring) (chem-ring-aromatic-p ring))
                (chem-rings-for-edge (chem-exp-svg-state-drawing state)
                                     (chem-edge-id edge)))))
    (when rings
      (if (cl-some (lambda (ring)
                     (eq 'circle
                         (chem-exp-svg--ring-aromatic-display state ring)))
                   rings)
          'circle
        'alternating))))

(defun chem-exp-svg--ring-distance-squared (drawing ring x y)
  "Return squared distance from (X, Y) to RING's center."
  (let ((center (chem-ring-center drawing ring)))
    (+ (expt (- (nth 0 center) x) 2)
       (expt (- (nth 1 center) y) 2))))

(defun chem-exp-svg--closest-edge-ring (state edge rings)
  "Return the ring in RINGS closest to EDGE's midpoint.
Break ties by ring id."
  (if (= 1 (length rings))
      (car rings)
    (let* ((drawing (chem-exp-svg-state-drawing state))
           (atoms (chem-drawing-atoms drawing))
           (atom-a (gethash (chem-edge-a edge) atoms))
           (atom-b (gethash (chem-edge-b edge) atoms))
           (mid-x (/ (+ (chem-atom-x atom-a) (chem-atom-x atom-b))
                     2.0))
           (mid-y (/ (+ (chem-atom-y atom-a) (chem-atom-y atom-b))
                     2.0)))
      (car (sort (copy-sequence rings)
                 (lambda (ring-a ring-b)
                   (let ((distance-a
                          (chem-exp-svg--ring-distance-squared
                           drawing ring-a mid-x mid-y))
                         (distance-b
                          (chem-exp-svg--ring-distance-squared
                           drawing ring-b mid-x mid-y)))
                     (if (= distance-a distance-b)
                         (< (chem-ring-id ring-a) (chem-ring-id ring-b))
                       (< distance-a distance-b)))))))))

(defun chem-exp-svg--edge-ring (state edge)
  "Return the ring used for EDGE's inner stroke.
For fused edges choose the ring whose center is closest to the edge
midpoint, breaking ties by ring id."
  (let* ((drawing (chem-exp-svg-state-drawing state))
         (rings (chem-rings-for-edge drawing (chem-edge-id edge))))
    (when rings
      (chem-exp-svg--closest-edge-ring state edge rings))))

(defun chem-exp-svg--edge-inner-ring (state edge)
  "Return the non-circle ring that should receive EDGE's inner stroke.
An aromatic circle replaces the inner strokes of its own ring.  When
EDGE is shared with another ring, keep the inner stroke on that
ring's side."
  (let* ((drawing (chem-exp-svg-state-drawing state))
         (rings (chem-rings-for-edge drawing (chem-edge-id edge)))
         (inner-rings
          (cl-remove-if
           (lambda (ring)
             (eq 'circle
                 (chem-exp-svg--ring-aromatic-display state ring)))
           rings)))
    (when inner-rings
      (chem-exp-svg--closest-edge-ring state edge inner-rings))))

(defun chem-exp-svg--ring-normal-offset (state edge ring distance)
  "Return a normal offset from EDGE toward RING by DISTANCE."
  (let* ((drawing (chem-exp-svg-state-drawing state))
         (atoms (chem-drawing-atoms drawing))
         (atom-a (gethash (chem-edge-a edge) atoms))
         (atom-b (gethash (chem-edge-b edge) atoms))
         (point-a (chem-exp-svg--point state
                                       (chem-atom-x atom-a)
                                       (chem-atom-y atom-a)))
         (point-b (chem-exp-svg--point state
                                       (chem-atom-x atom-b)
                                       (chem-atom-y atom-b)))
         (center (chem-exp-svg--ring-center-svg state ring))
         (dx (- (car point-b) (car point-a)))
         (dy (- (cdr point-b) (cdr point-a)))
         (length (sqrt (+ (* dx dx) (* dy dy))))
         (cx (- (car center) (car point-a)))
         (cy (- (cdr center) (cdr point-a)))
         (cross (- (* dx cy) (* dy cx))))
    (when (> length 1e-9)
      (let* ((normal-x (/ (- dy) length))
             (normal-y (/ dx length))
             (sign (if (>= cross 0.0) 1.0 -1.0)))
        (cons (* sign normal-x distance)
              (* sign normal-y distance))))))

(defun chem-exp-svg--shortened-line (point-a point-b fraction)
  "Return POINT-A and POINT-B shortened by FRACTION, centered."
  (let* ((x1 (car point-a)) (y1 (cdr point-a))
         (x2 (car point-b)) (y2 (cdr point-b))
         (dx (- x2 x1)) (dy (- y2 y1))
         (length (sqrt (+ (* dx dx) (* dy dy)))))
    (if (< length 1e-9)
        (list point-a point-b)
      (let* ((inset (* 0.5 (- 1.0 fraction) length))
             (ux (/ dx length))
             (uy (/ dy length)))
        (list (cons (+ x1 (* ux inset)) (+ y1 (* uy inset)))
              (cons (- x2 (* ux inset)) (- y2 (* uy inset))))))))

(defun chem-exp-svg--offset-point (point offset)
  "Return POINT shifted by OFFSET."
  (cons (+ (car point) (car offset))
        (+ (cdr point) (cdr offset))))

(defun chem-exp-svg--dash-pattern (state)
  "Return a deterministic dash pattern for STATE."
  (let ((thickness (chem-exp-svg--option state :svg-bond-thickness)))
    (format "%s %s"
            (chem-exp-svg--number (* 3.0 thickness))
            (chem-exp-svg--number (* 2.0 thickness)))))

(defun chem-exp-svg--ring-inner-line (state point-a point-b fraction offset dash)
  "Return a shortened, offset inner line."
  (let* ((short (chem-exp-svg--shortened-line point-a point-b fraction))
         (start (chem-exp-svg--offset-point (nth 0 short) offset))
         (end (chem-exp-svg--offset-point (nth 1 short) offset)))
    (chem-exp-svg--line state
                        (car start) (cdr start)
                        (car end) (cdr end)
                        dash)))

(defun chem-exp-svg--rings (state)
  "Return the ring SVG for STATE."
  (let* ((drawing (chem-exp-svg-state-drawing state))
         (scale (chem-exp-svg--option state :svg-scale))
         (spacing (chem-exp-svg--option state :svg-bond-spacing))
         (thickness (chem-exp-svg--option state :svg-bond-thickness)))
    (mapconcat
     (lambda (ring)
       (if (eq 'circle (chem-exp-svg--ring-aromatic-display state ring))
           (let* ((center (chem-exp-svg--ring-center-svg state ring))
                  (radius (- (* (chem-ring-apothem drawing ring) scale)
                             spacing)))
             (if (> radius 0.0)
                 (format "<circle cx=\"%s\" cy=\"%s\" r=\"%s\" fill=\"none\" stroke=\"%s\" stroke-width=\"%s\"/>\n"
                         (chem-exp-svg--number (car center))
                         (chem-exp-svg--number (cdr center))
                         (chem-exp-svg--number radius)
                         (chem-exp-svg--escape
                          (chem-exp-svg--bond-color state))
                         (chem-exp-svg--number thickness))
               ""))
         ""))
     (chem-drawing-rings drawing)
     "")))

;;; Renderer

(defun chem-exp-svg--background (state)
  "Return the background SVG."
  (let ((color (chem-exp-svg--theme-background
                (chem-exp-svg-state-theme state))))
    (if color
        (let ((viewbox (chem-exp-svg--viewbox state)))
          (format "<rect x=\"0\" y=\"0\" width=\"%s\" height=\"%s\" fill=\"%s\"/>\n"
                  (chem-exp-svg--number (nth 2 viewbox))
                  (chem-exp-svg--number (nth 3 viewbox))
                  (chem-exp-svg--escape color)))
      "")))

(defun chem-exp-svg--masked-content (state)
  "Return STATE's masked bond and ring content."
  (format "<g mask=\"url(#%s)\">\n%s%s%s</g>\n"
          (chem-exp-svg--escape (chem-exp-svg--mask-id))
          (chem-exp-svg--bonds state)
          (chem-exp-svg--rings state)
          (chem-exp-svg--cross-bonds state)))

(defun chem-exp-svg--make-state (drawing options)
  "Create an SVG renderer state for DRAWING and OPTIONS."
  (let* ((pseudo (chem-exp-svg--pseudo-elements drawing))
         (hidden (car pseudo))
         (groups (cdr pseudo))
         (state (chem-exp-svg-state-create
                 :drawing drawing
                 :options options
                 :hidden hidden
                 :pseudo groups
                 :bounds (chem-exp-svg--atom-bounds drawing hidden))))
    (setf (chem-exp-svg-state-theme state)
          (chem-exp-svg--theme state))
    (setf (chem-exp-svg-state-bounds state)
          (chem-exp-svg--expanded-bounds state))
    state))

(defun chem-exp-svg-render (drawing options)
  "Render DRAWING to an SVG string using OPTIONS."
  (let* ((state (chem-exp-svg--make-state drawing options))
         (body (concat (chem-exp-svg--defs state)
                       (chem-exp-svg--background state)
                       (chem-exp-svg--masked-content state)
                       (chem-exp-svg--atoms state)
                       (chem-exp-svg--stereo-labels state))))
    (chem-exp-svg--document state body)))

(chem-register-exporter 'svg #'chem-exp-svg-render
                        (list :label "SVG"
                              :options chem-exp-svg-option-specs
                              :target 'image))

(provide 'chem-exp-svg)
;;; chem-exp-svg.el ends here
