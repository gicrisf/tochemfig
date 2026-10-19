;;; chem-exp-svg-test.el --- Tests for the SVG exporter -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Giovanni Crisalfi

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Phase 1 tests for the pure-Elisp SVG exporter: option resolution,
;; primitives, themes, the initial document, and exporter registration.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'xml)

(let ((here (file-name-directory (or load-file-name buffer-file-name))))
  (when here (add-to-list 'load-path here)))

(let ((sibling (expand-file-name "emacs-indigo"
                                 (file-name-directory
                                  (directory-file-name
                                   (file-name-directory (or load-file-name buffer-file-name)))))))
  (when (file-directory-p sibling)
    (add-to-list 'load-path sibling)))

(require 'chem-mode)
(require 'chem-indigo)
(require 'chem-exp-chemfig)
(require 'chem-exp-svg)

(defconst chem-exp-svg-test--root
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the test files.")

(defun chem-exp-svg-test--distance (point-a point-b)
  "Return the distance between POINT-A and POINT-B."
  (sqrt (+ (expt (- (car point-a) (car point-b)) 2)
           (expt (- (cdr point-a) (cdr point-b)) 2))))

(defun chem-exp-svg-test--render (smiles &optional overrides)
  "Render SMILES to SVG with OVERRIDES."
  (let* ((chem-default-exporter 'svg)
         (options (chem--build-options overrides))
         (drawing (chem-from-string smiles options)))
    (chem-render drawing 'svg options)))

(defun chem-exp-svg-test--render-file (relative &optional overrides)
  "Render the molecule file RELATIVE to SVG with OVERRIDES."
  (let* ((chem-default-exporter 'svg)
         (options (chem--build-options overrides))
         (drawing (chem-from-file
                   (expand-file-name relative chem-exp-svg-test--root)
                   options)))
    (chem-render drawing 'svg options)))

(defun chem-exp-svg-test--xml (string)
  "Parse STRING as XML and return the parsed tree."
  (with-temp-buffer
    (insert string)
    (if (fboundp 'libxml-parse-xml-region)
        (libxml-parse-xml-region (point-min) (point-max))
      (xml-parse-region (point-min) (point-max)))))

(defun chem-exp-svg-test--count (regexp string)
  "Count matches of REGEXP in STRING."
  (let ((count 0)
        (start 0))
    (while (string-match regexp string start)
      (setq count (1+ count))
      (setq start (match-end 0)))
    count))

(defun chem-exp-svg-test--text-content (svg)
  "Return concatenated tspan text from SVG labels."
  (let ((start 0)
        (text ""))
    (while (string-match "<tspan[^>]*>\\([^<]*\\)</tspan>" svg start)
      (setq text (concat text (match-string 1 svg)))
      (setq start (match-end 0)))
    text))

(defun chem-exp-svg-test--edge-between (drawing a b)
  "Return DRAWING's visible edge between atom indices A and B."
  (let ((ends (sort (list a b) #'<)))
    (cl-find-if
     (lambda (edge)
       (equal ends
              (sort (list (chem-edge-a edge) (chem-edge-b edge)) #'<)))
     (chem-visible-edges drawing))))

(defun chem-exp-svg-test--line-count (string)
  "Return the number of SVG line elements in STRING."
  (chem-exp-svg-test--count "<line " string))

(defun chem-exp-svg-test--root (svg)
  "Return the opening SVG tag from SVG."
  (substring svg 0 (string-match ">" svg)))

(ert-deftest chem-exp-svg-exporter-registered ()
  "The SVG exporter is registered with image metadata."
  (let ((entry (chem-exporter 'svg)))
    (should entry)
    (let ((metadata (plist-get entry :metadata)))
      (should (eq 'image (plist-get metadata :target)))
      (should (equal chem-exp-svg-option-specs
                     (plist-get metadata :options))))))

(ert-deftest chem-exp-svg-option-keys-match-variables ()
  "Every SVG option key follows the :svg-* convention."
  (dolist (spec chem-exp-svg-option-specs)
    (let* ((key (plist-get spec :key))
           (variable (plist-get spec :variable))
           (variable-name (symbol-name variable))
           (suffix (substring variable-name
                              (length "chem-exp-svg-default-")))
           (expected (intern (concat ":svg-" suffix))))
      (should (eq key expected))))
  (should-not (cl-find :svg-background chem-exp-svg-option-specs
                       :key (lambda (spec) (plist-get spec :key)))))

(ert-deftest chem-exp-svg-option-resolution ()
  "SVG options resolve from variables and preserve explicit nil overrides."
  (let* ((chem-default-exporter 'svg)
         (chem-exp-svg-default-width 123)
         (options (chem--build-options nil))
         (drawing (chem-from-string "CCO" options))
         (state (chem-exp-svg--make-state drawing options)))
    (should (= 123 (plist-get options :svg-width)))
    (should (= 123 (chem-exp-svg--option state :svg-width))))
  (let* ((chem-default-exporter 'svg)
         (chem-exp-svg-default-width 123)
         (options (chem--build-options '(("svg-width" . nil))))
         (drawing (chem-from-string "CCO" options))
         (state (chem-exp-svg--make-state drawing options)))
    (should (plist-member options :svg-width))
    (should-not (chem-exp-svg--option state :svg-width))))

(ert-deftest chem-exp-svg-escape ()
  "XML metacharacters are escaped."
  (should (equal "&amp;&lt;&gt;&quot;&apos;"
                 (chem-exp-svg--escape "&<>\"'")))
  (should (equal "&amp;1" (chem-exp-svg--escape "&1")))
  (should (equal "" (chem-exp-svg--escape nil))))

(ert-deftest chem-exp-svg-number ()
  "Numbers are formatted without trailing zeros."
  (should (equal "1" (chem-exp-svg--number 1)))
  (should (equal "1.5" (chem-exp-svg--number 1.5)))
  (should (equal "-1.5" (chem-exp-svg--number -1.5)))
  (should (equal "0" (chem-exp-svg--number -0.0001))))

(ert-deftest chem-exp-svg-id ()
  "SVG IDs are deterministic."
  (should (equal "chem-exp-svg-text-mask"
                 (chem-exp-svg--id 'text-mask)))
  (should (equal (chem-exp-svg--id "text-mask")
                 (chem-exp-svg--id 'text-mask))))

(ert-deftest chem-exp-svg-theme-lookup ()
  "Theme colors resolve with deterministic fallbacks."
  (let ((light (alist-get 'light chem-exp-svg-themes)))
    (should (equal "#3498db" (chem-exp-svg--theme-color light "N")))
    (should (equal "#16a085" (chem-exp-svg--theme-color light "Cl")))
    (should (equal "#ffffff" (chem-exp-svg--theme-background light))))
  (let ((theme '(:foreground "#111111" :default "#333333" :C "#222222")))
    (should (equal "#333333" (chem-exp-svg--theme-color theme "Xe"))))
  (let ((theme '(:foreground "#111111" :C "#222222")))
    (should (equal "#111111" (chem-exp-svg--theme-color theme "Xe"))))
  (let ((theme '(:C "#222222")))
    (should (equal "#222222" (chem-exp-svg--theme-color theme "Xe"))))
  (should-not (chem-exp-svg--theme-background '(:background nil))))

(ert-deftest chem-exp-svg-theme-name-normalization ()
  "Theme names accept symbols and strings."
  (should (eq 'dark (chem-exp-svg--theme-name 'dark)))
  (should (eq 'dark (chem-exp-svg--theme-name "dark"))))

(ert-deftest chem-exp-svg-theme-selection ()
  "The selected theme resolves through the option plist."
  (let* ((chem-default-exporter 'svg)
         (options (chem--build-options '(("svg-theme" . "dark"))))
         (drawing (chem-from-string "CCO" options))
         (state (chem-exp-svg--make-state drawing options)))
    (should (equal "#141414"
                   (chem-exp-svg--theme-background
                    (chem-exp-svg-state-theme state))))))

(ert-deftest chem-exp-svg-unknown-theme ()
  "An unknown theme is a user error."
  (let* ((chem-default-exporter 'svg)
         (chem-exp-svg-default-theme 'missing)
         (options (chem--build-options nil))
         (drawing (chem-from-string "CCO" options)))
    (should-error (chem-exp-svg--make-state drawing options)
                  :type 'user-error)))

(ert-deftest chem-exp-svg-bounds-and-point ()
  "Coordinates are absolute and the viewBox is square."
  (let* ((chem-default-exporter 'svg)
         (options (chem--build-options '(("svg-scale" . 10.0)
                                         ("svg-padding" . 1.0))))
         (drawing (chem-from-string "CCO" options))
         (state (chem-exp-svg--make-state drawing options))
         (atom (gethash 0 (chem-drawing-atoms drawing)))
         (point (chem-exp-svg--point state
                                      (chem-atom-x atom)
                                      (chem-atom-y atom)))
         (viewbox (chem-exp-svg--viewbox state)))
    (should (= (* 10.0 (chem-atom-x atom)) (car point)))
    (should (= (* -10.0 (chem-atom-y atom)) (cdr point)))
    (should (= (nth 2 viewbox) (nth 3 viewbox)))
    (should (> (nth 2 viewbox) 0.0))))

(ert-deftest chem-exp-svg-render-xml ()
  "The renderer returns valid deterministic SVG."
  (let ((svg (chem-exp-svg-test--render "CCO")))
    (should (stringp svg))
    (should (string-match-p "\\`<svg " svg))
    (should (string-match-p "xmlns=\"http://www.w3.org/2000/svg\"" svg))
    (should (string-match-p "viewBox=\"" svg))
    (should (chem-exp-svg-test--xml svg))
    (should (equal svg (chem-exp-svg-test--render "CCO")))))

(ert-deftest chem-exp-svg-dimensions ()
  "The default output is 500x500; explicit dimensions override it."
  (let* ((svg (chem-exp-svg-test--render "CCO"))
         (root (chem-exp-svg-test--root svg)))
    (should (string-match-p " width=\"500\"" root))
    (should (string-match-p " height=\"500\"" root)))
  (let* ((svg (chem-exp-svg-test--render "CCO" '(("svg-width" . 300))))
         (root (chem-exp-svg-test--root svg)))
    (should (string-match-p " width=\"300\"" root))
    (should (string-match-p " height=\"" root)))
  (let* ((svg (chem-exp-svg-test--render "CCO" '(("svg-height" . 200))))
         (root (chem-exp-svg-test--root svg)))
    (should (string-match-p " width=\"" root))
    (should (string-match-p " height=\"200\"" root)))
  (let* ((svg (chem-exp-svg-test--render "CCO" '(("svg-width" . 300)
                                                 ("svg-height" . 200))))
         (root (chem-exp-svg-test--root svg)))
    (should (string-match-p "preserveAspectRatio=\"xMidYMid meet\"" root))))

(ert-deftest chem-exp-svg-background ()
  "The theme background is emitted as a rectangle."
  (let ((svg (chem-exp-svg-test--render "CCO")))
    (should (string-match-p "<rect[^>]*fill=\"#ffffff\"" svg)))
  (let ((chem-exp-svg-themes
         '((light . (:foreground "#222222"
                    :background nil
                    :default "#222222"
                    :C "#222222")))))
    (let ((svg (chem-exp-svg-test--render "CCO")))
      (should-not (string-match-p "<rect[^>]*fill=\"#ffffff\"" svg)))))

(ert-deftest chem-exp-svg-dark-background ()
  "Selecting the dark theme changes the background rectangle."
  (let ((svg (chem-exp-svg-test--render "CCO" '(("svg-theme" . "dark")))))
    (should (string-match-p "<rect[^>]*fill=\"#141414\"" svg))))

(ert-deftest chem-exp-svg-bond-orders ()
  "Single, double, and triple bonds emit one, two, and three lines."
  (should (= 2 (chem-exp-svg-test--count
                "<line " (chem-exp-svg-test--render "CCO"))))
  (should (= 2 (chem-exp-svg-test--count
                "<line " (chem-exp-svg-test--render "C=C"))))
  (should (= 3 (chem-exp-svg-test--count
                "<line " (chem-exp-svg-test--render "C#C")))))

(ert-deftest chem-exp-svg-aromatic-auto ()
  "Default auto mode draws an aromatic circle and single ring bonds."
  (let ((svg (chem-exp-svg-test--render "c1ccccc1")))
    (should (= 1 (chem-exp-svg-test--count "<circle " svg)))
    (should (= 6 (chem-exp-svg-test--count "<line " svg)))
    (should (string-match-p "<circle[^>]*fill=\"none\"" svg))))

(ert-deftest chem-exp-svg-aromatic-circle ()
  "Circle mode draws an aromatic circle."
  (let ((svg (chem-exp-svg-test--render
              "c1ccccc1" '(("svg-aromatic" . circle)))))
    (should (= 1 (chem-exp-svg-test--count "<circle " svg)))
    (should (= 6 (chem-exp-svg-test--count "<line " svg)))))

(ert-deftest chem-exp-svg-aromatic-alternating ()
  "Alternating mode draws dashed inner ring lines."
  (let ((svg (chem-exp-svg-test--render
              "c1ccccc1" '(("svg-aromatic" . alternating)))))
    (should (= 0 (chem-exp-svg-test--count "<circle " svg)))
    (should (= 12 (chem-exp-svg-test--count "<line " svg)))
    (should (= 6 (chem-exp-svg-test--count "stroke-dasharray" svg)))))

(ert-deftest chem-exp-svg-naphthalene-circles ()
  "Fused aromatic rings draw deterministic circles."
  (let ((svg (chem-exp-svg-test--render
              "c1ccc2ccccc2c1" '(("svg-aromatic" . circle)))))
    (should (= 2 (chem-exp-svg-test--count "<circle " svg)))
    (should (= 11 (chem-exp-svg-test--count "<line " svg)))))

(ert-deftest chem-exp-svg-naphthalene-alternating ()
  "Fused aromatic rings fall back to dashed inner lines."
  (let ((svg (chem-exp-svg-test--render
              "c1ccc2ccccc2c1" '(("svg-aromatic" . alternating)))))
    (should (= 0 (chem-exp-svg-test--count "<circle " svg)))
    (should (= 11 (chem-exp-svg-test--count "stroke-dasharray" svg)))))

(ert-deftest chem-exp-svg-ring-double-bond ()
  "Ring double bonds use a shortened inner stroke on the ring side."
  (let* ((chem-default-exporter 'svg)
         (options (chem--build-options nil))
         (drawing (chem-from-string "C1CC=CC1" options))
         (state (chem-exp-svg--make-state drawing options))
         (edge (cl-find-if (lambda (edge) (eq :double (chem-edge-order edge)))
                           (chem-visible-edges drawing)))
         (ring (chem-exp-svg--edge-ring state edge))
         (offset (chem-exp-svg--ring-normal-offset state edge ring 1.0)))
    (should ring)
    (should offset)
    (should-not (and (zerop (car offset)) (zerop (cdr offset))))
    (let ((svg (chem-render drawing 'svg options)))
      (should (= 0 (chem-exp-svg-test--count "<circle " svg)))
      (should (= 6 (chem-exp-svg-test--count "<line " svg))))))

(ert-deftest chem-exp-svg-bridged-ring-smoke ()
  "Bridged rings render without error."
  (let ((svg (chem-exp-svg-test--render "CC#CC")))
    (should (chem-exp-svg-test--xml svg))))

(ert-deftest chem-exp-svg-solid-wedge ()
  "Alanine renders a filled wedge."
  (let ((svg (chem-exp-svg-test--render "N[C@@H](C)C(=O)O")))
    (should (string-match-p "<polygon " svg))))

(ert-deftest chem-exp-svg-wedge-wide-end ()
  "The wedge widens at stereo-center."
  (let* ((chem-default-exporter 'svg)
         (options (chem--build-options nil))
         (drawing (chem-from-string "N[C@@H](C)C(=O)O" options))
         (state (chem-exp-svg--make-state drawing options))
         (atoms (chem-drawing-atoms drawing))
         (edge (cl-find-if #'chem-edge-stereo-center
                           (chem-visible-edges drawing)))
         (center (chem-edge-stereo-center edge))
         (atom-a (gethash (chem-edge-a edge) atoms))
         (atom-b (gethash (chem-edge-b edge) atoms))
         (point-a (chem-exp-svg--point state
                                       (chem-atom-x atom-a)
                                       (chem-atom-y atom-a)))
         (point-b (chem-exp-svg--point state
                                       (chem-atom-x atom-b)
                                       (chem-atom-y atom-b)))
         (wide (if (= center (chem-edge-a edge)) point-a point-b))
         (narrow (if (= center (chem-edge-a edge)) point-b point-a))
         (points (chem-exp-svg--wedge-points state narrow wide)))
    (should points)
    (should (> (chem-exp-svg-test--distance (nth 2 points) wide)
               (chem-exp-svg-test--distance (nth 0 points) narrow)))))

(ert-deftest chem-exp-svg-lactic-acid-wedge ()
  "Lactic acid renders a hashed wedge."
  (let ((svg (chem-exp-svg-test--render "C[C@H](O)C(=O)O")))
    (should (> (chem-exp-svg-test--count "<line " svg) 6))))

(ert-deftest chem-exp-svg-hashed-wedge ()
  "Mirroring alanine produces a hashed wedge."
  (let* ((chem-default-exporter 'svg)
         (options (chem--build-options nil))
         (drawing (chem-from-string "N[C@@H](C)C(=O)O" options)))
    (chem-flip-horizontal drawing)
    (let ((svg (chem-render drawing 'svg options)))
      (should-not (string-match-p "<polygon " svg))
      (should (> (chem-exp-svg-test--count "<line " svg) 6)))))

(ert-deftest chem-exp-svg-stereo-flip ()
  "Configuration-derived wedge direction flips with the coordinates."
  (let* ((chem-default-exporter 'svg)
         (options (chem--build-options nil))
         (drawing (chem-from-string "N[C@@H](C)C(=O)O" options))
         (edge (cl-find-if #'chem-edge-stereo-center
                           (chem-visible-edges drawing)))
         (center (chem-edge-stereo-center edge))
         (before (chem-exp-svg--stereo-wedge-up-p
                  (chem-exp-svg--make-state drawing options)
                  edge center)))
    (chem-flip-horizontal drawing)
    (let ((after (chem-exp-svg--stereo-wedge-up-p
                  (chem-exp-svg--make-state drawing options)
                  edge center)))
      (should (not (eq before after))))))

(ert-deftest chem-exp-svg-no-stereo ()
  "Disabling stereo draws a plain single bond."
  (let ((svg (chem-exp-svg-test--render
              "N[C@@H](C)C(=O)O" '(("svg-show-stereo" . nil)))))
    (should-not (string-match-p "<polygon " svg))
    (should (= 3 (chem-exp-svg-test--count "<line " svg)))))

(ert-deftest chem-exp-svg-either-stereo ()
  "Either stereochemistry falls back to a dashed bond."
  (let* ((chem-default-exporter 'svg)
         (options (chem--build-options nil))
         (drawing (chem-from-string "CCO" options))
         (edge (car (chem-visible-edges drawing))))
    (setf (chem-edge-stereo edge) :either)
    (let ((svg (chem-render drawing 'svg options)))
      (should (string-match-p "stroke-dasharray" svg)))))

(ert-deftest chem-exp-svg-allene-wedge ()
  "Allene input wedges are preserved."
  (let ((svg (chem-exp-svg-test--render-file "data/allene.mol")))
    (should (string-match-p "OH" (chem-exp-svg-test--text-content svg)))
    (should (string-match-p "<polygon " svg))))

(ert-deftest chem-exp-svg-two-stereocenters ()
  "Molecules with two stereocenters render both wedges."
  (let* ((chem-default-exporter 'svg)
         (options (chem--build-options nil))
         (drawing (chem-from-string "C[C@H](O)[C@@H](C)O" options))
         (centers 0))
    (maphash (lambda (_ atom)
               (when (chem-atom-stereocenter atom)
                 (setq centers (1+ centers))))
             (chem-drawing-atoms drawing))
    (should (= 2 centers))
    (let ((svg (chem-render drawing 'svg options)))
      (should (chem-exp-svg-test--xml svg))
      (should (> (chem-exp-svg-test--count "<line " svg) 10)))))

(ert-deftest chem-exp-svg-bromochloromethane ()
  "The bromochloromethane fixture renders a wedge."
  (let ((svg (chem-exp-svg-test--render-file
              "corpus/bromochloromethane_stereo.mol")))
    (should (chem-exp-svg-test--xml svg))
    (should (or (string-match-p "<polygon " svg)
                (> (chem-exp-svg-test--count "<line " svg) 4)))))

(ert-deftest chem-exp-svg-cip-labels ()
  "CIP labels render only when enabled."
  (let ((svg (chem-exp-svg-test--render
              "N[C@@H](C)C(=O)O" '(("svg-show-cip-labels" . t)))))
    (should (= 1 (chem-exp-svg-test--count "class=\"stereo-label\"" svg)))
    (should (string-match-p ">S</text>" svg)))
  (let ((svg (chem-exp-svg-test--render "N[C@@H](C)C(=O)O")))
    (should (= 0 (chem-exp-svg-test--count "class=\"stereo-label\"" svg)))))

(ert-deftest chem-exp-svg-cip-case ()
  "Lowercase CIP descriptors keep their case."
  (let* ((chem-default-exporter 'svg)
         (options (chem--build-options '(("svg-show-cip-labels" . t))))
         (drawing (chem-from-string "N[C@@H](C)C(=O)O" options))
         (center (chem-atom-stereocenter
                  (gethash 1 (chem-drawing-atoms drawing)))))
    (setf (chem-stereocenter-cip center) :r)
    (let ((svg (chem-render drawing 'svg options)))
      (should (string-match-p ">r</text>" svg)))))

(ert-deftest chem-exp-svg-ez-cip-ignored ()
  "Bond CIP descriptors are not rendered as stereocenter labels."
  (let* ((chem-default-exporter 'svg)
         (options (chem--build-options '(("svg-show-cip-labels" . t))))
         (drawing (chem-from-string "N[C@@H](C)C(=O)O" options))
         (center (chem-atom-stereocenter
                  (gethash 1 (chem-drawing-atoms drawing)))))
    (setf (chem-stereocenter-cip center) :E)
    (let ((svg (chem-render drawing 'svg options)))
      (should (= 0 (chem-exp-svg-test--count "class=\"stereo-label\"" svg))))))

(ert-deftest chem-exp-svg-enhanced-and ()
  "AND enhanced stereo labels are XML-escaped."
  (let ((svg (chem-exp-svg-test--render
              "N[C@@H](C)C(=O)O |&1:1|"
              '(("svg-show-enhanced-stereo" . t)))))
    (should (string-match-p ">&amp;1</text>" svg))))

(ert-deftest chem-exp-svg-enhanced-or ()
  "OR enhanced stereo labels are rendered."
  (let ((svg (chem-exp-svg-test--render
              "N[C@@H](C)C(=O)O |o1:1|"
              '(("svg-show-enhanced-stereo" . t)))))
    (should (string-match-p ">or1</text>" svg))))

(ert-deftest chem-exp-svg-enhanced-either ()
  "EITHER enhanced stereo labels are rendered."
  (let* ((chem-default-exporter 'svg)
         (options (chem--build-options
                   '(("svg-show-enhanced-stereo" . t))))
         (drawing (chem-from-string "N[C@@H](C)C(=O)O" options))
         (center (chem-atom-stereocenter
                  (gethash 1 (chem-drawing-atoms drawing)))))
    (setf (chem-stereocenter-type center) :either)
    (setf (chem-stereocenter-group center) 2)
    (let ((svg (chem-render drawing 'svg options)))
      (should (string-match-p ">e2</text>" svg)))))

(ert-deftest chem-exp-svg-enhanced-abs ()
  "ABS enhanced stereo produces no group label."
  (let ((svg (chem-exp-svg-test--render
              "N[C@@H](C)C(=O)O |a:1|"
              '(("svg-show-enhanced-stereo" . t)))))
    (should (= 0 (chem-exp-svg-test--count "class=\"stereo-label\"" svg)))))

(ert-deftest chem-exp-svg-combined-stereo-labels ()
  "CIP and enhanced-stereo labels combine deterministically."
  (let ((svg (chem-exp-svg-test--render
              "N[C@@H](C)C(=O)O |&1:1|"
              '(("svg-show-cip-labels" . t)
                ("svg-show-enhanced-stereo" . t)))))
    (should (string-match-p ">S, &amp;1</text>" svg))))

(ert-deftest chem-exp-svg-link-bonds ()
  "Link bonds are not drawn."
  (let ((svg (chem-exp-svg-test--render "[Na+].[Cl-]")))
    (should (= 0 (chem-exp-svg-test--count "<line " svg)))
    (should (= 2 (chem-exp-svg-test--count "<text " svg)))))

(ert-deftest chem-exp-svg-cross-bonds ()
  "Cross bonds render in a separate layer after normal bonds."
  (let* ((chem-default-exporter 'svg)
         (options (chem--build-options '(("cross-bond" . ((1 2))))))
         (drawing (chem-from-string "c1ccccc1" options))
         (state (chem-exp-svg--make-state drawing options))
         (normal (chem-exp-svg--bonds state))
         (cross (chem-exp-svg--cross-bonds state))
         (svg (chem-render drawing 'svg options)))
    (should (string-match-p "<line " normal))
    (should (string-match-p "<line " cross))
    (should (< (string-search normal svg)
               (string-search cross svg)))))

(ert-deftest chem-exp-svg-atom-labels ()
  "Heteroatom labels appear and hidden carbons stay hidden."
  (let ((svg (chem-exp-svg-test--render "CCO")))
    (should (= 1 (chem-exp-svg-test--count "<text " svg)))
    (should (string-match-p "OH" (chem-exp-svg-test--text-content svg)))))

(ert-deftest chem-exp-svg-caffeine ()
  "Caffeine renders as valid SVG."
  (let ((svg (chem-exp-svg-test--render "CN1C=NC2=C1C(=O)N(C(=O)N2C)C")))
    (should (chem-exp-svg-test--xml svg))
    (should (> (chem-exp-svg-test--count "<text " svg) 0))
    (should (> (chem-exp-svg-test--count "<line " svg) 0))))

(ert-deftest chem-exp-svg-circle-double-bonds ()
  "Explicit ring double bonds do not intrude into aromatic circles."
  (let* ((chem-default-exporter 'svg)
         (options (chem--build-options '(("svg-aromatic" . circle))))
         (drawing (chem-from-string "CN1C=NC2=C1C(=O)N(C(=O)N2C)C" options))
         (state (chem-exp-svg--make-state drawing options))
         (circle-edge (chem-exp-svg-test--edge-between drawing 2 3))
         (shared-edge (chem-exp-svg-test--edge-between drawing 4 5))
         (carbonyl-edge (chem-exp-svg-test--edge-between drawing 6 7))
         (inner-ring (chem-exp-svg--edge-inner-ring state shared-edge)))
    (should circle-edge)
    (should shared-edge)
    (should carbonyl-edge)
    (should (= 1 (chem-exp-svg-test--line-count
                  (chem-exp-svg--edge-svg state circle-edge))))
    (should (= 2 (chem-exp-svg-test--line-count
                  (chem-exp-svg--edge-svg state shared-edge))))
    (should (= 2 (chem-exp-svg-test--line-count
                  (chem-exp-svg--edge-svg state carbonyl-edge))))
    (should inner-ring)
    (should (= 1 (chem-ring-id inner-ring)))))

(ert-deftest chem-exp-svg-explicit-kekule-circle ()
  "Explicit double bonds inside an aromatic circle draw as single edges."
  (let* ((chem-default-exporter 'svg)
         (options (chem--build-options '(("svg-aromatic" . circle))))
         (drawing (chem-from-string "C1=CC=CC=C1" options))
         (state (chem-exp-svg--make-state drawing options))
         (double-edges
          (cl-remove-if-not (lambda (edge) (eq :double (chem-edge-order edge)))
                            (chem-visible-edges drawing))))
    (should (= 3 (length double-edges)))
    (dolist (edge double-edges)
      (should (= 1 (chem-exp-svg-test--line-count
                    (chem-exp-svg--edge-svg state edge)))))))

(ert-deftest chem-exp-svg-labels-centered-on-atoms ()
  "Atom labels are centered on their atoms, as in SmilesDrawer."
  (let* ((chem-default-exporter 'svg)
         (options (chem--build-options '(("svg-aromatic" . circle))))
         (drawing (chem-from-string "CN1C=NC2=C1C(=O)N(C(=O)N2C)C" options))
         (state (chem-exp-svg--make-state drawing options))
         (checked 0))
    (dolist (atom (chem-exp-svg--sorted-atoms drawing))
      (let ((text (chem-exp-svg--atom-text atom)))
        (when text
          (setq checked (1+ checked))
          (let* ((point (chem-exp-svg--point state
                                             (chem-atom-x atom)
                                             (chem-atom-y atom)))
                 (center (chem-exp-svg--label-center state atom text)))
            (should (< (abs (- (car center) (car point))) 1e-6))
            (should (< (abs (- (cadr center) (cdr point))) 1e-6))))))
    (should (> checked 0))))

(ert-deftest chem-exp-svg-linear-carbon-dot ()
  "Linear carbons receive SmilesDrawer-style carbon dots."
  (let ((svg (chem-exp-svg-test--render "CC#CC")))
    (should (string-match-p "<circle[^>]*r=\"0.75\"" svg))
    (should (string-match-p "<circle[^>]*r=\"1.5\" fill=\"black\"" svg))))

(ert-deftest chem-exp-svg-pseudo-elements ()
  "Terminal pseudo-element groups are combined into one label."
  (let* ((alanine (chem-exp-svg-test--render
                   "N[C@@H](C)C(=O)O" '(("svg-show-stereo" . nil))))
         (text (chem-exp-svg-test--text-content alanine)))
    (should (string-match-p "COOH\\|HOOC" text))
    (should (string-match-p "NH₂\\|H₂N" text))
    (should (= 2 (chem-exp-svg-test--count "<text " alanine)))
    (should (= 3 (chem-exp-svg-test--count "<line " alanine))))
  (let ((nitro (chem-exp-svg-test--render "O=[N+]([O-])c1ccccc1")))
    (should (string-match-p "N⁺O₂\\|O₂N⁺"
                            (chem-exp-svg-test--text-content nitro)))
    (should (= 1 (chem-exp-svg-test--count "<text " nitro)))
    (should (= 7 (chem-exp-svg-test--count "<line " nitro)))))

(ert-deftest chem-exp-svg-atom-numbers ()
  "Atom numbers replace hydrogen and charge annotations."
  (let ((svg (chem-exp-svg-test--render "CCO" '(("atom-numbers" . t)))))
    (should (= 3 (chem-exp-svg-test--count "<text " svg)))
    (should (string-match-p "1" (chem-exp-svg-test--text-content svg)))
    (should (string-match-p "2" (chem-exp-svg-test--text-content svg)))
    (should (string-match-p "O3" (chem-exp-svg-test--text-content svg)))
    (should-not (string-match-p "H" (chem-exp-svg-test--text-content svg)))))

(ert-deftest chem-exp-svg-charge-and-hydrogens ()
  "Charges and hydrogens use Unicode sub/superscripts."
  (let ((svg (chem-exp-svg-test--render "[NH4+]")))
    (should (string-match-p "N⁺H₄" (chem-exp-svg-test--text-content svg)))))

(ert-deftest chem-exp-svg-radical ()
  "Radicals are rendered as superscript dots."
  (let ((svg (chem-exp-svg-test--render "[CH3]")))
    (should (string-match-p "CH₃·" (chem-exp-svg-test--text-content svg)))))

(ert-deftest chem-exp-svg-markers ()
  "Markers become stable SVG IDs, including hidden atoms."
  (let ((svg (chem-exp-svg-test--render "CCO" '(("markers" . "a")))))
    (should (string-match-p "id=\"a1-2\"" svg))
    (should (string-match-p "id=\"a1\"" svg))
    (should (string-match-p "id=\"a2\"" svg))
    (should (string-match-p "id=\"a3\"" svg))))

(ert-deftest chem-exp-svg-marker-sanitization ()
  "Marker IDs are valid and deterministic."
  (should (equal "a1" (chem-exp-svg--sanitize-id "a1")))
  (should (equal "marker-1a" (chem-exp-svg--sanitize-id "1a")))
  (should (equal "a-b" (chem-exp-svg--sanitize-id "a b"))))

(ert-deftest chem-exp-svg-label-mask ()
  "The label mask applies to bonds and not to labels."
  (let ((svg (chem-exp-svg-test--render "CCO")))
    (should (string-match-p "<mask id=\"chem-exp-svg-label-mask\"" svg))
    (should (string-match-p
             "<g mask=\"url(#chem-exp-svg-label-mask)\">" svg))
    (should (= 1 (chem-exp-svg-test--count "fill=\"black\"" svg)))))

(ert-deftest chem-exp-svg-text-direction ()
  "Text direction is deterministic for every atom."
  (let* ((chem-default-exporter 'svg)
         (options (chem--build-options nil))
         (drawing (chem-from-string "CCO" options))
         (atoms (chem-drawing-atoms drawing)))
    (dolist (idx '(0 1 2))
      (should (memq (chem-exp-svg--text-direction (gethash idx atoms))
                    '(right left up down))))))

(ert-deftest chem-exp-svg-option-merge-is-exporter-specific ()
  "SVG and chemfig options do not leak into each other."
  (let ((chem-default-exporter 'svg))
    (should (plist-member (chem--build-options nil) :svg-width))
    (should-not (plist-member (chem--build-options nil) :terse)))
  (let ((chem-default-exporter 'chemfig))
    (should (plist-member (chem--build-options nil) :terse))
    (should-not (plist-member (chem--build-options nil) :svg-width))))

(ert-deftest chem-exp-svg-mode-generate ()
  "`chem-mode--generate' can render through the SVG exporter."
  (let ((chem-default-exporter 'svg))
    (should (string-match-p "\\`<svg "
                            (chem-mode--generate 'direct "CCO" nil)))))

(ert-deftest chem-exp-svg-corpus-smoke ()
  "Render every studies corpus MOL as SVG and parse the result."
  (let* ((root (expand-file-name "../studies/external-corpus"
                                 chem-exp-svg-test--root))
         (files (and (file-directory-p root)
                     (directory-files-recursively root "\\.mol\\'"))))
    (unless files
      (ert-skip "studies corpus not available"))
    (let ((failures nil)
          (count 0))
      (dolist (path files)
        (condition-case err
            (let* ((chem-default-exporter 'svg)
                   (options (chem--build-options nil))
                   (drawing (chem-from-file path options))
                   (svg (chem-render drawing 'svg options)))
              (unless (chem-exp-svg-test--xml svg)
                (push (list path "invalid XML") failures))
              (setq count (1+ count)))
          (error
           (push (list path (error-message-string err)) failures))))
      (dolist (path (cl-subseq files 0 (min 10 (length files))))
        (let* ((chem-default-exporter 'svg)
               (options (chem--build-options nil))
               (drawing (chem-from-file path options))
               (first (chem-render drawing 'svg options))
               (second (chem-render drawing 'svg options)))
          (unless (equal first second)
            (push (list path "non-deterministic output") failures))))
      (message "chem-exp-svg corpus: %d files, %d failures"
               count (length failures))
      (should (= 0 (length failures)))
      (should (> count 0)))))

(provide 'chem-exp-svg-test)
;;; chem-exp-svg-test.el ends here
