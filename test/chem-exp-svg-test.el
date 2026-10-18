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

(defun chem-exp-svg-test--render (smiles &optional overrides)
  "Render SMILES to SVG with OVERRIDES."
  (let* ((chem-default-exporter 'svg)
         (options (chem--build-options overrides))
         (drawing (chem-from-string smiles options)))
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
  "Bounds and coordinate mapping use the configured scale and padding."
  (let* ((chem-default-exporter 'svg)
         (options (chem--build-options '(("svg-scale" . 10.0)
                                         ("svg-padding" . 1.0))))
         (drawing (chem-from-string "CCO" options))
         (state (chem-exp-svg--make-state drawing options))
         (bounds (chem-exp-svg--bounds state))
         (point (chem-exp-svg--point state (nth 0 bounds) (nth 3 bounds)))
         (viewbox (chem-exp-svg--viewbox state)))
    (should (= 1.0 (car point)))
    (should (= 1.0 (cdr point)))
    (should (= 0.0 (nth 0 viewbox)))
    (should (= 0.0 (nth 1 viewbox)))
    (should (> (nth 2 viewbox) 0.0))
    (should (> (nth 3 viewbox) 0.0))))

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
  "Width and height are emitted only when requested."
  (let* ((svg (chem-exp-svg-test--render "CCO"))
         (root (chem-exp-svg-test--root svg)))
    (should-not (string-match-p " width=\"" root))
    (should-not (string-match-p " height=\"" root)))
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

(ert-deftest chem-exp-svg-aromatic-lines ()
  "Aromatic bonds fall back to single lines until ring rendering lands."
  (should (= 6 (chem-exp-svg-test--count
                "<line " (chem-exp-svg-test--render "c1ccccc1")))))

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
    (should (string-match-p ">OH</text>" svg))))

(ert-deftest chem-exp-svg-caffeine ()
  "Caffeine renders as valid SVG."
  (let ((svg (chem-exp-svg-test--render "CN1C=NC2=C1C(=O)N(C(=O)N2C)C")))
    (should (chem-exp-svg-test--xml svg))
    (should (> (chem-exp-svg-test--count "<text " svg) 0))
    (should (> (chem-exp-svg-test--count "<line " svg) 0))))

(ert-deftest chem-exp-svg-atom-numbers ()
  "Atom numbers replace hydrogen and charge annotations."
  (let ((svg (chem-exp-svg-test--render "CCO" '(("atom-numbers" . t)))))
    (should (= 3 (chem-exp-svg-test--count "<text " svg)))
    (should (string-match-p ">1</text>" svg))
    (should (string-match-p ">2</text>" svg))
    (should (string-match-p ">O3</text>" svg))
    (should-not (string-match-p ">H" svg))))

(ert-deftest chem-exp-svg-charge-and-hydrogens ()
  "Charges and hydrogens use Unicode sub/superscripts."
  (let ((svg (chem-exp-svg-test--render "[NH4+]")))
    (should (string-match-p ">NH₄⁺</text>" svg))))

(ert-deftest chem-exp-svg-radical ()
  "Radicals are rendered as superscript dots."
  (let ((svg (chem-exp-svg-test--render "[CH3]")))
    (should (string-match-p ">CH₃·</text>" svg))))

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

(provide 'chem-exp-svg-test)
;;; chem-exp-svg-test.el ends here
