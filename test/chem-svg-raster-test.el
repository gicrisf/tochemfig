;;; chem-svg-raster-test.el --- Raster parity gates -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Giovanni Crisalfi

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Hard raster gates for a curated shared-coordinate fixture set.  The native
;; renderer and the SmilesDrawer reference use the same coordinates, so these
;; gates protect the SVG renderer rather than the Indigo layout engine.
;;
;; The test skips when Inkscape or Pillow is unavailable.

;;; Code:

(require 'ert)

(let ((here (file-name-directory (or load-file-name buffer-file-name))))
  (when here (add-to-list 'load-path here)))

(require 'chem-svg-parity-test)
(require 'chem-svg-reaction-parity-test)

(defconst chem-svg-raster-test--script
  (expand-file-name "../tools/compare-png.py" chem-svg-parity-test--root)
  "Path to the PNG comparison helper.")

(defconst chem-svg-raster-test--limits
  '(("ethanol"          0.90  9.5  1.5)
    ("benzene"          2.50 20.0  4.0)
    ("indole"           2.90 22.0  5.0)
    ("naphthalene"      3.10 23.0  5.6)
    ("caffeine_aromatic" 2.50 20.0 4.6)
    ("caffeine_kekule"  1.80 17.0  3.6)
    ("alanine"          1.00 11.0  1.8)
    ("alanine_down"     1.00 11.0  1.8)
    ("butene_e"         1.10 12.0  1.9)
    ("bicyclic"         2.10 18.0  3.2)
    ("allene"           2.80 23.0  3.0)
    ("nitrobenzene"     2.30 19.0  4.0)
    ("spiro_decane"     1.80 16.0  3.2)
    ("quinoline"        3.50 23.5  6.5)
    ("tetramethylammonium" 1.20 12.0 2.5)
    ("two_stereocenters" 1.60 15.5 3.4)
    ("terminal_alkyne"  1.60 15.5  3.0))
  "Maximum (MAE RMS EDGE) for curated raster fixtures.")

(defun chem-svg-raster-test--write (path content)
  "Write CONTENT to PATH."
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file path
      (insert content))))

(defun chem-svg-raster-test--png (svg png)
  "Rasterize SVG to PNG with Inkscape."
  (unless (zerop (call-process "inkscape" nil nil nil
                               "--export-type=png"
                               (format "--export-filename=%s" png)
                               "--export-width=500"
                               "--export-height=500"
                               svg))
    (error "Inkscape failed for %s" svg)))

(defun chem-svg-raster-test--match-viewbox (reference native)
  "Return REFERENCE with NATIVE's viewBox.
Native text bounds can intentionally exceed SmilesDrawer's.  Rasterizing the
reference with the native viewBox keeps both drawings at the same scale and
reveals text that the reference viewBox would otherwise clip."
  (if (string-match "viewBox=\"\\([^\"]*\\)\"" native)
      (replace-regexp-in-string "viewBox=\"[^\"]*\""
                                (format "viewBox=\"%s\""
                                        (match-string 1 native))
                                reference)
    reference))

(defun chem-svg-raster-test--compare (reference native)
  "Return (MAE RMS EDGE) for REFERENCE and NATIVE PNGs."
  (let ((output (expand-file-name "comparison.png"
                                  (file-name-directory native))))
    (with-temp-buffer
      (unless (zerop (call-process "python3" nil t nil chem-svg-raster-test--script
                                   reference native output))
        (error "PNG comparison failed"))
      (unless (string-match
               "mae=\\([0-9.]+\\) rms=\\([0-9.]+\\) edge_error=\\([0-9.]+\\)"
               (buffer-string))
        (error "Unexpected comparison output: %s" (buffer-string)))
      (list (string-to-number (match-string 1 (buffer-string)))
            (string-to-number (match-string 2 (buffer-string)))
            (string-to-number (match-string 3 (buffer-string)))))))

(ert-deftest chem-svg-raster-metadata ()
  "Raster gate names have metadata and reference SVGs."
  (dolist (entry chem-svg-raster-test--limits)
    (let* ((name (car entry))
           (fixture (chem-svg-parity-test--load name)))
      (should (eq t (chem-svg-parity-test--get fixture "raster")))
      (should (stringp (chem-svg-parity-test--get fixture "svg"))))))

(ert-deftest chem-svg-raster-gates ()
  "Curated raster parity gates for the SVG renderer."
  (unless (and (executable-find "inkscape")
               (executable-find "python3"))
    (ert-skip "Inkscape or python3 is unavailable"))
  (unless (zerop (call-process "python3" nil nil nil "-c" "import PIL"))
    (ert-skip "Pillow is unavailable"))
  (let ((directory (make-temp-file "chem-svg-raster-" t)))
    (unwind-protect
        (dolist (entry chem-svg-raster-test--limits)
          (let* ((name (car entry))
                 (limits (cdr entry))
                 (fixture (chem-svg-parity-test--load name))
                 (drawing (chem-svg-parity-test--build-drawing fixture))
                 (native-svg
                  (chem-render drawing 'svg
                               (chem-svg-parity-test--viewbox-options fixture)))
                 (reference-svg
                  (chem-svg-raster-test--match-viewbox
                   (chem-svg-parity-test--get fixture "svg") native-svg))
                 (reference-svg-path
                  (expand-file-name (concat name "-reference.svg") directory))
                 (native-svg-path
                  (expand-file-name (concat name "-native.svg") directory))
                 (reference-png
                  (expand-file-name (concat name "-reference.png") directory))
                 (native-png
                  (expand-file-name (concat name "-native.png") directory)))
            (chem-svg-raster-test--write reference-svg-path reference-svg)
            (chem-svg-raster-test--write native-svg-path native-svg)
            (chem-svg-raster-test--png reference-svg-path reference-png)
            (chem-svg-raster-test--png native-svg-path native-png)
            (let* ((metrics (chem-svg-raster-test--compare
                             reference-png native-png))
                   (mae (nth 0 metrics))
                   (rms (nth 1 metrics))
                   (edge (nth 2 metrics)))
              (unless (and (<= mae (nth 0 limits))
                           (<= rms (nth 1 limits))
                           (<= edge (nth 2 limits)))
                (ert-fail
                 (format "%s raster regression: mae=%.3f/%.3f rms=%.3f/%.3f edge=%.3f/%.3f"
                         name mae (nth 0 limits)
                         rms (nth 1 limits)
                         edge (nth 2 limits)))))))
      (delete-directory directory t))))

(defconst chem-svg-raster-test--reaction-limits
  '(("esterification" 2.0 18.0 2.8)
    ("diels_alder" 3.7 27.0 4.8)
    ("two_products" 2.8 25.0 3.2)
    ("stereo" 5.1 30.0 8.0)
    ("agent_above" 2.4 23.0 3.7)
    ("charged_salt" 3.5 28.0 4.2))
  "Maximum (MAE RMS EDGE) for curated reaction raster fixtures.")

(ert-deftest chem-svg-raster-reaction-metadata ()
  "Reaction raster gate names have metadata and reference SVGs."
  (dolist (entry chem-svg-raster-test--reaction-limits)
    (let* ((name (car entry))
           (fixture (chem-svg-reaction-parity-test--load name)))
      (should (eq t (chem-svg-reaction-parity-test--get fixture "raster")))
      (should (stringp (chem-svg-reaction-parity-test--get fixture "svg")))
      (should (chem-svg-reaction-parity-test--get fixture "components")))))

(ert-deftest chem-svg-raster-reaction-gates ()
  "Curated raster parity gates for reaction composition."
  (unless (and (executable-find "inkscape")
               (executable-find "python3"))
    (ert-skip "Inkscape or python3 is unavailable"))
  (unless (zerop (call-process "python3" nil nil nil "-c" "import PIL"))
    (ert-skip "Pillow is unavailable"))
  (let ((directory (make-temp-file "chem-svg-reaction-raster-" t))
        (options (chem-svg-reaction-parity-test--options)))
    (unwind-protect
        (dolist (entry chem-svg-raster-test--reaction-limits)
          (let* ((name (car entry))
                 (limits (cdr entry))
                 (fixture (chem-svg-reaction-parity-test--load name))
                 (reaction
                  (chem-svg-reaction-parity-test--build-reaction
                   fixture options))
                 (native-svg (chem-render reaction 'svg options))
                 (reference-svg
                  (chem-svg-raster-test--match-viewbox
                   (chem-svg-reaction-parity-test--get fixture "svg")
                   native-svg))
                 (reference-svg-path
                  (expand-file-name (concat name "-reference.svg") directory))
                 (native-svg-path
                  (expand-file-name (concat name "-native.svg") directory))
                 (reference-png
                  (expand-file-name (concat name "-reference.png") directory))
                 (native-png
                  (expand-file-name (concat name "-native.png") directory)))
            (chem-svg-raster-test--write reference-svg-path reference-svg)
            (chem-svg-raster-test--write native-svg-path native-svg)
            (chem-svg-raster-test--png reference-svg-path reference-png)
            (chem-svg-raster-test--png native-svg-path native-png)
            (let* ((metrics (chem-svg-raster-test--compare
                             reference-png native-png))
                   (mae (nth 0 metrics))
                   (rms (nth 1 metrics))
                   (edge (nth 2 metrics)))
              (unless (and (<= mae (nth 0 limits))
                           (<= rms (nth 1 limits))
                           (<= edge (nth 2 limits)))
                (ert-fail
                 (format "%s reaction raster regression: mae=%.3f/%.3f rms=%.3f/%.3f edge=%.3f/%.3f"
                         name mae (nth 0 limits)
                         rms (nth 1 limits)
                         edge (nth 2 limits)))))))
      (delete-directory directory t))))

(provide 'chem-svg-raster-test)
;;; chem-svg-raster-test.el ends here
