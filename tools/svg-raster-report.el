;;; svg-raster-report.el --- Advisory SVG raster parity report -*- lexical-binding: t; -*-

;; This is a development-only tool.  It renders each SmilesDrawer reference
;; fixture and the shared-coordinate native SVG, rasterizes both with
;; Inkscape, and prints an advisory image difference.  It is not part of
;; `eldev test'.

;;; Code:

(require 'chem-svg-parity-test)
(require 'chem-svg-reaction-parity-test)

(defvar svg-raster-report--directory
  (expand-file-name "chem-svg-raster" temporary-file-directory))

(defun svg-raster-report--write (path content)
  "Write CONTENT to PATH."
  (make-directory (file-name-directory path) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file path
      (insert content))))

(defun svg-raster-report--png (svg png)
  "Rasterize SVG to PNG with Inkscape."
  (unless (executable-find "inkscape")
    (user-error "Inkscape is not available"))
  (unless (zerop (call-process "inkscape" nil nil nil
                               "--export-type=png"
                               (format "--export-filename=%s" png)
                               "--export-width=500"
                               svg))
    (user-error "Inkscape failed for %s" svg)))

(defun svg-raster-report--match-viewbox (reference native)
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

(defun svg-raster-report--compare (reference native output)
  "Compare REFERENCE and NATIVE PNG, writing OUTPUT."
  (let ((python (or (executable-find "python3")
                    (user-error "python3 is not available")))
        (script (expand-file-name "tools/compare-png.py"
                                  (file-name-directory
                                   (directory-file-name
                                    (file-name-directory
                                     (or load-file-name buffer-file-name)))))))
    (with-temp-buffer
      (unless (zerop (call-process python nil t nil script
                                   reference native output))
        (user-error "PNG comparison failed"))
      (string-trim (buffer-string)))))

(defun svg-raster-report--bond-cv (drawing)
  "Return the coefficient of variation of DRAWING's bond lengths."
  (let* ((lengths
          (mapcar (lambda (edge)
                    (chem-edge-length drawing edge))
                  (chem-visible-edges drawing)))
         (mean (/ (apply #'+ lengths) (float (length lengths))))
         (variance (/ (apply #'+ (mapcar (lambda (length)
                                           (expt (- length mean) 2))
                                         lengths))
                      (float (length lengths)))))
    (if (zerop mean) 0.0 (/ (sqrt variance) mean))))

(defun svg-raster-report--reference-cv (fixture)
  "Return the coefficient of variation of FIXTURE's bond lengths."
  (let* ((atoms (chem-svg-parity-test--list
                 (chem-svg-parity-test--get fixture "atoms")))
         (edges (chem-svg-parity-test--list
                 (chem-svg-parity-test--get fixture "edges")))
         (by-id (make-hash-table :test 'eql))
         (lengths
          (progn
            (dolist (atom atoms)
              (puthash (chem-svg-parity-test--number
                        (chem-svg-parity-test--get atom "id"))
                       atom by-id))
            (mapcar
             (lambda (edge)
               (let* ((a (gethash (chem-svg-parity-test--number
                                   (chem-svg-parity-test--get edge "a"))
                                  by-id))
                      (b (gethash (chem-svg-parity-test--number
                                   (chem-svg-parity-test--get edge "b"))
                                  by-id)))
                 (sqrt (+ (expt (- (chem-svg-parity-test--number
                                    (chem-svg-parity-test--get a "x"))
                                   (chem-svg-parity-test--number
                                    (chem-svg-parity-test--get b "x")))
                                 2)
                          (expt (- (chem-svg-parity-test--number
                                    (chem-svg-parity-test--get a "y"))
                                   (chem-svg-parity-test--number
                                    (chem-svg-parity-test--get b "y")))
                                 2)))))
             edges)))
         (mean (/ (apply #'+ lengths) (float (length lengths))))
         (variance (/ (apply #'+ (mapcar (lambda (length)
                                           (expt (- length mean) 2))
                                         lengths))
                      (float (length lengths)))))
    (if (zerop mean) 0.0 (/ (sqrt variance) mean))))

(defun svg-raster-report--run ()
  "Run the advisory raster and layout report."
  (interactive)
  (make-directory svg-raster-report--directory t)
  (dolist (entry (chem-svg-parity-test--strict-fixtures))
    (let* ((name (chem-svg-parity-test--get entry "name"))
           (fixture (chem-svg-parity-test--load name))
           (options (chem-svg-parity-test--viewbox-options entry))
           (drawing (chem-svg-parity-test--build-drawing fixture))
           (native-svg (chem-render drawing 'svg options))
           (reference-svg
            (svg-raster-report--match-viewbox
             (chem-svg-parity-test--get fixture "svg") native-svg))
           (reference-png (expand-file-name (concat name "-reference.png")
                                            svg-raster-report--directory))
           (native-png (expand-file-name (concat name "-native.png")
                                          svg-raster-report--directory))
           (comparison-png (expand-file-name (concat name "-comparison.png")
                                              svg-raster-report--directory))
           (reference-cv (svg-raster-report--reference-cv fixture))
           (native-cv
            (condition-case nil
                (let ((layout-drawing
                       (chem-from-string
                        (chem-svg-parity-test--get fixture "smiles")
                        options)))
                  (svg-raster-report--bond-cv layout-drawing))
              (error nil))))
      (svg-raster-report--write
       (expand-file-name (concat name "-reference.svg")
                         svg-raster-report--directory)
       reference-svg)
      (svg-raster-report--write
       (expand-file-name (concat name "-native.svg")
                         svg-raster-report--directory)
       native-svg)
      (svg-raster-report--png
       (expand-file-name (concat name "-reference.svg")
                         svg-raster-report--directory)
       reference-png)
      (svg-raster-report--png
       (expand-file-name (concat name "-native.svg")
                         svg-raster-report--directory)
       native-png)
      (princ (format "%s: %s; bond-cv reference=%.3f native=%s\n"
                     name
                     (svg-raster-report--compare reference-png native-png
                                                 comparison-png)
                     reference-cv
                     (if native-cv (format "%.3f" native-cv) "n/a"))))))

(defun svg-raster-report--run-reactions ()
  "Run the advisory raster report for reaction fixtures."
  (make-directory svg-raster-report--directory t)
  (let ((options (chem-svg-reaction-parity-test--options)))
    (dolist (entry (chem-svg-reaction-parity-test--list
                    (chem-svg-reaction-parity-test--get
                     (chem-svg-reaction-parity-test--manifest) "fixtures")))
      (let* ((name (chem-svg-reaction-parity-test--get entry "name"))
             (fixture (chem-svg-reaction-parity-test--load name))
             (reaction
              (chem-svg-reaction-parity-test--build-reaction fixture options))
             (native-svg (chem-render reaction 'svg options))
             (reference-svg
              (svg-raster-report--match-viewbox
               (chem-svg-reaction-parity-test--get fixture "svg") native-svg))
             (prefix (concat "reaction-" name))
             (reference-png (expand-file-name (concat prefix "-reference.png")
                                              svg-raster-report--directory))
             (native-png (expand-file-name (concat prefix "-native.png")
                                            svg-raster-report--directory))
             (comparison-png
              (expand-file-name (concat prefix "-comparison.png")
                                svg-raster-report--directory)))
        (svg-raster-report--write
         (expand-file-name (concat prefix "-reference.svg")
                           svg-raster-report--directory)
         reference-svg)
        (svg-raster-report--write
         (expand-file-name (concat prefix "-native.svg")
                           svg-raster-report--directory)
         native-svg)
        (svg-raster-report--png
         (expand-file-name (concat prefix "-reference.svg")
                           svg-raster-report--directory)
         reference-png)
        (svg-raster-report--png
         (expand-file-name (concat prefix "-native.svg")
                           svg-raster-report--directory)
         native-png)
        (princ (format "%s: %s\n"
                       prefix
                       (svg-raster-report--compare reference-png native-png
                                                   comparison-png)))))))

(svg-raster-report--run)
(svg-raster-report--run-reactions)

;;; svg-raster-report.el ends here
