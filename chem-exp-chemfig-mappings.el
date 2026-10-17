;;; chem-exp-chemfig-mappings.el --- Chemfig code formatting -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2025 Giovanni Crisalfi

;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Translation of mol2chemfigPy3's `chemfig_mappings.py'.  This module knows
;; how to turn atoms and bonds into chemfig syntax; it does not know anything
;; about molecules as such.
;;
;; Most functions mirror their Python counterparts closely so that the output
;; stays byte-for-byte identical.  Option values are passed around as a plist
;; with keyword keys (`:terse', `:relative-angles', `:angle-round', ...).

;;; Code:

(require 'cl-lib)

;;; Constants

(defconst chem--bond-code-width 50
  "Space reserved for bonds before the atom code; trimmed by `dedent'.")

(defconst chem--terse-line-width 75
  "Maximum line width when chunking terse output.")

(defconst chem--bond-codes
  '((double . "=") (triple . "~") (upto . "<") (downto . "<:")
    (upfrom . ">") (downfrom . ">:"))
  "Bond type to chemfig bond code; anything else defaults to \"-\".")

(defconst chem--bond-type-tikz
  '((link . "draw=none") (either . "mcfwavy"))
  "Bond type to TikZ style.")

(defconst chem--bond-styles
  '((cross . "mcfx={%(bgstart)s}{%(bgend)s}")
    (double_left . "dbl={%(start)s}{%(end)s}")
    (double_right . "dbr={%(start)s}{%(end)s}")
    (triple . "trpl={%(start)s}{%(end)s}")
    (cross_double_left . "dblx={%(start)s}{%(end)s}{%(bgstart)s}{%(bgend)s}")
    (cross_double_right . "dbrx={%(start)s}{%(end)s}{%(bgstart)s}{%(bgend)s}")
    (cross_triple . "trplx={%(start)s}{%(end)s}{%(bgstart)s}{%(bgend)s}"))
  "Combined TikZ style templates, keyed by a joined style name.")

(defconst chem--bond-style-shortcuts
  '(("dbr={58}{58}" . "drh") ("dbl={58}{58}" . "dlh")
    ("dbr={0}{58}" . "drhe") ("dbl={0}{58}" . "dlhe")
    ("dbr={58}{0}" . "drhs") ("dbl={58}{0}" . "dlhs")
    ("dbr={0}{0}" . "drn") ("dbl={0}{0}" . "dln"))
  "Shortcut aliases for common hexagon double-bond styles.")

(defconst chem--plus-charge "\\mcfplus")
(defconst chem--minus-charge "\\mcfminus")

(defconst chem--radical-templates
  '((east . "\\lewis{0%s,%s}") (north . "\\lewis{2%s,%s}")
    (west . "\\lewis{4%s,%s}") (south . "\\lewis{6%s,%s}"))
  "Legacy \\lewis templates for radical electrons (ChemFig < 1.6).")

(defconst chem--radical-templates2
  '((east . "\\charge{0=\\%s}{%s}") (north . "\\charge{90=\\%s}{%s}")
    (west . "\\charge{180=\\%s}{%s}") (south . "\\charge{270=\\%s}{%s}"))
  "Modern \\charge templates for radical electrons (ChemFig >= 1.6).")

;;; Small helpers

(defun chem--num-round (num sig)
  "Round NUM to SIG decimal digits, returning an integer when integral."
  (let ((res (/ (float (round (* num (expt 10 sig)))) (expt 10 sig))))
    (if (= res (truncate res)) (truncate res) res)))

(defun chem--str (v)
  "Return V as a string, the way Python's `str' would."
  (cond ((stringp v) v)
        ((integerp v) (number-to-string v))
        ((floatp v) (number-to-string v))
        (t (format "%s" v))))

(defun chem--fill-template (template data)
  "Substitute %(name)s placeholders in TEMPLATE using alist DATA."
  (let ((result template))
    (dolist (pair data result)
      (setq result
            (replace-regexp-in-string
             (concat "%(" (regexp-quote (symbol-name (car pair))) ")s")
             (lambda (_) (chem--str (cdr pair)))
             result t t)))))

(defun chem--format-angle (options angle parent-angle)
  "Format the angle specifier for a bond.
OPTIONS is the option plist; PARENT-ANGLE is the incoming bond angle."
  (let* ((relative (plist-get options :relative-angles))
         (use-relative (and relative (not (null parent-angle))))
         (a (if use-relative (- angle parent-angle) angle))
         (prefix (if use-relative "::" ":"))
         (rounded (chem--num-round a (or (plist-get options :angle-round) 1))))
    (concat prefix (chem--str (mod rounded 360)))))

(defun chem--specifier-default (val default)
  "Return VAL as string, or the empty string when it equals DEFAULT."
  (if (equal val default) "" (chem--str val)))

(defun chem--format-marker (marker)
  "Wrap MARKER in the chemfig marker syntax, or return nil."
  (when (and marker (not (equal marker "")))
    (format "@{%s}" marker)))

;;; Bond formatting

(cl-defun chem--format-bond (options angle parent-angle bond-type clockwise
                                          is-last length departure arrival
                                          tikz-styles tikz-values marker)
  "Render one bond.
OPTIONS is the option plist.  Return the chemfig bond code string."
  (when (null angle)
    (cl-return-from chem--format-bond ""))
  (let* ((angle (chem--format-angle options angle parent-angle))
         (angle (chem--specifier-default angle ":0"))
         (length (chem--num-round length (or (plist-get options :bond-round) 3)))
         (length (chem--specifier-default length 1))
         (departure (chem--specifier-default departure 0))
         (arrival (chem--specifier-default arrival 0))
         (bond-code (or (cdr (assq bond-type chem--bond-codes)) "-"))
         (tikz-filled nil))
    (let ((btt (cdr (assq bond-type chem--bond-type-tikz))))
      (when btt (push btt tikz-filled)))
    (when tikz-styles
      (let* ((key (mapconcat #'identity (sort (copy-sequence tikz-styles) #'string<) "_"))
             (template (or (cdr (assoc (intern key) chem--bond-styles)) ""))
             (tikz (chem--fill-template template tikz-values)))
        (push tikz tikz-filled)
        (when (and (string-match-p "cross" key) (not is-last))
          (setq departure ""))))
    (let* ((tikz (mapconcat #'identity (nreverse tikz-filled) ","))
           (tikz (or (cdr (assoc tikz chem--bond-style-shortcuts)) tikz))
           (specifiers (mapconcat #'chem--str
                                  (list angle length departure arrival tikz) ","))
           (specifiers (replace-regexp-in-string ",+$" "" specifiers)))
      (when (and marker (not (equal marker "")))
        (setq specifiers (concat (chem--format-marker marker) specifiers)))
      (when (not (equal specifiers ""))
        (setq specifiers (concat "[" specifiers "]")))
      (let ((modifier (if (and (eq bond-type 'double) (/= clockwise 0))
                          (if (= clockwise 1) "_" "^")
                        "")))
        (concat bond-code modifier specifiers)))))

;;; Atom formatting

(defconst chem--atom-templates
  '((atom_no (empty . "\\mcfatomno{%(number)s}") (east . "\\mcfright{%(element)s}{\\mcfatomno{%(number)s}}") (west . "\\mcfleft{\\mcfatomno{%(number)s}}{%(element)s}") (north . "\\mcfabove{%(element)s}{\\mcfatomno{%(number)s}}") (south . "\\mcfbelow{%(element)s}{\\mcfatomno{%(number)s}}"))
    (neutral (one_h (east . "%(element)sH") (west . "H%(element)s") (north . "\\mcfabove{%(element)s}{H}") (south . "\\mcfbelow{%(element)s}{H}"))
             (more_h (east . "%(element)sH_%(hydrogens)s") (west . "H_%(hydrogens)s%(element)s") (north . "\\mcfabove{%(element)s}{\\mcfright{H}{_%(hydrogens)s}}") (south . "\\mcfbelow{%(element)s}{\\mcfright{H}{_%(hydrogens)s}}")))
    (charged (no_h (top_right . "\\mcfright{%(element)s}{^{%(charge)s}}") (top_left . "^{%(charge)s}%(element)s") (top_center . "\\mcfabove{%(element)s}{_{%(charge)s}}") (bottom_right . "\\mcfright{%(element)s}{_{%(charge)s}}") (bottom_left . "_{%(charge)s}%(element)s") (bottom_center . "\\mcfbelow{%(element)s}{^{%(charge)s}}"))
             (one_h (east . "%(element)sH^{%(charge)s}") (west . "^{%(charge)s}H%(element)s") (north . "\\mcfaboveright{%(element)s}{H}{^{%(charge)s}}") (south . "\\mcfbelowright{%(element)s}{H}{^{%(charge)s}}"))
             (more_h (east . "%(element)sH_%(hydrogens)s^{%(charge)s}") (west . "^{%(charge)s}H_%(hydrogens)s%(element)s") (north . "\\mcfaboveright{%(element)s}{H}{^{%(charge)s}_%(hydrogens)s}") (south . "\\mcfbelowright{%(element)s}{H}{^{%(charge)s}_%(hydrogens)s}"))))
  "Nested template table, mirroring mol2chemfig's `atom_templates'.")

(defconst chem--atom-string-pos
  '((atom_no (empty . 0) (east . 0) (west . 0) (north . 0) (south . 0))
    (neutral (one_h (east . 1) (west . 2) (north . 0) (south . 0))
             (more_h (east . 1) (west . 2) (north . 0) (south . 0)))
    (charged (no_h (top_right . 0) (top_left . 2) (top_center . 0) (bottom_right . 0) (bottom_left . 2) (bottom_center . 0))
             (one_h (east . 1) (west . 3) (north . 0) (south . 0))
             (more_h (east . 1) (west . 3) (north . 0) (south . 0))))
  "String positions (bond attachment points) for the atom templates.")

(defun chem--template (table keys)
  "Drill into nested alist TABLE following KEYS."
  (let ((thing table))
    (dolist (key keys)
      (setq thing (cdr (assq key thing))))
    thing))

(defun chem--fill-atom (keys data phantom &optional phantom-pos)
  "Finalize atom code using templates KEYS and DATA.
Return (template-string string-position phantom phantom-position)."
  (let ((template (chem--template chem--atom-templates keys))
        (string-pos (chem--template chem--atom-string-pos keys)))
    (list (chem--fill-template template data)
          string-pos
          phantom
          (or phantom-pos 0))))

(cl-defun chem--format-atom (options idx label number element hydrogens charge radical
                                          first-quadrant second-quadrant charge-angle)
  "Render an atom.
LABEL is the display label, or nil for a hidden carbon.  NUMBER is the
1-based atom number when numbering is requested.  Return
\(atom-code string-position phantom phantom-position).  IDX is 1-based."
  (let* ((radicals (if (plist-get options :legacy-lewis)
                       chem--radical-templates
                     chem--radical-templates2))
         (radical-element
          (if (or (null radical) (= radical 0))
              element
            (let ((symbol (if (= radical 1) "." ":"))
                  (quadrant (if (and hydrogens (> hydrogens 0))
                                second-quadrant first-quadrant)))
              (format (cdr (assq (intern quadrant) radicals)) symbol element))))
         (data (list (cons 'number (or number idx))
                     (cons 'hydrogens hydrogens)
                     (cons 'element radical-element)))
         (element-phantom (format "\\phantom{%s}" (cdr (assoc 'element data)))))
    ;; Atom numbers take precedence.
    (when number
      (if (null label)
          (cl-return-from chem--format-atom
            (chem--fill-atom '(atom_no empty) data (format "\\phantom{%s}" number)))
        (cl-return-from chem--format-atom
          (chem--fill-atom (list 'atom_no (intern first-quadrant)) data element-phantom))))
    ;; Hidden carbon.
    (when (null label)
      (cl-return-from chem--format-atom
        (list "" 0 "" 0)))
    (if (or (null charge) (= charge 0))
        ;; Neutral atoms.
        (cond
         ((or (null hydrogens) (= hydrogens 0))
          (list (cdr (assoc 'element data)) 0 element-phantom 0))
         (t
          (let ((keys (if (= hydrogens 1)
                          (list 'neutral 'one_h (intern first-quadrant))
                        (list 'neutral 'more_h (intern first-quadrant)))))
            (chem--fill-atom keys data element-phantom))))
      ;; Charged atoms.
      (let* ((charge-macro (if (> charge 0)
                               chem--plus-charge
                             chem--minus-charge))
             (charge-str (if (> (abs charge) 1)
                             (concat (number-to-string (abs charge)) charge-macro)
                           charge-macro))
             (data (append data (list (cons 'charge charge-str))))
             (keys (cond
                    ((or (null hydrogens) (= hydrogens 0))
                     (list 'charged 'no_h (intern (or charge-angle "top_right"))))
                    ((= hydrogens 1)
                     (list 'charged 'one_h (intern first-quadrant)))
                    (t
                     (list 'charged 'more_h (intern first-quadrant))))))
        (chem--fill-atom keys data element-phantom)))))

(defun chem--format-atom-comment (options idx)
  "Return the end-of-line comment for atom IDX, or empty when terse."
  (if (plist-get options :terse) "" (number-to-string idx)))

(defun chem--format-closure-comment (options idx)
  "Return the end-of-line comment for a ring closure to IDX."
  (if (plist-get options :terse) "" (format "-> %s" idx)))

(defun chem--format-aromatic-ring (options angle parent-angle length radius)
  "Return (ring-bond-code ring-code comment) for an aromatic circle."
  (let* ((values (list (cons 'angle (chem--format-angle options angle parent-angle))
                       (cons 'length (chem--specifier-default length 1))))
         (ring-bond-code (chem--fill-template "-[%(angle)s,%(length)s,,,draw=none]" values))
         (ring-code (format "\\mcfcringle{%s}" radius))
         (comment (if (plist-get options :terse) "" "(o)")))
    (list ring-bond-code ring-code comment)))

;;; Output formatting

(defun chem--strip-output (lines)
  "Strip whitespace and comments from LINES, then chunk them for terse output."
  (let ((stripped (mapcar (lambda (line)
                            (string-trim (car (split-string line "%"))))
                          lines)))
    (let ((chunked nil) (acc ""))
      (while stripped
        (let ((popped (pop stripped)))
          (if (> (+ (length acc) (length popped)) chem--terse-line-width)
              (progn (push acc chunked) (setq acc popped))
            (setq acc (concat acc popped)))))
      (when (not (equal acc ""))
        (push acc chunked))
      (nreverse chunked))))

(defun chem--dedent (lines)
  "Remove the common leading whitespace from LINES."
  (let ((margin nil))
    (dolist (line lines)
      (unless (string-match-p "\\`[ \t]*\\'" line)
        (let ((ws (progn (string-match "\\`\\([ \t]*\\)" line) (match-string 1 line))))
          (when (or (null margin) (< (length ws) (length margin)))
            (setq margin ws)))))
    (if (null margin)
        lines
      (mapcar (lambda (line)
                (cond ((string-match-p "\\`[ \t]*\\'" line) "")
                      ((string-prefix-p margin line) (substring line (length margin)))
                      (t line)))
              lines))))

(defun chem--format-output (options output-list)
  "Wrap and prettify OUTPUT-LIST according to OPTIONS.  Returns a string."
  (let* ((indent (make-string (or (plist-get options :indent) 4) ?\s))
         (out (chem--dedent (split-string (mapconcat #'identity output-list "\n") "\n")))
         (out (mapcar (lambda (line) (concat indent line)) out))
         (submol (plist-get options :submol-name))
         (chemfig (plist-get options :wrap-chemfig)))
    (cond
     (submol
      (push (format "\\definesubmol{%s}{" submol) out)
      (setq out (append out (list "}"))))
     (chemfig
      (push "\\chemfig{" out)
      (setq out (append out (list "}")))))
    (if (plist-get options :terse)
        (mapconcat #'identity (chem--strip-output out) "%\n")
      (mapconcat #'identity out "\n"))))

(provide 'chem-exp-chemfig-mappings)
;;; chem-exp-chemfig-mappings.el ends here
