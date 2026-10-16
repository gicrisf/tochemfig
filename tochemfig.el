;;; tochemfig.el --- Generate chemfig code from molecules -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2025 Giovanni Crisalfi

;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Maintainer: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Created: novembre 04, 2022
;; Modified: 2025
;; Version: 0.3.0
;; Keywords: chemistry mol smiles chemfig convenience data extensions files languages lisp tex tools
;; Homepage: https://github.com/gicrisf/tochemfig
;; Package-Requires: ((emacs "29.1") (indigo "0.10.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides an Emacs interface for generating chemfig (LaTeX)
;; code from molecular structures.  It uses the emacs-indigo library for
;; molecular structure manipulation and generates chemfig output natively
;; in Elisp.
;;
;; Supported input formats:
;; - SMILES strings (direct input)
;; - MOL files
;;
;; Usage:
;;   M-x tochemfig-input-direct RET <SMILES> RET
;;   M-x tochemfig-input-file RET <path-to-mol-file> RET
;;
;; For a context-aware experience enable `tochemfig-mode' (see
;; tochemfig-mode.el), which adds commands under the `C-c m' prefix and a
;; transient options menu.

;;; Code:

(require 'indigo)
;; Emacs-indigo only defines its resource-management macros (`indigo-with-*')
;; once its dynamic module has loaded (they live in `indigo-mol'/`indigo-iter',
;; required from inside `indigo-load-module').  When byte-compilation happens in
;; an environment where the module cannot be loaded, those macros are absent and
;; the compiler bakes in unexpanded calls (which then fail at runtime with
;; "void-function mol").  Load the pure-Elisp macro libraries explicitly so
;; compilation never depends on the module.
(eval-when-compile (require 'indigo-mol))
(require 'tochemfig-molecule)

(unless (macrop 'indigo-with-molecule)
  (error "tochemfig requires emacs-indigo (>= 0.10.1), but the \
`indigo-with-molecule' macro is unavailable.  Install or upgrade emacs-indigo \
\(e.g. `M-x straight-pull-package RET indigo' or `doom sync -u') and rebuild \
tochemfig"))

;;; Customization

(defgroup tochemfig nil
  "Generate chemfig LaTeX code from molecular structures."
  :prefix "tochemfig-"
  :group 'applications)

(defcustom tochemfig-default-input 'direct
  "Default input mode for molecule specification.
With `file', expects a filename.
With `direct', the argument is interpreted as a SMILES string."
  :group 'tochemfig
  :type '(choice (const :tag "SMILES string" direct)
                 (const :tag "File path" file)))

(defcustom tochemfig-default-terse nil
  "Remove all whitespace from the output when non-nil."
  :group 'tochemfig
  :type 'boolean)

(defcustom tochemfig-default-strict t
  "Abide by Indigo's chemical structure validation when non-nil.
When nil, atoms whose implicit hydrogens cannot be computed are silently
treated as having none instead of signalling an error."
  :group 'tochemfig
  :type 'boolean)

(defcustom tochemfig-default-indent 4
  "Number of spaces to use for indenting molecule branches in generated code.
Without effect when `tochemfig-default-terse' is non-nil."
  :group 'tochemfig
  :type 'integer)

(defcustom tochemfig-default-recalculate-coordinates nil
  "Discard existing coordinates and calculate new ones when non-nil.
For SMILES input this is performed implicitly."
  :group 'tochemfig
  :type 'boolean)

(defcustom tochemfig-default-angle 0.0
  "Rotate molecule counterclockwise by this angle (in degrees)."
  :group 'tochemfig
  :type 'number)

(defcustom tochemfig-default-relative-angles nil
  "Use relative bond angles when non-nil."
  :group 'tochemfig
  :type 'boolean)

(defcustom tochemfig-default-flip nil
  "Flip the structure horizontally when non-nil."
  :group 'tochemfig
  :type 'boolean)

(defcustom tochemfig-default-flop nil
  "Flip the structure vertically when non-nil."
  :group 'tochemfig
  :type 'boolean)

(defcustom tochemfig-default-show-carbons nil
  "Show element symbol for carbon atoms when non-nil."
  :group 'tochemfig
  :type 'boolean)

(defcustom tochemfig-default-show-methyls nil
  "Show element symbols for methyl groups when non-nil.
Implied if `tochemfig-default-show-carbons' is non-nil."
  :group 'tochemfig
  :type 'boolean)

(defcustom tochemfig-default-hydrogens 'keep
  "How to deal with explicit hydrogen atoms.
One of `keep', `add', or `delete'.
Note that `add' will also trigger calculation of new coordinates."
  :group 'tochemfig
  :type '(choice (const :tag "Keep as-is" keep)
                 (const :tag "Add explicit H" add)
                 (const :tag "Remove explicit H" delete)))

(defcustom tochemfig-default-aromatic-circles nil
  "Draw a circle instead of double bonds inside aromatic rings when non-nil."
  :group 'tochemfig
  :type 'boolean)

(defcustom tochemfig-default-fancy-bonds nil
  "Draw fancier double and triple bonds when non-nil."
  :group 'tochemfig
  :type 'boolean)

(defcustom tochemfig-default-atom-numbers nil
  "Show the molfile number of each atom next to it when non-nil.
When this option is set, charges and implicit hydrogens will not be shown."
  :group 'tochemfig
  :type 'boolean)

(defcustom tochemfig-default-bond-scale 'normalize
  "How to scale the lengths of bonds.
One of `keep', `scale', or `normalize'."
  :group 'tochemfig
  :type '(choice (const :tag "Keep original lengths" keep)
                 (const :tag "Scale by factor" scale)
                 (const :tag "Normalize to average" normalize)))

(defcustom tochemfig-default-bond-stretch 1.0
  "Scaling factor or target average for bond lengths.
The meaning depends on `tochemfig-default-bond-scale':
- With `scale': multiplication factor for all bond lengths
- With `normalize': target average bond length"
  :group 'tochemfig
  :type 'number)

(defcustom tochemfig-default-markers nil
  "Prefix for atom and bond markers, or nil to disable them.
With value \"a\", atom 2 will be labeled @{a2} and its bond to atom 5
@{a2-5}."
  :group 'tochemfig
  :type '(choice (const :tag "Disabled" nil) string))

(defcustom tochemfig-default-cross-bond nil
  "Bonds to draw on top of others they cross over.
A list of (START END) atom number pairs, using 1-based molfile numbering."
  :group 'tochemfig
  :type '(repeat (list integer integer)))

(defcustom tochemfig-default-legacy-lewis nil
  "Use the legacy \\lewis macro for radicals (ChemFig < 1.6) when non-nil."
  :group 'tochemfig
  :type 'boolean)

(defcustom tochemfig-default-wrap-chemfig nil
  "Wrap generated code into \\chemfig{...} when non-nil."
  :group 'tochemfig
  :type 'boolean)

(defcustom tochemfig-default-submol-name nil
  "Wrap generated code into \\definesubmol{NAME}{...} when non-nil."
  :group 'tochemfig
  :type '(choice (const :tag "Disabled" nil) string))

(defcustom tochemfig-default-entry-atom nil
  "Number of the first atom to render, or nil to pick automatically.
Relevant only when generated code is used as a sub-molecule."
  :group 'tochemfig
  :type '(choice (const :tag "Automatic" nil) integer))

(defcustom tochemfig-default-exit-atom nil
  "Number of the last atom to render, or nil to pick automatically.
Relevant only when generated code is used as a sub-molecule."
  :group 'tochemfig
  :type '(choice (const :tag "Automatic" nil) integer))

;;; Internal Functions

(defun tochemfig--build-options (overrides)
  "Build the option plist from defaults and OVERRIDES alist.
OVERRIDES maps option name strings to values, e.g. ((\"terse\" . t))."
  (let ((opts (list :angle tochemfig-default-angle
                    :relative-angles tochemfig-default-relative-angles
                    :flip tochemfig-default-flip
                    :flop tochemfig-default-flop
                    :show-carbons tochemfig-default-show-carbons
                    :show-methyls tochemfig-default-show-methyls
                    :hydrogens tochemfig-default-hydrogens
                    :aromatic-circles tochemfig-default-aromatic-circles
                    :fancy-bonds tochemfig-default-fancy-bonds
                    :atom-numbers tochemfig-default-atom-numbers
                    :bond-scale tochemfig-default-bond-scale
                    :bond-stretch tochemfig-default-bond-stretch
                    :markers tochemfig-default-markers
                    :cross-bond tochemfig-default-cross-bond
                    :legacy-lewis tochemfig-default-legacy-lewis
                    :wrap-chemfig tochemfig-default-wrap-chemfig
                    :submol-name tochemfig-default-submol-name
                    :entry-atom tochemfig-default-entry-atom
                    :exit-atom tochemfig-default-exit-atom
                    :terse tochemfig-default-terse
                    :indent tochemfig-default-indent
                    :strict tochemfig-default-strict
                    :recalculate-coordinates tochemfig-default-recalculate-coordinates
                    :angle-round 1 :bond-round 3 :quadrant-tolerance 0.1)))
    (dolist (override overrides)
      (let ((key (intern (concat ":" (car override))))
            (val (cdr override)))
        (setq opts (plist-put opts key val))))
    opts))

(defun tochemfig--prepare-mol (mol options)
  "Apply hydrogen handling and coordinate layout to MOL per OPTIONS."
  (pcase (plist-get options :hydrogens)
    ('add (indigo-unfold-hydrogens mol) (indigo-layout mol))
    ('delete (indigo-fold-hydrogens mol)))
  (when (or (not (indigo-has-coordinates mol))
            (plist-get options :recalculate-coordinates))
    (indigo-layout mol)))

(defun tochemfig--generate-from-mol (mol &optional overrides)
  "Generate chemfig code from Indigo molecule MOL.
OVERRIDES is an alist of option overrides."
  (let ((options (tochemfig--build-options overrides)))
    (tochemfig--prepare-mol mol options)
    (tochemfig-generate mol options)))

(defun tochemfig--generate-from-string (smiles &optional overrides)
  "Generate chemfig code from SMILES string.
OVERRIDES is an alist of option overrides."
  (when (or (null smiles) (string-empty-p (string-trim smiles)))
    (user-error "Empty SMILES input"))
  (indigo-with-molecule (mol smiles)
    (tochemfig--generate-from-mol mol overrides)))

(defun tochemfig--generate-from-file (path &optional overrides)
  "Generate chemfig code from molecule file at PATH.
OVERRIDES is an alist of option overrides."
  (indigo-with-mol-file (mol path)
    (tochemfig--generate-from-mol mol overrides)))

;;; Interactive Commands

;;;###autoload
(defun tochemfig-default (molecule)
  "Generate chemfig code for MOLECULE using default settings.
MOLECULE is interpreted according to `tochemfig-default-input'."
  (interactive "sEnter molecule: ")
  (insert (if (eq tochemfig-default-input 'file)
              (tochemfig--generate-from-file molecule)
            (tochemfig--generate-from-string molecule))))

;;;###autoload
(defun tochemfig-input-file (path)
  "Generate chemfig code for a molecule from file at PATH."
  (interactive "fEnter molecule file: ")
  (insert (tochemfig--generate-from-file path)))

;;;###autoload
(defun tochemfig-input-direct (molecule)
  "Generate chemfig code for MOLECULE from a SMILES string."
  (interactive "sEnter SMILES: ")
  (insert (tochemfig--generate-from-string molecule)))

;;;###autoload
(defun tochemfig-input-direct-output-file (molecule path)
  "Generate chemfig code for MOLECULE and save to file at PATH."
  (interactive (list (read-string "Enter SMILES: ")
                     (read-file-name "Output file: ")))
  (with-temp-file path
    (insert (tochemfig--generate-from-string molecule))))

;;;###autoload
(defun tochemfig-input-file-output-file (inpath outpath)
  "Generate chemfig code from molecule file at INPATH, save to OUTPATH."
  (interactive (list (read-file-name "Input molecule file: ")
                     (read-file-name "Output file: ")))
  (with-temp-file outpath
    (insert (tochemfig--generate-from-file inpath))))

;;;###autoload
(defun tochemfig-terse (molecule)
  "Generate chemfig code for MOLECULE with compact output."
  (interactive "sEnter SMILES: ")
  (insert (tochemfig--generate-from-string molecule '(("terse" . t)))))

;;;###autoload
(defun tochemfig-verbose (molecule)
  "Generate chemfig code for MOLECULE with formatted output."
  (interactive "sEnter SMILES: ")
  (insert (tochemfig--generate-from-string molecule '(("terse" . nil)))))

;;;###autoload
(defun tochemfig-indent (molecule indent)
  "Generate chemfig code for MOLECULE with INDENT spaces for branches."
  (interactive (list (read-string "Enter SMILES: ")
                     (read-number "Indentation spaces: " 4)))
  (insert (tochemfig--generate-from-string
           molecule
           (list (cons "indent" indent) (cons "terse" nil)))))

;;;###autoload
(defun tochemfig-rotate (molecule angle flip flop)
  "Generate chemfig code for MOLECULE rotated by ANGLE degrees.
FLIP mirrors horizontally, FLOP mirrors vertically."
  (interactive (list (read-string "Enter SMILES: ")
                     (read-number "Rotation angle: " 0.0)
                     (y-or-n-p "Flip horizontally? ")
                     (y-or-n-p "Flip vertically? ")))
  (insert (tochemfig--generate-from-string
           molecule
           (list (cons "angle" angle) (cons "flip" flip) (cons "flop" flop)))))

;;;###autoload
(defun tochemfig-show-carbons (molecule)
  "Generate chemfig code for MOLECULE showing carbon labels."
  (interactive "sEnter SMILES: ")
  (insert (tochemfig--generate-from-string molecule '(("show-carbons" . t)))))

;;;###autoload
(defun tochemfig-show-methyls (molecule)
  "Generate chemfig code for MOLECULE showing methyl group labels."
  (interactive "sEnter SMILES: ")
  (insert (tochemfig--generate-from-string molecule '(("show-methyls" . t)))))

;;;###autoload
(defun tochemfig-add-hydrogens (molecule)
  "Generate chemfig code for MOLECULE with explicit hydrogen atoms."
  (interactive "sEnter SMILES: ")
  (insert (tochemfig--generate-from-string molecule '(("hydrogens" . add)))))

;;;###autoload
(defun tochemfig-delete-hydrogens (molecule)
  "Generate chemfig code for MOLECULE with hydrogen atoms removed."
  (interactive "sEnter SMILES: ")
  (insert (tochemfig--generate-from-string molecule '(("hydrogens" . delete)))))

;;;###autoload
(defun tochemfig-fancy-bonds (molecule)
  "Generate chemfig code for MOLECULE with fancy double/triple bonds."
  (interactive "sEnter SMILES: ")
  (insert (tochemfig--generate-from-string molecule '(("fancy-bonds" . t)))))

;;;###autoload
(defun tochemfig-vanilla-bonds (molecule)
  "Generate chemfig code for MOLECULE with standard bond rendering."
  (interactive "sEnter SMILES: ")
  (insert (tochemfig--generate-from-string molecule '(("fancy-bonds" . nil)))))

;;;###autoload
(defun tochemfig-aromatic-circles (molecule)
  "Generate chemfig code for MOLECULE with circles in aromatic rings."
  (interactive "sEnter SMILES: ")
  (insert (tochemfig--generate-from-string molecule '(("aromatic-circles" . t)))))

;;;###autoload
(defun tochemfig-no-aromatic-circles (molecule)
  "Generate chemfig code for MOLECULE with explicit aromatic double bonds."
  (interactive "sEnter SMILES: ")
  (insert (tochemfig--generate-from-string molecule '(("aromatic-circles" . nil)))))

;;;###autoload
(defun tochemfig-show-atom-numbers (molecule)
  "Generate chemfig code for MOLECULE showing atom numbers."
  (interactive "sEnter SMILES: ")
  (insert (tochemfig--generate-from-string molecule '(("atom-numbers" . t)))))

;;;###autoload
(defun tochemfig-hide-atom-numbers (molecule)
  "Generate chemfig code for MOLECULE hiding atom numbers."
  (interactive "sEnter SMILES: ")
  (insert (tochemfig--generate-from-string molecule '(("atom-numbers" . nil)))))

;;;###autoload
(defun tochemfig-bond-scale (molecule factor)
  "Generate chemfig code for MOLECULE with bonds scaled by FACTOR."
  (interactive (list (read-string "Enter SMILES: ")
                     (read-number "Scale factor: " 1.0)))
  (insert (tochemfig--generate-from-string
           molecule
           (list (cons "bond-scale" 'scale) (cons "bond-stretch" factor)))))

;;;###autoload
(defun tochemfig-bond-normalize (molecule average)
  "Generate chemfig code for MOLECULE with bonds normalized to AVERAGE length."
  (interactive (list (read-string "Enter SMILES: ")
                     (read-number "Target average length: " 1.0)))
  (insert (tochemfig--generate-from-string
           molecule
           (list (cons "bond-scale" 'normalize) (cons "bond-stretch" average)))))

;;;###autoload
(defun tochemfig-markers (molecule prefix)
  "Generate chemfig code for MOLECULE with atom/bond markers PREFIX."
  (interactive (list (read-string "Enter SMILES: ")
                     (read-string "Marker prefix: " "a")))
  (insert (tochemfig--generate-from-string
           molecule (list (cons "markers" prefix)))))

;;;###autoload
(defun tochemfig-cross-bond (molecule start end)
  "Generate chemfig code for MOLECULE drawing the START-END bond on top."
  (interactive (list (read-string "Enter SMILES: ")
                     (read-number "Cross bond start atom: ")
                     (read-number "Cross bond end atom: ")))
  (insert (tochemfig--generate-from-string
           molecule (list (cons "cross-bond" (list (list start end)))))))

;;;###autoload
(defun tochemfig-wrap-chemfig (molecule)
  "Generate chemfig code for MOLECULE wrapped in \\chemfig{...}."
  (interactive "sEnter SMILES: ")
  (insert (tochemfig--generate-from-string molecule '(("wrap-chemfig" . t)))))

;;;###autoload
(defun tochemfig-wrap-submol (molecule submol)
  "Generate chemfig code for MOLECULE wrapped as \\definesubmol{SUBMOL}{...}."
  (interactive (list (read-string "Enter SMILES: ")
                     (read-string "Submol name: ")))
  (insert (tochemfig--generate-from-string
           molecule (list (cons "submol-name" submol)))))

;;;###autoload
(defun tochemfig-unwrap (molecule)
  "Generate chemfig code for MOLECULE without any wrapper."
  (interactive "sEnter SMILES: ")
  (insert (tochemfig--generate-from-string
           molecule
           '(("wrap-chemfig" . nil) ("submol-name" . nil)))))

;;;###autoload
(defun tochemfig-partial-submol (molecule submol entry exit)
  "Generate chemfig code for MOLECULE as submol with ENTRY and EXIT atoms."
  (interactive (list (read-string "Enter SMILES: ")
                     (read-string "Submol name: ")
                     (read-number "Entry atom index: ")
                     (read-number "Exit atom index: ")))
  (insert (tochemfig--generate-from-string
           molecule
           (list (cons "submol-name" submol)
                 (cons "entry-atom" entry)
                 (cons "exit-atom" exit)))))

(provide 'tochemfig)
;;; tochemfig.el ends here
