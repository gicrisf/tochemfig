;;; chem-options.el --- Options and registry for chem-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2025 Giovanni Crisalfi

;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Common drawing options, chemfig output options, and metadata shared by the
;; generic UI.  Common options are applied by `chem-indigo' and
;; `chem-apply-drawing-options'; exporters render the resulting IR attributes.
;;
;; The transient menu is manually grouped around these specs.  The metadata
;; is shared so future frontends can expose the same common surface.

;;; Code:

(require 'cl-lib)
(require 'chem-export)

(defgroup chem-mode nil
  "Generate chemical drawings from molecular structures."
  :prefix "chem-"
  :group 'applications)

;;; Common drawing options

(defcustom chem-default-input 'direct
  "Default input mode for molecule specification.
With `file', expects a filename.
With `direct', the argument is interpreted as a SMILES string."
  :group 'chem-mode
  :type '(choice (const :tag "SMILES string" direct)
                 (const :tag "File path" file)))

(defcustom chem-default-strict t
  "Abide by Indigo's chemical structure validation when non-nil.
When nil, atoms whose implicit hydrogens cannot be computed are silently
treated as having none instead of signalling an error."
  :group 'chem-mode
  :type 'boolean)

(defcustom chem-default-recalculate-coordinates nil
  "Discard existing coordinates and calculate new ones when non-nil.
For SMILES input this is performed implicitly."
  :group 'chem-mode
  :type 'boolean)

(defcustom chem-default-angle 0.0
  "Rotate molecule counterclockwise by this angle (in degrees)."
  :group 'chem-mode
  :type 'number)

(defcustom chem-default-flip nil
  "Flip the structure horizontally when non-nil."
  :group 'chem-mode
  :type 'boolean)

(defcustom chem-default-flop nil
  "Flip the structure vertically when non-nil."
  :group 'chem-mode
  :type 'boolean)

(defcustom chem-default-show-carbons nil
  "Show element symbol for carbon atoms when non-nil."
  :group 'chem-mode
  :type 'boolean)

(defcustom chem-default-show-methyls nil
  "Show element symbols for methyl groups when non-nil.
Implied if `chem-default-show-carbons' is non-nil."
  :group 'chem-mode
  :type 'boolean)

(defcustom chem-default-hydrogens 'keep
  "How to deal with explicit hydrogen atoms.
One of `keep', `add', or `delete'.
Note that `add' will also trigger calculation of new coordinates."
  :group 'chem-mode
  :type '(choice (const :tag "Keep as-is" keep)
                 (const :tag "Add explicit H" add)
                 (const :tag "Remove explicit H" delete)))

(defcustom chem-default-aromatic-circles nil
  "Draw a circle instead of double bonds inside aromatic rings when non-nil."
  :group 'chem-mode
  :type 'boolean)

(defcustom chem-default-fancy-bonds nil
  "Draw fancier double and triple bonds when non-nil."
  :group 'chem-mode
  :type 'boolean)

(defcustom chem-default-atom-numbers nil
  "Show the molfile number of each atom next to it when non-nil.
When this option is set, charges and implicit hydrogens will not be shown."
  :group 'chem-mode
  :type 'boolean)

(defcustom chem-default-bond-scale 'normalize
  "How to scale the lengths of bonds.
One of `keep', `scale', or `normalize'."
  :group 'chem-mode
  :type '(choice (const :tag "Keep original lengths" keep)
                 (const :tag "Scale by factor" scale)
                 (const :tag "Normalize to average" normalize)))

(defcustom chem-default-bond-stretch 1.0
  "Scaling factor or target average for bond lengths.
The meaning depends on `chem-default-bond-scale':
- With `scale': multiplication factor for all bond lengths
- With `normalize': target average bond length"
  :group 'chem-mode
  :type 'number)

(defcustom chem-default-markers nil
  "Prefix for atom and bond markers, or nil to disable them.
With value \"a\", atom 2 will be labeled @{a2} and its bond to atom 5
@{a2-5}."
  :group 'chem-mode
  :type '(choice (const :tag "Disabled" nil) string))

(defcustom chem-default-cross-bond nil
  "Bonds to draw on top of others they cross over.
A list of (START END) atom number pairs, using 1-based molfile numbering."
  :group 'chem-mode
  :type '(repeat (list integer integer)))

(defcustom chem-default-entry-atom nil
  "Number of the first atom to render, or nil to pick automatically.
Relevant only when generated code is used as a sub-molecule."
  :group 'chem-mode
  :type '(choice (const :tag "Automatic" nil) integer))

(defcustom chem-default-exit-atom nil
  "Number of the last atom to render, or nil to pick automatically.
Relevant only when generated code is used as a sub-molecule."
  :group 'chem-mode
  :type '(choice (const :tag "Automatic" nil) integer))

;;; Reaction options

(defcustom chem-default-reaction-arrow 'forward
  "Default reaction arrow style.
One of `forward', `reverse', or `equilibrium'."
  :group 'chem-mode
  :type '(choice (const :tag "Forward" forward)
                 (const :tag "Reverse" reverse)
                 (const :tag "Equilibrium" equilibrium)))

(defcustom chem-default-reaction-above ""
  "Default text above the reaction arrow."
  :group 'chem-mode
  :type 'string)

(defcustom chem-default-reaction-below ""
  "Default text below the reaction arrow."
  :group 'chem-mode
  :type 'string)

(defcustom chem-default-reaction-spacing 10.0
  "Default spacing between reaction layout parts."
  :group 'chem-mode
  :type 'number)

(defcustom chem-default-reaction-arrow-length 40.0
  "Default reaction arrow length."
  :group 'chem-mode
  :type 'number)

(defcustom chem-default-reaction-plus-size 9.0
  "Default reaction plus-sign box size."
  :group 'chem-mode
  :type 'number)

;;; Chemfig output options

(defcustom chem-exp-chemfig-default-terse nil
  "Remove all whitespace from the output when non-nil."
  :group 'chem-mode
  :type 'boolean)

(defcustom chem-exp-chemfig-default-indent 4
  "Number of spaces to use for indenting molecule branches in generated code.
Without effect when `chem-exp-chemfig-default-terse' is non-nil."
  :group 'chem-mode
  :type 'integer)

(defcustom chem-exp-chemfig-default-relative-angles nil
  "Use relative bond angles when non-nil."
  :group 'chem-mode
  :type 'boolean)

(defcustom chem-exp-chemfig-default-legacy-lewis nil
  "Use the legacy \\lewis macro for radicals (ChemFig < 1.6) when non-nil."
  :group 'chem-mode
  :type 'boolean)

(defcustom chem-exp-chemfig-default-wrap-chemfig nil
  "Wrap generated code into \\chemfig{...} when non-nil."
  :group 'chem-mode
  :type 'boolean)

(defcustom chem-exp-chemfig-default-submol-name nil
  "Wrap generated code into \\definesubmol{NAME}{...} when non-nil."
  :group 'chem-mode
  :type '(choice (const :tag "Disabled" nil) string))

(defcustom chem-exp-chemfig-default-reaction-wrap t
  "Wrap reaction output in \\schemestart...\\schemestop when non-nil."
  :group 'chem-mode
  :type 'boolean)

(defcustom chem-exp-chemfig-default-reaction-arrow-chemfig nil
  "Explicit chemfig arrow specification for reactions, or nil.
When nil, derive the arrow from `chem-default-reaction-arrow'."
  :group 'chem-mode
  :type '(choice (const :tag "Automatic" nil) string))

;;; Option registry

(defconst chem-common-option-specs
  '((:key :strict
     :variable chem-default-strict
     :label "Strict validation"
     :type boolean
     :group input)
    (:key :hydrogens
     :variable chem-default-hydrogens
     :label "Hydrogens"
     :type choice
     :choices (keep add delete)
     :group input)
    (:key :recalculate-coordinates
     :variable chem-default-recalculate-coordinates
     :label "Recalculate coordinates"
     :type boolean
     :group input)
    (:key :angle
     :variable chem-default-angle
     :label "Rotation angle"
     :type number
     :group geometry)
    (:key :flip
     :variable chem-default-flip
     :label "Flip horizontally"
     :type boolean
     :group geometry)
    (:key :flop
     :variable chem-default-flop
     :label "Flip vertically"
     :type boolean
     :group geometry)
    (:key :bond-scale
     :variable chem-default-bond-scale
     :label "Bond scaling"
     :type choice
     :choices (normalize keep scale)
     :group bonds)
    (:key :bond-stretch
     :variable chem-default-bond-stretch
     :label "Bond stretch"
     :type number
     :group bonds)
    (:key :show-carbons
     :variable chem-default-show-carbons
     :label "Show carbons"
     :type boolean
     :group atoms)
    (:key :show-methyls
     :variable chem-default-show-methyls
     :label "Show methyls"
     :type boolean
     :group atoms)
    (:key :atom-numbers
     :variable chem-default-atom-numbers
     :label "Atom numbers"
     :type boolean
     :group atoms)
    (:key :aromatic-circles
     :variable chem-default-aromatic-circles
     :label "Aromatic circles"
     :type boolean
     :group bonds)
    (:key :fancy-bonds
     :variable chem-default-fancy-bonds
     :label "Fancy bonds"
     :type boolean
     :group bonds)
    (:key :markers
     :variable chem-default-markers
     :label "Marker prefix"
     :type string
     :group output)
    (:key :cross-bond
     :variable chem-default-cross-bond
     :label "Cross bonds"
     :type cross-bond
     :group advanced)
    (:key :entry-atom
     :variable chem-default-entry-atom
     :label "Entry atom"
     :type optional-number
     :group sub-molecule)
    (:key :exit-atom
     :variable chem-default-exit-atom
     :label "Exit atom"
     :type optional-number
     :group sub-molecule))
  "Common drawing option metadata.")

(defconst chem-reaction-option-specs
  '((:key :reaction-arrow
     :variable chem-default-reaction-arrow
     :label "Reaction arrow"
     :type choice
     :choices (forward reverse equilibrium)
     :group reaction)
    (:key :reaction-above
     :variable chem-default-reaction-above
     :label "Text above arrow"
     :type string
     :group reaction)
    (:key :reaction-below
     :variable chem-default-reaction-below
     :label "Text below arrow"
     :type string
     :group reaction)
    (:key :reaction-spacing
     :variable chem-default-reaction-spacing
     :label "Reaction spacing"
     :type number
     :group reaction)
    (:key :reaction-arrow-length
     :variable chem-default-reaction-arrow-length
     :label "Arrow length"
     :type number
     :group reaction)
    (:key :reaction-plus-size
     :variable chem-default-reaction-plus-size
     :label "Plus size"
     :type number
     :group reaction))
  "Reaction option metadata.")

(defun chem-common-option-specs-for (group)
  "Return the common option specs in GROUP."
  (cl-remove-if-not (lambda (spec) (eq (plist-get spec :group) group))
                    chem-common-option-specs))

;;; Option construction

(defun chem--option-specs-for-exporter (&optional exporter)
  "Return the option specs for EXPORTER or the selected exporter."
  (append chem-common-option-specs
          (chem-exporter-option-specs
           (or exporter
               (if (boundp 'chem-default-exporter)
                   chem-default-exporter
                 'chemfig)))))

(defun chem--standard-option-value (variable)
  "Return VARIABLE's standard defcustom value."
  (let ((standard (get variable 'standard-value)))
    (if standard
        (eval (car standard) t)
      (default-value variable))))

(defun chem--build-options (overrides &optional exporter standard extra-specs)
  "Build the option plist from defaults and OVERRIDES alist.
OVERRIDES maps option name strings to values, e.g. ((\"terse\" . t)).
EXPORTER selects the option specs.  When STANDARD is non-nil, use each
option's standard value instead of its current value.  EXTRA-SPECS are
additional option specs appended to the exporter's specs."
  (let ((opts (list :angle-round 1 :bond-round 3 :quadrant-tolerance 0.1)))
    (dolist (spec (append (chem--option-specs-for-exporter exporter)
                          extra-specs))
      (let ((key (plist-get spec :key))
            (variable (plist-get spec :variable)))
        (when (and key variable)
          (setq opts (plist-put opts key
                                (if standard
                                    (chem--standard-option-value variable)
                                  (symbol-value variable)))))))
    (dolist (override overrides)
      (let ((key (intern (concat ":" (car override))))
            (val (cdr override)))
        (setq opts (plist-put opts key val))))
    opts))

(defun chem--build-reaction-options (overrides &optional exporter standard)
  "Build options for a reaction with OVERRIDES.
EXPORTER and STANDARD are passed to `chem--build-options'."
  (chem--build-options overrides exporter standard
                       chem-reaction-option-specs))

(provide 'chem-options)
;;; chem-options.el ends here
