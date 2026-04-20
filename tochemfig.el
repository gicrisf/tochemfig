;;; tochemfig.el --- Generate chemfig code from molecules -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2025 Giovanni Crisalfi

;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Maintainer: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Created: novembre 04, 2022
;; Modified: 2025
;; Version: 0.2.0
;; Keywords: chemistry mol smiles chemfig convenience data extensions files languages lisp tex tools
;; Homepage: https://github.com/gicrisf/tochemfig
;; Package-Requires: ((emacs "25.1"))

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

;;; Code:

(require 'indigo)
(require 'tochemfig-core)

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

(defcustom tochemfig-default-indent 4
  "Number of spaces to use for indenting molecule branches in generated code.
Without effect when `tochemfig-default-terse' is non-nil."
  :group 'tochemfig
  :type 'integer)

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

(defcustom tochemfig-default-wrap-chemfig nil
  "Wrap generated code into \\chemfig{...} when non-nil."
  :group 'tochemfig
  :type 'boolean)

;;; Internal Functions

(defun tochemfig--build-options (overrides)
  "Build options plist from defaults and OVERRIDES alist."
  (let ((opts (list :angle tochemfig-default-angle
                    :flip tochemfig-default-flip
                    :flop tochemfig-default-flop
                    :show-carbons tochemfig-default-show-carbons
                    :show-methyls tochemfig-default-show-methyls
                    :fancy-bonds tochemfig-default-fancy-bonds
                    :atom-numbers tochemfig-default-atom-numbers
                    :relative-angles tochemfig-default-relative-angles
                    :terse tochemfig-default-terse
                    :indent tochemfig-default-indent
                    :bond-scale tochemfig-default-bond-scale
                    :bond-stretch tochemfig-default-bond-stretch
                    :wrap-chemfig tochemfig-default-wrap-chemfig)))
    ;; Apply overrides
    (dolist (override overrides)
      (let ((key (intern (concat ":" (car override))))
            (val (cdr override)))
        (setq opts (plist-put opts key val))))
    opts))

(defun tochemfig--handle-hydrogens (mol mode)
  "Handle hydrogen atoms in MOL according to MODE.
MODE is one of `keep', `add', or `delete'."
  (pcase mode
    ('add (indigo-unfold-hydrogens mol))
    ('delete (indigo-fold-hydrogens mol))
    (_ nil))) ; 'keep does nothing

(defun tochemfig--generate-from-mol (mol &optional overrides)
  "Generate chemfig code from Indigo molecule MOL.
OVERRIDES is an alist of option overrides."
  (let ((options (tochemfig--build-options overrides))
        (hydrogens (or (cdr (assoc "hydrogens" overrides))
                       tochemfig-default-hydrogens)))
    ;; Handle hydrogen folding/unfolding
    (tochemfig--handle-hydrogens mol hydrogens)
    ;; Generate chemfig code
    (tochemfig-generate mol options)))

(defun tochemfig--generate-from-string (smiles &optional overrides)
  "Generate chemfig code from SMILES string.
OVERRIDES is an alist of option overrides."
  (indigo-with-molecule (mol smiles)
    (tochemfig--generate-from-mol mol overrides)))

(defun tochemfig--generate-from-file (path &optional overrides)
  "Generate chemfig code from molecule file at PATH.
OVERRIDES is an alist of option overrides."
  (indigo-with-mol-file (mol path)
    (tochemfig--generate-from-mol mol overrides)))

;;; Argument Selector (for custom command)

(defun tochemfig--custom-arg-selector (items)
  "Show a list of arguments for editing, displaying current ITEMS values."
  (let* ((args (list (cons "input" (or (cdr (assoc "input" items))
                                       tochemfig-default-input))
                     (cons "terse" (if (assoc "terse" items)
                                       (if (cdr (assoc "terse" items)) "t" "nil")
                                     (if tochemfig-default-terse "t" "nil")))
                     (cons "indent" (or (cdr (assoc "indent" items))
                                        tochemfig-default-indent))
                     (cons "angle" (or (cdr (assoc "angle" items))
                                       tochemfig-default-angle))
                     (cons "relative-angles" (if (assoc "relative-angles" items)
                                                 (if (cdr (assoc "relative-angles" items)) "t" "nil")
                                               (if tochemfig-default-relative-angles "t" "nil")))
                     (cons "flip" (if (assoc "flip" items)
                                      (if (cdr (assoc "flip" items)) "t" "nil")
                                    (if tochemfig-default-flip "t" "nil")))
                     (cons "flop" (if (assoc "flop" items)
                                      (if (cdr (assoc "flop" items)) "t" "nil")
                                    (if tochemfig-default-flop "t" "nil")))
                     (cons "show-carbons" (if (assoc "show-carbons" items)
                                              (if (cdr (assoc "show-carbons" items)) "t" "nil")
                                            (if tochemfig-default-show-carbons "t" "nil")))
                     (cons "show-methyls" (if (assoc "show-methyls" items)
                                              (if (cdr (assoc "show-methyls" items)) "t" "nil")
                                            (if tochemfig-default-show-methyls "t" "nil")))
                     (cons "hydrogens" (or (cdr (assoc "hydrogens" items))
                                           tochemfig-default-hydrogens))
                     (cons "fancy-bonds" (if (assoc "fancy-bonds" items)
                                             (if (cdr (assoc "fancy-bonds" items)) "t" "nil")
                                           (if tochemfig-default-fancy-bonds "t" "nil")))
                     (cons "atom-numbers" (if (assoc "atom-numbers" items)
                                              (if (cdr (assoc "atom-numbers" items)) "t" "nil")
                                            (if tochemfig-default-atom-numbers "t" "nil")))
                     (cons "bond-scale" (or (cdr (assoc "bond-scale" items))
                                            tochemfig-default-bond-scale))
                     (cons "bond-stretch" (or (cdr (assoc "bond-stretch" items))
                                              tochemfig-default-bond-stretch))
                     (cons "wrap-chemfig" (if (assoc "wrap-chemfig" items)
                                              (if (cdr (assoc "wrap-chemfig" items)) "t" "nil")
                                            (if tochemfig-default-wrap-chemfig "t" "nil")))
                     (cons "submol-name" (or (cdr (assoc "submol-name" items)) ""))
                     (cons "entry-atom" (or (cdr (assoc "entry-atom" items)) ""))
                     (cons "exit-atom" (or (cdr (assoc "exit-atom" items)) ""))))
         (choices (mapcar (lambda (arg)
                            (cons (format "%s (selected: %s)" (car arg) (cdr arg))
                                  (car arg)))
                          args))
         (choice (completing-read "Select argument to edit: "
                                  (mapcar #'car choices))))
    (cdr (assoc choice choices))))

;;; Minor Mode

;;;###autoload
(define-minor-mode tochemfig-mode
  "Minor mode for generating chemfig code from molecules.
Provides commands to convert molecular structures (SMILES, MOL files)
into LaTeX chemfig code."
  :lighter " tochemfig"
  :group 'tochemfig)

;;; Interactive Commands

;;;###autoload
(defun tochemfig-default (molecule)
  "Generate chemfig code for MOLECULE using default settings.
MOLECULE is interpreted according to `tochemfig-default-input'."
  (interactive "sEnter molecule: ")
  (let ((result (if (eq tochemfig-default-input 'file)
                    (tochemfig--generate-from-file molecule)
                  (tochemfig--generate-from-string molecule))))
    (insert result)))

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
  (interactive (list
                (read-string "Enter SMILES: ")
                (read-file-name "Output file: ")))
  (let ((result (tochemfig--generate-from-string molecule)))
    (with-temp-file path
      (insert result))))

;;;###autoload
(defun tochemfig-input-file-output-file (inpath outpath)
  "Generate chemfig code from molecule file at INPATH, save to OUTPATH."
  (interactive (list
                (read-file-name "Input molecule file: ")
                (read-file-name "Output file: ")))
  (let ((result (tochemfig--generate-from-file inpath)))
    (with-temp-file outpath
      (insert result))))

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
  (interactive (list
                (read-string "Enter SMILES: ")
                (read-number "Indentation spaces: " 4)))
  (insert (tochemfig--generate-from-string
           molecule
           (list (cons "indent" indent) (cons "terse" nil)))))

;;;###autoload
(defun tochemfig-rotate (molecule angle flip flop)
  "Generate chemfig code for MOLECULE rotated by ANGLE degrees.
FLIP mirrors horizontally, FLOP mirrors vertically."
  (interactive (list
                (read-string "Enter SMILES: ")
                (read-number "Rotation angle: " 0.0)
                (y-or-n-p "Flip horizontally? ")
                (y-or-n-p "Flip vertically? ")))
  (insert (tochemfig--generate-from-string
           molecule
           (list (cons "angle" angle)
                 (cons "flip" flip)
                 (cons "flop" flop)))))

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
  (interactive (list
                (read-string "Enter SMILES: ")
                (read-number "Scale factor: " 1.0)))
  (insert (tochemfig--generate-from-string
           molecule
           (list (cons "bond-scale" 'scale)
                 (cons "bond-stretch" factor)))))

;;;###autoload
(defun tochemfig-bond-normalize (molecule average)
  "Generate chemfig code for MOLECULE with bonds normalized to AVERAGE length."
  (interactive (list
                (read-string "Enter SMILES: ")
                (read-number "Target average length: " 1.0)))
  (insert (tochemfig--generate-from-string
           molecule
           (list (cons "bond-scale" 'normalize)
                 (cons "bond-stretch" average)))))

;;;###autoload
(defun tochemfig-wrap-chemfig (molecule)
  "Generate chemfig code for MOLECULE wrapped in \\chemfig{...}."
  (interactive "sEnter SMILES: ")
  (insert (tochemfig--generate-from-string molecule '(("wrap-chemfig" . t)))))

;;;###autoload
(defun tochemfig-wrap-submol (molecule submol)
  "Generate chemfig code for MOLECULE wrapped as \\definesubmol{SUBMOL}{...}."
  (interactive (list
                (read-string "Enter SMILES: ")
                (read-string "Submol name: ")))
  (insert (tochemfig--generate-from-string
           molecule
           (list (cons "submol-name" submol)))))

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
  (interactive (list
                (read-string "Enter SMILES: ")
                (read-string "Submol name: ")
                (read-number "Entry atom index: ")
                (read-number "Exit atom index: ")))
  (insert (tochemfig--generate-from-string
           molecule
           (list (cons "submol-name" submol)
                 (cons "entry-atom" entry)
                 (cons "exit-atom" exit)))))

;;;###autoload
(defun tochemfig-custom ()
  "Interactively customize all options and generate chemfig code."
  (interactive)
  (let ((wizargs '())
        (input tochemfig-default-input)
        (continue t))
    ;; Collect arguments
    (while continue
      (let ((selected (tochemfig--custom-arg-selector wizargs)))
        (cond
         ;; Boolean options
         ((member selected '("terse" "relative-angles" "flip" "flop"
                             "show-carbons" "show-methyls" "fancy-bonds"
                             "atom-numbers" "wrap-chemfig"))
          (let ((val (equal (completing-read (format "%s? " selected) '("true" "false"))
                            "true")))
            (push (cons selected val) wizargs)))
         ;; Input mode
         ((equal selected "input")
          (let ((val (completing-read "Select input mode: " '("file" "direct"))))
            (setq input (intern val))
            (push (cons selected input) wizargs)))
         ;; Integer options
         ((equal selected "indent")
          (push (cons selected (read-number "Indentation spaces: " 4)) wizargs))
         ;; Float options
         ((equal selected "angle")
          (push (cons selected (read-number "Rotation angle: " 0.0)) wizargs))
         ((equal selected "bond-stretch")
          (push (cons selected (read-number "Bond stretch factor: " 1.0)) wizargs))
         ;; Choice options
         ((equal selected "hydrogens")
          (let ((val (completing-read "Hydrogen handling: " '("add" "delete" "keep"))))
            (push (cons selected (intern val)) wizargs)))
         ((equal selected "bond-scale")
          (let ((val (completing-read "Bond scaling mode: " '("scale" "normalize" "keep"))))
            (push (cons selected (intern val)) wizargs)))
         ;; String options
         ((equal selected "submol-name")
          (push (cons selected (read-string "Submol name: ")) wizargs))
         ((member selected '("entry-atom" "exit-atom"))
          (let ((val (read-number (format "%s index: " selected))))
            (push (cons selected val) wizargs)))))
      ;; Continue?
      (unless (y-or-n-p "Edit another option? ")
        (setq continue nil)))
    ;; Get molecule and generate
    (let ((molecule (if (eq input 'file)
                        (read-file-name "Molecule file: ")
                      (read-string "Enter SMILES: "))))
      (insert (if (eq input 'file)
                  (tochemfig--generate-from-file molecule wizargs)
                (tochemfig--generate-from-string molecule wizargs))))))

(provide 'tochemfig)
;;; tochemfig.el ends here
