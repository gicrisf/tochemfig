;;; tochemfig.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Giovanni Crisalfi
;;
;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Maintainer: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Created: novembre 04, 2022
;; Modified: novembre 04, 2022
;; Version: 0.0.3
;; Keywords: chemistry mol smiles chemfig convenience data extensions files languages lisp tex tools unix
;; Homepage: https://github.com/gicrisf/tochemfig
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;; This package requires a working Python environment
;;; with the `mol2chemfigPy3' package installed and accessible via shell.
;;; If you miss the package, install it with this command (through `pip'):
;;; $ pip install -U mol2chemfigPy3
;;
;;  Description
;;  Emacs interface to mol2chemfig. Generate chemfig code from mol or SMILES.
;;
;;; Code:

(defgroup tochemfig nil

  "Manipulating molecules through LaTeX from Emacs."
  :prefix "tochemfig-"
  :group 'comm)

(defcustom tochemfig-default-input 'file'
  "How to interpret the argument. With ’file’, mol2chemfig expects a filename.
With ’direct’, the argument is intrepreted directly;
don’t forget to put quotes around it.
With ’pubchem’, the argument is treated as an identifier for the PubChem database."
  :group 'tochemfig
  :type 'string)

(defcustom tochemfig-default-terse nil
  "Remove all whitespace and comments from the output.
If you can still read it afterwards, Bill Gates wants your resume."
  :group 'tochemfig
  :type 'boolean)

(defcustom tochemfig-default-strict t
  "Abide by Indigo’s chemical structure validation.
If true, mol2chemfig will fail if Indigo reports that something
is wrong with the molecule, like a carbon with five bonds.
If false, mol2chemfig will ignore such errors."
  :group 'tochemfig
  :type 'boolean)

(defcustom tochemfig-default-indent 4
  "Number of spaces to use for indenting molecule branches in generated code.
Without effect when ’terse’ option is passed.
Affects only the generated tex code, not the rendered molecule."
  :group 'tochemfig
  :type 'integer)

(defcustom tochemfig-default-recalculate-coordinates nil
  "Discard existing coordinate and calculate new ones from covalent structure.
For smiles input, this is performed implicitly."
  :group 'tochemfig
  :type 'boolean)

(defcustom tochemfig-default-angle 0.0
  "Rotate molecule counterclockwise by this angle."
  :group 'tochemfig
  :type 'number)

(defcustom tochemfig-default-relative-angles nil
  "Use relative bond angles."
  :group 'tochemfig
  :type 'boolean)

(defcustom tochemfig-default-flip nil
  "Flip the structure horizontally."
  :group 'tochemfig
  :type 'boolean)

(defcustom tochemfig-default-flop nil
  "Flip the structure vertically."
  :group 'tochemfig
  :type 'boolean)

(defcustom tochemfig-default-show-carbons nil
  "Show element symbol for carbon atoms."
  :group 'tochemfig
  :type 'boolean)

;; (implied if tochemfig-default-show-carbons is t)
(defcustom tochemfig-default-show-methyls nil
  "Show element symbols for methyl groups."
  :group 'tochemfig
  :type 'boolean)

(defcustom tochemfig-default-hydrogens 'keep'
  "How to deal with explicit hydrogen atoms.
One of ’keep’, ’add’ or ’delete’.
Note that 5  ’add’ will also trigger calculation
of new coordinates for the entire molecule.
Option ’keep’ does nothing."
  :group 'tochemfig
  :type 'string)

(defcustom tochemfig-default-aromatic-circles nil
  "Draw circles instead of double bonds inside aromatic rings."
  :group 'tochemfig
  :type 'boolean)

(defcustom tochemfig-default-fancy-bonds nil
  "Draw fancier double and triple bonds."
  :group 'tochemfig
  :type 'boolean)

;; Doesn't make sense to create a default of this one
;; (defcustom tochemfig-default-markers nil
;;  "Give each atom and each bond a unique marker that can be used for attaching electron movement arrows.
;;  With value ’a’, atom 2 will be labeled @{a2}, and its bond to atom 5 @{a2-5}."
;;  :group 'tochemfig
;;  :type 'string)

;; When this option is set, charges and implicit hydrogens will not be shown.
(defcustom tochemfig-default-atom-numbers nil
  "Show the molfile number of each atom next to it."
  :group 'tochemfig
  :type 'boolean)

(defcustom tochemfig-default-bond-scale 'normalize'
  "How to scale the lengths of bonds (one of ’keep’, ’scale’, or ’normalize’)."
  :group 'tochemfig
  :type 'string)

(defcustom tochemfig-default-bond-stretch 1.0
  "Scaling factor or average for bond lengths (depends on bond scale option).
The meaning of this option depends on the setting of the previous option.
Used as scaling factor (with tochemfig-default-bond-scale=scale)
or average (with tochemfig-default-bond-scale=normalize) for bond lengths."
  :group 'tochemfig
  :type 'number)

(defcustom tochemfig-default-wrap-chemfig nil
  "Wrap generated code into \\chemfig{...}."
  :group 'tochemfig
  :type 'boolean)

;; Doesn't make sense to create a default of this one
;; (defcustom tochemfig-default-submol-name nil
;;  "If a name is given, wrap generated code into chemfig \definesubmol{name}{...} command"
;;  :group 'tochemfig
;;  :type 'string)

;; Doesn't make sense to create a default of this one
;; (defcustom tochemfig-default-entry-atom nil
;;  "Number of first atom to be rendered. Relevant only if generated code is to be used as sub-molecule"
;;  :group 'tochemfig
;;  :type 'number)

;; (defcustom tochemfig-default-exit-atom nil
;;  "Number of last atom to be rendered. Relevant only if generated code is to be used as sub-molecule"
;;  :group 'tochemfig
;;  :type 'number)

;; Doesn't make sense to create a default of this one
;; Specify bonds that should be drawn on top
;; of others they cross over. Give the start
;; and the end atoms. Example for one bond:
;; --cross-bond=5-6 Example for two bonds:
;; --crossbond=4-8,12-13 (Default: None)
;; (setq chemfig-default-cross-bond '')

(defun tochemfig-args-builder (&optional xOpt)
  "Build default arguments which will be passed to mol2chemfig.
XOPT is optional. If given, must be an alist."
  (let ((args '()))
    ;; read optional parameters, if given
    (let ((input (or (cdr (assoc "input" xOpt)) tochemfig-default-input))
          (terse (or (cdr (assoc "terse" xOpt)) tochemfig-default-terse))
          (strict (or (cdr (assoc "strict" xOpt)) tochemfig-default-strict))
          (indent (or (cdr (assoc "indent" xOpt)) tochemfig-default-indent))
          (recalculate-coordinates (or (cdr (assoc "recalculate-coordinates" xOpt)) tochemfig-default-recalculate-coordinates))
          (angle (or (cdr (assoc "angle" xOpt)) tochemfig-default-angle))
          (relative-angles (or (cdr (assoc "relative-angles" xOpt)) tochemfig-default-relative-angles))
          (flip (or (cdr (assoc "flip" xOpt)) tochemfig-default-flip))
          (flop (or (cdr (assoc "flop" xOpt)) tochemfig-default-flop))
          (show-carbons (or (cdr (assoc "show-carbons" xOpt)) tochemfig-default-show-carbons))
          (show-methyls (or (cdr (assoc "show-methyls" xOpt)) tochemfig-default-show-methyls))
          (hydrogens (or (cdr (assoc "hydrogens" xOpt)) tochemfig-default-hydrogens))
          (aromatic-circles (or (cdr (assoc "aromatic-circles" xOpt)) tochemfig-default-aromatic-circles))
          (fancy-bonds (or (cdr (assoc "hydrogens" xOpt)) tochemfig-default-fancy-bonds))
          (atom-numbers (or (cdr (assoc "atom-numbers" xOpt)) tochemfig-default-atom-numbers))
          (bond-scale (or (cdr (assoc "bond-scale" xOpt)) tochemfig-default-bond-scale))
          (bond-stretch (or (cdr (assoc "bond-stretch" xOpt)) tochemfig-default-bond-stretch))
          (wrap-chemfig (or (cdr (assoc "wrap-chemfig" xOpt)) tochemfig-default-wrap-chemfig)))

      ;; build the command substring list
      ;; TODO check types!
      (when (not (equal (format "%s" input) "file"))
        (push (concat "--input " (format "%s" input)) args))
      (when terse (push "--terse" args))
      (when strict (push "--strict" args))
      (when (not (equal indent 4))
        (push (concat "--indent " (format "%s" indent)) args))
      (when recalculate-coordinates (push "--recalculate-coordinates" args))
      (when (not (equal angle 0.0))
        (push (concat "--angle " (format "%s" angle)) args))
      (when relative-angles (push "--relative-angles" args))
      (when flip (push "--flip" args))
      (when flop (push "--flop" args))
      (when show-carbons (push "--show-carbons" args))
      (when show-methyls (push "--show-methyls" args))
      (when (not (equal (format "%s" hydrogens) "keep"))
        (push (concat "--hydrogens " (format "%s" hydrogens)) args))
      (when aromatic-circles (push "--aromatic-circles" args))
      (when fancy-bonds (push "--fancy-bonds" args))
      (when atom-numbers (push "--atom-numbers" args))
      (when (not (equal (format "%s" bond-scale) "normalize"))
        (push (concat "--bond-scale " (format "%s" bond-scale)) args))
      (when (not (equal bond-stretch 1.0))
        (push (concat "--bond-stretch " (format "%s" bond-stretch)) args))
      (when wrap-chemfig (push "--wrap-chemfig" args))

      ;; convert list to string
      ;; http://xahlee.info/emacs/emacs/elisp_list.html
      (let ((strargs (mapconcat #'identity args " ")))
        ;; print out the final string (for testing)
        ;; (message strargs)
        strargs))))

;; Interactive functions

;;;###autoload
(defun tochemfig (molecule)
  "Generate chemfig code for a MOLECULE using the default settings."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat "python -m mol2chemfigPy3 " (tochemfig-args-builder) " " molecule))))

;; The following one totally ignores defaults and directly inject custom flags

;;;###autoload
(defun tochemfig-custom (molecule custom_args)
  "Generate chemfig code for a MOLECULE specifying all the needed CUSTOM_ARGS."
  (interactive
   (list
    (read-string "sEnter molecule: ")
    (read-string "sEnter custom arguments: ")))
  (insert (shell-command-to-string
           (concat "python -m mol2chemfigPy3 " custom_args " " molecule))))

;; The following ones force a specific argument over the defaults leaving the rest untouched

;;;###autoload
(defun tochemfig-input-pubchem (identifier)
  "Generate chemfig code for a molecule retrieved with its pubchem IDENTIFIER.
Obviously, you have to be online for this input mode to work."
  (interactive "sEnter molecule name for pubchem search: ")
  (insert (shell-command-to-string
           (concat "python -m mol2chemfigPy3 "
                   (tochemfig-args-builder '(("input" . "pubchem"))) " " identifier))))

;;;###autoload
(defun tochemfig-input-file (path)
  "Generate chemfig code for a molecule from its file's PATH.
The file must contain a molecule’s description in either molfile or SMILES,
widely used file formats that can be exported from any chemical drawing program."
  (interactive "sEnter molecule name for pubchem search: ")
  (insert (shell-command-to-string
           (concat "python -m mol2chemfigPy3 "
                   (tochemfig-args-builder '(("input" . "file"))) " " path))))

;;;###autoload
(defun tochemfig-input-direct (molecule)
  "Generate chemfig code for a MOLECULE from a verbatim string."
  (interactive "sEnter molecule as verbatim string: ")
  (insert (shell-command-to-string
           (concat "python -m mol2chemfigPy3 "
                   (tochemfig-args-builder '(("input" . "direct"))) " " molecule))))

;;;###autoload
(defun tochemfig-terse (molecule)
  "Generate chemfig code for a MOLECULE removing whitespaces and comments."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat "python -m mol2chemfigPy3 "
                   (tochemfig-args-builder '(("terse" . t))) " " molecule))))

;;;###autoload
(defun tochemfig-verbose (molecule)
  "Generate chemfig code for a MOLECULE leaving whitespaces and comments."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat "python -m mol2chemfigPy3 "
                   (tochemfig-args-builder '(("terse" . nil))) " " molecule))))

;;;###autoload
(defun tochemfig-strict (molecule)
  "Generate chemfig code for a MOLECULE strictly abiding by structure validation."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat "python -m mol2chemfigPy3 "
                   (tochemfig-args-builder '(("strict" . t))) " " molecule))))

;;;###autoload
(defun tochemfig-chill (molecule)
  "Generate chemfig code for a MOLECULE even if it fails structure validation."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat "python -m mol2chemfigPy3 "
                   (tochemfig-args-builder '(("strict" . nil))) " " molecule))))

;;;###autoload
(defun tochemfig-indent (molecule int)
  "Generate chemfig code for a MOLECULE and indent its branches by INT spaces.
Forced to be verbose, because indentation doesn't make sense otherwise."
  (interactive
   (list
    (read-string "sEnter molecule: ")
    (read-string "sEnter integer for indentation: ")))
  (insert (shell-command-to-string
           (concat "python -m mol2chemfigPy3 "
                   (tochemfig-args-builder
                    (list (cons "indent" int) (cons "terse" nil))) " " molecule))))

;;;###autoload
(defun tochemfig-recalculate-coordinates (molecule)
  "Generate chemfig code for a MOLECULE calculating new coordinates.
Existing coordinates are discarded and new ones are derived from structure."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat "python -m mol2chemfigPy3 "
                   (tochemfig-args-builder '(("recalculate-coordinates" . t))) " " molecule))))

;;;###autoload
(defun tochemfig-rotate (molecule angle flip flop)
  "Generate chemfig code for a MOLECULE and rotate it clockwise by a given ANGLE.
Then, choose if you want to FLIP it (horizontally) or FLOP it (vertically)."
  (interactive
   (list
    (read-string "sEnter molecule: ")
    (read-string "sEnter angle for rotation (write 0.0 to leave as is): ")
    (read-string "sFlipping horizontally? (t/nil) ")
    (read-string "sFlipping vertically? (t/nil) ")))
  (insert (shell-command-to-string
           (concat "python -m mol2chemfigPy3 "
                   (tochemfig-args-builder
                    (list
                     (cons "angle" angle)
                     (cons "flip" flip)
                     (cons "flop" flop))) " " molecule))))

;;;###autoload
(defun tochemfig-show-carbons (molecule)
  "Generate chemfig code for a MOLECULE and show element symbol for carbon atoms."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat "python -m mol2chemfigPy3 "
                   (tochemfig-args-builder '(("show-carbons" . t))) " " molecule))))

;;;###autoload
(defun tochemfig-show-methyls (molecule)
  "Generate chemfig code for a MOLECULE and show element symbols for methyl groups.
This is implied, if carbon atoms are already showed."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat "python -m mol2chemfigPy3 "
                   (tochemfig-args-builder '(("show-methyls" . t))) " " molecule))))

;;;###autoload
(defun tochemfig-add-hydrogens (molecule)
  "Generate chemfig code for a MOLECULE and show explicit symbols for hydrogen.
This will also trigger calculation of new coordinates for the entire molecule."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat "python -m mol2chemfigPy3 "
                   (tochemfig-args-builder '(("hydrogens" . "add"))) " " molecule))))

;;;###autoload
(defun tochemfig-delete-hydrogens (molecule)
  "Generate chemfig code for a MOLECULE and delete explicit symbols for hydrogen."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat "python -m mol2chemfigPy3 "
                   (tochemfig-args-builder '(("hydrogens" . "delete"))) " " molecule))))

;;;###autoload
(defun tochemfig-aromatic-circles (molecule)
  "Generate chemfig code for a MOLECULE and draw circles inside aromatic rings."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat "python -m mol2chemfigPy3 "
                   (tochemfig-args-builder '(("aromatic-circles" . t))) " " molecule))))

;;;###autoload
(defun tochemfig-fancy-bonds (molecule)
  "Generate chemfig code for a MOLECULE drawing fancier double and triple bonds."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat "python -m mol2chemfigPy3 "
                   (tochemfig-args-builder '(("fancy-bonds" . t))) " " molecule))))

;;;###autoload
(defun tochemfig-vanilla-bonds (molecule)
  "Generate chemfig code for a MOLECULE drawing standard double and triple bonds."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat "python -m mol2chemfigPy3 "
                   (tochemfig-args-builder '(("fancy-bonds" . nil))) " " molecule))))

;;;###autoload
(defun tochemfig-show-atom-numbers (molecule)
  "Generate chemfig code for a MOLECULE showing the molfile number of each atom."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat "python -m mol2chemfigPy3 "
                   (tochemfig-args-builder '(("atom-numbers" . t))) " " molecule))))


;;;###autoload
(defun tochemfig-hide-atom-numbers (molecule)
  "Generate chemfig code for a MOLECULE hiding the molfile number of each atom."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat "python -m mol2chemfigPy3 "
                   (tochemfig-args-builder '(("atom-numbers" . nil))) " " molecule))))

;;;###autoload
(defun tochemfig-scale-bond (molecule factor)
  "Generate chemfig code for a MOLECULE and scale the bonds by a given FACTOR."
  (interactive (list
                (read-string "sEnter molecule: ")
                (read-string "sEnter scaling factor: ")))
  (insert (shell-command-to-string
           (concat "python -m mol2chemfigPy3 "
                   (tochemfig-args-builder
                    (list
                     (cons "bond-scale" "scale")
                     (cons "bond-stretch" factor))) " " molecule))))

;;;###autoload
(defun tochemfig-normalize-bond (molecule average)
  "Generate chemfig code for a MOLECULE and normalize the bonds to a given AVERAGE."
  (interactive (list
                (read-string "sEnter molecule: ")
                (read-string "sEnter average length: ")))
  (insert (shell-command-to-string
           (concat "python -m mol2chemfigPy3 "
                   (tochemfig-args-builder
                    (list
                     (cons "bond-scale" "normalize")
                     (cons "bond-stretch" average))) " " molecule))))

;;;###autoload
(defun tochemfig-wrap (molecule)
  "Generate chemfig code for a MOLECULE and wrap it into a \\chemfig{...} command."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat "python -m mol2chemfigPy3 "
                   (tochemfig-args-builder '(("wrap-chemfig" . t))) " " molecule))))

;;;###autoload
(defun tochemfig-unwrap (molecule)
  "Generate chemfig code for a MOLECULE without wrapping it into any command."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat "python -m mol2chemfigPy3 "
                   (tochemfig-args-builder '(("wrap-chemfig" . nil))) " " molecule))))

(provide 'tochemfig)
;;; tochemfig.el ends here
