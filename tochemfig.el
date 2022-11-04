;;; tochemfig.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 gicrisf
;;
;; Author: gicrisf <giovanni.crisalfi@protonmail.com>
;; Maintainer: gicrisf <giovanni.crisalfi@protonmail.com>
;; Created: novembre 04, 2022
;; Modified: novembre 04, 2022
;; Version: 0.0.1
;; Keywords: convenience data extensions files languages lisp tex tools unix
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

  "Manipulating molecules from Emacs."
  :prefix "tochemfig-"
  :group 'comm)

;; Should I disable this default?
;; Maybe better working with two different functions
;;
;; With ’direct’, the argument is intrepreted directly; don’t forget to put quotes around it.
;; With ’pubchem’, the argument is treated as an identifier for the PubChem database."
(defcustom tochemfig-default-input 'file'
  "How to interpret the argument. With ’file’, mol2chemfig expects a filename."
  :group 'tochemfig
  :type 'string)

;; If you can still read it afterwards, Bill Gates wants your resume.
(defcustom tochemfig-default-terse nil
  "Remove all whitespace and comments from the output."
  :group 'tochemfig
  :type 'boolean)

;; If true, mol2chemfig will fail if Indigo reports that something is wrong with the molecule,
;; like a carbon with five bonds.
;; If false, mol2chemfig will ignore such errors.
(defcustom tochemfig-default-strict t
  "Abide by Indigo’s chemical structure validation."
  :group 'tochemfig
  :type 'boolean)

;; Without effect when ’terse’ option is passed.
;; Affects only the generated tex code, not the rendered molecule.
(defcustom tochemfig-default-indent 4
  "Number of spaces to use for indenting molecule branches in generated code."
  :group 'tochemfig
  :type 'integer)

;; For smiles input, this is performed implicitly.
(defcustom tochemfig-default-recalculate-coordinates nil
  "Discard existing coordinate and calculate new ones from covalent structure."
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

;; One of ’keep’, ’add’ or ’delete’.
;; Note that 5  ’add’ will also trigger calculation of new coordinates for the entire molecule.
;; Option ’keep’ does nothing.
(defcustom tochemfig-default-hydrogens 'keep'
  "How to deal with explicit hydrogen atoms."
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
;;  "Give each atom and each bond a unique marker that can be used for attaching electron movement arrows. With value ’a’, atom 2 will be labeled @{a2}, and its bond to atom 5 @{a2-5}."
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

;; The meaning of this option depends on the setting of the previous option.
;; Used as scaling factor (with tochemfig-default-bond-scale=scale)
;; or average (with tochemfig-default-bond-scale=normalize) for bond lengths.
(defcustom tochemfig-default-bond-stretch 1.0
  "Scaling factor or average for bond lengths (depends on bond scale option)."
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

(defun tochemfig-default-args-builder ()
  "Build default arguments which will be passed to mol2chemfig."
  (let ((args '()))
    (when (not (equal (format "%s" tochemfig-default-input) "file"))
      (push (concat "--input " (format "%s" tochemfig-default-input)) args))
    (when tochemfig-default-terse (push "--terse" args))
    (when tochemfig-default-strict (push "--strict" args))
    (when (not (equal tochemfig-default-indent 4))
      (push (concat "--indent " (format "%s" tochemfig-default-indent)) args))
    (when tochemfig-default-recalculate-coordinates (push "--recalculate-coordinates" args))
    (when (not (equal tochemfig-default-angle 0.0))
      (push (concat "--indent " (format "%s" tochemfig-default-indent)) args))
    (when tochemfig-default-relative-angles (push "--relative-angles" args))
    (when tochemfig-default-flip (push "--flip" args))
    (when tochemfig-default-flop (push "--flop" args))
    (when tochemfig-default-show-carbons (push "--show-carbons" args))
    (when tochemfig-default-show-methyls (push "--show-methyls" args))
    (when (not (equal (format "%s" tochemfig-default-hydrogens) "keep"))
      (push (concat "--hydrogens " (format "%s" tochemfig-default-hydrogens)) args))
    (when tochemfig-default-aromatic-circles (push "--aromatic-circles" args))
    (when tochemfig-default-fancy-bonds (push "--fancy-bonds" args))
    (when tochemfig-default-atom-numbers (push "--atom-numbers" args))
    (when (not (equal (format "%s" tochemfig-default-bond-scale) "normalize"))
      (push (concat "--bond-scale " (format "%s" tochemfig-default-bond-scale)) args))
    (when (not (equal tochemfig-default-bond-stretch 1.0))
      (push (concat "--bond-stretch " (format "%s" tochemfig-default-bond-stretch)) args))
    (when tochemfig-default-wrap-chemfig (push "--wrap-chemfig" args))

    ;; convert list to string
    ;; http://xahlee.info/emacs/emacs/elisp_list.html
    (let ((strargs (mapconcat #'identity args " ")))
      ;; print out the final string (for testing)
      ;; (message strargs)
      strargs)))

;;;###autoload
(defun tochemfig (molecule)
  "Generate chemfig code for a MOLECULE from mol or SMILES strings."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat "python -m mol2chemfigPy3 " (tochemfig-default-args-builder) molecule))))

(provide 'tochemfig)
;;; tochemfig.el ends here
