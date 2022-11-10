;;; tochemfig.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Giovanni Crisalfi
;;
;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Maintainer: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Created: novembre 04, 2022
;; Modified: novembre 04, 2022
;; Version: 0.1.3
;; Keywords: chemistry mol smiles chemfig convenience data extensions files languages lisp tex tools unix
;; Homepage: https://github.com/gicrisf/tochemfig
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;; This package requires a working Python environment
;;; with `mol2chemfig' or `mol2chemfigPy3' package installed OR
;;; you can use the `mol2chemfig' LUA web client.
;;
;;  Description
;;  Emacs interface to mol2chemfig. Generate chemfig code from mol or SMILES.
;;  From mol2chemfig documentation’s abstract:
;;  "mol2chemfig is a Python program that generates TeX graphics of chemical structures provided in molfile or SMILES format.
;;  Its output is written in the syntax of the chemfig package, which in turn is based on TiKZ."
;;
;;; Code:

(defgroup tochemfig nil

  "Manipulating molecules through LaTeX from Emacs."
  :prefix "tochemfig-"
  :group 'comm)

;; Edit this command as you prefer;
;; Write `python - m mol2chemfig' if you have installed the local version of mol2chemfig;
;; Write 'python -m mol2chemfigPy3' if you installed the Python3 version;
;; You could avoid the `python -m ' substring by adding `mol2chemfig` on your PATH;
;; To use the web client version, you should use 'mol2chemfig.lua' as command.
(defcustom tochemfig-default-command "python -m mol2chemfigPy3"
  "Command for calling mol2chemfig.
Can be used the original mol2chemfig, the LUA client or mol2chemfigPy3."
  :group 'tochemfig
  :type 'string)

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

(defun tochemfig--args-builder (molecule &optional xOpt)
  "Build arguments that will be passed to mol2chemfig.
You must give MOLECULE source. XOPT contains optional arguments."
  (let ((args '()))
    ;; Read optional parameters, if given;
    (let ((tochemfig-arg-input (or (cdr (assoc "input" xOpt)) tochemfig-default-input))
          (tochemfig-arg-terse (or (cdr (assoc "terse" xOpt)) tochemfig-default-terse))
          (tochemfig-arg-strict (or (cdr (assoc "strict" xOpt)) tochemfig-default-strict))
          (tochemfig-arg-indent (or (cdr (assoc "indent" xOpt)) tochemfig-default-indent))
          (tochemfig-arg-recalculate-coordinates (or (cdr (assoc "recalculate-coordinates" xOpt)) tochemfig-default-recalculate-coordinates))
          (tochemfig-arg-angle (or (cdr (assoc "angle" xOpt)) tochemfig-default-angle))
          (tochemfig-arg-relative-angles (or (cdr (assoc "relative-angles" xOpt)) tochemfig-default-relative-angles))
          (tochemfig-arg-flip (or (cdr (assoc "flip" xOpt)) tochemfig-default-flip))
          (tochemfig-arg-flop (or (cdr (assoc "flop" xOpt)) tochemfig-default-flop))
          (tochemfig-arg-show-carbons (or (cdr (assoc "show-carbons" xOpt)) tochemfig-default-show-carbons))
          (tochemfig-arg-show-methyls (or (cdr (assoc "show-methyls" xOpt)) tochemfig-default-show-methyls))
          (tochemfig-arg-hydrogens (or (cdr (assoc "hydrogens" xOpt)) tochemfig-default-hydrogens))
          (tochemfig-arg-aromatic-circles (or (cdr (assoc "aromatic-circles" xOpt)) tochemfig-default-aromatic-circles))
          (tochemfig-arg-markers (or (cdr (assoc "markers" xOpt)) nil))
          (tochemfig-arg-fancy-bonds (or (cdr (assoc "fancy-bonds" xOpt)) tochemfig-default-fancy-bonds))
          (tochemfig-arg-atom-numbers (or (cdr (assoc "atom-numbers" xOpt)) tochemfig-default-atom-numbers))
          (tochemfig-arg-bond-scale (or (cdr (assoc "bond-scale" xOpt)) tochemfig-default-bond-scale))
          (tochemfig-arg-bond-stretch (or (cdr (assoc "bond-stretch" xOpt)) tochemfig-default-bond-stretch))
          (tochemfig-arg-wrap-chemfig (or (cdr (assoc "wrap-chemfig" xOpt)) tochemfig-default-wrap-chemfig))
          (tochemfig-arg-submol-name (or (cdr (assoc "submol-name" xOpt)) ""))
          (tochemfig-arg-entry-atom (or (cdr (assoc "entry-atom" xOpt)) ""))
          (tochemfig-arg-exit-atom (or (cdr (assoc "exit-atom" xOpt)) ""))
          (tochemfig-arg-cross-bond (or (cdr (assoc "cross-bond" xOpt)) ""))
          (tochemfig-arg-output (or (cdr (assoc "output" xOpt)) "")))

      ;; Add optional post-molecule bash commands
      ;; You build the command from right to left
      (when (not (string= "" tochemfig-arg-output))
        (push (concat "> " (format "%s" tochemfig-arg-output)) args))

      ;; Add molecule;
      (push (concat "\"" molecule "\"" ) args)

      ;; Build the command substring list and typecheck the arguments on place;
      (when (not (equal (format "%s" tochemfig-arg-input) "file"))
        (push (concat "--input " (format "%s" tochemfig-arg-input)) args))
      (when tochemfig-arg-terse (push "--terse" args))
      (when tochemfig-arg-strict (push "--strict" args))
      (when (not (equal tochemfig-arg-indent 4))
        (push (concat "--indent " (format "%d" tochemfig-arg-indent)) args))
      (when tochemfig-arg-recalculate-coordinates (push "--recalculate-coordinates" args))
      (when (not (equal tochemfig-arg-angle 0.0))
        (push (concat "--angle " (format "%f" tochemfig-arg-angle)) args))
      (when tochemfig-arg-relative-angles (push "--relative-angles" args))
      (when tochemfig-arg-flip (push "--flip" args))
      (when tochemfig-arg-flop (push "--flop" args))
      (when tochemfig-arg-show-carbons (push "--show-carbons" args))
      (when tochemfig-arg-show-methyls (push "--show-methyls" args))
      (let ((hstr (format "%s" tochemfig-arg-hydrogens)))
        (when (not (equal hstr "keep"))
          (if (or (equal hstr "add") (equal hstr "delete"))
              (push (concat "--hydrogens " hstr) args)
            (error "Hydrogens can only be 'keep', 'add' or 'delete'"))))
      (when tochemfig-arg-aromatic-circles (push "--aromatic-circles" args))
      (when (boundp 'tochemfig-arg-markers)
        (push (concat "--markers" (format "%s" tochemfig-arg-markers)) args))
      (when tochemfig-arg-fancy-bonds (push "--fancy-bonds" args))
      (when tochemfig-arg-atom-numbers (push "--atom-numbers" args))
      (when (not (equal (format "%s" tochemfig-arg-bond-scale) "normalize"))
        (push (concat "--bond-scale " (format "%s" tochemfig-arg-bond-scale)) args))
      (when (not (equal tochemfig-arg-bond-stretch 1.0))
        (push (concat "--bond-stretch " (format "%s" tochemfig-arg-bond-stretch)) args))
      (when tochemfig-arg-wrap-chemfig (push "--wrap-chemfig" args))
      (when (not (string= "" tochemfig-arg-submol-name))
        (push (concat "--submol-name " (format "%s" tochemfig-arg-submol-name)) args))
      (when (not (string= "" tochemfig-arg-entry-atom))
        (push (concat "--entry-atom " (format "%d" tochemfig-arg-entry-atom)) args))
      (when (not (string= "" tochemfig-arg-exit-atom))
        (push (concat "--exit-atom " (format "%d" tochemfig-arg-exit-atom)) args))
      (when (not (string= "" tochemfig-arg-cross-bond))
        (push (concat "--cross-bond=" (format "%s" tochemfig-arg-cross-bond)) args))

      ;; debug function
      ;; (message (mapconcat #'identity args " "))

      ;; Convert list to string:
      ;; http://xahlee.info/emacs/emacs/elisp_list.html
      (let ((strargs (mapconcat #'identity args " "))) strargs))))

(defun tochemfig--wizard-arg-selector (items)
  "Show a list of arguments, while keeping the current ITEMS on the side."
  ;; Store data in cons like this:
  ;; ("input (selected: direct)" . "input")
  ;;
  ;; Debug test
  ;; (when (assoc "terse" items) (message (cdr (assoc "terse" items))))
  ;; Debug test
  ;; (let ((wiz (mapconcat #'identity items " "))) wiz)
  ;;
  ;; ... So, I know for sure now that the data are optimally passed in the loop;
  ;; Now we have to extract the data;
  (let* ((args (list (cons "input" (if (assoc "input" items)
                                       (cdr (assoc "input" items)) tochemfig-default-input))
                     ;; TODO make a function out of this one; the code is too redundant.
                     (cons "terse" (if (assoc "terse" items)
                                       (if (equal (cdr (assoc "terse" items)) t) "t" "nil")
                               tochemfig-default-terse))
                     (cons "strict" (if (assoc "strict" items)
                                       (if (equal (cdr (assoc "strict" items)) t) "t" "nil")
                               tochemfig-default-strict))
                     (cons "indent" (or (cdr (assoc "indent" items))
                               tochemfig-default-indent))
                     (cons "recalculate-coordinates" (if (assoc "recalculate-coordinates" items)
                                       (if (equal (cdr (assoc "recalculate-coordinates" items)) t) "t" "nil")
                               tochemfig-default-recalculate-coordinates))
                     (cons "angle" (or (cdr (assoc "angle" items))
                               tochemfig-default-angle))
                     (cons "relative-angles" (if (assoc "relative-angles" items)
                                                 (if (equal (cdr (assoc "relative-angles" items)) t) "t" "nil")
                                               tochemfig-default-relative-angles))
                     (cons "flip" (if (assoc "flip" items)
                                        (if (equal (cdr (assoc "flip" items)) t) "t" "nil")
                                      tochemfig-default-flip))
                     (cons "flop" (if (assoc "flop" items)
                                        (if (equal (cdr (assoc "flop" items)) t) "t" "nil")
                                      tochemfig-default-flop))
                     (cons "show-carbons" (if (assoc "show-carbons" items)
                                        (if (equal (cdr (assoc "show-carbons" items)) t) "t" "nil")
                                      tochemfig-default-show-carbons))
                     (cons "show-methyls" (if (assoc "show-methyls" items)
                                        (if (equal (cdr (assoc "show-methyls" items)) t) "t" "nil")
                                      tochemfig-default-show-methyls))
                     (cons "hydrogens" (or (cdr (assoc "hydrogens" items))
                               tochemfig-default-hydrogens))
                     (cons "aromatic-circles" (if (assoc "aromatic-circles" items)
                                        (if (equal (cdr (assoc "aromatic-circles" items)) t) "t" "nil")
                                      tochemfig-default-aromatic-circles))
                     (cons "markers" (or (cdr (assoc "markers" items)) ""))
                     (cons "fancy-bonds" (if (assoc "fancy-bonds" items)
                                        (if (equal (cdr (assoc "fancy-bonds" items)) t) "t" "nil")
                                      tochemfig-default-fancy-bonds))
                     (cons "atom-numbers" (if (assoc "atom-numbers" items)
                                        (if (equal (cdr (assoc "atom-numbers" items)) t) "t" "nil")
                                      tochemfig-default-atom-numbers))
                     (cons "bond-scale" (or (cdr (assoc "bond-scale" items))
                               tochemfig-default-bond-scale))
                     (cons "bond-stretch" (or (cdr (assoc "bond-stretch" items))
                               tochemfig-default-bond-stretch))
                     (cons "wrap-chemfig" (if (assoc "wrap-chemfig" items)
                                        (if (equal (cdr (assoc "wrap-chemfig" items)) t) "t" "nil")
                                      tochemfig-default-wrap-chemfig))
                     ;; TODO custom ones
                     (cons "submol-name" (or (cdr (assoc "submol-name" items)) ""))
                     (cons "entry-atom" (or (cdr (assoc "entry-atom" items)) ""))
                     (cons "exit-atom" (or (cdr (assoc "exit-atom" items)) ""))
                     (cons "cross-bond" (or (cdr (assoc "cross-bond" items)) ""))
                     (cons  "output" (or (cdr (assoc "output" items)) ""))))
         (choices
          (mapcar #'(lambda (arg)
                  (cons (format "%s (selected: %s)" (car arg) (cdr arg)) (car arg))) args))
         (choice
          (completing-read "Select what argument you wish to edit: "
                           (mapcar #'(lambda (el) (car el)) choices) )))

    ;; print choices for debug
    ;; (let ((strargs (mapconcat #'(lambda (arg) (concat (car arg) " - " (cdr arg))) choices " "))) strargs)

    (cdr (assoc choice choices))))

;; Interactive functions

;;;###autoload
(defun tochemfig-default (molecule)
  "Generate chemfig code for a MOLECULE using the default settings."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " " (tochemfig--args-builder molecule)))))

;; TODO make "tochemfig-custom" the most complex fun and name this "tochemfig-raw";
;; The following one totally ignores defaults and directly inject custom flags;
;;;###autoload
(defun tochemfig-custom-raw (molecule custom_args)
  "Generate chemfig code for a MOLECULE specifying all the needed CUSTOM_ARGS."
  (interactive
   (list
    (read-string "sEnter molecule: ")
    (read-string "sEnter custom arguments: ")))
  (insert (shell-command-to-string
           (concat tochemfig-default-command " " custom_args " " molecule))))

;; The following ones force a specific argument over the defaults leaving the rest untouched

;;;###autoload
(defun tochemfig-input-pubchem (identifier)
  "Generate chemfig code for a molecule retrieved with its pubchem IDENTIFIER.
Obviously, you have to be online for this input mode to work."
  (interactive "sEnter molecule name for pubchem search: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder identifier '(("input" . "pubchem")))))))

;;;###autoload
(defun tochemfig-input-file (path)
  "Generate chemfig code for a molecule from its file's PATH.
The file must contain a molecule’s description in either molfile or SMILES,
widely used file formats that can be exported from any chemical drawing program."
  (interactive "fEnter molecule location: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder path '(("input" . "file")))))))

;;;###autoload
(defun tochemfig-input-direct (molecule)
  "Generate chemfig code for a MOLECULE from a verbatim string."
  (interactive "sEnter molecule as verbatim string: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder molecule '(("input" . "direct")))))))

;;;###autoload
(defun tochemfig-input-direct-output-file (molecule path)
  "Generate chemfig code for a MOLECULE;
use output redirection to save it in a file, of which you must give the PATH."
  (interactive (list
    (read-string "sEnter molecule as verbatim string: ")
    (read-file-name "fEnter output location: ")))
  (shell-command-to-string
   (concat tochemfig-default-command " "
           (tochemfig--args-builder molecule (list
                                              (cons "input" "direct")
                                              (cons "output" path))))))

;;;###autoload
(defun tochemfig-input-file-output-file (inpath outpath)
  "Generate chemfig code for a molecule from a file placed in INPATH;
use output redirection to save it in a file, of which you must give the OUTPATH."
  (interactive (list
    (read-file-name "fEnter molecule location: ")
    (read-file-name "fEnter output location: ")))
  (shell-command-to-string
   (concat tochemfig-default-command " "
           (tochemfig--args-builder inpath (list
                                              (cons "input" "file")
                                              (cons "output" outpath))))))

;;;###autoload
(defun tochemfig-terse (molecule)
  "Generate chemfig code for a MOLECULE removing whitespaces and comments."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder molecule '(("terse" . t)))))))

;;;###autoload
(defun tochemfig-verbose (molecule)
  "Generate chemfig code for a MOLECULE leaving whitespaces and comments."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder molecule '(("terse" . nil)))))))

;;;###autoload
(defun tochemfig-strict (molecule)
  "Generate chemfig code for a MOLECULE strictly abiding by structure validation."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder molecule '(("strict" . t)))))))

;;;###autoload
(defun tochemfig-chill (molecule)
  "Generate chemfig code for a MOLECULE even if it fails structure validation."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder molecule '(("strict" . nil)))))))

;;;###autoload
(defun tochemfig-indent (molecule int)
  "Generate chemfig code for a MOLECULE and indent its branches by INT spaces.
Forced to be verbose, because indentation doesn't make sense otherwise."
  (interactive
   (list
    (read-string "Enter molecule: ")
    (read-number "Enter an integer for indentation: ")))
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder molecule
                    (list (cons "indent" int) (cons "terse" nil)))))))

;;;###autoload
(defun tochemfig-recalculate-coordinates (molecule)
  "Generate chemfig code for a MOLECULE calculating new coordinates.
Existing coordinates are discarded and new ones are derived from structure."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder molecule '(("recalculate-coordinates" . t)))))))

;;;###autoload
(defun tochemfig-rotate (molecule angle flip flop)
  "Generate chemfig code for a MOLECULE and rotate it clockwise by a given ANGLE.
Then, choose if you want to FLIP it (horizontally) or FLOP it (vertically)."
  (interactive
   (list
    (read-string "Enter molecule: ")
    (read-number "Enter rotation angle (write 0.0 to leave as is): ")
    (y-or-n-p "Flipping horizontally? ")
    (y-or-n-p "Flipping vertically?")))
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder molecule
                    (list
                     (cons "angle" angle)
                     (cons "flip" flip)
                     (cons "flop" flop)))))))

;;;###autoload
(defun tochemfig-show-carbons (molecule)
  "Generate chemfig code for a MOLECULE and show element symbol for carbon atoms."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder molecule '(("show-carbons" . t)))))))

;;;###autoload
(defun tochemfig-show-methyls (molecule)
  "Generate chemfig code for a MOLECULE and show element symbols for methyl groups.
This is implied, if carbon atoms are already showed."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder molecule '(("show-methyls" . t)))))))

;;;###autoload
(defun tochemfig-add-hydrogens (molecule)
  "Generate chemfig code for a MOLECULE and show explicit symbols for hydrogen.
This will also trigger calculation of new coordinates for the entire molecule."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder molecule '(("hydrogens" . "add")))))))

;;;###autoload
(defun tochemfig-delete-hydrogens (molecule)
  "Generate chemfig code for a MOLECULE and delete explicit symbols for hydrogen."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder molecule '(("hydrogens" . "delete")))))))

;;;###autoload
(defun tochemfig-aromatic-circles (molecule)
  "Generate chemfig code for a MOLECULE and draw circles inside aromatic rings."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder molecule '(("aromatic-circles" . t)))))))

;;;###autoload
(defun tochemfig-markers (molecule markers)
  "Generate chemfig code for a MOLECULE and add unique MARKERS to each atom/bond."
  (interactive (list
                (read-string "sEnter molecule: ")
                (read-string "sEnter markers: ")))
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder molecule
                    (list (cons "markers" markers)))))))

;;;###autoload
(defun tochemfig-fancy-bonds (molecule)
  "Generate chemfig code for a MOLECULE drawing fancier double and triple bonds."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder molecule '(("fancy-bonds" . t)))))))

;;;###autoload
(defun tochemfig-vanilla-bonds (molecule)
  "Generate chemfig code for a MOLECULE drawing standard double and triple bonds."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder molecule '(("fancy-bonds" . nil)))))))

;;;###autoload
(defun tochemfig-show-atom-numbers (molecule)
  "Generate chemfig code for a MOLECULE showing the molfile number of each atom."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder molecule '(("atom-numbers" . t)))))))

;;;###autoload
(defun tochemfig-hide-atom-numbers (molecule)
  "Generate chemfig code for a MOLECULE hiding the molfile number of each atom."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder molecule '(("atom-numbers" . nil)))))))

;;;###autoload
(defun tochemfig-bond-scale (molecule factor)
  "Generate chemfig code for a MOLECULE and scale the bonds by a given FACTOR."
  (interactive (list
                (read-string "sEnter molecule: ")
                (read-string "sEnter scaling factor: ")))
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder molecule
                    (list
                     (cons "bond-scale" "scale")
                     (cons "bond-stretch" factor)))))))

;;;###autoload
(defun tochemfig-bond-normalize (molecule average)
  "Generate chemfig code for a MOLECULE and normalize the bonds to a given AVERAGE."
  (interactive (list
                (read-string "sEnter molecule: ")
                (read-string "sEnter average length: ")))
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder molecule
                    (list
                     (cons "bond-scale" "normalize")
                     (cons "bond-stretch" average)))))))

;;;###autoload
(defun tochemfig-wrap-chemfig (molecule)
  "Generate chemfig code for a MOLECULE and wrap it into a \\chemfig{...} command."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder molecule '(("wrap-chemfig" . t)))))))

;;;###autoload
(defun tochemfig-wrap-submol (molecule submol)
  "Generate chemfig code for a MOLECULE and wrap it as SUBMOL.
The \\definesubmol macro defines a named shortcut for a molecule or fragment.
This is useful if you want to integrate the generated code into larger,
manually assembled structures or drawings."
  (interactive (list
                (read-string "sEnter molecule: ")
                (read-string "sEnter submol name: ")))
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder molecule
                    (list (cons "submol-name" submol)))))))

;;;###autoload
(defun tochemfig-unwrap (molecule)
  "Generate chemfig code for a MOLECULE without wrapping it into any command."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder molecule '(("wrap-chemfig" . nil)))))))

;;;###autoload
(defun tochemfig-partial-submol (molecule submol entryatom exitatom)
  "Generate chemfig code for a MOLECULE and wrap a fragment of it as SUBMOL.
The \\definesubmol macro defines a named shortcut for a molecule or fragment.
This is useful if you want to integrate the generated code into larger,
manually assembled structures or drawings.
ENTRYATOM is the number of first atom to be rendered.
EXITATOM is the number of last atom to be rendered."
  (interactive (list
                (read-string "sEnter molecule: ")
                (read-string "sEnter submol name: ")
                (read-string "nEnter entry atom: ")
                (read-string "nEnter exit atom: ")))
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder molecule
                    (list (cons "submol-name" submol)
                          (cons "entry-atom" entryatom)
                          (cons "exit-atom" exitatom)))))))

(defun tochemfig--read-bond ()
  "Pick a bond giving the start and the end atoms."
  (let ((start (read-number "Enter the start atom of the bond: "))
        (end (read-number "Enter the end atom of the bond: ")))
    (format "%d-%d" start end)))

(defun tochemfig--collect-bonds ()
  "Collect a bunch of bonds."
  (let ((bonds '())
        (continue t))
    ;; Repeat read-bond until the user stops it;
    (while continue
      (setq bonds (cons (tochemfig--read-bond) bonds))
      (when (not (y-or-n-p "Should we add another bond to the list?"))
        (setq continue nil)))
    bonds))

;;;###autoload
(defun tochemfig-cross-bond (molecule)
  "Generate chemfig code for a MOLECULE and specify special bonds to draw on top;
those should be drawn on top of others they cross over."
  (interactive "sEnter molecule: ")

  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder molecule
                    (list (cons "cross-bond"
                                (mapconcat #'identity (tochemfig--collect-bonds) ",")) ))))))

;;;###autoload
(defun tochemfig-wizard ()
  (interactive)
  (let ((wizargs '())
        (input tochemfig-default-input) ;; var here just for easiness of use;
        (continue t))
    (while continue
      (let ((selected (tochemfig--wizard-arg-selector wizargs)))
        ;; Now you should change the value of the argument and collect it in wizargs;
        ;; You can pass to the function a string with the value you're about to change
        ;; Se l'argomento richiede una stringa tra varie opzioni, proponi custom
        ;; Se l'argomento richiede un numero, vai di conseguenza ecc.

        ;; If the selected item want a bool...
        (if (member selected '("terse" "strict" "recalculate-coordinates" "relative-angles"
                               "flip" "flop" "show-carbons" "show-methyls" "fancy-bonds"
                               "aromatic-circles" "atom-numbers" "wrap-chemfig"))
            ;; Else if
            (if (equal (completing-read "Activating this one? " '("true" "false")) "false")
                (push (cons selected nil) wizargs)
              (push (cons selected t) wizargs))
          ;; Else if
          (if (equal selected "input")
              (let ((inpOpts '("file" "direct" "pubchem")))
                ;; This is a special assign, just to make it simpler to retrieve this;
                (setq input (completing-read "Select input mode: " inpOpts))
                (push (cons selected input) wizargs))
            ;; Else if
            (if (equal selected "indent")
                (push (cons selected (read-number "Enter an integer for indentation: ")) wizargs)
              ;; Else if
              (if (equal selected "angle")
                  (push (cons selected (read-number "Enter rotation grades : ")) wizargs)
                ;; Else if
                (if (equal selected "hydrogens")
                    (let ((hydroOpts '("add" "delete" "keep")))
                      (push (cons selected (completing-read "Adding or deleting hydrogens? " hydroOpts)) wizargs))
                  ;; Else if
                  (if (equal selected "markers")
                      (push (cons selected (read-string "Enter markers: ")) wizargs)
                    ;; Else if
                    (if (equal selected "bond-scale")
                        (let ((inpOpts '("scale" "normalize")))
                          (push (cons selected (completing-read "Scale or normalize bonds " inpOpts)) wizargs))
                      ;; Else if
                      (if (equal selected "bond-stretch")
                          (push (cons selected (read-number "Enter length factor : ")) wizargs)
                        ;; Else if
                        (if (equal selected "submol-name")
                            (push (cons selected (read-string "Enter markers: ")) wizargs)
                          ;; Else if
                          (if (equal selected "entry-atom")
                              (push (cons selected (read-number "Entry atom : ")) wizargs)
                            ;; Else if
                            (if (equal selected "exit-atom")
                                (push (cons selected (read-number "Exit atom : ")) wizargs)
                              ;; Else if
                              ;; TODO support "cross-bondS", plural
                              (if (equal selected "cross-bond")
                                  (push (cons selected (tochemfig--read-bond)) wizargs)
                                ;; Else if
                                (if (equal selected "output")
                                    (push (cons selected (read-file-name "fEnter location: ")) wizargs)
                                  ;; Else if
                                  (message "nessuno"))))))))))))))

        ;; stop the loop
        (if (y-or-n-p "Wanna edit another argument? ")
            (message "well, to the next argument, then.")
            (setq continue nil))))

    ;; get the molecule according to input
    (let ((molecule (if (equal input "file")
                        (read-file-name "Enter molecule location: ")
                      (read-string "Enter molecule: "))))

      ;; return the actual string
      (insert (shell-command-to-string
               (concat tochemfig-default-command " "
                       (tochemfig--args-builder molecule wizargs)))))))

(provide 'tochemfig)
;;; tochemfig.el ends here
