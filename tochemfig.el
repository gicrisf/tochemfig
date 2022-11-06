;;; tochemfig.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Giovanni Crisalfi
;;
;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Maintainer: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Created: novembre 04, 2022
;; Modified: novembre 04, 2022
;; Version: 0.1.0
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
;;
;;; Code:

(defgroup tochemfig nil

  "Manipulating molecules through LaTeX from Emacs."
  :prefix "tochemfig-"
  :group 'comm)

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

(defun tochemfig--args-builder (&optional xOpt)
  "Build default arguments which will be passed to mol2chemfig.
XOPT is an optional argument. If given, it must be an alist."
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
          (tochemfig-arg-cross-bond (or (cdr (assoc "cross-bond" xOpt)) "")))

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

;; Interactive functions

;;;###autoload
(defun tochemfig (molecule)
  "Generate chemfig code for a MOLECULE using the default settings."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " " (tochemfig--args-builder) " " molecule))))

;; The following one totally ignores defaults and directly inject custom flags

;;;###autoload
(defun tochemfig-custom (molecule custom_args)
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
                   (tochemfig--args-builder '(("input" . "pubchem"))) " " identifier))))

;;;###autoload
(defun tochemfig-input-file (path)
  "Generate chemfig code for a molecule from its file's PATH.
The file must contain a molecule’s description in either molfile or SMILES,
widely used file formats that can be exported from any chemical drawing program."
  (interactive "fEnter molecule name for pubchem search: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder '(("input" . "file"))) " " path))))

;;;###autoload
(defun tochemfig-input-direct (molecule)
  "Generate chemfig code for a MOLECULE from a verbatim string."
  (interactive "sEnter molecule as verbatim string: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder '(("input" . "direct"))) " " molecule))))

;;;###autoload
(defun tochemfig-terse (molecule)
  "Generate chemfig code for a MOLECULE removing whitespaces and comments."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder '(("terse" . t))) " " molecule))))

;;;###autoload
(defun tochemfig-verbose (molecule)
  "Generate chemfig code for a MOLECULE leaving whitespaces and comments."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder '(("terse" . nil))) " " molecule))))

;;;###autoload
(defun tochemfig-strict (molecule)
  "Generate chemfig code for a MOLECULE strictly abiding by structure validation."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder '(("strict" . t))) " " molecule))))

;;;###autoload
(defun tochemfig-chill (molecule)
  "Generate chemfig code for a MOLECULE even if it fails structure validation."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder '(("strict" . nil))) " " molecule))))

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
                   (tochemfig--args-builder
                    (list (cons "indent" int) (cons "terse" nil)))
                   " " molecule))))

;;;###autoload
(defun tochemfig-recalculate-coordinates (molecule)
  "Generate chemfig code for a MOLECULE calculating new coordinates.
Existing coordinates are discarded and new ones are derived from structure."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder '(("recalculate-coordinates" . t))) " " molecule))))

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
                   (tochemfig--args-builder
                    (list
                     (cons "angle" angle)
                     (cons "flip" flip)
                     (cons "flop" flop)))
                   " " molecule))))

;;;###autoload
(defun tochemfig-show-carbons (molecule)
  "Generate chemfig code for a MOLECULE and show element symbol for carbon atoms."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder '(("show-carbons" . t))) " " molecule))))

;;;###autoload
(defun tochemfig-show-methyls (molecule)
  "Generate chemfig code for a MOLECULE and show element symbols for methyl groups.
This is implied, if carbon atoms are already showed."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder '(("show-methyls" . t))) " " molecule))))

;;;###autoload
(defun tochemfig-add-hydrogens (molecule)
  "Generate chemfig code for a MOLECULE and show explicit symbols for hydrogen.
This will also trigger calculation of new coordinates for the entire molecule."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder '(("hydrogens" . "add"))) " " molecule))))

;;;###autoload
(defun tochemfig-delete-hydrogens (molecule)
  "Generate chemfig code for a MOLECULE and delete explicit symbols for hydrogen."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder '(("hydrogens" . "delete"))) " " molecule))))

;;;###autoload
(defun tochemfig-aromatic-circles (molecule)
  "Generate chemfig code for a MOLECULE and draw circles inside aromatic rings."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder '(("aromatic-circles" . t))) " " molecule))))

;;;###autoload
(defun tochemfig-markers (molecule markers)
  "Generate chemfig code for a MOLECULE and add unique MARKERS to each atom/bond."
  (interactive (list
                (read-string "sEnter molecule: ")
                (read-string "sEnter markers: ")))
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder
                    (list (cons "markers" markers)))
                   " " molecule))))

;;;###autoload
(defun tochemfig-fancy-bonds (molecule)
  "Generate chemfig code for a MOLECULE drawing fancier double and triple bonds."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder '(("fancy-bonds" . t))) " " molecule))))

;;;###autoload
(defun tochemfig-vanilla-bonds (molecule)
  "Generate chemfig code for a MOLECULE drawing standard double and triple bonds."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder '(("fancy-bonds" . nil))) " " molecule))))

;;;###autoload
(defun tochemfig-show-atom-numbers (molecule)
  "Generate chemfig code for a MOLECULE showing the molfile number of each atom."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder '(("atom-numbers" . t))) " " molecule))))

;;;###autoload
(defun tochemfig-hide-atom-numbers (molecule)
  "Generate chemfig code for a MOLECULE hiding the molfile number of each atom."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder '(("atom-numbers" . nil))) " " molecule))))

;;;###autoload
(defun tochemfig-bond-scale (molecule factor)
  "Generate chemfig code for a MOLECULE and scale the bonds by a given FACTOR."
  (interactive (list
                (read-string "sEnter molecule: ")
                (read-string "sEnter scaling factor: ")))
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder
                    (list
                     (cons "bond-scale" "scale")
                     (cons "bond-stretch" factor)))
                   " " molecule))))

;;;###autoload
(defun tochemfig-bond-normalize (molecule average)
  "Generate chemfig code for a MOLECULE and normalize the bonds to a given AVERAGE."
  (interactive (list
                (read-string "sEnter molecule: ")
                (read-string "sEnter average length: ")))
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder
                    (list
                     (cons "bond-scale" "normalize")
                     (cons "bond-stretch" average)))
                   " " molecule))))

;;;###autoload
(defun tochemfig-wrap-chemfig (molecule)
  "Generate chemfig code for a MOLECULE and wrap it into a \\chemfig{...} command."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder '(("wrap-chemfig" . t))) " " molecule))))

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
                   (tochemfig--args-builder
                    (list (cons "submol-name" submol)))
                   " " molecule))))

;;;###autoload
(defun tochemfig-unwrap (molecule)
  "Generate chemfig code for a MOLECULE without wrapping it into any command."
  (interactive "sEnter molecule: ")
  (insert (shell-command-to-string
           (concat tochemfig-default-command " "
                   (tochemfig--args-builder '(("wrap-chemfig" . nil))) " " molecule))))

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
                   (tochemfig--args-builder
                    (list (cons "submol-name" submol)
                          (cons "entry-atom" entryatom)
                          (cons "exit-atom" exitatom)))
                   " " molecule))))

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
                   (tochemfig--args-builder
                    (list (cons "cross-bond"
                                (mapconcat #'identity (tochemfig--collect-bonds) ",")) ))
                   " " molecule))))

(provide 'tochemfig)
;;; tochemfig.el ends here
