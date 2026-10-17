;;; tochemfig-atom.el --- Atom representation for chemfig generation -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2025 Giovanni Crisalfi

;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Translation of mol2chemfigPy3's `atom.py'.  An atom knows its element,
;; coordinates, charge, radical state and implicit hydrogens, and can render
;; itself to chemfig (choosing where to place hydrogens and charges so that
;; they do not collide with bonds).

;;; Code:

(require 'cl-lib)
(require 'chem-ir)

;; Transitional aliases while the atom struct lives in `chem-ir'.
(eval-and-compile
  (chem-ir-alias-accessor 'tochemfig-atom-create 'chem-atom-create)
  (chem-ir-alias-accessor 'tochemfig-atom-p 'chem-atom-p)
  (chem-ir-alias-accessor 'copy-tochemfig-atom 'copy-chem-atom)
  (dolist (slot '(options idx x y element hydrogens charge radical neighbors
                         bond-angles label number marker))
    (chem-ir-alias-accessor
     (intern (concat "tochemfig-atom-" (symbol-name slot)))
     (intern (concat "chem-atom-" (symbol-name slot))))))

(provide 'tochemfig-atom)
;;; tochemfig-atom.el ends here
