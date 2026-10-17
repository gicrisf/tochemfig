;;; chem-options.el --- Common option registry for chem-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Giovanni Crisalfi

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Metadata for the common drawing options.  These options are applied by
;; `chem-indigo' and `chem-apply-drawing-options'; exporters render the
;; resulting IR attributes.
;;
;; The transient menu is manually grouped around these specs.  The metadata
;; is shared so future frontends can expose the same common surface.

;;; Code:

(require 'cl-lib)

(defconst chem-common-option-specs
  '((:key :strict
     :variable tochemfig-default-strict
     :label "Strict validation"
     :type boolean
     :group input)
    (:key :hydrogens
     :variable tochemfig-default-hydrogens
     :label "Hydrogens"
     :type choice
     :choices (keep add delete)
     :group input)
    (:key :recalculate-coordinates
     :variable tochemfig-default-recalculate-coordinates
     :label "Recalculate coordinates"
     :type boolean
     :group input)
    (:key :angle
     :variable tochemfig-default-angle
     :label "Rotation angle"
     :type number
     :group geometry)
    (:key :flip
     :variable tochemfig-default-flip
     :label "Flip horizontally"
     :type boolean
     :group geometry)
    (:key :flop
     :variable tochemfig-default-flop
     :label "Flip vertically"
     :type boolean
     :group geometry)
    (:key :bond-scale
     :variable tochemfig-default-bond-scale
     :label "Bond scaling"
     :type choice
     :choices (normalize keep scale)
     :group bonds)
    (:key :bond-stretch
     :variable tochemfig-default-bond-stretch
     :label "Bond stretch"
     :type number
     :group bonds)
    (:key :show-carbons
     :variable tochemfig-default-show-carbons
     :label "Show carbons"
     :type boolean
     :group atoms)
    (:key :show-methyls
     :variable tochemfig-default-show-methyls
     :label "Show methyls"
     :type boolean
     :group atoms)
    (:key :atom-numbers
     :variable tochemfig-default-atom-numbers
     :label "Atom numbers"
     :type boolean
     :group atoms)
    (:key :aromatic-circles
     :variable tochemfig-default-aromatic-circles
     :label "Aromatic circles"
     :type boolean
     :group bonds)
    (:key :fancy-bonds
     :variable tochemfig-default-fancy-bonds
     :label "Fancy bonds"
     :type boolean
     :group bonds)
    (:key :markers
     :variable tochemfig-default-markers
     :label "Marker prefix"
     :type string
     :group output)
    (:key :cross-bond
     :variable tochemfig-default-cross-bond
     :label "Cross bonds"
     :type cross-bond
     :group advanced)
    (:key :entry-atom
     :variable tochemfig-default-entry-atom
     :label "Entry atom"
     :type optional-number
     :group sub-molecule)
    (:key :exit-atom
     :variable tochemfig-default-exit-atom
     :label "Exit atom"
     :type optional-number
     :group sub-molecule))
  "Common drawing option metadata.")

(defun chem-common-option-specs-for (group)
  "Return the common option specs in GROUP."
  (cl-remove-if-not (lambda (spec) (eq (plist-get spec :group) group))
                    chem-common-option-specs))

(provide 'chem-options)
;;; chem-options.el ends here
