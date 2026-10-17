;;; tochemfig-molecule.el --- Chemfig rendering pipeline -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2025 Giovanni Crisalfi

;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Transitional chemfig rendering pipeline.  The Indigo ingestion now lives in
;; `chem-indigo'; this file walks the prepared drawing tree and renders it.
;; The exporter extraction will move this code to `chem-exp-chemfig'.

;;; Code:

(require 'cl-lib)
(require 'chem-ir)
(require 'chem-indigo)
(require 'tochemfig-atom)
(require 'tochemfig-bond)
(require 'tochemfig-mappings)

;;; Rendering

(defun tochemfig--render-branches (mol output level bonds)
  "Append BONDS rendered as indented branches to OUTPUT.
OUTPUT is the reversed line accumulator; returns the updated accumulator."
  (let ((indent (or (plist-get (tochemfig-molecule-options mol) :indent) 4)))
    (dolist (bond bonds output)
      (let ((width (+ (* level indent) tochemfig--bond-code-width)))
        (setq output (cons (concat (make-string (1- width) ?\s) "(") output))
        (setq output (tochemfig--render mol output bond level))
        (setq output (cons (concat (make-string (1- width) ?\s) ")") output))))))

(defun tochemfig--render (mol output bond level)
  "Recursively render BOND into OUTPUT at LEVEL; returns the accumulator."
  (setq output (cons (tochemfig-bond-render bond level) output))
  (let ((branches (copy-sequence (tochemfig-bond-descendants bond))))
    (cond
     ((eq bond (tochemfig-molecule-exit-bond mol))
      (tochemfig--render-branches mol output (1+ level) branches))
     ((null branches) output)
     (t
      (let ((trunk-idx (cl-position-if #'tochemfig-bond-is-trunk branches))
            (first nil))
        (if trunk-idx
            (setq first (nth trunk-idx branches)
                  branches (append (cl-subseq branches 0 trunk-idx)
                                   (cl-subseq branches (1+ trunk-idx))))
          (setq first (car branches)
                branches (cdr branches)))
        (setq output (tochemfig--render-branches mol output (1+ level) branches))
        (tochemfig--render mol output first level))))))

(defun tochemfig--render-molecule (mol)
  "Render MOL to a list of lines."
  (nreverse (tochemfig--render mol nil (tochemfig-molecule-root mol) 0)))

;;; Public entry point

(defun tochemfig-generate (mol options)
  "Generate chemfig code for molecule MOL with OPTIONS plist."
  (let ((state (chem-indigo-build mol options)))
    (maphash (lambda (_ atom) (tochemfig-atom-score-angles atom))
             (chem-drawing-atoms state))
    (tochemfig--format-output options (tochemfig--render-molecule state))))

(provide 'tochemfig-molecule)
;;; tochemfig-molecule.el ends here
