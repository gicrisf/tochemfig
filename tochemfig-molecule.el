;;; tochemfig-molecule.el --- Chemfig generation entry point -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2025 Giovanni Crisalfi

;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Transitional entry point.  Ingestion lives in `chem-indigo'; rendering
;; lives in `chem-exp-chemfig'.  This wrapper routes the current public API
;; through the exporter registry.

;;; Code:

(require 'chem-indigo)
(require 'chem-export)
(require 'chem-exp-chemfig)

(defun tochemfig-generate (mol options)
  "Generate chemfig code for molecule MOL with OPTIONS plist."
  (chem-export (chem-indigo-build mol options) 'chemfig options))

(provide 'tochemfig-molecule)
;;; tochemfig-molecule.el ends here
