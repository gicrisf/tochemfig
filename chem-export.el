;;; chem-export.el --- Exporter registry for chem-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Giovanni Crisalfi

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Exporters turn a prepared `chem-drawing' into an output string.  The
;; registry keeps exporters independent: loading an exporter library
;; registers it, and callers select it by name.

;;; Code:

(require 'cl-lib)

(defvar chem-exporters (make-hash-table :test #'eq)
  "Registered exporters, keyed by name.
Each value is a plist with :renderer and optional metadata.")

(defun chem-register-exporter (name renderer &optional metadata)
  "Register RENDERER under NAME.
METADATA is an optional plist; it may contain :label, :options, and
other exporter-specific information."
  (puthash name
           (list :renderer renderer :metadata metadata)
           chem-exporters)
  name)

(defun chem-exporter (name)
  "Return the registration plist for exporter NAME, or nil."
  (gethash name chem-exporters))

(defun chem-registered-exporters ()
  "Return the list of registered exporter names."
  (let (names)
    (maphash (lambda (name _) (push name names)) chem-exporters)
    (sort names #'string<)))

(defun chem-exporter-option-specs (name)
  "Return the option specs declared by exporter NAME."
  (plist-get (plist-get (chem-exporter name) :metadata) :options))

(defun chem-export (drawing exporter &optional options)
  "Render DRAWING with EXPORTER, passing OPTIONS to the exporter.
DRAWING must already be prepared; common drawing options are not applied
here."
  (let ((entry (chem-exporter exporter)))
    (unless entry
      (error "Unknown chem exporter: %s" exporter))
    (funcall (plist-get entry :renderer) drawing options)))

(provide 'chem-export)
;;; chem-export.el ends here
