;;; chem-reaction.el --- Reaction layout helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Giovanni Crisalfi

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Renderer-neutral layout for prepared `chem-reaction' objects.  This file
;; must not depend on Indigo or on an exporter.

;;; Code:

(require 'cl-lib)
(require 'chem-ir)
(require 'chem-geometry)

(defconst chem-reaction--default-spacing 0.3333
  "Default spacing between reaction layout parts.")

(defconst chem-reaction--default-arrow-length 4.0
  "Default arrow length in reaction coordinates.")

(defconst chem-reaction--default-plus-size 0.3
  "Default plus-sign box size in reaction coordinates.")

(defconst chem-reaction--arrow-height 0.3
  "Arrow layout box height in reaction coordinates.")

(defconst chem-reaction--text-height 0.3
  "Approximate text height in reaction coordinates.")

(defconst chem-reaction--text-char-width 0.18
  "Approximate text width per character in reaction coordinates.")

(defconst chem-reaction--text-above-y 0.15
  "Lower-left y coordinate for text above the arrow.")

(defconst chem-reaction--text-below-y -0.45
  "Lower-left y coordinate for text below the arrow.")

(defun chem-reaction--number (options key default)
  "Return KEY from OPTIONS as a number, or DEFAULT."
  (let ((value (plist-get options key)))
    (cond
     ((null value) (float default))
     ((numberp value) (float value))
     (t (user-error "Invalid %s value: %S" key value)))))

(defun chem-reaction--string (options key)
  "Return KEY from OPTIONS as a string, or the empty string."
  (let ((value (plist-get options key)))
    (cond
     ((null value) "")
     ((stringp value) value)
     (t (user-error "Invalid %s value: %S" key value)))))

(defun chem-reaction--text-width (text)
  "Return an approximate width for TEXT in reaction coordinates."
  (* chem-reaction--text-char-width (length (or text ""))))

(defun chem-reaction--text-part (role text center-x y)
  "Return a centered :text part with ROLE at CENTER-X and Y."
  (when (and text (not (string-empty-p text)))
    (let ((width (chem-reaction--text-width text)))
      (chem-reaction-part-create
       :kind :text
       :role role
       :text text
       :x (- center-x (/ width 2.0))
       :y y
       :width width
       :height chem-reaction--text-height
       :scale 1.0))))

(defun chem-reaction--arrow-style (options)
  "Return the normalized arrow style from OPTIONS."
  (pcase (or (plist-get options :reaction-arrow) :forward)
    ((or :forward 'forward "forward") :forward)
    ((or :reverse 'reverse "reverse") :reverse)
    ((or :equilibrium 'equilibrium "equilibrium") :equilibrium)
    (other (user-error "Invalid :reaction-arrow value: %S" other))))

(defun chem-reaction--conditions (options)
  "Return reaction conditions from OPTIONS."
  (list :above (chem-reaction--string options :reaction-above)
        :below (chem-reaction--string options :reaction-below)))

(defun chem-reaction--part-box (drawing)
  "Return (WIDTH . HEIGHT) for DRAWING's layout box."
  (let ((bounds (chem-drawing-bounds drawing)))
    (unless bounds
      (user-error "Cannot lay out an empty reaction component"))
    (cons (max 0.0 (- (nth 2 bounds) (nth 0 bounds)))
          (max 0.0 (- (nth 3 bounds) (nth 1 bounds))))))

(defun chem-reaction--layout-side (drawings kind cursor spacing plus-size)
  "Lay out DRAWINGS of KIND.
Return (PARTS . NEXT-CURSOR).  CURSOR is the left edge, SPACING is the
gap between parts, and PLUS-SIZE is the plus-sign box size."
  (let ((parts nil)
        (first t))
    (dolist (drawing drawings)
      (unless first
        (push (chem-reaction-part-create
               :kind :plus
               :text "+"
               :x cursor
               :y (- (/ plus-size 2.0))
               :width plus-size
               :height plus-size
               :scale 1.0)
              parts)
        (setq cursor (+ cursor plus-size spacing)))
      (let* ((box (chem-reaction--part-box drawing))
             (width (car box))
             (height (cdr box)))
        (push (chem-reaction-part-create
               :kind kind
               :drawing drawing
               :x cursor
               :y (- (/ height 2.0))
               :width width
               :height height
               :scale 1.0)
              parts)
        (setq cursor (+ cursor width spacing))
        (setq first nil)))
    (cons (nreverse parts) cursor)))

(defun chem-reaction-apply-layout (reaction options)
  "Compute REACTION's layout from OPTIONS and return REACTION."
  (let* ((spacing (chem-reaction--number
                   options :reaction-spacing
                   chem-reaction--default-spacing))
         (arrow-length (chem-reaction--number
                        options :reaction-arrow-length
                        chem-reaction--default-arrow-length))
         (plus-size (chem-reaction--number
                     options :reaction-plus-size
                     chem-reaction--default-plus-size))
         (agent-text
          (mapconcat #'chem-reaction-agent-label
                     (chem-reaction-agents reaction) ", "))
         (above (plist-get (chem-reaction-conditions reaction) :above))
         (below (plist-get (chem-reaction-conditions reaction) :below))
         (above-text
          (mapconcat #'identity
                     (delq nil (list (unless (string-empty-p agent-text)
                                       agent-text)
                                     (unless (string-empty-p above)
                                       above)))
                     ", "))
         (arrow-width (max arrow-length
                           (chem-reaction--text-width above-text)
                           (chem-reaction--text-width below)))
         (cursor 0.0)
         (parts nil))
    (let ((laid (chem-reaction--layout-side
                 (chem-reaction-reactants reaction)
                 :reactant cursor spacing plus-size)))
      (setq parts (car laid))
      (setq cursor (cdr laid)))
    (setq parts
          (append parts
                  (list (chem-reaction-part-create
                         :kind :arrow
                         :x cursor
                         :y (- (/ chem-reaction--arrow-height 2.0))
                         :width arrow-width
                         :height chem-reaction--arrow-height
                         :scale 1.0))))
    (setq cursor (+ cursor arrow-width spacing))
    (let ((laid (chem-reaction--layout-side
                 (chem-reaction-products reaction)
                 :product cursor spacing plus-size)))
      (setq parts (append parts (car laid))))
    (let* ((arrow (cl-find :arrow parts :key #'chem-reaction-part-kind))
           (center-x (+ (chem-reaction-part-x arrow)
                        (/ (chem-reaction-part-width arrow) 2.0)))
           (above-part (chem-reaction--text-part
                        (if (string-empty-p above) :agent-label :above)
                        above-text center-x chem-reaction--text-above-y))
           (below-part (chem-reaction--text-part
                        :below below center-x chem-reaction--text-below-y)))
      (dolist (part (list above-part below-part))
        (when part
          (setq parts (append parts (list part))))))
    (setf (chem-reaction-layout reaction) parts)
    reaction))

(defun chem-reaction-bounds (reaction)
  "Return (MIN-X MIN-Y MAX-X MAX-Y) for REACTION's layout, or nil."
  (let ((parts (chem-reaction-layout reaction))
        (min-x nil)
        (min-y nil)
        (max-x nil)
        (max-y nil))
    (dolist (part parts)
      (let ((x (chem-reaction-part-x part))
            (y (chem-reaction-part-y part))
            (width (chem-reaction-part-width part))
            (height (chem-reaction-part-height part)))
        (setq min-x (if min-x (min min-x x) x))
        (setq min-y (if min-y (min min-y y) y))
        (setq max-x (if max-x (max max-x (+ x width)) (+ x width)))
        (setq max-y (if max-y (max max-y (+ y height)) (+ y height)))))
    (when min-x
      (list min-x min-y max-x max-y))))

(provide 'chem-reaction)
;;; chem-reaction.el ends here
