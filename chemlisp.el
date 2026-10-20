;;; chemlisp.el --- S-expression drawing language for chem-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Giovanni Crisalfi

;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; EXPERIMENTAL: CheMLisp is an experimental drawing DSL.  Its syntax, public
;; API, and rendered output may change without notice.  Do not treat it as a
;; stable file format yet.
;;
;; CheMLisp compiles a hand-authored S-expression drawing into the shared
;; `chem-drawing' IR.  It is a source format for drawings, not a replacement
;; for SMILES or MOL.  The compiler builds the graph, traversal tree, labels,
;; and rings directly, then uses Indigo for the default coordinates and
;; implicit hydrogen perception.
;;
;; A document contains top-level `chem-def' forms and exactly one drawing form:
;;
;;   (chem-def hydroxyl ()
;;     (O))
;;
;;   (molecule
;;     (C
;;       (C (hydroxyl))))
;;
;; `chem-let' binds local values:
;;
;;   (chem-let ((hydroxyl (O)))
;;     (molecule (C (C (hydroxyl)))))
;;
;; Definitions and local bindings expand away before IR construction.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'chem-ir)
(require 'chem-geometry)
(require 'chem-export)

(eval-when-compile (require 'indigo-mol))
(eval-when-compile (require 'indigo-iter))
(eval-when-compile (require 'indigo-atom))

(declare-function indigo-layout "indigo-mol" (molecule))
(declare-function indigo-xyz "indigo-module" (atom))
(declare-function indigo-count-implicit-hydrogens "indigo-module" (atom))
(declare-function indigo-index "indigo-module" (atom))
(declare-function indigo-map "indigo-iter" (function iterator))
(declare-function indigo-iterate-atoms "indigo-module" (molecule))
(declare-function indigo-free "indigo-module" (handle))
(declare-function indigo-load-molecule-from-string "indigo-io" (string))

(define-error 'chemlisp-error "CheMLisp error" 'user-error)

(defvar chemlisp--current-source nil
  "Source string used for diagnostics, or nil.")

;;; Elements and names

(defconst chemlisp--aromatic-elements
  '((c . "C") (n . "N") (o . "O") (s . "S") (p . "P") (b . "B")
    (se . "Se") (as . "As"))
  "Lowercase aromatic atoms mapped to their element symbols.")

(defconst chemlisp--elements
  '("H" "He" "Li" "Be" "B" "C" "N" "O" "F" "Ne"
    "Na" "Mg" "Al" "Si" "P" "S" "Cl" "Ar" "K" "Ca"
    "Sc" "Ti" "V" "Cr" "Mn" "Fe" "Co" "Ni" "Cu" "Zn"
    "Ga" "Ge" "As" "Se" "Br" "Kr" "Rb" "Sr" "Y" "Zr"
    "Nb" "Mo" "Tc" "Ru" "Rh" "Pd" "Ag" "Cd" "In" "Sn"
    "Sb" "Te" "I" "Xe" "Cs" "Ba" "La" "Ce" "Pr" "Nd"
    "Pm" "Sm" "Eu" "Gd" "Tb" "Dy" "Ho" "Er" "Tm" "Yb"
    "Lu" "Hf" "Ta" "W" "Re" "Os" "Ir" "Pt" "Au" "Hg"
    "Tl" "Pb" "Bi" "Po" "At" "Rn" "Fr" "Ra" "Ac" "Th"
    "Pa" "U" "Np" "Pu" "Am" "Cm" "Bk" "Cf" "Es" "Fm"
    "Md" "No" "Lr" "Rf" "Db" "Sg" "Bh" "Hs" "Mt" "Ds"
    "Rg" "Cn" "Nh" "Fl" "Mc" "Lv" "Ts" "Og")
  "Supported element symbols.")

(defconst chemlisp--reserved-form-names
  '(molecule chem-let chem-let* chem-def close ->)
  "Names reserved for CheMLisp forms.")

(defconst chemlisp--atom-keys
  '(:id :bond :stereo :angle :turn :length :charge :h :radical :show :exit :at)
  "Attributes accepted on an atom node.")

(defconst chemlisp--use-keys
  '(:bond :stereo :angle :turn :length :charge :h :radical :show :exit :at)
  "Attributes accepted on a value use.")

(defconst chemlisp--template-use-keys
  '(:bond :stereo :angle :turn :length :charge :h :radical :show :exit :at)
  "Reserved use-site attributes on a template call.")

(defconst chemlisp--hole-keys chemlisp--template-use-keys
  "Attributes accepted on a template hole.")

(defconst chemlisp--closure-keys
  '(:bond :stereo)
  "Attributes accepted on a closure.")

(defconst chemlisp--molecule-keys
  '(:rotation :flip-x :flip-y :scale :translation)
  "Attributes accepted on a molecule form.")

(defconst chemlisp--reserved-attr-keys
  (delete-dups
   (append chemlisp--atom-keys chemlisp--molecule-keys '(:hydrogens)))
  "Attribute keywords that cannot be used as parameter names.")

(defun chemlisp--position-string (pos)
  "Return a human-readable line and column for POS."
  (if (and chemlisp--current-source (integerp pos))
      (let ((line 1)
            (column 1)
            (limit (min pos (length chemlisp--current-source))))
        (dotimes (i limit)
          (if (= (aref chemlisp--current-source i) ?\n)
              (setq line (1+ line)
                    column 1)
            (setq column (1+ column))))
        (format "line %d, column %d" line column))
    (format "position %s" pos)))

(defun chemlisp--error (fmt &rest args)
  "Signal a CheMLisp error with FMT and ARGS."
  (signal 'chemlisp-error (list (apply #'format fmt args))))

(defun chemlisp--error-at (pos fmt &rest args)
  "Signal a CheMLisp error at POS with FMT and ARGS."
  (signal 'chemlisp-error
          (list (format "%s: %s"
                        (chemlisp--position-string pos)
                        (apply #'format fmt args)))))

(defun chemlisp--element-info (symbol)
  "Return (ELEMENT . AROMATIC-P) for SYMBOL, or nil."
  (when (symbolp symbol)
    (let ((aromatic (cdr (assq symbol chemlisp--aromatic-elements))))
      (cond
       (aromatic (cons aromatic t))
       ((and (not (memq symbol '(t nil)))
             (member (symbol-name symbol) chemlisp--elements))
        (cons (symbol-name symbol) nil))))))

(defun chemlisp--valid-id-p (symbol)
  "Return non-nil when SYMBOL is a valid atom id."
  (and (symbolp symbol)
       (not (keywordp symbol))
       (not (memq symbol '(t nil)))
       (let ((name (symbol-name symbol)))
         (and (> (length name) 0)
              (not (memq (aref name 0) '(?= ?> ?< ?~ ?_)))
              (not (string-prefix-p "≡" name))))))

(defun chemlisp--valid-name-p (symbol &optional parameter-p)
  "Return non-nil when SYMBOL is a valid definition or binding name.
When PARAMETER-P is non-nil, also reject names that collide with reserved
attribute keywords."
  (and (symbolp symbol)
       (not (keywordp symbol))
       (not (memq symbol '(t nil)))
       (not (chemlisp--element-info symbol))
       (not (memq symbol chemlisp--reserved-form-names))
       (let ((name (symbol-name symbol)))
         (and (> (length name) 0)
              (not (memq (aref name 0) '(?= ?> ?< ?~ ?_)))
              (not (string-prefix-p "≡" name))))
       (or (not parameter-p)
           (not (memq (intern (concat ":" (symbol-name symbol)))
                      chemlisp--reserved-attr-keys)))))

;;; Structs

(cl-defstruct (chemlisp-document (:constructor chemlisp-document-create))
  defs drawing source)

(cl-defstruct (chemlisp-def (:constructor chemlisp-def-create))
  name params body env pos)

(cl-defstruct (chemlisp-param (:constructor chemlisp-param-create))
  value)

(cl-defstruct (chemlisp-nil (:constructor chemlisp-nil-create))
  pos)

(cl-defstruct (chemlisp-node (:constructor chemlisp-node-create))
  head attrs children pos key)

(cl-defstruct (chemlisp-closure (:constructor chemlisp-closure-create))
  target attrs pos target-key target-index)

(cl-defstruct (chemlisp-molecule (:constructor chemlisp-molecule-create))
  attrs roots pos)

(cl-defstruct (chemlisp-let (:constructor chemlisp-let-create))
  sequential-p bindings body pos)

(cl-defstruct (chemlisp-expander (:constructor chemlisp-expander-create))
  counter stack global-env)

(cl-defstruct (chemlisp-scope (:constructor chemlisp-scope-create))
  ids)

(cl-defstruct (chemlisp-tree (:constructor chemlisp-tree-create))
  key attrs children link-p from-key root-p node)

(cl-defstruct (chemlisp-tree-closure (:constructor chemlisp-tree-closure-create))
  target-key target-index attrs pos)

;;; Reader

(defun chemlisp-read (source)
  "Read all top-level forms from SOURCE.
Return a list of (FORM . POSITION).  Read-time evaluation is disabled."
  (unless (stringp source)
    (chemlisp--error "Source must be a string"))
  (let ((position 0)
        (length (length source))
        (forms nil))
    (condition-case err
        (while (< position length)
          (let* ((start position)
                 (result (with-no-warnings
                           (let ((read-eval nil)
                                 (read-circle nil))
                             (read-from-string source position)))))
            (setq position (cdr result))
            (push (cons (car result) start) forms)))
      (end-of-file nil)
      (error
       (chemlisp--error-at position "Cannot read form: %s"
                           (error-message-string err))))
    (nreverse forms)))

;;; Parser

(defun chemlisp--parse-keyword-pairs (items pos)
  "Parse leading keyword pairs from ITEMS.
Return (ATTRIBUTES . REST).  POS is used for diagnostics."
  (let ((rest items)
        (attributes nil))
    (while (and rest (keywordp (car rest)))
      (unless (cdr rest)
        (chemlisp--error-at pos "Missing value for %s" (car rest)))
      (push (cons (car rest) (cadr rest)) attributes)
      (setq rest (cddr rest)))
    (cons (nreverse attributes) rest)))

(defun chemlisp--split-marker (symbol)
  "Split a leading bond marker from SYMBOL.
Return (HEAD . ATTRIBUTES)."
  (let ((name (symbol-name symbol)))
    (cond
     ((string-prefix-p "≡" name)
      (let ((head (substring name 1)))
        (when (string-empty-p head)
          (chemlisp--error "Bond marker has no atom: %s" symbol))
        (cons (intern head) '((:bond . :triple)))))
     ((and (> (length name) 0) (eq (aref name 0) ?=))
      (let ((head (substring name 1)))
        (when (string-empty-p head)
          (chemlisp--error "Bond marker has no atom: %s" symbol))
        (cons (intern head) '((:bond . :double)))))
     ((and (> (length name) 0) (eq (aref name 0) ?>))
      (let ((head (substring name 1)))
        (when (string-empty-p head)
          (chemlisp--error "Bond marker has no atom: %s" symbol))
        (cons (intern head) '((:bond . :single) (:stereo . :up)))))
     ((and (> (length name) 0) (eq (aref name 0) ?<))
      (let ((head (substring name 1)))
        (when (string-empty-p head)
          (chemlisp--error "Bond marker has no atom: %s" symbol))
        (cons (intern head) '((:bond . :single) (:stereo . :down)))))
     ((and (> (length name) 0) (eq (aref name 0) ?~))
      (let ((head (substring name 1)))
        (when (string-empty-p head)
          (chemlisp--error "Bond marker has no atom: %s" symbol))
        (cons (intern head) '((:bond . :aromatic)))))
     ((and (> (length name) 0) (eq (aref name 0) ?_))
      (let ((head (substring name 1)))
        (when (string-empty-p head)
          (chemlisp--error "Bond marker has no atom: %s" symbol))
        (cons (intern head) '((:bond . :link)))))
     (t (cons symbol nil)))))

(defun chemlisp--parse-node (raw pos)
  "Parse RAW as an atom or use node at POS."
  (let* ((head (car raw))
         (split (chemlisp--split-marker head))
         (node-head (car split))
         (marker-attrs (cdr split))
         (parsed (chemlisp--parse-keyword-pairs (cdr raw) pos))
         (attrs (append marker-attrs (car parsed)))
         (children (mapcar (lambda (child) (chemlisp--parse-form child pos))
                           (cdr parsed))))
    (chemlisp-node-create :head node-head
                          :attrs attrs
                          :children children
                          :pos pos)))

(defun chemlisp--parse-closure (raw pos)
  "Parse closure form RAW at POS."
  (unless (>= (length raw) 2)
    (chemlisp--error-at pos "A closure needs a target"))
  (let ((target (nth 1 raw))
        (parsed (chemlisp--parse-keyword-pairs (cddr raw) pos)))
    (unless (or (integerp target)
                (and (symbolp target) (not (memq target '(t nil)))))
      (chemlisp--error-at pos "Invalid closure target: %S" target))
    (when (cdr parsed)
      (chemlisp--error-at pos "A closure cannot have children"))
    (chemlisp-closure-create :target target
                             :attrs (car parsed)
                             :pos pos)))

(defun chemlisp--parse-molecule (raw pos)
  "Parse molecule form RAW at POS."
  (let* ((parsed (chemlisp--parse-keyword-pairs (cdr raw) pos))
         (attrs (car parsed))
         (roots (mapcar (lambda (root) (chemlisp--parse-form root pos))
                        (cdr parsed))))
    (unless roots
      (chemlisp--error-at pos "A molecule needs at least one root"))
    (chemlisp-molecule-create :attrs attrs :roots roots :pos pos)))

(defun chemlisp--parse-def (raw pos)
  "Parse chem-def form RAW at POS."
  (unless (= (length raw) 4)
    (chemlisp--error-at pos "chem-def needs a name, parameter list, and body"))
  (let ((name (nth 1 raw))
        (params (nth 2 raw))
        (body (nth 3 raw)))
    (unless (symbolp name)
      (chemlisp--error-at pos "chem-def name must be a symbol"))
    (unless (proper-list-p params)
      (chemlisp--error-at pos "chem-def parameter list must be a proper list"))
    (dolist (param params)
      (unless (symbolp param)
        (chemlisp--error-at pos "chem-def parameter must be a symbol: %S" param)))
    (chemlisp-def-create :name name
                         :params params
                         :body (chemlisp--parse-form body pos)
                         :pos pos)))

(defun chemlisp--parse-let (raw pos sequential-p)
  "Parse chem-let or chem-let* form RAW at POS.
SEQUENTIAL-P selects chem-let*."
  (unless (= (length raw) 3)
    (chemlisp--error-at pos "chem-let needs a binding list and body"))
  (let ((binding-forms (nth 1 raw))
        (body (nth 2 raw)))
    (unless (proper-list-p binding-forms)
      (chemlisp--error-at pos "chem-let bindings must be a proper list"))
    (let ((bindings nil))
      (dolist (binding binding-forms)
        (unless (and (proper-list-p binding) (= (length binding) 2))
          (chemlisp--error-at pos "Each binding must be (NAME VALUE)"))
        (let ((name (car binding))
              (value (cadr binding)))
          (unless (symbolp name)
            (chemlisp--error-at pos "Binding name must be a symbol: %S" name))
          (push (cons name (chemlisp--parse-form value pos)) bindings)))
      (chemlisp-let-create :sequential-p sequential-p
                           :bindings (nreverse bindings)
                           :body (chemlisp--parse-form body pos)
                           :pos pos))))

(defun chemlisp--parse-form (raw pos)
  "Parse RAW at POS into a CheMLisp AST form."
  (cond
   ((null raw) (chemlisp-nil-create :pos pos))
   ((not (proper-list-p raw))
    (chemlisp--error-at pos "Expected a proper list, got %S" raw))
   ((not (symbolp (car raw)))
    (chemlisp--error-at pos "Form head must be a symbol: %S" (car raw)))
   (t
    (let ((head (car raw)))
      (cond
       ((eq head 'chem-def) (chemlisp--parse-def raw pos))
       ((eq head 'chem-let) (chemlisp--parse-let raw pos nil))
       ((eq head 'chem-let*) (chemlisp--parse-let raw pos t))
       ((eq head 'molecule) (chemlisp--parse-molecule raw pos))
       ((memq head '(close ->)) (chemlisp--parse-closure raw pos))
       ((keywordp head)
        (chemlisp--error-at pos "Invalid form head: %S" head))
       (t (chemlisp--parse-node raw pos)))))))

;;; Attribute validation

(defun chemlisp--normalize-key (key)
  "Normalize attribute KEY."
  (if (eq key :hydrogens) :h key))

(defun chemlisp--keyword-value (value)
  "Return VALUE as a keyword when it is a plain symbol."
  (cond
   ((keywordp value) value)
   ((and (symbolp value) (not (memq value '(t nil))))
    (intern (concat ":" (symbol-name value))))
   (t value)))

(defun chemlisp--check-attr-value (key value pos)
  "Validate VALUE for attribute KEY at POS.  Return the normalized value."
  (pcase key
    (:bond
     (let ((value (chemlisp--keyword-value value)))
       (unless (memq value '(:single :double :triple :aromatic :link))
         (chemlisp--error-at pos "Invalid bond order: %S" value))
       value))
    (:stereo
     (let ((value (chemlisp--keyword-value value)))
       (unless (memq value '(:up :down :either :cis :trans))
         (chemlisp--error-at pos "Invalid bond stereo: %S" value))
       value))
    (:show
     (let ((value (chemlisp--keyword-value value)))
       (unless (memq value '(:auto :always :never))
         (chemlisp--error-at pos "Invalid :show value: %S" value))
       value))
    ((or :angle :turn :length :rotation :scale)
     (unless (numberp value)
       (chemlisp--error-at pos "%s must be a number" key))
     (when (and (memq key '(:length :scale)) (<= value 0))
       (chemlisp--error-at pos "%s must be positive" key))
     value)
    ((or :charge :h :radical)
     (unless (integerp value)
       (chemlisp--error-at pos "%s must be an integer" key))
     (when (and (memq key '(:h :radical)) (< value 0))
       (chemlisp--error-at pos "%s cannot be negative" key))
     value)
    ((or :exit :flip-x :flip-y)
     (unless (memq value '(t nil))
       (chemlisp--error-at pos "%s must be t or nil" key))
     value)
    ((or :at :translation)
     (unless (and (proper-list-p value)
                  (= (length value) 2)
                  (cl-every #'numberp value))
       (chemlisp--error-at pos "%s must be a list of two numbers" key))
     value)
    (:id
     (unless (chemlisp--valid-id-p value)
       (chemlisp--error-at pos "Invalid atom id: %S" value))
     value)
    (_ value)))

(defun chemlisp--validate-attrs (attrs allowed context pos)
  "Validate ATTRS for CONTEXT against ALLOWED keys at POS.
Return a normalized alist."
  (let ((seen nil)
        (result nil))
    (dolist (pair attrs)
      (let* ((key (chemlisp--normalize-key (car pair)))
             (value (cdr pair)))
        (unless (keywordp key)
          (chemlisp--error-at pos "Invalid attribute key in %s: %S" context key))
        (when (memq key seen)
          (chemlisp--error-at pos "Duplicate attribute %s in %s" key context))
        (unless (memq key allowed)
          (chemlisp--error-at pos "Unsupported attribute %s in %s" key context))
        (push key seen)
        (push (cons key (chemlisp--check-attr-value key value pos)) result)))
    (when (and (assq :angle result) (assq :turn result))
      (chemlisp--error-at pos "Use either :angle or :turn, not both"))
    (nreverse result)))

(defun chemlisp--attr (attrs key)
  "Return the value of KEY in ATTRS."
  (alist-get key attrs))

;;; Static validation

(defun chemlisp--validate-document (document)
  "Validate DOCUMENT."
  (let ((names (make-hash-table :test #'eq)))
    (dolist (def (chemlisp-document-defs document))
      (let ((name (chemlisp-def-name def)))
        (when (gethash name names)
          (chemlisp--error-at (chemlisp-def-pos def)
                              "Duplicate definition: %s" name))
        (puthash name t names)
        (chemlisp--validate-def def)))
    (chemlisp--validate-drawing-body (chemlisp-document-drawing document) nil)))

(defun chemlisp--validate-def (def)
  "Validate definition DEF."
  (let ((name (chemlisp-def-name def))
        (params (chemlisp-def-params def))
        (pos (chemlisp-def-pos def)))
    (unless (chemlisp--valid-name-p name)
      (chemlisp--error-at pos "Invalid definition name: %S" name))
    (let ((seen nil))
      (dolist (param params)
        (unless (chemlisp--valid-name-p param t)
          (chemlisp--error-at pos "Invalid parameter name: %S" param))
        (when (memq param seen)
          (chemlisp--error-at pos "Duplicate parameter: %s" param))
        (push param seen)))
    (chemlisp--validate-tree-form (chemlisp-def-body def) params)
    (chemlisp--validate-def-root (chemlisp-def-body def) params)
    (let ((counts (make-hash-table :test #'eq)))
      (chemlisp--check-param-use (chemlisp-def-body def) params counts pos)
      (dolist (param params)
        (unless (= 1 (gethash param counts 0))
          (chemlisp--error-at pos
                              "Parameter %s must be used exactly once" param))))))

(defun chemlisp--validate-tree-form (form params)
  "Validate FORM as a single-root tree form with PARAMS."
  (cond
   ((chemlisp-node-p form) nil)
   ((chemlisp-let-p form) (chemlisp--validate-let form params nil))
   ((chemlisp-nil-p form)
    (chemlisp--error-at (chemlisp-nil-pos form) "nil is not a valid tree"))
   ((chemlisp-molecule-p form)
    (chemlisp--error-at (chemlisp-molecule-pos form)
                        "A molecule form is not valid here"))
   ((chemlisp-closure-p form)
    (chemlisp--error-at (chemlisp-closure-pos form)
                        "A closure cannot be a root"))
   ((chemlisp-def-p form)
    (chemlisp--error-at (chemlisp-def-pos form)
                        "chem-def is only allowed at top level"))
   (t (chemlisp--error "Invalid tree form"))))

(defun chemlisp--validate-drawing-body (form params)
  "Validate FORM as a drawing body with PARAMS."
  (cond
   ((or (chemlisp-node-p form) (chemlisp-let-p form))
    (if (chemlisp-let-p form)
        (chemlisp--validate-let form params t)
      nil))
   ((chemlisp-molecule-p form)
    (chemlisp--validate-attrs (chemlisp-molecule-attrs form)
                              chemlisp--molecule-keys 'molecule
                              (chemlisp-molecule-pos form))
    (unless (chemlisp-molecule-roots form)
      (chemlisp--error-at (chemlisp-molecule-pos form)
                          "A molecule needs at least one root"))
    (dolist (root (chemlisp-molecule-roots form))
      (chemlisp--validate-tree-form root params)))
   (t (chemlisp--validate-tree-form form params))))

(defun chemlisp--validate-let (form params allow-molecule)
  "Validate chem-let FORM with PARAMS.
When ALLOW-MOLECULE is non-nil, the body may be a molecule form."
  (let ((names (make-hash-table :test #'eq))
        (pos (chemlisp-let-pos form)))
    (dolist (binding (chemlisp-let-bindings form))
      (let ((name (car binding))
            (value (cdr binding)))
        (unless (chemlisp--valid-name-p name t)
          (chemlisp--error-at pos "Invalid binding name: %S" name))
        (when (gethash name names)
          (chemlisp--error-at pos "Duplicate binding: %s" name))
        (puthash name t names)
        (when (memq name params)
          (chemlisp--error-at pos
                              "Binding %s shadows a template parameter" name))
        (chemlisp--validate-tree-form value params)))
    (if allow-molecule
        (chemlisp--validate-drawing-body (chemlisp-let-body form) params)
      (chemlisp--validate-tree-form (chemlisp-let-body form) params))))

(defun chemlisp--validate-def-root (form params)
  "Check that the root of definition body FORM has no incoming edge.
PARAMS is the definition parameter list."
  (cond
   ((chemlisp-node-p form)
    (when (memq (chemlisp-node-head form) params)
      (chemlisp--error-at (chemlisp-node-pos form)
                          "A template hole cannot be the definition root"))
    (dolist (key '(:bond :stereo :angle :turn :length :at))
      (when (assq key (chemlisp-node-attrs form))
        (chemlisp--error-at (chemlisp-node-pos form)
                            "Definition root cannot have %s" key))))
   ((chemlisp-let-p form)
    (chemlisp--validate-def-root (chemlisp-let-body form) params))
   (t nil)))

(defun chemlisp--check-param-use (form params counts pos)
  "Count parameter uses in FORM and validate holes.
PARAMS is the parameter list, COUNTS an eq hash, and POS the definition."
  (cond
   ((chemlisp-node-p form)
    (let ((head (chemlisp-node-head form)))
      (if (memq head params)
          (progn
            (when (chemlisp-node-children form)
              (chemlisp--error-at pos "Parameter %s cannot have children" head))
            (puthash head (1+ (gethash head counts 0)) counts))
        (dolist (child (chemlisp-node-children form))
          (chemlisp--check-param-use child params counts pos)))))
   ((chemlisp-closure-p form)
    (let ((target (chemlisp-closure-target form)))
      (when (and (symbolp target) (memq target params))
        (chemlisp--error-at pos "Closure target cannot be a parameter: %s"
                            target))))
   ((chemlisp-let-p form)
    (dolist (binding (chemlisp-let-bindings form))
      (when (memq (car binding) params)
        (chemlisp--error-at pos "Binding %s shadows a template parameter"
                            (car binding)))
      (chemlisp--check-param-use (cdr binding) params counts pos))
    (chemlisp--check-param-use (chemlisp-let-body form) params counts pos))
   ((chemlisp-molecule-p form)
    (chemlisp--error-at pos "A molecule form is not valid in a definition body"))
   ((chemlisp-def-p form)
    (chemlisp--error-at pos "chem-def is only allowed at top level"))
   ((chemlisp-nil-p form)
    (chemlisp--error-at pos "nil is not valid here"))))

;;; Expansion

(defun chemlisp--new-key (expander)
  "Return a fresh atom key from EXPANDER."
  (cl-incf (chemlisp-expander-counter expander)))

(defun chemlisp--new-scope ()
  "Return a fresh identifier scope."
  (chemlisp-scope-create :ids (make-hash-table :test #'eq)))

(defun chemlisp--merge-attrs (base overrides)
  "Merge OVERRIDES into BASE.  OVERRIDES wins."
  (let ((result (copy-sequence base)))
    (dolist (pair overrides)
      (let ((cell (assq (car pair) result)))
        (if cell
            (setcdr cell (cdr pair))
          (push (cons (car pair) (cdr pair)) result))))
    result))

(defun chemlisp--expand-atom (node env scope expander)
  "Expand atom NODE in ENV and SCOPE using EXPANDER."
  (let* ((pos (chemlisp-node-pos node))
         (attrs (chemlisp--validate-attrs (chemlisp-node-attrs node)
                                          chemlisp--atom-keys 'atom pos))
         (id (chemlisp--attr attrs :id))
         (key (chemlisp--new-key expander)))
    (when id
      (when (gethash id (chemlisp-scope-ids scope))
        (chemlisp--error-at pos "Duplicate atom id in this scope: %s" id))
      (puthash id key (chemlisp-scope-ids scope)))
    (chemlisp-node-create
     :head (chemlisp-node-head node)
     :attrs attrs
     :children (chemlisp--expand-children (chemlisp-node-children node)
                                          env scope expander)
     :pos pos
     :key key)))

(defun chemlisp--expand-children (children env scope expander)
  "Expand CHILDREN in ENV and SCOPE using EXPANDER."
  (let (result)
    (dolist (child children)
      (cond
       ((chemlisp-nil-p child)
        (chemlisp--error-at (chemlisp-nil-pos child)
                            "nil is not a valid child"))
       ((chemlisp-closure-p child)
        (push (chemlisp--expand-closure child scope) result))
       ((chemlisp-node-p child)
        (let ((expanded (chemlisp--expand-node child env scope expander)))
          (when expanded (push expanded result))))
       ((chemlisp-let-p child)
        (let ((expanded (chemlisp--expand-let child env scope expander)))
          (unless (chemlisp-node-p expanded)
            (chemlisp--error-at (chemlisp-let-pos child)
                                "chem-let in a child position must expand to one node"))
          (push expanded result)))
       (t
        (chemlisp--error "Invalid child"))))
    (nreverse result)))

(defun chemlisp--expand-closure (closure scope)
  "Expand CLOSURE in SCOPE."
  (let* ((pos (chemlisp-closure-pos closure))
         (attrs (chemlisp--validate-attrs (chemlisp-closure-attrs closure)
                                          chemlisp--closure-keys 'closure pos))
         (target (chemlisp-closure-target closure))
         (target-key nil)
         (target-index nil))
    (cond
     ((integerp target)
      (when (< target 1)
        (chemlisp--error-at pos "Closure index must be positive"))
      (setq target-index target))
     ((symbolp target)
      (setq target-key (gethash target (chemlisp-scope-ids scope)))
      (unless target-key
        (chemlisp--error-at pos "Unknown closure target: %s" target)))
     (t
      (chemlisp--error-at pos "Invalid closure target: %S" target)))
    (chemlisp-closure-create :target target
                             :attrs attrs
                             :pos pos
                             :target-key target-key
                             :target-index target-index)))

(defun chemlisp--expand-param (hole param)
  "Expand template HOLE using PARAM."
  (let* ((pos (chemlisp-node-pos hole))
         (attrs (chemlisp--validate-attrs (chemlisp-node-attrs hole)
                                          chemlisp--hole-keys 'hole pos))
         (value (chemlisp-param-value param)))
    (cond
     ((null value) nil)
     ((not (chemlisp-node-p value))
      (chemlisp--error-at pos "Template argument must be a node or nil"))
     (t
      (chemlisp-node-create
       :head (chemlisp-node-head value)
       :attrs (chemlisp--merge-attrs attrs (chemlisp-node-attrs value))
       :children (chemlisp-node-children value)
       :pos (chemlisp-node-pos value)
       :key (chemlisp-node-key value))))))

(defun chemlisp--expand-node (node env scope expander)
  "Expand node NODE in ENV and SCOPE using EXPANDER."
  (let* ((head (chemlisp-node-head node))
         (entry (assq head env)))
    (cond
     ((chemlisp-param-p (cdr entry))
      (chemlisp--expand-param node (cdr entry)))
     ((chemlisp-def-p (cdr entry))
      (chemlisp--expand-use node (cdr entry) env scope expander))
     ((chemlisp--element-info head)
      (chemlisp--expand-atom node env scope expander))
     (t
      (chemlisp--error-at (chemlisp-node-pos node)
                          "Unknown atom or definition: %s" head)))))

(defun chemlisp--def-expansion-env (def expander)
  "Return the environment used to expand DEF with EXPANDER."
  (or (chemlisp-def-env def)
      (chemlisp-expander-global-env expander)))

(defun chemlisp--expand-def-body (def env scope expander)
  "Expand the body of DEF in ENV and SCOPE using EXPANDER."
  (let ((result (chemlisp--expand-form (chemlisp-def-body def)
                                       env scope expander)))
    (unless (chemlisp-node-p result)
      (chemlisp--error-at (chemlisp-def-pos def)
                          "Definition %s must expand to one root"
                          (chemlisp-def-name def)))
    result))

(defun chemlisp--expand-use (node def env scope expander)
  "Expand use NODE of DEF in ENV and SCOPE using EXPANDER."
  (when (memq def (chemlisp-expander-stack expander))
    (chemlisp--error-at (chemlisp-node-pos node)
                        "Recursive definition: %s" (chemlisp-def-name def)))
  (push def (chemlisp-expander-stack expander))
  (unwind-protect
      (if (chemlisp-def-params def)
          (chemlisp--expand-template-use node def env scope expander)
        (chemlisp--expand-value-use node def env scope expander))
    (pop (chemlisp-expander-stack expander))))

(defun chemlisp--expand-value-use (node def env scope expander)
  "Expand value use NODE of DEF in ENV and SCOPE using EXPANDER."
  (let* ((pos (chemlisp-node-pos node))
         (attrs (chemlisp--validate-attrs (chemlisp-node-attrs node)
                                          chemlisp--use-keys 'use pos))
         (def-env (chemlisp--def-expansion-env def expander))
         (instance-scope (chemlisp--new-scope))
         (root (chemlisp--expand-def-body def def-env instance-scope expander))
         (children (chemlisp--expand-children
                    (chemlisp-node-children node) env scope expander)))
    (setf (chemlisp-node-attrs root)
          (chemlisp--merge-attrs (chemlisp-node-attrs root) attrs))
    (setf (chemlisp-node-children root)
          (append (chemlisp-node-children root) children))
    root))

(defun chemlisp--template-arguments (node params pos)
  "Split NODE attributes into parameter arguments and use-site attributes.
PARAMS is the definition parameter list.  POS is the use position.
Return (ARGUMENTS . USE-ATTRS) where ARGUMENTS is an alist of
\(PARAM . RAW-VALUE)."
  (let ((arguments nil)
        (use-attrs nil)
        (keyword-args nil))
    (dolist (pair (chemlisp-node-attrs node))
      (let* ((key (car pair))
             (name (intern (substring (symbol-name key) 1))))
        (cond
         ((memq name params)
          (when (assq name arguments)
            (chemlisp--error-at pos "Duplicate template argument: %s" name))
          (push (cons name (cdr pair)) arguments)
          (setq keyword-args t))
         ((memq key chemlisp--template-use-keys)
          (when (assq key use-attrs)
            (chemlisp--error-at pos "Duplicate use-site attribute: %s" key))
          (push pair use-attrs))
         (t
          (chemlisp--error-at pos
                              "Unknown template argument or attribute: %s" key)))))
    (when (and keyword-args (chemlisp-node-children node))
      (chemlisp--error-at pos
                          "A template call cannot mix keyword and positional arguments"))
    (cons (nreverse arguments) (nreverse use-attrs))))

(defun chemlisp--expand-template-argument (raw pos env scope expander)
  "Expand template argument RAW at POS in ENV and SCOPE with EXPANDER."
  (cond
   ((null raw) nil)
   ((chemlisp-nil-p raw) nil)
   ((chemlisp-node-p raw)
    (chemlisp--expand-node raw env scope expander))
   (t
    (let ((form (chemlisp--parse-form raw pos)))
      (cond
       ((chemlisp-nil-p form) nil)
       ((chemlisp-node-p form)
        (chemlisp--expand-node form env scope expander))
       ((chemlisp-closure-p form)
        (chemlisp--error-at pos "A closure is not a valid template argument"))
       ((chemlisp-let-p form)
        (chemlisp--error-at pos "A chem-let is not a valid template argument"))
       (t
        (chemlisp--error-at pos "A template argument must be a node or nil")))))))

(defun chemlisp--expand-template-use (node def env scope expander)
  "Expand template use NODE of DEF in ENV and SCOPE using EXPANDER."
  (let* ((pos (chemlisp-node-pos node))
         (params (chemlisp-def-params def))
         (split (chemlisp--template-arguments node params pos))
         (keyword-arguments (car split))
         (use-attrs (chemlisp--validate-attrs
                     (cdr split) chemlisp--template-use-keys 'template-use pos))
         (children (chemlisp-node-children node))
         (values nil))
    (if children
        (progn
          (when (> (length children) (length params))
            (chemlisp--error-at pos "Too many template arguments"))
          (let ((index 0))
            (dolist (param params)
              (let ((child (nth index children)))
                (push (cons param
                            (if child
                                (chemlisp--expand-template-argument
                                 child pos env scope expander)
                              nil))
                      values))
              (setq index (1+ index)))))
      (dolist (param params)
        (let ((cell (assq param keyword-arguments)))
          (push (cons param
                      (if cell
                          (chemlisp--expand-template-argument
                           (cdr cell) pos env scope expander)
                        nil))
                values))))
    (setq values (nreverse values))
    (let* ((def-env (chemlisp--def-expansion-env def expander))
           (param-env
            (append
             (mapcar (lambda (pair)
                       (cons (car pair)
                             (chemlisp-param-create :value (cdr pair))))
                     values)
             def-env))
           (instance-scope (chemlisp--new-scope))
           (root (chemlisp--expand-def-body def param-env instance-scope expander)))
      (setf (chemlisp-node-attrs root)
            (chemlisp--merge-attrs (chemlisp-node-attrs root) use-attrs))
      root)))

(defun chemlisp--expand-let (form env scope expander)
  "Expand chem-let FORM in ENV and SCOPE using EXPANDER."
  (let ((names (make-hash-table :test #'eq))
        (local-env env))
    (dolist (binding (chemlisp-let-bindings form))
      (let ((name (car binding))
            (value (cdr binding)))
        (when (gethash name names)
          (chemlisp--error-at (chemlisp-let-pos form)
                              "Duplicate binding: %s" name))
        (puthash name t names)
        (let ((entry (assq name local-env)))
          (when (and entry (chemlisp-param-p (cdr entry)))
            (chemlisp--error-at (chemlisp-let-pos form)
                                "Binding %s shadows a template parameter" name)))
        (let ((def (chemlisp-def-create
                    :name name
                    :params nil
                    :body value
                    :env (if (chemlisp-let-sequential-p form)
                             local-env
                           env)
                    :pos (chemlisp-let-pos form))))
          (push (cons name def) local-env))))
    (chemlisp--expand-form (chemlisp-let-body form) local-env scope expander)))

(defun chemlisp--expand-molecule (form env scope expander)
  "Expand molecule FORM in ENV and SCOPE using EXPANDER."
  (let* ((pos (chemlisp-molecule-pos form))
         (attrs (chemlisp--validate-attrs (chemlisp-molecule-attrs form)
                                          chemlisp--molecule-keys
                                          'molecule pos))
         (roots nil))
    (dolist (root (chemlisp-molecule-roots form))
      (let ((expanded (chemlisp--expand-form root env scope expander)))
        (unless (chemlisp-node-p expanded)
          (chemlisp--error-at pos "A molecule root must expand to one node"))
        (push expanded roots)))
    (chemlisp-molecule-create :attrs attrs
                              :roots (nreverse roots)
                              :pos pos)))

(defun chemlisp--expand-form (form env scope expander)
  "Expand FORM in ENV and SCOPE using EXPANDER."
  (cond
   ((chemlisp-node-p form)
    (chemlisp--expand-node form env scope expander))
   ((chemlisp-let-p form)
    (chemlisp--expand-let form env scope expander))
   ((chemlisp-molecule-p form)
    (chemlisp--expand-molecule form env scope expander))
   ((chemlisp-closure-p form)
    (chemlisp--error-at (chemlisp-closure-pos form)
                        "A closure cannot be a root"))
   ((chemlisp-def-p form)
    (chemlisp--error-at (chemlisp-def-pos form)
                        "chem-def is only allowed at top level"))
   ((chemlisp-nil-p form)
    (chemlisp--error-at (chemlisp-nil-pos form) "nil is not a valid form"))
   (t
    (chemlisp--error "Invalid form"))))

(defun chemlisp-parse (source)
  "Parse SOURCE and return a validated `chemlisp-document'."
  (let ((chemlisp--current-source source))
    (let ((defs nil)
          (drawing nil))
      (dolist (cell (chemlisp-read source))
        (let ((form (chemlisp--parse-form (car cell) (cdr cell))))
          (cond
           ((chemlisp-def-p form)
            (push form defs))
           (drawing
            (chemlisp--error-at (cdr cell) "Multiple drawing forms"))
           (t
            (setq drawing form)))))
      (unless drawing
        (chemlisp--error "No drawing form"))
      (let ((document (chemlisp-document-create
                       :defs (nreverse defs)
                       :drawing drawing
                       :source source)))
        (chemlisp--validate-document document)
        document))))

(defun chemlisp-expand (document-or-source)
  "Expand DOCUMENT-OR-SOURCE into a resolved AST."
  (let* ((document (if (stringp document-or-source)
                       (chemlisp-parse document-or-source)
                     document-or-source))
         (chemlisp--current-source (chemlisp-document-source document))
         (global-env
          (mapcar (lambda (def) (cons (chemlisp-def-name def) def))
                  (chemlisp-document-defs document)))
         (expander (chemlisp-expander-create
                    :counter 0
                    :stack nil
                    :global-env global-env))
         (scope (chemlisp--new-scope))
         (resolved (chemlisp--expand-form (chemlisp-document-drawing document)
                                          global-env scope expander)))
    (unless (or (chemlisp-node-p resolved) (chemlisp-molecule-p resolved))
      (chemlisp--error "The drawing must expand to a node or molecule"))
    resolved))

;;; Tree construction and layout

(defun chemlisp--ast->tree (node root-p)
  "Convert resolved NODE to a `chemlisp-tree'.
ROOT-P marks NODE as a fragment root."
  (let ((tree (chemlisp-tree-create
               :key (chemlisp-node-key node)
               :attrs (chemlisp-node-attrs node)
               :root-p root-p
               :node node
               :children nil)))
    (setf (chemlisp-tree-children tree)
          (mapcar
           (lambda (child)
             (cond
              ((chemlisp-node-p child)
               (let ((child-tree (chemlisp--ast->tree child nil)))
                 (setf (chemlisp-tree-from-key child-tree)
                       (chemlisp-node-key node))
                 child-tree))
              ((chemlisp-closure-p child)
               (chemlisp-tree-closure-create
                :target-key (chemlisp-closure-target-key child)
                :target-index (chemlisp-closure-target-index child)
                :attrs (chemlisp-closure-attrs child)
                :pos (chemlisp-closure-pos child)))
              (t
               (chemlisp--error "Invalid resolved child"))))
           (chemlisp-node-children node)))
    tree))

(defun chemlisp--tree-key-map (tree)
  "Return a hash mapping atom keys to tree nodes in TREE."
  (let ((map (make-hash-table :test #'eql)))
    (cl-labels ((walk (node)
                  (puthash (chemlisp-tree-key node) node map)
                  (dolist (child (chemlisp-tree-children node))
                    (when (chemlisp-tree-p child)
                      (walk child)))))
      (walk tree))
    map))

(defun chemlisp--first-child-terminal (tree)
  "Return the terminal tree node of TREE's first-child path."
  (let ((child (cl-find-if #'chemlisp-tree-p
                           (chemlisp-tree-children tree))))
    (if child
        (chemlisp--first-child-terminal child)
      tree)))

(defun chemlisp--collect-exit-keys (trees)
  "Return all atom keys marked with :exit in TREES."
  (let (keys)
    (cl-labels ((walk (tree)
                  (when (chemlisp--attr (chemlisp-tree-attrs tree) :exit)
                    (push (chemlisp-tree-key tree) keys))
                  (dolist (child (chemlisp-tree-children tree))
                    (when (chemlisp-tree-p child)
                      (walk child)))))
      (dolist (tree trees) (walk tree)))
    (nreverse keys)))

(defun chemlisp--fragment-exit-key (tree global-exit)
  "Return the exit key of TREE, preferring GLOBAL-EXIT when present."
  (if (and global-exit
           (gethash global-exit (chemlisp--tree-key-map tree)))
      global-exit
    (chemlisp-tree-key (chemlisp--first-child-terminal tree))))

(defun chemlisp--attach-fragments (trees global-exit)
  "Connect fragment TREES with invisible links.
GLOBAL-EXIT is the optional explicit exit key.
Return (ROOT-TREE . EXIT-KEY)."
  (let ((root (car trees))
        (previous (car trees)))
    (dolist (next (cdr trees))
      (let* ((exit-key (chemlisp--fragment-exit-key previous global-exit))
             (exit-node (gethash exit-key (chemlisp--tree-key-map previous)))
             (link (chemlisp-tree-create
                    :key (chemlisp-tree-key next)
                    :attrs (chemlisp-tree-attrs next)
                    :children (chemlisp-tree-children next)
                    :link-p t
                    :from-key exit-key
                    :root-p nil
                    :node (chemlisp-tree-node next))))
        (setf (chemlisp-tree-children exit-node)
              (append (chemlisp-tree-children exit-node) (list link)))
        (setq previous link)))
    (cons root (or global-exit
                   (chemlisp--fragment-exit-key previous nil)))))

(defun chemlisp--preorder-nodes (tree)
  "Return TREE nodes in pre-order, excluding closures."
  (let (nodes)
    (cl-labels ((walk (node)
                  (push node nodes)
                  (dolist (child (chemlisp-tree-children node))
                    (when (chemlisp-tree-p child)
                      (walk child)))))
      (walk tree))
    (nreverse nodes)))

(defun chemlisp--preorder-closures (tree)
  "Return TREE's (CLOSURE . PARENT) pairs in pre-order."
  (let (closures)
    (cl-labels ((walk (node)
                  (dolist (child (chemlisp-tree-children node))
                    (cond
                     ((chemlisp-tree-p child) (walk child))
                     ((chemlisp-tree-closure-p child)
                      (push (cons child node) closures))))))
      (walk tree))
    (nreverse closures)))

;;; IR construction

(defun chemlisp--compute-bond-angles (drawing)
  "Populate bond angles on DRAWING's atoms."
  (let ((atoms (chem-drawing-atoms drawing)))
    (dolist (edge (chem-edges drawing))
      (unless (chem-edge-link-p edge)
        (let* ((a (chem-edge-a edge))
               (b (chem-edge-b edge))
               (aa (gethash a atoms))
               (bb (gethash b atoms))
               (ab (chem--compare-positions
                    (chem-atom-x aa) (chem-atom-y aa)
                    (chem-atom-x bb) (chem-atom-y bb)))
               (ba (chem--compare-positions
                    (chem-atom-x bb) (chem-atom-y bb)
                    (chem-atom-x aa) (chem-atom-y aa))))
          (setf (chem-atom-bond-angles aa)
                (append (chem-atom-bond-angles aa) (list (nth 1 ab))))
          (setf (chem-atom-bond-angles bb)
                (append (chem-atom-bond-angles bb) (list (nth 1 ba))))))))
  drawing)

(defun chemlisp--parent-map (nodes tree-edge-ids key->idx)
  "Return a hash from atom index to (PARENT-INDEX . EDGE-ID).
NODES, TREE-EDGE-IDS, and KEY->IDX describe the drawing tree."
  (let ((parents (make-hash-table :test #'eql)))
    (dolist (node nodes)
      (let ((from (chemlisp-tree-from-key node)))
        (when from
          (puthash (gethash (chemlisp-tree-key node) key->idx)
                   (cons (gethash from key->idx)
                         (gethash node tree-edge-ids))
                   parents))))
    parents))

(defun chemlisp--tree-path (source target parents)
  "Return (ATOMS . BONDS) for the tree path from SOURCE to TARGET.
PARENTS maps atom index to (PARENT . EDGE-ID)."
  (let ((source-chain nil)
        (target-chain nil))
    (let ((node source))
      (while node
        (push node source-chain)
        (setq node (car (gethash node parents)))))
    (setq source-chain (nreverse source-chain))
    (let ((node target))
      (while node
        (push node target-chain)
        (setq node (car (gethash node parents)))))
    (setq target-chain (nreverse target-chain))
    (let ((lca (cl-find-if (lambda (node) (memq node target-chain))
                           source-chain)))
      (when lca
        (let* ((source-part (cl-subseq source-chain
                                      0 (1+ (cl-position lca source-chain))))
               (target-prefix (cl-subseq target-chain
                                         0 (cl-position lca target-chain)))
               (target-part (nreverse target-prefix))
               (atoms (append source-part target-part))
               (source-edges
                (mapcar (lambda (node)
                          (cdr (gethash node parents)))
                        (butlast source-part)))
               (target-edges
                (mapcar (lambda (node)
                          (cdr (gethash node parents)))
                        target-part)))
          (cons atoms (append source-edges target-edges)))))))

(defun chemlisp--ring-chord-p (drawing atoms bonds)
  "Return non-nil when the cycle ATOMS/BONDS has a chord in DRAWING."
  (let ((cycle-atoms (make-hash-table :test #'eql))
        (cycle-bonds (make-hash-table :test #'eql)))
    (dolist (idx atoms) (puthash idx t cycle-atoms))
    (dolist (id bonds) (puthash id t cycle-bonds))
    (let ((chord nil))
      (dolist (edge (chem-edges drawing) chord)
        (unless (or chord (chem-edge-link-p edge))
          (when (and (gethash (chem-edge-a edge) cycle-atoms)
                     (gethash (chem-edge-b edge) cycle-atoms)
                     (not (gethash (chem-edge-id edge) cycle-bonds)))
            (setq chord t)))))))

(defun chemlisp--compute-rings (drawing closures parents key->idx
                                        tree-edge-ids closure-edge-ids
                                        closure-targets aromatic)
  "Compute ring records for DRAWING.
CLOSURES, PARENTS, KEY->IDX, TREE-EDGE-IDS, CLOSURE-EDGE-IDS, and
CLOSURE-TARGETS describe the graph; AROMATIC is the aromatic atom hash."
  (let ((seen (make-hash-table :test #'equal))
        (sequence 0)
        (rings nil)
        (tree-edge-set (make-hash-table :test #'eql)))
    (maphash (lambda (_ id) (puthash id t tree-edge-set)) tree-edge-ids)
    (dolist (pair closures)
      (let* ((closure (car pair))
             (edge-id (gethash closure closure-edge-ids))
             (source (gethash (chemlisp-tree-key (cdr pair)) key->idx))
             (target (gethash closure closure-targets))
             (edge (and target (chem-get-edge drawing source target))))
        (when (and edge edge-id (not (chem-edge-link-p edge)))
          (let* ((path (chemlisp--tree-path source target parents))
                 (atoms (car path))
                 (bonds (append (cdr path) (list (chem-edge-id edge)))))
            (unless (cl-some (lambda (id)
                               (chem-edge-link-p (chem-edge-at drawing id)))
                             bonds)
              (let ((key (sort (copy-sequence atoms) #'<)))
                (unless (or (gethash key seen)
                            (chemlisp--ring-chord-p drawing atoms bonds))
                  (puthash key t seen)
                  (let* ((aromatic-p
                          (cl-every (lambda (idx) (gethash idx aromatic)) atoms))
                         (anchor
                          (or (cl-find-if (lambda (id)
                                            (gethash id tree-edge-set))
                                          bonds)
                              (car bonds))))
                    (push (chem-ring-create
                           :atoms atoms
                           :bonds bonds
                           :anchor anchor
                           :aromatic-p aromatic-p
                           :sequence sequence)
                          rings)
                    (setq sequence (1+ sequence))))))))))
    (setq rings
          (sort (nreverse rings)
                (lambda (a b)
                  (let ((aa (chem-ring-atoms a))
                        (bb (chem-ring-atoms b)))
                    (cond
                     ((/= (length aa) (length bb))
                      (< (length aa) (length bb)))
                     (t
                      (cl-loop for x in aa
                               for y in bb
                               when (/= x y)
                               return (< x y)
                               finally return nil)))))))
    (let ((id 0))
      (dolist (ring rings)
        (setf (chem-ring-id ring) id)
        (setf (chem-ring-sequence ring) id)
        (setq id (1+ id))))
    (setf (chem-drawing-rings drawing) rings)
    rings))

(defun chemlisp--edge-order (attrs from-aromatic to-aromatic link-p)
  "Return the bond order for edge ATTRS.
FROM-AROMATIC and TO-AROMATIC describe the endpoints; LINK-P hides the edge."
  (let ((bond (chemlisp--attr attrs :bond))
        (stereo (chemlisp--attr attrs :stereo)))
    (cond
     (link-p :single)
     ((eq bond :link) :single)
     (bond bond)
     ((and from-aromatic to-aromatic) :aromatic)
     ((memq stereo '(:cis :trans)) :double)
     (t :single))))

(defun chemlisp--edge-stereo (attrs from to)
  "Return canonical edge stereo for ATTRS from FROM to TO.
Return (STEREO . STEREO-CENTER)."
  (let* ((stereo (or (chemlisp--attr attrs :stereo) :none))
         (canonical
          (if (<= from to)
              stereo
            (pcase stereo
              ((or :up :down) (if (eq stereo :up) :down :up))
              (_ stereo))))
         (center (when (memq stereo '(:up :down)) from)))
    (cons canonical center)))

(defun chemlisp--create-edge (drawing from to attrs link-p from-aromatic to-aromatic)
  "Create a canonical edge in DRAWING from FROM to TO.
ATTRS holds bond attributes, LINK-P marks an invisible edge, and
FROM-AROMATIC and TO-AROMATIC select the default order."
  (let* ((a (min from to))
         (b (max from to))
         (order (chemlisp--edge-order attrs from-aromatic to-aromatic link-p))
         (link-p (or link-p (eq (chemlisp--attr attrs :bond) :link)))
         (stereo (if link-p
                     (cons :none nil)
                   (chemlisp--edge-stereo attrs from to)))
         (id (hash-table-count (chem-drawing-edges drawing)))
         (edge (chem-edge-create
                :id id
                :a a
                :b b
                :order order
                :stereo (car stereo)
                :stereo-center (cdr stereo)
                :link-p link-p
                :cross-p nil
                :marker ""
                :decoration 'plain)))
    (when (gethash (cons a b) (chem-drawing-edge-lookup drawing))
      (chemlisp--error "Duplicate bond between atoms %d and %d" (1+ a) (1+ b)))
    (puthash id edge (chem-drawing-edges drawing))
    (puthash (cons a b) edge (chem-drawing-edge-lookup drawing))
    (let ((from-atom (gethash from (chem-drawing-atoms drawing)))
          (to-atom (gethash to (chem-drawing-atoms drawing))))
      (setf (chem-atom-neighbors from-atom)
            (append (chem-atom-neighbors from-atom) (list to)))
      (setf (chem-atom-neighbors to-atom)
            (append (chem-atom-neighbors to-atom) (list from)))
      (puthash from
               (append (gethash from (chem-drawing-adjacency drawing))
                       (list (cons id to)))
               (chem-drawing-adjacency drawing))
      (puthash to
               (append (gethash to (chem-drawing-adjacency drawing))
                       (list (cons id from)))
               (chem-drawing-adjacency drawing)))
    edge))

(defun chemlisp--resolve-closure-target (closure key->idx keys source)
  "Resolve CLOSURE's target index using KEY->IDX and KEYS.
SOURCE is the source atom index."
  (let* ((target-key (chemlisp-tree-closure-target-key closure))
         (target-index (chemlisp-tree-closure-target-index closure))
         (target
          (cond
           (target-key
            (or (gethash target-key key->idx)
                (chemlisp--error-at (chemlisp-tree-closure-pos closure)
                                    "Closure target is outside the drawing")))
           (target-index
            (when (> target-index (length keys))
              (chemlisp--error-at (chemlisp-tree-closure-pos closure)
                                  "Closure index %d is out of range" target-index))
            (let ((key (aref keys (1- target-index))))
              (gethash key key->idx)))
           (t
            (chemlisp--error-at (chemlisp-tree-closure-pos closure)
                                "Closure has no target")))))
    (when (>= target source)
      (chemlisp--error-at (chemlisp-tree-closure-pos closure)
                          "Closure target must be an earlier atom"))
    target))

(defun chemlisp--build-chem-node (tree key->idx tree-edge-ids closure-edge-ids
                                       closure-targets)
  "Build a `chem-node' tree from TREE.
KEY->IDX, TREE-EDGE-IDS, CLOSURE-EDGE-IDS, and CLOSURE-TARGETS map
the compiled graph."
  (let* ((idx (gethash (chemlisp-tree-key tree) key->idx))
         (from-key (chemlisp-tree-from-key tree))
         (node (if from-key
                   (chem-node-create
                    :edge-id (gethash tree tree-edge-ids)
                    :from (gethash from-key key->idx)
                    :to idx
                    :link-p (chemlisp-tree-link-p tree))
                 (chem-node-create :edge-id nil :from nil :to idx))))
    (let (children)
      (dolist (child (chemlisp-tree-children tree))
        (cond
         ((chemlisp-tree-p child)
          (let ((child-node (chemlisp--build-chem-node
                             child key->idx tree-edge-ids
                             closure-edge-ids closure-targets)))
            (setf (chem-node-parent child-node) node)
            (push child-node children)))
         ((chemlisp-tree-closure-p child)
          (let ((closure-node (chem-node-create
                               :edge-id (gethash child closure-edge-ids)
                               :from idx
                               :to (gethash child closure-targets)
                               :closure-p t
                               :phantom-p t)))
            (setf (chem-node-parent closure-node) node)
            (push closure-node children)))))
      (setf (chem-node-children node) (nreverse children)))
    node))

(defun chemlisp--find-exit-node (node idx)
  "Find the non-closure node in NODE's tree with TO equal to IDX."
  (cond
   ((and (not (chem-node-closure-p node))
         (= (chem-node-to node) idx))
    node)
   (t
    (cl-some (lambda (child) (chemlisp--find-exit-node child idx))
             (chem-node-children node)))))

(defun chemlisp--mark-trunk (exit-node entry-idx)
  "Mark the path from EXIT-NODE up to ENTRY-IDX as the trunk."
  (let ((node exit-node))
    (while (and node (/= (chem-node-to node) entry-idx))
      (setf (chem-node-trunk-p node) t)
      (setq node (chem-node-parent node)))))

(defun chemlisp--apply-view (drawing view)
  "Apply molecule VIEW transforms to DRAWING."
  (when (chemlisp--attr view :flip-x)
    (chem-flip-horizontal drawing))
  (when (chemlisp--attr view :flip-y)
    (chem-flip-vertical drawing))
  (let ((rotation (chemlisp--attr view :rotation)))
    (when (and rotation (/= rotation 0))
      (chem-rotate drawing rotation)))
  (let ((scale (chemlisp--attr view :scale)))
    (when (and scale (/= scale 1.0))
      (chem-scale drawing scale)))
  (let ((translation (chemlisp--attr view :translation)))
    (when translation
      (maphash (lambda (_ atom)
                 (setf (chem-atom-x atom)
                       (+ (chem-atom-x atom) (nth 0 translation)))
                 (setf (chem-atom-y atom)
                       (+ (chem-atom-y atom) (nth 1 translation))))
               (chem-drawing-atoms drawing))))
  drawing)

(defun chemlisp--apply-markers (drawing options)
  "Apply marker OPTIONS to DRAWING."
  (let ((prefix (plist-get options :markers)))
    (when prefix
      (maphash (lambda (_ atom)
                 (setf (chem-atom-marker atom)
                       (format "%s%d" prefix (1+ (chem-atom-idx atom)))))
               (chem-drawing-atoms drawing))
      (maphash (lambda (_ edge)
                 (setf (chem-edge-marker edge)
                       (if (chem-edge-link-p edge)
                           ""
                         (format "%s%d-%d" prefix
                                 (1+ (chem-edge-a edge))
                                 (1+ (chem-edge-b edge))))))
               (chem-drawing-edges drawing))))
  drawing)

(defun chemlisp--apply-show (drawing shows)
  "Apply per-atom :show overrides in SHOWS to DRAWING."
  (maphash
   (lambda (idx show)
     (let ((atom (gethash idx (chem-drawing-atoms drawing))))
       (pcase show
         (:always (setf (chem-atom-label atom) (chem-atom-element atom)))
         (:never (setf (chem-atom-label atom) nil))
         (_ nil))))
   shows)
  drawing)

(defun chemlisp--tree-atom-keys (tree)
  "Return TREE's atom keys, excluding closures."
  (let (keys)
    (cl-labels ((walk (node)
                  (push (chemlisp-tree-key node) keys)
                  (dolist (child (chemlisp-tree-children node))
                    (when (chemlisp-tree-p child)
                      (walk child)))))
      (walk tree))
    (nreverse keys)))

(defun chemlisp--ring-edge-set (drawing)
  "Return a hash of edge ids that belong to DRAWING's rings."
  (let ((edges (make-hash-table :test #'eql)))
    (dolist (ring (chem-drawing-rings drawing))
      (dolist (id (chem-ring-bonds ring))
        (puthash id t edges)))
    edges))

(defun chemlisp--drawing-to-mol (drawing explicit-h)
  "Serialize DRAWING to V3000 MOL for Indigo layout.
EXPLICIT-H maps atom indices to :h values that are added as explicit
hydrogen atoms."
  (let ((atoms (chem-drawing-atoms drawing))
        (n (hash-table-count (chem-drawing-atoms drawing)))
        (atom-count (hash-table-count (chem-drawing-atoms drawing)))
        (bond-count 0)
        (atom-lines nil)
        (bond-lines nil))
    (dotimes (idx n)
      (let* ((atom (gethash idx atoms))
             (charge (or (chem-atom-charge atom) 0))
             (radical (or (chem-atom-radical atom) 0))
             (properties
              (concat (unless (zerop charge)
                        (format " CHG=%d" charge))
                      (pcase radical
                        (1 " RAD=2")
                        (2 " RAD=3")
                        (_ "")))))
        (push (format "M  V30 %d %s 0 0 0 0%s\n"
                      (1+ idx) (chem-atom-element atom) properties)
              atom-lines)))
    (dotimes (idx n)
      (let ((h (gethash idx explicit-h)))
        (when (and h (> h 0))
          (when (equal (chem-atom-element (gethash idx atoms)) "H")
            (chemlisp--error "Cannot specify :h on a hydrogen atom"))
          (dotimes (_ h)
            (cl-incf atom-count)
            (push (format "M  V30 %d H 0 0 0 0\n" atom-count) atom-lines)
            (cl-incf bond-count)
            (push (format "M  V30 %d 1 %d %d\n"
                          bond-count (1+ idx) atom-count)
                  bond-lines)))))
    (dolist (edge (chem-edges drawing))
      (unless (chem-edge-link-p edge)
        (let* ((order (pcase (chem-edge-order edge)
                        (:single 1)
                        (:double 2)
                        (:triple 3)
                        (:aromatic 4)
                        (_ 1)))
               (a (chem-edge-a edge))
               (b (chem-edge-b edge)))
          (cl-incf bond-count)
          (push (format "M  V30 %d %d %d %d\n"
                        bond-count order (1+ a) (1+ b))
                bond-lines))))
    (with-temp-buffer
      (insert "chemlisp\n  chemlisp layout\n\n")
      (insert "  0  0  0  0  0  0  0  0  0  0  0 V3000\n")
      (insert "M  V30 BEGIN CTAB\n")
      (insert (format "M  V30 COUNTS %d %d 0 0 0\n" atom-count bond-count))
      (insert "M  V30 BEGIN ATOM\n")
      (dolist (line (nreverse atom-lines)) (insert line))
      (insert "M  V30 END ATOM\n")
      (insert "M  V30 BEGIN BOND\n")
      (dolist (line (nreverse bond-lines)) (insert line))
      (insert "M  V30 END BOND\n")
      (insert "M  V30 END CTAB\n")
      (insert "M  END\n")
      (buffer-string))))

(defun chemlisp--normalize-layout (drawing)
  "Scale DRAWING so its most common bond length is 1.0."
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (edge (chem-visible-edges drawing))
      (let ((length (chem-edge-length drawing edge)))
        (when (> length 1e-9)
          (let ((key (round (* length 1000))))
            (puthash key (1+ (or (gethash key counts) 0)) counts)))))
    (let (items)
      (maphash (lambda (length count) (push (cons length count) items)) counts)
      (setq items
            (sort items
                  (lambda (a b)
                    (if (/= (cdr a) (cdr b))
                        (> (cdr a) (cdr b))
                      (< (car a) (car b))))))
      (let ((best (car items)))
        (when (and best (> (car best) 0))
          (let ((scale (/ 1000.0 (car best))))
            (unless (= scale 1.0)
              (chem-scale drawing scale)))))))
  drawing)

(defun chemlisp--indigo-layout (drawing explicit-h aromatic)
  "Lay out DRAWING with Indigo and set hydrogens.
EXPLICIT-H maps atom indices to :h values; AROMATIC is the aromatic
atom hash."
  (require 'chem-indigo)
  (let* ((atoms (chem-drawing-atoms drawing))
         (n (hash-table-count atoms))
         (mol-string (chemlisp--drawing-to-mol drawing explicit-h)))
    (condition-case err
        (progn
          (indigo-with-molecule (mol mol-string)
            (indigo-layout mol)
            (indigo-with-atoms-iterator (it mol)
              (indigo-map
               (lambda (atom)
                 (let ((idx (indigo-index atom)))
                   (when (< idx n)
                     (let* ((xyz (indigo-xyz atom))
                            (h (condition-case nil
                                   (indigo-count-implicit-hydrogens atom)
                                 (error -1)))
                            (explicit (gethash idx explicit-h))
                            (target (gethash idx atoms)))
                       (when (null xyz)
                         (chemlisp--error
                          "Cannot read Indigo coordinates for atom %d"
                          (1+ idx)))
                       (when (equal (chem-atom-element target) "H")
                         (setq h 0))
                       (when (and (= h -1)
                                  (gethash idx aromatic)
                                  (not (equal (chem-atom-element target) "C")))
                         (setq h 0))
                       (when (< (or h 0) 0)
                         (chemlisp--error
                          "Cannot calculate implicit hydrogens for atom %d"
                          (1+ idx)))
                       (setf (chem-atom-x target) (nth 0 xyz))
                       (setf (chem-atom-y target) (nth 1 xyz))
                       (if explicit
                           (progn
                             (unless (= h 0)
                               (chemlisp--error
                                "Atom %d has :h %d but Indigo still sees %d implicit hydrogens"
                                (1+ idx) explicit h))
                             (setf (chem-atom-hydrogens target) explicit))
                         (setf (chem-atom-hydrogens target) h)))))
                 nil)
               it)))
          (chemlisp--normalize-layout drawing)
          drawing)
      (chemlisp-error (signal (car err) (cdr err)))
      (error
       (chemlisp--error "Cannot lay out CheMLisp drawing: %s"
                        (error-message-string err))))))

(defun chemlisp--apply-fragment-at (drawing fragments key->idx)
  "Translate DRAWING's FRAGMENTS with :at to their root position.
KEY->IDX maps atom keys to indices."
  (let ((atoms (chem-drawing-atoms drawing)))
    (dolist (tree fragments)
      (let ((at (chemlisp--attr (chemlisp-tree-attrs tree) :at)))
        (when at
          (let* ((root (gethash (gethash (chemlisp-tree-key tree) key->idx)
                                atoms))
                 (dx (- (nth 0 at) (chem-atom-x root)))
                 (dy (- (nth 1 at) (chem-atom-y root))))
            (dolist (key (chemlisp--tree-atom-keys tree))
              (let ((atom (gethash (gethash key key->idx) atoms)))
                (setf (chem-atom-x atom) (+ (chem-atom-x atom) dx))
                (setf (chem-atom-y atom) (+ (chem-atom-y atom) dy)))))))))
  drawing)

(defun chemlisp--coordinate-distance (a b)
  "Return the distance between atoms A and B."
  (let ((dx (- (chem-atom-x b) (chem-atom-x a)))
        (dy (- (chem-atom-y b) (chem-atom-y a))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun chemlisp--coordinate-angle (a b)
  "Return the counterclockwise angle from A to B in degrees."
  (let ((dx (- (chem-atom-x b) (chem-atom-x a)))
        (dy (- (chem-atom-y b) (chem-atom-y a))))
    (when (and (zerop dx) (zerop dy))
      (chemlisp--error "Cannot apply explicit geometry to a zero-length bond"))
    (mod (chem--rad-to-deg (atan dy dx)) 360.0)))

(defun chemlisp--apply-explicit-geometry (drawing fragments key->idx tree-edge-ids)
  "Apply :angle, :turn, and :length overrides to DRAWING.
FRAGMENTS are the authored fragment trees.  KEY->IDX maps atom keys to
indices; TREE-EDGE-IDS maps tree nodes to edge ids.  Explicit geometry
on ring bonds is rejected."
  (let ((atoms (chem-drawing-atoms drawing))
        (ring-edges (chemlisp--ring-edge-set drawing)))
    (cl-labels
        ((subtree-atoms (tree)
           (mapcar (lambda (key)
                     (gethash (gethash key key->idx) atoms))
                   (chemlisp--tree-atom-keys tree)))
         (rotate-subtree (tree px py delta)
           (let ((cosine (cos delta))
                 (sine (sin delta)))
             (dolist (atom (subtree-atoms tree))
               (let ((dx (- (chem-atom-x atom) px))
                     (dy (- (chem-atom-y atom) py)))
                 (setf (chem-atom-x atom) (+ px (- (* dx cosine) (* dy sine))))
                 (setf (chem-atom-y atom) (+ py (+ (* dx sine) (* dy cosine))))))))
         (translate-subtree (tree dx dy)
           (dolist (atom (subtree-atoms tree))
             (setf (chem-atom-x atom) (+ (chem-atom-x atom) dx))
             (setf (chem-atom-y atom) (+ (chem-atom-y atom) dy))))
         (walk (tree direction)
           (dolist (child (chemlisp-tree-children tree))
             (when (chemlisp-tree-p child)
               (let* ((attrs (chemlisp-tree-attrs child))
                      (angle (chemlisp--attr attrs :angle))
                      (turn (chemlisp--attr attrs :turn))
                      (length (chemlisp--attr attrs :length))
                      (edge-id (gethash child tree-edge-ids))
                      (parent (gethash (gethash (chemlisp-tree-key tree) key->idx)
                                       atoms))
                      (child-atom (gethash (gethash (chemlisp-tree-key child)
                                                    key->idx)
                                           atoms))
                      (current (chemlisp--coordinate-angle parent child-atom))
                      (desired (cond (angle (mod angle 360.0))
                                     (turn (mod (+ (or direction 0) turn) 360.0))
                                     (t current)))
                      (delta (mod (- desired current) 360.0)))
                 (when (> delta 180.0) (setq delta (- delta 360.0)))
                 (when (and (or angle turn length) (gethash edge-id ring-edges))
                   (chemlisp--error-at
                    (chemlisp-node-pos (chemlisp-tree-node child))
                    "Explicit geometry on ring bonds is not supported; remove :angle/:turn/:length or use the molecule view"))
                 (unless (zerop delta)
                   (rotate-subtree child
                                   (chem-atom-x parent)
                                   (chem-atom-y parent)
                                   (* delta (/ float-pi 180.0))))
                 (when length
                   (let* ((radians (* desired (/ float-pi 180.0)))
                          (delta-length (- length
                                           (chemlisp--coordinate-distance
                                            parent child-atom))))
                     (translate-subtree child
                                        (* delta-length (cos radians))
                                        (* delta-length (sin radians)))))
                 (walk child desired))))))
      (dolist (tree fragments)
        (walk tree 0.0))))
  drawing)

(defun chemlisp--compile-resolved (resolved options)
  "Compile resolved AST RESOLVED into a `chem-drawing' using OPTIONS."
  (let* ((roots (if (chemlisp-molecule-p resolved)
                    (chemlisp-molecule-roots resolved)
                  (list resolved)))
         (view (if (chemlisp-molecule-p resolved)
                   (chemlisp-molecule-attrs resolved)
                 nil))
         (fragments (mapcar (lambda (root) (chemlisp--ast->tree root t)) roots))
         (exit-keys (chemlisp--collect-exit-keys fragments)))
    (when (> (length exit-keys) 1)
      (chemlisp--error "Only one :exit atom is allowed"))
    (let* ((global-exit (car exit-keys))
           (attach (chemlisp--attach-fragments fragments global-exit))
           (root-tree (car attach))
           (exit-key (cdr attach))
           (nodes (chemlisp--preorder-nodes root-tree))
           (keys (vconcat (mapcar #'chemlisp-tree-key nodes)))
           (key->idx (make-hash-table :test #'eql)))
      (let ((index 0))
        (dolist (node nodes)
          (puthash (chemlisp-tree-key node) index key->idx)
          (setq index (1+ index))))
      (let* ((drawing (chem-drawing-create
                       :atoms (make-hash-table :test #'eql)
                       :edges (make-hash-table :test #'eql)
                       :edge-lookup (make-hash-table :test #'equal)
                       :adjacency (make-hash-table :test #'eql)))
             (aromatic (make-hash-table :test #'eql))
             (explicit-h (make-hash-table :test #'eql))
             (shows (make-hash-table :test #'eql))
             (tree-edge-ids (make-hash-table :test #'eq))
             (closure-edge-ids (make-hash-table :test #'eq))
             (closure-targets (make-hash-table :test #'eq)))
        (dolist (node nodes)
          (let* ((idx (gethash (chemlisp-tree-key node) key->idx))
                 (attrs (chemlisp--validate-attrs
                         (chemlisp-tree-attrs node)
                         chemlisp--atom-keys 'atom
                         (chemlisp-node-pos (chemlisp-tree-node node))))
                 (info (chemlisp--element-info
                        (chemlisp-node-head (chemlisp-tree-node node))))
                 (from-key (chemlisp-tree-from-key node))
                 (fragment-root (or (null from-key)
                                    (chemlisp-tree-link-p node))))
            (unless info
              (chemlisp--error "Unknown atom: %s"
                               (chemlisp-node-head (chemlisp-tree-node node))))
            (when (and (not fragment-root) (chemlisp--attr attrs :at))
              (chemlisp--error "Only a root can have :at"))
            (when (and fragment-root
                       (cl-some (lambda (key) (assq key attrs))
                                '(:bond :stereo :angle :turn :length)))
              (chemlisp--error "A root cannot have an incoming bond"))
            (let ((atom (chem-atom-create
                         :idx idx
                         :x 0.0
                         :y 0.0
                         :element (car info)
                         :hydrogens nil
                         :charge (or (chemlisp--attr attrs :charge) 0)
                         :radical (or (chemlisp--attr attrs :radical) 0)
                         :neighbors nil
                         :bond-angles nil
                         :label nil
                         :number nil
                         :marker nil
                         :stereocenter nil
                         :allene nil)))
              (puthash idx atom (chem-drawing-atoms drawing))
              (when (cdr info) (puthash idx t aromatic))
              (when (chemlisp--attr attrs :h)
                (puthash idx (chemlisp--attr attrs :h) explicit-h))
              (when (chemlisp--attr attrs :show)
                (puthash idx (chemlisp--attr attrs :show) shows)))))
        (dolist (node nodes)
          (when (chemlisp-tree-from-key node)
            (let* ((from (gethash (chemlisp-tree-from-key node) key->idx))
                   (to (gethash (chemlisp-tree-key node) key->idx))
                   (edge (chemlisp--create-edge
                          drawing from to (chemlisp-tree-attrs node)
                          (chemlisp-tree-link-p node)
                          (gethash from aromatic)
                          (gethash to aromatic))))
              (puthash node (chem-edge-id edge) tree-edge-ids))))
        (dolist (pair (chemlisp--preorder-closures root-tree))
          (let* ((closure (car pair))
                 (parent (cdr pair))
                 (source (gethash (chemlisp-tree-key parent) key->idx))
                 (target (chemlisp--resolve-closure-target
                          closure key->idx keys source))
                 (edge (chemlisp--create-edge
                        drawing source target
                        (chemlisp-tree-closure-attrs closure)
                        nil
                        (gethash source aromatic)
                        (gethash target aromatic))))
            (puthash closure target closure-targets)
            (puthash closure (chem-edge-id edge) closure-edge-ids)))
        (let* ((closure-pairs (chemlisp--preorder-closures root-tree))
               (parents (chemlisp--parent-map nodes tree-edge-ids key->idx)))
          (dolist (pair closure-pairs)
            (let* ((closure (car pair))
                   (parent (cdr pair))
                   (source (gethash (chemlisp-tree-key parent) key->idx))
                   (target (gethash closure closure-targets))
                   (node source))
              (while (and node (/= node target))
                (setq node (car (gethash node parents))))
              (unless node
                (chemlisp--error-at
                 (chemlisp-tree-closure-pos closure)
                 "Closure target must be an ancestor of its source"))))
          (chemlisp--compute-rings drawing closure-pairs
                                   parents key->idx tree-edge-ids
                                   closure-edge-ids closure-targets aromatic))
        (chemlisp--indigo-layout drawing explicit-h aromatic)
        (chemlisp--apply-fragment-at drawing fragments key->idx)
        (chemlisp--apply-explicit-geometry drawing fragments key->idx
                                           tree-edge-ids)
        (when (plist-get options :flip)
          (chem-flip-horizontal drawing))
        (when (plist-get options :flop)
          (chem-flip-vertical drawing))
        (let ((rotation (or (plist-get options :angle) 0)))
          (unless (zerop rotation)
            (chem-rotate drawing rotation)))
        (chemlisp--apply-view drawing view)
        (chemlisp--compute-bond-angles drawing)
        (let* ((chem-root (chemlisp--build-chem-node
                           root-tree key->idx tree-edge-ids
                           closure-edge-ids closure-targets))
               (entry-idx (gethash (chemlisp-tree-key root-tree) key->idx))
               (exit-idx (gethash exit-key key->idx))
               (exit-node (chemlisp--find-exit-node chem-root exit-idx)))
          (unless exit-node
            (chemlisp--error "Cannot find the exit atom"))
          (setf (chem-drawing-root drawing) chem-root)
          (setf (chem-drawing-entry-atom drawing)
                (gethash entry-idx (chem-drawing-atoms drawing)))
          (setf (chem-drawing-exit-atom drawing)
                (gethash exit-idx (chem-drawing-atoms drawing)))
          (setf (chem-drawing-exit-node drawing) exit-node)
          (chemlisp--mark-trunk exit-node entry-idx))
        (chem-apply-drawing-options drawing options)
        (chemlisp--apply-show drawing shows)
        (chemlisp--apply-markers drawing options)
        drawing))))

;;; Public API

(defun chemlisp-compile (source &optional options)
  "Compile SOURCE to a `chem-drawing' using OPTIONS."
  (let* ((document (if (stringp source)
                       (let ((chemlisp--current-source source))
                         (chemlisp-parse source))
                     source))
         (chemlisp--current-source (chemlisp-document-source document)))
    (chemlisp--compile-resolved (chemlisp-expand document) options)))

(defun chemlisp-render (source exporter &optional options)
  "Render SOURCE with EXPORTER and OPTIONS."
  (let ((drawing (chemlisp-compile source options)))
    (unless (chem-exporter exporter)
      (require (intern (format "chem-exp-%s" exporter)) nil t))
    (unless (chem-exporter exporter)
      (chemlisp--error "Unknown exporter: %s" exporter))
    (chem-render drawing exporter options)))

(provide 'chemlisp)
;;; chemlisp.el ends here
