;;; tochemfig-chemfig-parser.el --- Parse generated chemfig back into a graph -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Giovanni Crisalfi

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A small, dependency-free parser that turns the chemfig code produced by
;; `tochemfig' back into a molecular graph, so tests can assert that the
;; rendering encodes the same molecule as the Indigo input.
;;
;; The generated code is a depth-first rendering of a spanning tree where the
;; end atom of every bond is written on the same line as that bond.  Branches
;; are `(...)' and ring closures are branch bonds whose end is a `\phantom{X}'
;; atom (or the empty label of a hidden carbon) together with a `% -> n'
;; comment naming the partner atom.  Those comments are therefore load bearing
;; and must not be stripped before parsing.
;;
;; Only the constructs the generator emits for the *default* options are
;; interpreted structurally.  Aromatic circles (`\mcfcringle') and fancy style
;; shortcuts are recognised but the structural comparison is expected to run
;; on non-terse, non-fancy output.

;;; Code:

(require 'cl-lib)

;;; Low-level scanning helpers

(defun tochemfig-cp--split-line (line)
  "Split LINE at the first percent sign into (CODE . COMMENT)."
  (let ((pct (string-match "%" line)))
    (if pct
        (cons (string-trim (substring line 0 pct))
              (string-trim (substring line (1+ pct))))
      (cons (string-trim line) ""))))

(defun tochemfig-cp--read-group (s i)
  "Read a {...} group in S starting at the brace index I.
Return (CONTENT . INDEX-AFTER)."
  (let ((depth 0) (start (1+ i)) (j i) (n (length s)) (result nil))
    (while (and (< j n) (null result))
      (let ((c (aref s j)))
        (cond ((= c ?{) (setq depth (1+ depth)))
              ((= c ?})
               (setq depth (1- depth))
               (when (= depth 0)
                 (setq result (cons (substring s start j) (1+ j)))))))
      (setq j (1+ j)))
    (or result (error "Unbalanced braces in %S" s))))

(defun tochemfig-cp--find-bracket (s i)
  "Return the index of the `]' matching the `[' at I in S."
  (let ((j (1+ i)) (n (length s)))
    (while (and (< j n) (not (eq (aref s j) ?\]))) (setq j (1+ j)))
    (if (< j n) j (error "Unbalanced bracket in %S" s))))

(defun tochemfig-cp--strip-markers (s)
  "Remove leading `@{...}' atom markers from S."
  (while (and (> (length s) 1) (string-prefix-p "@{" s))
    (let ((j (cl-position ?} s)))
      (setq s (if j (substring s (1+ j)) ""))))
  s)

(defun tochemfig-cp--marker-index (s)
  "Return the atom index encoded in a leading `@{aN}' marker, or nil."
  (when (and (string-prefix-p "@{" s) (cl-position ?} s))
    (let ((inner (substring s 2 (cl-position ?} s))))
      (when (string-match "\\([0-9]+\\)\\'" inner)
        (string-to-number (match-string 1 inner))))))

(defun tochemfig-cp--macro-groups (s name)
  "Return the brace groups following the first NAME macro in S."
  (when (string-match (regexp-quote name) s)
    (let ((i (match-end 0)) (groups nil) (n (length s)))
      (while (and (< i n) (eq (aref s i) ?{))
        (let ((g (tochemfig-cp--read-group s i)))
          (push (car g) groups)
          (setq i (cdr g))))
      (nreverse groups))))

;;; Bond parsing

(defun tochemfig-cp--bond-order (bond-char spec)
  "Return the structural bond order for BOND-CHAR and SPEC."
  (let ((spec (or spec "")))
    (cond
     ((string-match-p "mcfwavy" spec) 4)
     ((string-match-p "trpl" spec) 3)
     ((string-match-p "db[lr]" spec) 2)
     ((string-match-p "d[lr][hn]" spec) 2)
     ((eq bond-char ?=) 2)
     ((eq bond-char ?~) 3)
     (t 1))))

(defun tochemfig-cp--parse-bond (code)
  "If CODE starts with a bond, return (PLIST . REST); otherwise (nil . CODE).
PLIST has :order and :link (non-nil for invisible `draw=none' bonds)."
  (let ((n (length code)))
    (if (or (zerop n) (not (memq (aref code 0) '(?- ?= ?~ ?< ?>))))
        (cons nil code)
      (let ((c (aref code 0)) (i 1))
        (when (and (memq c '(?< ?>)) (< i n) (eq (aref code i) ?:))
          (setq i (1+ i)))
        (when (and (= c ?=) (< i n) (memq (aref code i) '(?_ ?^)))
          (setq i (1+ i)))
        (let ((spec nil))
          (when (and (< i n) (eq (aref code i) ?\[))
            (let ((j (tochemfig-cp--find-bracket code i)))
              (setq spec (substring code (1+ i) j))
              (setq i (1+ j))))
          (cons (list :order (tochemfig-cp--bond-order c spec)
                      :link (and spec (string-match-p "draw=none" spec)))
                (substring code i)))))))

;;; Atom label parsing

(defun tochemfig-cp--strip-charge-groups (s)
  "Remove `^{...}' and `_{...}' charge groups from S."
  (replace-regexp-in-string "[_^]{[^{}]*}" "" s))

(defun tochemfig-cp--plain-element (s)
  "Extract a plain element symbol from S.
Prefer the first non-hydrogen run, falling back to hydrogen, then carbon."
  (let ((runs nil) (start 0) (case-fold-search nil))
    (while (string-match "[A-Z][a-z]?" s start)
      (push (match-string 0 s) runs)
      (setq start (match-end 0)))
    (setq runs (nreverse runs))
    (or (cl-find-if (lambda (r) (not (equal r "H"))) runs)
        (car runs)
        "C")))

(defun tochemfig-cp--element-token-p (s)
  "Return non-nil when S is a bare element symbol."
  (and (stringp s) (let ((case-fold-search nil))
                     (string-match-p "\\`[A-Z][a-z]?\\'" s))))

(defun tochemfig-cp--element (s)
  "Return the element symbol described by atom label S."
  (cond
   ((string-match "\\\\charge{" s)
    (tochemfig-cp--plain-element
     (or (nth 1 (tochemfig-cp--macro-groups s "\\charge")) "")))
   ((string-match "\\\\lewis{" s)
    (tochemfig-cp--plain-element
     (or (nth 1 (tochemfig-cp--macro-groups s "\\lewis")) "")))
   ((string-match "\\\\mcf\\(aboveright\\|belowright\\|right\\|left\\|above\\|below\\){" s)
    (let* ((m (match-string 0 s))
           (name (substring m 0 (1- (length m))))
           (groups (tochemfig-cp--macro-groups s name)))
      (or (cl-find-if #'tochemfig-cp--element-token-p groups) "C")))
   ((string-match "\\\\mcfatomno{" s) "C")
   (t (tochemfig-cp--plain-element (tochemfig-cp--strip-charge-groups s)))))

(defun tochemfig-cp--charge (s)
  "Return the formal charge described by atom label S."
  (cond
   ((string-match "\\([0-9]*\\)\\\\mcfplus" s)
    (let ((m (match-string 1 s))) (if (equal m "") 1 (string-to-number m))))
   ((string-match "\\([0-9]*\\)\\\\mcfminus" s)
    (let ((m (match-string 1 s))) (- (if (equal m "") 1 (string-to-number m)))))
   (t 0)))

(defun tochemfig-cp--radical (s)
  "Return the number of radical electrons described by atom label S."
  (cond
   ((string-match "\\\\charge{" s)
    (let ((spec (or (car (tochemfig-cp--macro-groups s "\\charge")) "")))
      (cond ((string-match-p "\\\\:" spec) 2)
            ((string-match-p "\\\\\\." spec) 1)
            (t 1))))
   ((string-match "\\\\lewis{" s)
    (let ((spec (or (car (tochemfig-cp--macro-groups s "\\lewis")) "")))
      (if (string-match-p ":" spec) 2 1)))
   (t 0)))

(defun tochemfig-cp--closure-target (comment)
  "Return the ring-closure partner named by COMMENT, or nil."
  (when (string-match "\\`->[[:space:]]*\\([0-9]+\\)" comment)
    (string-to-number (match-string 1 comment))))

(defun tochemfig-cp--atom-number (comment)
  "Return the original atom number named by COMMENT, or nil."
  (when (string-match "\\`\\([0-9]+\\)\\'" comment)
    (string-to-number (match-string 1 comment))))

;;; Entry point

(defun tochemfig-parse-chemfig (code)
  "Parse CODE into a molecular graph.
Return (ATOMS . BONDS).  ATOMS is a list of plists with :id, :element,
:charge, :radical, :orig.  BONDS is a list of (A B ORDER) with 0-based
parser ids.  Invisible link bonds and aromatic-circle bonds are dropped."
  (let ((atoms nil)
        (bonds nil)
        (orig->id (make-hash-table :test 'equal))
        (stack nil)
        (current nil))
    (cl-labels ((add-atom (element charge radical orig)
                  (let ((id (length atoms)))
                    (push (list :id id :element element :charge charge
                                :radical radical :orig orig)
                          atoms)
                    (when orig (puthash orig id orig->id))
                    id))
                (add-bond (a b order)
                  (when (and a b order)
                    (push (list (min a b) (max a b) order) bonds))))
      (dolist (line (split-string code "\n"))
        (let* ((split (tochemfig-cp--split-line line))
               (c (car split))
               (comment (cdr split)))
          (cond
           ((and (string= c "") (string= comment "")) nil)
           ;; Wrapper lines from `wrap-chemfig' / `submol-name': `\chemfig{',
           ;; `\definesubmol{...}{' and the matching lone `}'.
           ((or (string-prefix-p "\\chemfig{" c)
                (string-prefix-p "\\definesubmol{" c)
                (string= c "}"))
            nil)
           ((string= c "(") (push current stack))
           ((string= c ")") (setq current (pop stack)))
           (t
            (let* ((bp (tochemfig-cp--parse-bond c))
                   (binfo (car bp))
                   (raw (cdr bp))
                   (marker (tochemfig-cp--marker-index raw))
                   (rest (tochemfig-cp--strip-markers raw))
                   (target (tochemfig-cp--closure-target comment))
                   (link (plist-get binfo :link)))
              (cond
               ;; Ring closure to an already-emitted atom.
               (target
                (add-bond current (gethash target orig->id)
                          (or (plist-get binfo :order) 1)))
               ;; Aromatic circle pseudo-bond: no atom.
               ((string-match-p "\\\\mcfcringle" rest) nil)
               ;; A phantom whose partner comment is missing: drop it.
               ((string-prefix-p "\\phantom{" rest) nil)
               ;; Real atom (empty label means a hidden carbon).
               (t
                (let* ((orig (or (tochemfig-cp--atom-number comment) marker))
                       (element (if (string= rest "")
                                    "C"
                                  (tochemfig-cp--element rest)))
                       (id (add-atom element
                                     (tochemfig-cp--charge rest)
                                     (tochemfig-cp--radical rest)
                                     orig)))
                  (unless link
                    (add-bond current id (plist-get binfo :order)))
                  (setq current id))))))))))
    (cons (nreverse atoms) (nreverse bonds))))

(provide 'tochemfig-chemfig-parser)
;;; tochemfig-chemfig-parser.el ends here
