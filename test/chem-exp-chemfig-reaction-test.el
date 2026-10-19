;;; chem-exp-chemfig-reaction-test.el --- Chemfig reaction tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Giovanni Crisalfi

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for chemfig reaction rendering.  Molecule parity tests live in
;; `chem-parity-test.el'; this file covers the scheme wrapper, arrows, plus
;; signs, agent/condition formatting, escaping, and option handling.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((here (file-name-directory (or load-file-name buffer-file-name))))
  (when here (add-to-list 'load-path here)))

(let ((sibling (expand-file-name "emacs-indigo"
                                 (file-name-directory
                                  (directory-file-name
                                   (file-name-directory (or load-file-name buffer-file-name)))))))
  (when (file-directory-p sibling)
    (add-to-list 'load-path sibling)))

(require 'chem-mode)
(require 'chem-indigo)
(require 'chem-options)
(require 'chem-export)
(require 'chem-exp-chemfig)
(require 'chem-reaction)

(defconst chem-exp-chemfig-reaction-test--root
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the test files.")

(defun chem-exp-chemfig-reaction-test--path (relative)
  "Resolve RELATIVE against the test directory."
  (expand-file-name relative chem-exp-chemfig-reaction-test--root))

(defun chem-exp-chemfig-reaction-test--options (overrides)
  "Return chemfig reaction options from OVERRIDES."
  (chem--build-reaction-options overrides 'chemfig))

(defun chem-exp-chemfig-reaction-test--render (smiles overrides)
  "Render reaction SMILES with OVERRIDES as chemfig."
  (let* ((options (chem-exp-chemfig-reaction-test--options overrides))
         (reaction (chem-from-reaction-string smiles options)))
    (chem-render reaction 'chemfig options)))

(defun chem-exp-chemfig-reaction-test--golden (name)
  "Return the reaction golden file NAME."
  (with-temp-buffer
    (insert-file-contents
     (chem-exp-chemfig-reaction-test--path
      (concat "golden/reactions/" name ".tex")))
    (buffer-string)))

(defun chem-exp-chemfig-reaction-test--count (regexp string)
  "Count matches of REGEXP in STRING."
  (let ((count 0)
        (start 0))
    (while (string-match regexp string start)
      (setq count (1+ count))
      (setq start (match-end 0)))
    count))

(defun chem-exp-chemfig-reaction-test--balanced-p (code)
  "Return non-nil when CODE has balanced delimiters."
  (let ((stack nil)
        (ok t))
    (dolist (char (string-to-list code))
      (cond
       ((memq char '(?\( ?\[ ?{))
        (push char stack))
       ((memq char '(?\) ?\] ?}))
        (if (eq (car stack) (cdr (assq char '((?\) . ?\() (?\] . ?\[) (?} . ?{)))))
            (pop stack)
          (setq ok nil)))))
    (and ok (null stack))))

(defconst chem-exp-chemfig-reaction-test--cases
  '(("simple" "CCO>>CC=O" nil)
    ("two_reactants" "CC(=O)O.OCC>>CC(=O)OCC.O" nil)
    ("two_products" "CCO>>C=C.O" nil)
    ("agent_above" "CC(=O)O.OCC>[H+]>CC(=O)OCC.O" nil)
    ("agent_and_above"
     "CC(=O)O.OCC>[H+]>CC(=O)OCC.O"
     (("reaction-above" . "heat")))
    ("below_only" "CCO>>CC=O" (("reaction-below" . "cat.")))
    ("above_below" "CCO>>CC=O"
     (("reaction-above" . "heat")
      ("reaction-below" . "cat.")))
    ("equilibrium" "CCO>>CC=O" (("reaction-arrow" . "equilibrium")))
    ("charged_aromatic_stereo"
     "N[C@@H](C)C(=O)[O-].c1ccccc1>>N[C@@H](C)C(=O)O.c1ccccc1"
     nil)
    ("ring_formation" "C=C.C=C>>C1CCC1" nil)
    ("indent" "CCO>>CC=O" (("indent" . 2)))
    ("terse" "CCO>>CC=O" (("terse" . t)))
    ("no_wrap" "CCO>>CC=O" (("reaction-wrap" . nil)))
    ("arrow_override" "CCO>>CC=O"
     (("reaction-arrow-chemfig" . "<->")
      ("reaction-above" . "heat")))
    ("escaping" "CCO>>CC=O"
     (("reaction-above" . "a%b&c_d{e}f\\g")
      ("reaction-below" . "x_y"))))
  "Golden reaction cases.")

(ert-deftest chem-exp-chemfig-reaction-goldens ()
  "Reaction goldens match and have valid scheme structure."
  (dolist (case chem-exp-chemfig-reaction-test--cases)
    (let* ((name (nth 0 case))
           (smiles (nth 1 case))
           (overrides (nth 2 case))
           (options (chem-exp-chemfig-reaction-test--options overrides))
           (reaction (chem-from-reaction-string smiles options))
           (output (chem-render reaction 'chemfig options))
           (reactants (length (chem-reaction-reactants reaction)))
           (products (length (chem-reaction-products reaction)))
           (plus-count (+ (max 0 (1- reactants))
                          (max 0 (1- products)))))
      (should (equal (chem-exp-chemfig-reaction-test--golden name)
                     output))
      (should (chem-exp-chemfig-reaction-test--balanced-p output))
      (should (= 1 (chem-exp-chemfig-reaction-test--count
                    (regexp-quote "\\arrow{") output)))
      (should (= plus-count
                 (chem-exp-chemfig-reaction-test--count
                  (regexp-quote "\\+") output)))
      (should (= (+ reactants products)
                 (chem-exp-chemfig-reaction-test--count
                  (regexp-quote "\\chemfig{") output)))
      (if (plist-get options :reaction-wrap)
          (progn
            (should (= 1 (chem-exp-chemfig-reaction-test--count
                          (regexp-quote "\\schemestart") output)))
            (should (= 1 (chem-exp-chemfig-reaction-test--count
                          (regexp-quote "\\schemestop") output))))
        (should-not (string-match-p (regexp-quote "\\schemestart")
                                    output))
        (should-not (string-match-p (regexp-quote "\\schemestop")
                                    output))))))

(ert-deftest chem-exp-chemfig-reaction-arrow-styles ()
  "Arrow styles are derived from the prepared reaction."
  (should (string-match-p
           (regexp-quote "\\arrow{<-}")
           (chem-exp-chemfig-reaction-test--render
            "CCO>>CC=O" '(("reaction-arrow" . "reverse")))))
  (should (string-match-p
           (regexp-quote "\\arrow{<->}")
           (chem-exp-chemfig-reaction-test--render
            "CCO>>CC=O" '(("reaction-arrow" . "equilibrium"))))))

(ert-deftest chem-exp-chemfig-reaction-conditions ()
  "Conditions use the expected optional arrow arguments."
  (should (string-match-p
           (regexp-quote "\\arrow{->[heat]}")
           (chem-exp-chemfig-reaction-test--render
            "CCO>>CC=O" '(("reaction-above" . "heat")))))
  (should (string-match-p
           (regexp-quote "\\arrow{->[][cat.]}")
           (chem-exp-chemfig-reaction-test--render
            "CCO>>CC=O" '(("reaction-below" . "cat.")))))
  (should (string-match-p
           (regexp-quote "\\arrow{->[heat][cat.]}")
           (chem-exp-chemfig-reaction-test--render
            "CCO>>CC=O"
            '(("reaction-above" . "heat")
              ("reaction-below" . "cat."))))))

(ert-deftest chem-exp-chemfig-reaction-agent-formatting ()
  "Agent formulas use subscripts and math-mode charges."
  (let ((output (chem-exp-chemfig-reaction-test--render
                 "CC>O.CCO>CC" nil)))
    (dolist (needle '("H$_{2}$O" "C$_{2}$H$_{6}$O"))
      (should (string-match-p (regexp-quote needle) output))))
  (let ((output (chem-exp-chemfig-reaction-test--render
                 "CC>[H+]>CC" nil)))
    (should (string-match-p (regexp-quote "H$^{+}$") output))))

(ert-deftest chem-exp-chemfig-reaction-escaping ()
  "LaTeX special characters in conditions are escaped or rejected."
  (let ((output (chem-exp-chemfig-reaction-test--render
                 "CCO>>CC=O"
                 '(("reaction-above" . "a%b&c_d{e}f\\g")
                   ("reaction-below" . "x_y")))))
    (dolist (escaped '("\\%" "\\&" "\\_" "\\{" "\\}"
                       "\\textbackslash{}"))
      (should (string-match-p (regexp-quote escaped) output))))
  (should-error
   (chem-exp-chemfig-reaction-test--render
    "CCO>>CC=O"
    '(("reaction-above" . "50%")
      ("terse" . t)))
   :type 'user-error))

(ert-deftest chem-exp-chemfig-reaction-option-errors ()
  "Molecule-only wrappers are rejected for reactions."
  (should-error
   (chem-exp-chemfig-reaction-test--render
    "CCO>>CC=O" '(("submol-name" . "rxn")))
   :type 'user-error)
  (should-error
   (chem-exp-chemfig-reaction-test--render
    "CCO>>CC=O" '(("wrap-chemfig" . t)))
   :type 'user-error))

(ert-deftest chem-exp-chemfig-reaction-arrow-override ()
  "Explicit chemfig arrows keep their condition arguments."
  (let ((output (chem-exp-chemfig-reaction-test--render
                 "CCO>>CC=O"
                 '(("reaction-arrow-chemfig" . "<->")
                   ("reaction-above" . "heat")))))
    (should (string-match-p
             (regexp-quote "\\arrow{<->[heat]}")
             output))))

(ert-deftest chem-exp-chemfig-reaction-molecule-path ()
  "The existing molecule path is unchanged."
  (let* ((options (chem--build-options nil 'chemfig))
         (drawing (chem-from-file
                   (chem-exp-chemfig-reaction-test--path
                    "corpus/ethanol.mol")
                   options))
         (output (chem-render drawing 'chemfig options))
         (golden
          (with-temp-buffer
            (insert-file-contents
             (chem-exp-chemfig-reaction-test--path
              "golden/ethanol__default.tex"))
            (buffer-string))))
    (should (equal golden output))
    (should-not (string-match-p (regexp-quote "\\schemestart")
                                output))))

(provide 'chem-exp-chemfig-reaction-test)
;;; chem-exp-chemfig-reaction-test.el ends here
