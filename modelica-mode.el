;;; modelica-mode.el --- major mode for editing Modelica files

;; Copyright (C) 2010       Dietmar Winkler
;; Copyright (C) 1997--2001 Ruediger Franke
;; Copyright (C) 1997--2001 Free Software Foundation, Inc.

;; Keywords: languages, continuous system modeling
;; Author:   Ruediger Franke <rfranke@users.sourceforge.net>

;; This code has been written for use with Emacs and shares its licensing.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst modelica-mode-version "1.4.1")

;;; Synched up with: GNU Emacs 20.7, XEmacs 21.1.

;;; Commentary:

;; This package provides a fundamental Modelica mode.
;; It covers:
;;  - show / hide of annotations
;;      C-c C-s  show annotation of current statement
;;      C-c C-h  hide annotation of current statement
;;      M-s      show all annotations
;;      M-h      hide all annotations
;;
;;  - indentation of lines, e.g.
;;      TAB      indent current line
;;      M-x C-\  indent current region
;;      C-j      indent current line, create a new line, indent it
;;               (like TAB ENTER TAB)
;;
;;  - hide/show of annotations
;;      C-h      hide annotations
;;      C-s      show annotations
;;
;;  - automatic insertion of end statements
;;      C-c C-e  search backwards for the last unended begin of a code block,
;;               insert the according end-statement
;;
;;  - move commands which know about statements and statement blocks
;;      M-f      move to next beginning of a statement
;;      M-b      move to previous beginning of a statement
;;      M-n      move to next beginning of a statement block
;;      M-p      move to previous beginning of a statement block
;;      M-a      move to beginning of current statement block
;;      M-e      move to end of current statement block
;;
;;  - commands for writing comments treat documentation strings as well
;;      M-;      insert a comment for current statement (standard Emacs)
;;      M-"      insert a documentation string for current statement
;;      M-j      continue comment or documentation string on next line
;;
;;  - syntax highlighting using font-lock-mode
;;
;; Current limitations:
;;  - conditional expessions are only supported on right hand sides of
;;    equations; otherwise simple expressions are assumed
;;
;; Installation:
;; (1) Put the file
;;        modelica-mode.el
;;     to an Emacs lisp directory, e.g. ~/elisp
;;
;; (2) Add the following lines to your ~/.emacs file
;;
;;  (setq load-path (cons "~/elisp" load-path))
;;  (autoload 'modelica-mode "modelica-mode" "Modelica Editing Mode" t)
;;  (setq auto-mode-alist (cons '("\.mo$" . modelica-mode) auto-mode-alist))
;;
;; (3) Activate the mode by loading a file with the extension ".mo"
;;     or by invoking
;;      M-x modelica-mode
;;
;; (4) Optionally byte-compile the lisp code
;;
;; (5) Please send comments and suggestions to
;;     Ruediger Franke <rfranke@users.sourceforge.net>

;;; History
;;    see ChangeLog

;;; constants

(defconst mdc-class-modifier-keyword
  "\\(encapsulated\\|final\\|inner\\|outer\\|partial\\|re\\(declare\\|placeable\\)\\)[ \t\n\r]+"
  "*Keyword regexp optionally found before a class keyword.")

(defconst mdc-class-keyword
  "\\(block\\|c\\(lass\\|onnector\\)\\|function\\|model\\|package\\|record\\|type\\)[ \t\n\r]+"
  "*Keyword regexp preceding a Modelica class declaration or definition.")

;;; Interface to font-lock

(defvar mdc-font-lock-keywords nil
  "Keywords to highlight for Modelica. See variable `font-lock-keywords'.")

(if mdc-font-lock-keywords
    ()
  (setq mdc-font-lock-keywords
	(list
	 (list (concat "\\<"
		       "\\(do\\|"
		       "\\(end[ \t\n]+\\(if\\|for\\|wh\\(en\\|ile\\)\\)\\)\\|"
		       ; (regexp-opt
		       ; '("import" "within" "extends"
		       ;   "for" "while" "in" "loop" "when"
		       ;   "if" "then" "else" "elseif" "elsewhen"
		       ;   "and" "not" "or"))
		       "and\\|e\\(lse\\(if\\|when\\)?\\|xtends\\)\\|for\\|"
		       "i\\(mport\\|[fn]\\)\\|loop\\|not\\|or\\|then\\|"
		       "w\\(h\\(en\\|ile\\)\\|ithin\\)"
		       "\\)\\>")
	       0 'font-lock-keyword-face)
	 (list (concat "\\<"
		       ;(regexp-opt
		       ; '("algorithm" "equation" "public" "protected") t)
		       "\\(algorithm\\|equation\\|p\\(rotected\\|ublic\\)\\)"
		       "\\>")
	       0 'font-lock-keyword-face)
	 (list (concat "\\<"
		       ;(regexp-opt
		       ; '("redeclare" "final" "partial" "replaceable"
		       ;   "inner" "outer" "encapsulated"
		       ;   "discrete" "parameter" "constant"
		       ;   "flow" "input" "output" "external"
		       ;   "block" "class" "connector" "function" "model"
		       ;   "package" "record" "type"
		       ;   "end") t)
		       "\\(block\\|c\\(lass\\|on\\(nector\\|stant\\)\\)\\|"
		       "discrete\\|e\\(n\\(capsulated\\|d\\)\\|xternal\\)\\|"
		       "f\\(inal\\|low\\|unction\\)\\|in\\(ner\\|put\\)\\|"
		       "model\\|out\\(er\\|put\\)\\|pa\\(ckage\\|r\\(ameter\\|"
		       "tial\\(\\)?\\)\\)\\|re\\(cord\\|declare\\|"
		       "placeable\\)\\|type\\)"
		       "\\>")
	       0 'font-lock-type-face)
	 (list (concat "\\<"
		       ;(regexp-opt
		       ; '("der" "analysisType" "initial" "terminal"
		       ;   "noEvent" "samle" "pre" "edge" "change"
		       ;   "reinit" "abs" "sign" "sqrt" "div" "mod"
		       ;   "rem" "ceil" "floor" "integer" "delay"
		       ;   "cardinality"
		       ;   "promote" "ndims" "size" "scalar" "vector" "matrix"
		       ;   "transpose" "outerProduct" "identity" "diagonal"
		       ;   "zeros" "ones" "fill" "linspace" "min" "max" "sum"
		       ;   "product" "symmetric" "cross" "skew"
		       ;) t)
		       "\\(a\\(bs\\|nalysisType\\)\\|c\\(ardinality\\|eil\\|"
		       "hange\\|ross\\)\\|d\\(e\\(lay\\|r\\)\\|i\\(agonal\\|"
		       "v\\)\\)\\|edge\\|f\\(ill\\|loor\\)\\|i\\(dentity\\|"
		       "n\\(itial\\|teger\\)\\)\\|linspace\\|m\\(a\\(trix\\|"
		       "x\\)\\|in\\|od\\)\\|n\\(dims\\|oEvent\\)\\|o\\(nes\\|"
		       "uterProduct\\)\\|pr\\(e\\|o\\(duct\\|mote\\)\\)\\|"
		       "re\\(init\\|m\\)\\|s\\(amle\\|calar\\|i\\(gn\\|"
		       "ze\\)\\|kew\\|qrt\\|um\\|ymmetric\\)\\|t\\(erminal\\|"
		       "ranspose\\)\\|vector\\|zeros\\)"
		       "\\>")
	       0 'font-lock-function-name-face)
	 (list (concat "\\<"
		       ;(regexp-opt
		       ; '("assert" "terminate") t)
		       "\\(assert\\|terminate\\)"
		       "\\>")
	       0 'font-lock-warning-face)
	 (list (concat "\\<"
		       ;(regexp-opt
		       ; '("annotation" "connect") t)
		       "\\(annotation\\|connect\\)"
		       "\\>")
	       0 (if (string-match "XEmacs" (emacs-version))
		     ;; XEmacs 21.1 still uses old font-lock version
		     (identity 'font-lock-preprocessor-face)
		   (identity 'font-lock-builtin-face)))
	 (list (concat "\\<"
		       ;(regexp-opt
		       ; '("false" "true") t)
		       "\\(false\\|true\\)"
		       "\\>")
	       0 (if (string-match "XEmacs" (emacs-version))
		     ;; XEmacs 21.1 still uses old font-lock version
		     (identity 'font-lock-reference-face)
		   (identity 'font-lock-constant-face)))
	 (list (concat "\\<"
		       ;(regexp-opt
		       ; '("time") t)
		       "\\(time\\)"
		       "\\>")
	       0 'font-lock-variable-name-face))))

;;; The mode

(defvar mdc-basic-offset 2
  "*basic offset for indentation in Modelica Mode")

(defvar mdc-comment-offset 3
  "*offset for indentation in comments in Modelica Mode")

(defvar mdc-statement-offset 2
  "*offset for indentation in statements in Modelica Mode")

(defvar mdc-mode-syntax-table nil
  "Syntax table used while in Modelica mode.")

(defvar mdc-mode-abbrev-table nil
  "Abbrev table used while in Modelica mode.")
(define-abbrev-table 'mdc-mode-abbrev-table ())

(if mdc-mode-syntax-table
    ()              ; Do not change the table if it is already set up.
  (setq mdc-mode-syntax-table (make-syntax-table))

  (modify-syntax-entry ?_ "w"       mdc-mode-syntax-table)
  (modify-syntax-entry ?. "w"       mdc-mode-syntax-table)
  (if (string-match "XEmacs" (emacs-version))
      (modify-syntax-entry ?/  ". 1456" mdc-mode-syntax-table)
    (modify-syntax-entry ?/  ". 124b" mdc-mode-syntax-table))

  (modify-syntax-entry ?*  ". 23"   mdc-mode-syntax-table)
  (modify-syntax-entry ?\n "> b"    mdc-mode-syntax-table))

(defvar mdc-mode-map nil
  "Keymap for Modelica mode.")

(if mdc-mode-map
    ()
  (setq mdc-mode-map (make-sparse-keymap))
  (define-key mdc-mode-map "\C-j"  	'mdc-newline-and-indent)
  (define-key mdc-mode-map "\C-c\C-e" 	'mdc-insert-end)
  (define-key mdc-mode-map "\C-c\C-s" 	'mdc-show-annotation)
  (define-key mdc-mode-map "\C-c\C-h" 	'mdc-hide-annotation)
  (define-key mdc-mode-map "\es" 	'mdc-show-all-annotations)
  (define-key mdc-mode-map "\eh" 	'mdc-hide-all-annotations)
  (define-key mdc-mode-map "\C-c\C-c" 	'comment-region)
  (define-key mdc-mode-map "\e\""       'mdc-indent-for-docstring)
  (define-key mdc-mode-map "\e;"        'mdc-indent-for-comment)
  (define-key mdc-mode-map "\ej"        'mdc-indent-new-comment-line)
  (define-key mdc-mode-map "\ef"        'mdc-forward-statement)
  (define-key mdc-mode-map "\eb"        'mdc-backward-statement)
  (define-key mdc-mode-map "\en"        'mdc-forward-block)
  (define-key mdc-mode-map "\ep"        'mdc-backward-block)
  (define-key mdc-mode-map "\ea"        'mdc-to-block-begin)
  (define-key mdc-mode-map "\ee"        'mdc-to-block-end))

(defvar mdc-mode-menu
  '("Modelica"
    ("Move to"
     [" - next statement"        mdc-forward-statement t]
     [" - previous statement"    mdc-backward-statement t]
     [" - start of code block"   mdc-to-block-begin t]
     [" - end of code block"     mdc-to-block-end t]
     )
    [" - next code block"        mdc-forward-block t]
    [" - previous code block"    mdc-backward-block t]
    "-"
    ("Annotation"
     [" - show all"              mdc-show-all-annotations t]
     [" - hide all"              mdc-hide-all-annotations t]
     )
     [" - show current"          mdc-show-annotation t]
     [" - hide current"          mdc-hide-annotation
      :keys "C-c C-h" :active t]
    "-"
    ("Indent"
     [" - for comment"           mdc-indent-for-comment t]
     [" - for docstring"         mdc-indent-for-docstring t]
     ["Newline and indent"       mdc-newline-and-indent
      :keys "C-j" :active t]
     ["New comment line"         mdc-indent-new-comment-line t]
     )
    [" - line"                   indent-for-tab-command t]
    [" - region"                 indent-region (mark)]
    "-"
    ["Comment out region"        comment-region  (mark)]
    ["Uncomment region"          (comment-region (point) (mark) '(4))
     :keys "C-u C-c C-c" :active (mark)]
    "-"
    ["End code block"            mdc-insert-end t]
    )
  "Menu for Modelica mode.")

;; define Modelica menu if easymenu is available
(if (condition-case nil
	(require 'easymenu)
      (error nil))
    (easy-menu-define mdc-mode-menu-symbol
		      mdc-mode-map
		      "Menu for Modelica mode"
		      mdc-mode-menu))

;;;###autoload
(defun modelica-mode ()
  "Major mode for editing Modelica files."
  (interactive)
  (kill-all-local-variables)

  (setq major-mode 'modelica-mode)
  (setq mode-name "Modelica")

  (use-local-map mdc-mode-map)
  (set-syntax-table mdc-mode-syntax-table)
  (setq local-abbrev-table mdc-mode-abbrev-table)

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'mdc-indent-line)

  ;; comment syntax
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-multi-line)
  (setq	comment-column 32
	comment-start "// "
	comment-start-skip "/\\*+ *\\|// *"
	comment-end ""
	comment-multi-line nil)

  ;; settings for font-lock-mode
  (make-local-variable 'font-lock-keywords)
  (setq font-lock-keywords mdc-font-lock-keywords)
  ;; font-lock-mode for newer GNU Emacs versions
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(mdc-font-lock-keywords nil nil))

  ;; hide/show annotations
  (make-local-variable 'line-move-ignore-invisible)
  (setq line-move-ignore-invisible t)
  (if (functionp 'add-to-invisibility-spec)
      (add-to-invisibility-spec '(mdc-annotation . t))
    ;; XEmacs 21.1 does not know function add-to-invisibility-spec
    (make-local-variable 'buffer-invisibility-spec)
    (setq buffer-invisibility-spec '((mdc-annotation . t))))
  (mdc-hide-all-annotations)

  ;; add menu
  (if mdc-mode-menu-symbol
      (easy-menu-add mdc-mode-menu-symbol))

  (run-hooks 'modelica-mode-hook))

(defun mdc-indent-for-comment ()
  "Indent this line's comment to comment-column,
   or insert an empty comment."
  (interactive)
  (indent-for-comment)
  (mdc-indent-line))

(defun mdc-indent-for-docstring ()
  "Indent this statement's documentation string to comment-column,
   or insert an empty documentation string."
  (interactive)
  (let ((deleted "") save-point)
    ;; move behind current statement
    (skip-chars-forward " \t")
    (condition-case nil
	(progn
	  (mdc-forward-statement)
	  (forward-comment (- (point-max))))
      (error
       (progn
	 (end-of-line)
	 (skip-chars-backward " \t"))))
    ;; remove ending ";", if any, and store it in "deleted"
    (if (or (looking-at "[ \t\n]")
	    (eobp))
	(forward-char -1))
    (if (looking-at ";")
	(progn
	  (delete-char 1)
	  (setq deleted ";"))
      (forward-char 1))
    ;; move backwards to last non-blank
    (skip-chars-backward " \t")
    (if (or (looking-at "[ \t\n]")
	    (eobp))
	(forward-char -1))
    (if (looking-at "\"")
	;; indent docstring
	(progn
	  (forward-char 1)
	  (insert-string deleted)
	  (forward-char (- (1+ (length deleted))))
	  (mdc-within-string t)
	  (while (mdc-behind-string t))
	  (setq save-point (point))
	  (skip-chars-backward " \t")
	  (delete-region (point) save-point)
	  (indent-to (max comment-column (1+ (current-column))))
	  (forward-char 1))
      ;; insert new docstring
      (forward-char 1)
      (indent-to (max comment-column (1+ (current-column))))
      (insert-string (concat "\"\"" deleted))
      (forward-char (- (1+ (length deleted))))))
  (mdc-indent-line))

(defun mdc-indent-new-comment-line ()
  "indent-new-comment-line for Modelica mode. The function additionally
   considers documentation strings"
  (interactive)
  (mdc-indent-line)
  (let (starter)
    (cond
     ;; treat documentation string
     ((mdc-within-string)
      (insert "\"\n\""))
     ;; adapt comment-multi-line and
     ;; call default indent-new-comment-line
     (t
      (setq starter (mdc-within-comment))
      (if (equal starter "/*")
	  (setq comment-multi-line t)
	(setq comment-multi-line nil))
      (indent-new-comment-line))))
  (mdc-indent-line))

(defun mdc-indent-line ()
  "Indentation for Modelica."
  (let ((pos (- (point-max) (point))) beg beg-anno end-anno)
    (beginning-of-line)
    (setq beg (point))
    ;; no indentation of invisible text (hidden annotations)
    (if (mdc-within-overlay 'invisible)
	()
      ;; no indentation if preceeding newline is quoted
      (if (and (> (point) 2)
	       (progn
		 (forward-char -2)
		 (looking-at "[\\]\n")))
	  (forward-char 2)
	;; else indent line
	(goto-char beg)
	(skip-chars-forward " \t")
	(let ((indent (mdc-calculate-indent)))
	  (if (= indent (current-column))
	      ;; nothing to be done
	      ()
	    (delete-region beg (point))
	    (indent-to indent)))))
    ;; return to the old position inside the line
    (if (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos)))))

(defun mdc-calculate-indent ()
  "Calculate indentation for current line;
   assumes point to be over the first non-blank of the line"
  (save-excursion
    (let ((case-fold-search nil)
	  offset (last-open nil) (save-point (point))
	  ref-point ref-column)
      (cond
       ;; multi-line comment has fixed indentation, relative to its start
       ((mdc-within-comment t)
	(setq ref-column (current-column))
	(goto-char save-point)
	(if (looking-at "\\*/")
	    ref-column
	  (+ ref-column mdc-comment-offset)))
       ;; concatenation of strings
       ((and (looking-at "\"")
	     (mdc-behind-string t))
	;; move point to the very first string constant
	;; in order to consider concatenation on the same line
	(while (mdc-behind-string t))
	(current-column))
       ;; continued single-line comment
       ((and (looking-at "//")
	     (forward-comment -1)
	     (looking-at "//"))
	(current-column))
       ;; default looks for last unended begin-like statement
       (t
	(goto-char save-point) ; needed after check for singele-line comments
	(setq offset mdc-basic-offset)
	;; goto left for labels, end's etc.
	(if (looking-at
	     (concat
	      ; ("algorithm" "elseif" "elsewhen" "end" "equation" "external"
	      ;  "in" "loop" "protected" "public")
	      "\\(algorithm\\|e\\(lse\\(if\\|when\\)\\|nd\\|quation\\|"
	      "xternal\\)\\|in\\|loop\\|p\\(rotected\\|ublic\\)\\)"
	      "\\>"))
	    (setq offset (- offset mdc-basic-offset)))
	(if (and
	     (looking-at
	      (concat
	       ; ("else" "then")
	       "\\(else\\|then\\)"
	       "\\>"))
	     (not (mdc-within-equation)))
	    (setq offset (- offset mdc-basic-offset)))
	(condition-case nil
	    (let ()
	      (mdc-last-unended-begin t)
	      ;; correct offset
	      (if (looking-at "end\\>")
		  ;; found an 'end', means no basic offset
		  (setq offset (- offset mdc-basic-offset)))
	      ;; check indentation in statements
	      (setq ref-column (current-column))
	      (setq ref-point (point))
	      (goto-char save-point)
	      (mdc-statement-start ref-point)
	      (if (>= (point) save-point)
		  ;; indent relative to ref-point as new statement starts
		  (max 0 (+ ref-column offset))
		;; else add mdc-statement-offset
		;; provided that point is behind ref-point
		;; and point is not within a begin-like statement
		(if (and (> (point) ref-point)
			 (not (and (mdc-forward-begin)
				   (> (point) save-point))))
		    (setq offset (+ offset mdc-statement-offset)))
		(setq last-open
			(car (cdr (parse-partial-sexp (point) save-point))))
		(if (not last-open)
		    (max 0 (+ ref-column offset))
		  (goto-char last-open)
		  (+ 1 (current-column)))))
	  (error 0)))))))

(defun mdc-empty-line ()
  "Return t if current line is empty, else return nil"
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (eolp)))

(defun mdc-within-comment (&optional move-point)
  "Return comment starter if point is within a comment, nil otherwise;
   optionally move point to the beginning of the comment"
  (let ((starter nil) (save-point (point)))
    ;; check single-line comment
    (setq starter (mdc-within-single-line-comment move-point))
    (if (not starter)
	;; check multi-line comment
	(condition-case nil
	    (if (and (re-search-backward "/\\*\\|\\*/")
		     (looking-at "/\\*"))
		;; check if we arrived in a single-line comment
		(if (progn (forward-char)
			   (mdc-within-single-line-comment move-point))
		    ;; then the original starting point is not a comment
		    ()
		  ;; else accept multi-line comment
		  (backward-char)
		  (setq starter "/*")))
	  (error nil)))
    (if (and starter move-point)
	(setq save-point (point)))
    (goto-char save-point)
    starter))

(defun mdc-within-single-line-comment (&optional move-point)
  "Return comment starter if point is within a single-line comment,
   nil otherwise; optionally move point to the beginning of the comment"
  (let ((starter nil) (save-point (point)))
    ;; check single-line comment
    (condition-case nil
	(if (and (re-search-backward "//\\|\n")
		 (looking-at "//"))
	    (setq starter "//"))
      (error nil))
    (if (and starter move-point)
	(setq save-point (point)))
    (goto-char save-point)
    starter))

(defun mdc-within-string (&optional move-point)
  "Return t if point is within a string constant, nil otherwise;
   optionally move point to the starting double quote of the string"
  (if (and (boundp 'font-lock-mode) font-lock-mode)
      ;; use result of font-lock-mode to also cover multi-line strings
      (let (within-string)
	(setq within-string
	      (text-property-any (point) (min (+ (point) 1) (point-max))
				 'face 'font-lock-string-face))
	(if (and within-string move-point)
	    (let ((point-next
		   (text-property-not-all (point) (point-max)
					  'face 'font-lock-string-face)))
	      (goto-char point-next)
	      (backward-sexp)))
	within-string)
    (let ((within-string nil) (save-point (point)) (start-point nil))
      (condition-case nil
	  (while
	      (progn
		(re-search-backward "\\(^\\|[^\\]\\)[\"\n]")
		(looking-at "\\(^\\|.\\)\""))
	    (if (not (looking-at "^"))
		(forward-char 1))
	    (if within-string
		(setq within-string nil)
	      (setq within-string t)
	      (or start-point (setq start-point (point)))))
	(error nil))
      (if (and within-string move-point start-point)
	  (goto-char start-point)
	(goto-char save-point))
      within-string)))

(defun mdc-behind-string (&optional move-point)
  "Check for string concatenation. Return t if only blanks are between point
   and the preceeding string constant, nil otherwise. Optionally move point
   to the starting double quote of the preceeding string."
  (let ((behind-string nil)
	(save-point (point)))
    (if (and (> (point) 1)
	     (progn
	       (skip-chars-backward " \t\n")
	       (if (> (point) 1)
		   (forward-char -1))
	       (looking-at "\""))
	     (mdc-within-string move-point))
	(setq behind-string t))
    (if (or (not move-point)
	    (not behind-string))
	(goto-char save-point))
    behind-string))

(defun mdc-within-matrix-expression (&optional move-point)
  "Return t if an opening bracket is found backwards from point,
   nil otherwise; optionally move point to the bracket"
  (let ((save-point (point)) (matrix-expression nil))
    (condition-case nil
	(let ()
	  (while (progn
		   (re-search-backward "[\]\[]")
		   (mdc-within-comment t)))
	  (if (looking-at "[\[]")
	      (progn
		(setq matrix-expression t)
		(if move-point
		    (setq save-point (point))))))
      (error nil))
    (goto-char save-point)
    matrix-expression))

(defun mdc-within-equation (&optional move-point)
  "return t if point is within right hand side of an equation, nil otherwise;
   optionally move point to the identifying '=' or ':='"
  (let ((equation nil) (save-point (point)))
    (condition-case nil
	(let ()
	  (while (progn
		   (re-search-backward
		    (concat "\\([^=]:?=[^=]\\)\\|;"))
		   (mdc-within-comment)))
	  (if (looking-at ";")
	      (setq equation nil)
	    (setq equation t)
	    (if (looking-at "[^:]=")
		(forward-char 1))
	    (if move-point
		(setq save-point (point)))))
      (error nil))
    (goto-char save-point)
    equation))

(defun mdc-statement-start (&optional ref-point)
  "Move point to the first character of the current statement;
   optional argument points to the last end or unended begin"
  (let ((save-point (point)))
    (if ref-point
	()
      (condition-case nil
	  (mdc-last-unended-begin t)
	(error (goto-char (point-min))))
      (setq ref-point (point))
      (goto-char save-point))
    (while (progn
	     (re-search-backward
	      ;; ("]" ")" ";"
	      ;;  "algorithm" "equation" "external"
	      ;;  "else" "elseif" "elsewhen"
	      ;;  "loop" "protected" "public" "then")
	      (concat
	       "[\]\);]\\|"
	       "\\<"
	       "\\(algorithm\\|e\\(lse\\(if\\|when\\)?\\|quation\\|"
	       "xternal\\)\\|loop\\|p\\(rotected\\|ublic\\)\\|then\\)"
	       "\\>")
	      ref-point 'no-error)
	     (and
	      (> (point) ref-point)
	      (or (mdc-within-comment t)
		  (mdc-within-string)
		  (if (looking-at "[\]\)]")
		      (progn
			(forward-char 1)
			(forward-sexp -1)
			t))
		  (if (looking-at ";")
		      (mdc-within-matrix-expression t)
		    (mdc-within-equation t))))))
    (cond
     ((= (point) ref-point)
      ;; we arrived at last unended begin,
      ;; but might be looking for first statement of block
      (mdc-forward-begin)
      (forward-comment (- (buffer-size)))
      (if (> (point) save-point)
	  (goto-char ref-point)))
     ((looking-at ";")
      (forward-char 1))
     (t
      (forward-word 1)))
    (forward-comment (buffer-size))))

(defun mdc-short-class-definition ()
  "return t if point is over a short class definition"
  (looking-at (concat
	       "\\(" mdc-class-modifier-keyword "\\)*"
	       mdc-class-keyword
	       "[A-Za-z_][0-9A-Za-z_]*[ \t\n\r]+=")))

(defun mdc-end-ident ()
  "return t if last word is an 'end'"
  (save-excursion
    (forward-word -1)
    (looking-at "end\\>")))

(defun mdc-last-unended-begin (&optional indentation-only)
  "Position point at last unended begin;
   raise an error if nothing found.
   If indentation-only is true, then position point at last begin or end."
  ;; find last unended begin-like keyword
  (let ((depth 1))
    (while (>= depth 1)
      (while (progn
	       (re-search-backward
		(concat
		 "\\<"
		 ; ("block" "class" "connector" "end"
		 ;  "for" "function" "if" "model" "package"
		 ;  "record" "type" "when" "while")
		 "\\(block\\|c\\(lass\\|onnector\\)\\|end\\|"
		 "f\\(or\\|unction\\)\\|if\\|model\\|package\\|"
		 "record\\|type\\|wh\\(en\\|ile\\)\\)"
		 "\\>"))
	       (or (mdc-within-comment t)
		   (mdc-short-class-definition)
		   (mdc-within-string)
		   (mdc-end-ident)
		   (and (looking-at "if") (mdc-within-equation t)))))
      (if (looking-at "end\\>")
	  (if indentation-only
	      (setq depth -1)
	    (setq depth (+ depth 1)))
	(setq depth (- depth 1))))
    ;; step backwards over class prefixes
    (if (>= depth 0)
	(let ((save-point (point)))
	  (while (progn
		   (forward-word -1)
		   (and
		    (looking-at mdc-class-modifier-keyword)
		    (not (mdc-within-comment))))
	    (setq save-point (point)))
	  (goto-char save-point)))))

(defun mdc-forward-begin ()
  "Move point forward over a begin-like statement.
   Return block ident (string) or nil if not found.
   Point is assumed over the start of the begin-like statement upon call."
  (let ((ident nil) (save-point (point)) start-point)
    (cond
     ((looking-at "\\(for\\|while\\)\\>")
      (setq ident (buffer-substring (match-beginning 0) (match-end 0)))
      (while (progn
	       (re-search-forward "\\<loop\\>")
	       (mdc-within-comment))))
     ;;(regexp-opt '("if" "elseif" "when" "elsewhen"))
     ((looking-at "\\(else\\(if\\|when\\)\\|if\\|when\\)\\>")
      (setq ident (buffer-substring (match-beginning 0) (match-end 0)))
      (while (progn
	       (re-search-forward "\\<then\\>")
	       (mdc-within-comment))))
     ((looking-at
       (concat "\\(" mdc-class-modifier-keyword "\\)\\|"
	       "\\(" mdc-class-keyword "\\)"))
      ;; move over class modifiers
      (while (looking-at mdc-class-modifier-keyword)
	(forward-word 1)
	(forward-comment (buffer-size)))
      ;; check and move over class specifier
      (if (not (looking-at mdc-class-keyword))
	  (goto-char save-point)
	(forward-word 1)
	(forward-comment (buffer-size))
	;; move over class name and look it up
	(setq start-point (point))
	(re-search-forward "\\>")
	(setq ident (buffer-substring start-point (point)))
	(forward-comment (buffer-size))
	;; check short class definition
	(if (looking-at "=")
	    (progn
	      (setq ident nil)
	      (goto-char save-point))
	  ;; else move over documentation strings
	  (while (looking-at "\"")
	    (forward-char 1)
	    (if (looking-at "\"")
		(forward-char 1)
	      (re-search-forward "[^\\]\""))
	    (forward-comment (buffer-size)))))))
    (forward-comment (buffer-size))
    ident))

(defun mdc-newline-and-indent ()
  "Indent current line before calling 'newline-and-indent'"
  (interactive)
  (mdc-indent-line)
  (newline-and-indent))

(defun mdc-insert-end ()
  "Insert end statement for current block."
  (interactive)
  (let ((case-fold-search nil)
	indentation (save-point (point)) (block-start nil) (end-ident ""))
    (save-excursion
      (condition-case nil
	  (mdc-last-unended-begin)
	(error (error "Couldn't find unended begin.")))
      (setq indentation (current-column))
      (setq end-ident (mdc-forward-begin))
      (if (<= save-point (point))
	  (setq block-start t)))
    ;; insert newline or clear up an empty line
    (if (not (mdc-empty-line))
	(insert "\n")
      (setq save-point (point))
      (beginning-of-line)
      (delete-region (point) save-point))
    ;; insert proper end
    (indent-to indentation)
    (insert (concat "end " end-ident ";"))
    ;; step back if block just starts
    (if (not block-start)
	()
      (forward-line -1)
      (end-of-line))
    ;; insert newline
    (insert "\n")
    (mdc-indent-line)))

;; active regions, and auto-newline/hungry delete key
;; (copied from cc-mode.el)
(defun mdc-keep-region-active ()
  ;; Do whatever is necessary to keep the region active in
  ;; XEmacs 19. ignore byte-compiler warnings you might see
  (and (boundp 'zmacs-region-stays)
       (setq zmacs-region-stays t)))

(defun mdc-forward-statement ()
  "Move point to next beginning of a statement"
  (interactive)
  (mdc-keep-region-active)
  (let ((case-fold-search nil)
	(save-point (point))
	pos)
    (mdc-statement-start)
    (if (> (point) save-point)
	;; ready after skipping leading comment or blanks
	()
      ;; else move forward
      (setq save-point (point))
      (if (mdc-forward-begin)
	  ;; ready after moving over begin-like statement
	  ()
	;; move forward behind next ";" ending a statement
	(while
	    (progn
	      (re-search-forward ";" (point-max) t)
	      (or (mdc-within-comment)
		  (mdc-within-string)
		  (mdc-within-matrix-expression))))
	(forward-comment (point-max))
	(if (= (point) (point-max))
	    (progn
	      (goto-char save-point)
	      (error "No next statement"))
	  ;; move backwards over possibly skipped statements
	  ;; without ending ";"
	  (setq pos (point))
	  (while (> (point) save-point)
	    (setq pos (point))
	    (forward-comment (- (point-max)))
	    (if (> (point) (point-min))
		(forward-char -1))
	    (mdc-statement-start))
	  (goto-char pos))))))

(defun mdc-backward-statement ()
  "Move point to previous beginning of a statement"
  (interactive)
  (mdc-keep-region-active)
  (let ((case-fold-search nil) (save-point (point)))
    (mdc-statement-start)
    (if (< (point) save-point)
	;; ready after having moved to start of current statement
	()
      ;; else move backward
      (setq save-point (point))
      (forward-comment (- (point-max)))
      (if (> (point) (point-min))
	  (forward-char -1))
      (mdc-statement-start)
      (if (= (point) save-point)
	  (error "No previous statement")))))

(defun mdc-forward-block ()
  "Move point to next beginning of a block at the same nesting level
   or a level higher if no next block found on the same level."
  (interactive)
  (let ((save-point (point)))
    (condition-case nil
	(progn
	  (mdc-to-block-begin)
	  (if (> (point) save-point)
	      ;; we moved already forward to a block begin
	      ()
	    (mdc-to-block-end)
	    (mdc-forward-statement)
	    (mdc-to-block-begin)
	    (if (< (point) save-point)
		(mdc-forward-block))))
      ;; in case of error and if we did move yet,
      ;; move forward one statement
      (error (if (= (point) save-point)
		 (mdc-forward-statement))))))

(defun mdc-backward-block ()
  "Move point to previous beginning of a block at the same nesting level
   or a level higher if no previous block found on the same level."
  (interactive)
  (let ((save-point (point)))
    (condition-case nil
	(progn
	  (mdc-to-block-begin)
	  (if (< (point) save-point)
	      ;; we moved already backward to the beginning of a block
	      ()
	    (mdc-backward-statement)
	    (mdc-to-block-begin)))
      ;; in case of error and if we did not move yet,
      ;; move backward one statement
      ;; and move to beginning of that block if one ends there
      (error (progn
	       (if (= (point) save-point)
		   (progn
		     (mdc-backward-statement)
		     (if (looking-at "\\<end\\>")
			 (mdc-to-block-begin)))))))))

(defun mdc-to-block-begin ()
  "Move point to beginning of current statement block"
  (interactive)
  (mdc-keep-region-active)
  (let ((case-fold-search nil)
	(save-point (point)))
    (condition-case nil
	(progn
	  (mdc-statement-start)
	  (mdc-forward-begin)
	  (mdc-last-unended-begin))
      (error (progn
	       (goto-char save-point)
	       (error "No statement block"))))))

(defun mdc-to-block-end ()
  "Move point to end of current statement block"
  (interactive)
  (mdc-keep-region-active)
  (let ((case-fold-search nil)
	ident (save-point (point)))
    (condition-case nil
	(progn
	  (mdc-statement-start)
	  (mdc-forward-begin)
	  (mdc-last-unended-begin)
	  (setq ident (mdc-forward-begin))
	  (while (progn
		   (re-search-forward
		    (concat "\\<end[ \t\n]+" ident "\\>"))
		   (or
		    (mdc-within-comment)
		    (mdc-within-string)))))
      (error (progn
	       (goto-char save-point)
	       (error (if ident
			  (format "Missing \"end %s\"" ident)
			"No statement block to end")))))))

;; snarfed from outline.el (outline-flag-region)
;; Comments about GNU Emacs 20.7 and XEmacs 21.1:
;; GNU Emacs:
;;  - ellipse ... is not displayed as we stop hiding before end of line
;;    (advantages of only hiding embraced annotation text are
;;     correct visible syntax and visible end of hidden text)
;; XEmacs:
;;  - isearch-open-invisible property does not work, i.e.
;;    hidden text is not shown if isearch finds it
;; General:
;;  - intangible property, to skip hidden text when moving by chars,
;;    does not work with XEmacs and does not fully work with GNU Emacs
;;    (e.g. C-a/C-e stop in hidden annotation, search/replace finds
;;     at most one occurence, reopen of externally modified files is strange)
;;  - read-only property, to avoid modifications of hidden text,
;;    does not work with GNU Emacs
;;  --> we don't set intangible or read-only property
(defun mdc-flag-region (from to flag)
  "Hides or shows lines from FROM to TO, according to FLAG.
If FLAG is nil then text is shown, while if FLAG is t the text is hidden."
  (save-excursion
    (goto-char from)
    (mdc-discard-overlays from to 'mdc-annotation)
    (if flag
	(let ((o (make-overlay from to)))
	  (overlay-put o 'invisible 'mdc-annotation)
	  (overlay-put o 'isearch-open-invisible
		       'mdc-isearch-open-invisible)))))

;; snarfed from outline.el (outline-isearch-open-invisible);
;; Function to be set as an outline-isearch-open-invisible' property
;; to the overlay that makes the outline invisible
;; (see `mdc-flag-region').
(defun mdc-isearch-open-invisible (overlay) ())

;; snarfed from outline.el (outline-discard-overlays)
(defun mdc-discard-overlays (beg end value)
  (if (< end beg)
      (setq beg (prog1 end (setq end beg))))
  (save-excursion
    (let ((overlays (overlays-in beg end))
	  o)
      (while overlays
	(setq o (car overlays))
 	(if (eq (overlay-get o 'invisible) value)
	    (delete-overlay o))
	(setq overlays (cdr overlays))))))

;; define overlay functions for XEmacs 21.1
(if (not (functionp 'overlays-in))
    (defun overlays-in (beg end)
      (extent-list (current-buffer) beg end)))

(if (not (functionp 'make-overlay))
    (defalias 'make-overlay 'make-extent))

(if (not (functionp 'delete-overlay))
    (defalias 'delete-overlay 'delete-extent))

(if (not (functionp 'overlay-put))
    (defalias 'overlay-put 'set-extent-property))

(if (not (functionp 'overlay-get))
    (defalias 'overlay-get 'extent-property))

;; test for overlay
(if (not (functionp 'overlays-at))
    ;; XEmacs 21.1
    (defun mdc-within-overlay (prop)
      "Return overlay value if point is contained in an overlay
       with property prop, nil otherwise."
      (extent-at (point) (current-buffer) prop))
  (defun mdc-within-overlay (prop)
    "Return overlay value if point is contained in an overlay
     with property prop, nil otherwise."
    (let ((overlays (overlays-at (point)))
	  (value nil)
	  o)
      (while (and overlays (not value))
	(setq o (car overlays))
	(setq value (overlay-get o prop))
	(setq overlays (cdr overlays)))
      value)))

(defun mdc-hide-annotations (beg end)
  "Hide all annotations."
  (save-excursion
    (let (beg-hide end-hide)
      (goto-char beg)
      (while
	  (and (< (point) end)
	       (search-forward-regexp "\\<annotation[ \t\n]*\(" end t))
	(setq beg-hide (match-end 0))
	(backward-char)
	(forward-sexp)
	(setq end-hide (- (point) 1))
	(mdc-flag-region beg-hide end-hide t)))))

(defun mdc-show-annotations (beg end)
  "Show annotations from beg to end"
  (mdc-flag-region beg end nil))

(defun mdc-hide-all-annotations ()
  "Hide all annotations"
  (interactive)
  (mdc-hide-annotations (point-min) (point-max)))

(defun mdc-hide-annotation ()
  "Hide annotation of current statement"
  (interactive)
  (save-excursion
    (let (beg end)
      ;; move to beginning of current statement
      (mdc-statement-start)
      (setq beg (point))
      ;; move to beginning of next statement
      (mdc-forward-statement)
      (setq end (point))
      ;; hide annotations from beg to end
      (mdc-hide-annotations beg end))))

(defun mdc-show-all-annotations ()
  "Show all annotations"
  (interactive)
  (mdc-show-annotations (point-min) (point-max)))

(defun mdc-show-annotation ()
  "Show annotation of current statement"
  (interactive)
  (save-excursion
    (let (beg end)
      ;; move to beginning of current statement
      (mdc-statement-start)
      (setq beg (point))
      ;; move to beginning of next statement
      (mdc-forward-statement)
      (setq end (point))
      ;; show annotations from beg to end
      (mdc-show-annotations beg end))))

(provide 'modelica-mode)

;;; modelica-mode.el ends here
