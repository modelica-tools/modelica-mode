;;!emacs
;;
;; FILE:         br-mdc.el
;;               (derived from Bob Weiner's br-java.el)
;; SUMMARY:      Support routines for Modelica inheritance browsing.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     continuous systems modeling, oop, tools
;;
;; AUTHOR:       Ruediger Franke
;; ORG:          Modelica Design Group
;;
;; ORIG-DATE:    20-Nov-97
;; LAST-MOD:     25-Apr-01
;;
;; Copyright (C) 1997--2001  Free Software Foundation, Inc.
;; See the file BR-COPY for license information.
;;
;; This file is intended for use with OO-Browser.
;;

(defconst br-mdc-version "1.2.1")

;; Synched up with OO-Browser 4.07

;; DESCRIPTION:  
;;   see file mdc-browse.el
;;
;;   Note:
;;   1.) This version does not look into classes referenced in extends 
;;   or import clauses if the referenced classes are definid in 
;;   different files.
;;   That is why the completion of names does not fully work!
;;   The problem can be circumvented by 
;;      a) not using import name.* notation
;;      b) explicitly stating outer classes in extends clauses, e.g.
;;         package A
;;           class B
;;           end B;
;;         end A;
;;         class C
;;           extends A;   // o.k.
;;           extends B;   // B is not found inside A in a different file!
;;           extends A.B; // o.k.
;;         end C;
;;   2.) This version may give wrong results if a class of the same
;;   relative name is defined multiple times in the same file.
;;
;; DESCRIP-END.

;;; HISTORY:     see ChangeLog

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(mapcar 'require '(br-lib hasht modelica-mode))

;;; ************************************************************************
;;; User visible variables
;;; ************************************************************************

(defvar mdc-default-classes
  '("Boolean" "Integer" "Real" "String")
  "*List of default Modelica class names handled by OO-Browser.")

(defvar mdc-lib-search-dirs nil
  "List of directories below which Modelica library source files are found.
Subdirectories of Library source are also searched.  A Library is a stable
group of classes.")

(defvar mdc-sys-search-dirs nil
  "List of directories below which Modelica system source files are found.
Subdirectories of System source are also searched.  A System class is one
that is not yet reusable and is likely to change before release.")

(defconst mdc-narrow-view-to-class nil
 "*Non-nil means narrow buffer to just the matching class definition when displayed.")

;;; ************************************************************************
;;; Internal functions, required by OO Browser
;;; ************************************************************************

(defun mdc-get-classes-from-source (filename &optional skip-tags
					     skip-tags-cleanup)
  "Scans FILENAME and returns cons of class list with parents-class alist.
Handles multiple inheritance.  Assumes file existence and readability have
already been checked.
   With optional SKIP-TAGS non-nil, does not compute and store lookup tags
for member definitions.  If SKIP-TAGS is nil, normally a cleanup
function is called after scanning the members.  SKIP-TAGS-CLEANUP
non-nil suppresses this action."
  ;; initialize hash table of class names
  (mdc-create-class-names-htable)
  ;; load file into a buffer and call mdc-obtain-class-info
  (let ((classes nil) (parents nil)
	(package (mdc-obtain-package-info filename)))
    ;; get classes from file
    (mdc-get-file-buffer filename)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(mdc-obtain-class-info (point-max) t package nil)))
    (mdc-release-file-buffer)
    ;; delete hash table of class names to free stored data
    (mdc-delete-class-names-htable)
    ;; return classes and parents
    (cons classes (delq nil parents))))

(defun mdc-class-definition-regexp (class &optional regexp-flag)
  "Return regexp to uniquely match the definition of CLASS name.
Optional REGEXP-FLAG non-nil means CLASS has already been quoted for use in a
regular expression."
  (setq class (mdc-narrow-name class))
  (concat mdc-class-name-before
	  (if regexp-flag
	      class
	    (regexp-quote class))
	  mdc-identifier-after))

(defun mdc-get-parents-from-source (filename class-name)
  "Scan source in FILENAME and return list of parents of CLASS-NAME.
Assume file existence has already been checked."
    (cond ((null class-name) nil)
	  ((equal filename br-null-path) nil)
	  (t (car (car (br-rassoc
			class-name
			(cdr (mdc-get-classes-from-source filename t))))))))

(defun mdc-select-path (paths-htable-elt &optional feature-p)
  "Select proper pathname from PATHS-HTABLE-ELT based upon value of optional FEATURE-P.
Selection is between path of class definition and path for features associated
with the class."
  (let ((elt (cdr paths-htable-elt)))
    (if (consp elt) 
	(if feature-p (cdr elt) (car elt))
      ;; Both paths are the same.
      elt)))

(defun mdc-set-case (type)
  "Return string TYPE identifier for use as a class name."
  type)

(defun mdc-set-case-type (class-name)
  "Return string CLASS-NAME for use as a type identifier."
  class-name)

(defun mdc-to-class-end ()
  "Assuming point is at start of class, move to start of line after end of class."
  (interactive)
  (let (class)
    (looking-at mdc-class-def-regexp)
    (setq class (buffer-substring
		 (match-beginning mdc-class-def-name-grpn)
		 (match-end mdc-class-def-name-grpn)))
    (goto-char (match-end 0))
    (goto-char (mdc-class-end-pos class))
    (forward-line 1)))

(defun mdc-to-comments-begin ()
  "Generally this function should skip back from current point past 
   any preceding blank lines and comments. This is not done for Modelica
   as the documentation string starts behind the class name."
;  (forward-comment (- (buffer-size)))
;  (skip-chars-forward " \t\n\r")
  (beginning-of-line))

(defun mdc-store-class-info (class)
  "Lookup Modelica doc string for class or method/function"
  (setq mdc-docstring (mdc-lookup-docstring class)))

(defun mdc-insert-class-info ()
  "Use the info facility to display Modelica doc strings"
  (interactive)
  (insert mdc-docstring))

(defun mdc-to-definition (&optional other-win)
  "If point is over a class name, move to its definition.
   With OTHER-WIN non-nil, show it in another window."
  (interactive)
  (let (end class)
    ;; store current name in class
    (save-excursion
      (re-search-forward (concat "[^" mdc-identifier-chars "]"))
      (goto-char (match-beginning 0))
      (setq end (point))
      (re-search-backward (concat "[^" mdc-identifier-chars "]"))
      (goto-char (match-end 0))
      (setq class (buffer-substring (point) end)))
    ;; obtain normalized class name
    (mdc-create-class-names-htable)
    (mdc-import-classes-from-source (buffer-file-name))
    (setq class (mdc-normalize-class-name class))
    (mdc-delete-class-names-htable)
    ;; check for class
    (cond
     ((br-check-for-class class other-win))
     (t	(beep)
	(message
	 (concat "(OO-Browser): Can't find class definition for \"" class "\"."))
	nil))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun mdc-obtain-package-info (filename)
  "Obtain and return package name recursively defined in package.mo files.
   Furthermore the names of *.mo files are stored."
  (let (dirname updirname package packagefilename)
    (setq dirname (file-name-directory filename))
    (setq updirname (file-name-directory (directory-file-name dirname)))
    (setq packagefilename
	  (car (directory-files dirname t "^package.mo$")))
    (if packagefilename
	(progn
	  ;; first obtain package info from parent directory
	  (setq package
		(mdc-obtain-package-info (directory-file-name dirname)))
	  ;; append local package name
	  (if (equal (file-relative-name filename dirname) "package.mo")
	      ;; nothing to do if filename is package.mo
	      ()
	    ;; obtain class info from file package.mo in this directory
	    (mdc-get-file-buffer packagefilename)
	    (save-excursion
	      (save-restriction
		(widen)
		(goto-char (point-min))
		(mdc-obtain-class-info (point-max) nil package nil)))
	    (mdc-release-file-buffer)
	    ;; obtain package name from directory name
	    (setq package
		  (concat package (if package ".")
			  (file-relative-name
			   (directory-file-name dirname) updirname)))
	    ;; store names of subdirectories and *.mo files 
	    ;; (i.e. additional class names defined in this package)
	    (let (class file (file-list (directory-files dirname t)))
	      (while file-list
		(setq file (car file-list))
		(setq file-list (cdr file-list))
		(if (equal file filename)
		    ;; don't store name given as calling argument
		    ()
		  ;; obtain class name
		  ;; first check for .mo extension
		  (if (equal (file-name-extension file) "mo")
		      (setq class 
			    (file-name-sans-extension
			     (file-relative-name file dirname)))
		    ;; furthermore check for directory containing package.mo
		    (if (and (file-directory-p file)
			     (directory-files file t "^package.mo$"))
			(setq class
			      (file-relative-name
			       (directory-file-name file) dirname))
		      (setq class nil)))
		  ;; store class name
		  (if (or (not class)
			  (equal class ".")
			  (equal class "..")
			  (equal class "package"))
		      ;; don't store
		      ()
		    (mdc-store-class-name
		     (concat package (if package ".") class)))))))))
    ;; return obtained package name
    package))

(defun mdc-obtain-class-info (obtain-end-pos &optional obtain-parents
					     class defined-in-file)
  "Scan current buffer up to OBTAIN-END-POS for class information.
Calls itself recursively to obtain information for a local CLASS.
The point is assumed to be after a match of mdc-class-def-regexp.
Class information is stored in dynamically bounded variable 
'mdc-class-names-htable', as well as in 'classes' and 'parents'
if OBTAIN-PARENTS, DEFINED-IN-FILE, and CLASS are non nil."
  (let (parent (parent-list nil) parent-cons 
	save-point local-class (class-represent class))
    ;; store class name so that it will be known for local classes
    (if (and class defined-in-file)
	(progn
	  (if (member class mdc-default-classes)
	      (setq class-represent (concat "\[" class "\]")))
	  (mdc-store-class-name class-represent)))
    ;; skip optional docstring
    ;; (this is required as mdc-within-string is limited to single lines)
    (forward-comment (buffer-size))
    (while (looking-at "\"")
      (forward-char 1)
      (if (looking-at "\"")
	  (forward-char 1)
	(re-search-forward "\""))
      (forward-comment (buffer-size)))
    ;; treat short class definition
    (if (looking-at mdc-short-definition-regexp)
	(progn
	  (setq parent (buffer-substring
			(match-beginning mdc-short-definition-grpn)
			(match-end mdc-short-definition-grpn)))
	  ;; skip possible class_modification for parent
	  (goto-char (match-end 0))
	  (forward-comment (buffer-size))
	  (if (looking-at "(")
	      (progn (forward-sexp)
		     (setq save-point (point))))
	  ;; store parent for normalization of class names
	  ;; (append a "." to class to avoid replacement for e.g.
	  ;; type MyReal = Real; MyReal r, but replace e.g.
	  ;; package SIunits = Modelica.SIunits; SIunits.Temp_K T,
	  ;; see mdc-normalize-class-name)
	  (setq parent (mdc-normalize-class-name parent))
	  (mdc-store-class-name-as parent (concat (mdc-narrow-name class) "."))
	  ;; store parent for inheritance browsing
	  (if (and class defined-in-file obtain-parents)
	      (setq parent-list (cons parent parent-list))))
      ;; else obtain parent-list from extends_clauses
      (while (re-search-forward 
	      (concat mdc-identifier-before
		      "\\(annotation\\|block\\|c\\(lass\\|onnector\\)\\|"
		      "extends\\|function\\|import\\|model\\|package\\|"
		      "record\\|type\\)"
		      mdc-identifier-after)
	      obtain-end-pos t)
	(setq save-point (point))
	(goto-char (match-beginning 0))
	(cond
	 ;; treat comments and strings
	 ((or (mdc-within-comment)
	      (mdc-within-string))
	  ())
	 ;; skip annotations
	 ((looking-at "annotation")
	  (goto-char (match-end 0))
	  (forward-comment (buffer-size))
	  (forward-sexp)
	  (setq save-point (point)))
	 ;; import other model files
	 ;; (is not valid anymore in Modelica 1.4)
	 ((looking-at mdc-import-file-regexp)
	  (mdc-import-classes-from-source
	   (buffer-substring
	    (match-beginning mdc-import-file-grpn)
	    (match-end mdc-import-file-grpn))))
	 ;; import class name
	 ((looking-at mdc-import-regexp)
	  (let ((name (buffer-substring
		       (match-beginning mdc-import-name-grpn)
		       (match-end mdc-import-name-grpn))))
	    (goto-char (match-end 0))
	    (forward-comment (buffer-size))
	    (if (looking-at mdc-short-definition-regexp)
		;; statement has form "import A = B.C.D"
		(let ((new-name name))
		  (setq name (buffer-substring
			      (match-beginning mdc-short-definition-grpn)
			      (match-end mdc-short-definition-grpn)))
		  (mdc-store-class-name-as name new-name))
	      ;; statement has form "import B.C.D"
	      (mdc-store-class-name name))))
	 ;; obtain parent information
	 ((looking-at mdc-parent-regexp)
	  (setq parent (buffer-substring
			(match-beginning mdc-parent-name-grpn)
			(match-end mdc-parent-name-grpn)))
	  ;; skip possible class_modification for parent
	  (goto-char (match-end 0))
	  (forward-comment (buffer-size))
	  (if (looking-at "(")
	      (progn (forward-sexp)
		     (setq save-point (point))))
	  ;; store parent for normalization of class names
	  (setq parent (mdc-normalize-class-name parent))
	  (mdc-store-class-name parent)
	  ;; store parent for inheritance browsing
	  (if (and class defined-in-file obtain-parents)
	      (setq parent-list (cons parent parent-list))))
	 ;; call mdc-obtain-class-info recursively for local classes
	 ((looking-at mdc-class-def-regexp)
	  (setq local-class (buffer-substring
			     (match-beginning mdc-class-def-name-grpn)
			     (match-end mdc-class-def-name-grpn)))
	  (goto-char (match-end 0))
	  ;; backup current class names htable so that locally defined
	  ;; classes do not overwrite global ones for further analysis
	  ;; (not yet used as extends and import are not fully supported)
;	  (mdc-backup-class-names)
	  (mdc-obtain-class-info 
	   (mdc-class-end-pos local-class)
	   obtain-parents
	   (concat class (if class ".") local-class) t)
	  (setq save-point (point))
;	  (mdc-restore-class-names)
	  ))
	(goto-char save-point)))
    ;; store class and parent information for browsing
    (if (and class defined-in-file obtain-parents)
	(setq parent-cons (cons parent-list class-represent)
	      classes (cons class-represent classes)
	      parents (cons parent-cons parents)))))

(defun mdc-create-class-names-htable ()
  (setq mdc-class-names-htable (hash-make 7))
  (mapcar
   (function
    (lambda (class)
      (hash-add (concat "\[" class "\]") (mdc-narrow-name class)
		mdc-class-names-htable)))
   mdc-default-classes))

(defun mdc-delete-class-names-htable ()
  (setq mdc-class-names-htable nil))

(defun mdc-backup-class-names ()
  (setq mdc-class-names-htable-backup (hash-copy mdc-class-names-htable)))

(defun mdc-restore-class-names ()
  (setq mdc-class-names-htable mdc-class-names-htable-backup))

(defun mdc-lookup-docstring (class)
  "Looks up the doc string for CLASS."
  (let (filename line docstring pos
	(class-regexp (mdc-class-definition-regexp class)))
    (setq filename (br-class-path class))
    (if (not filename)
	(error (format "(mdc-lookup-docstring): Entry \"%s\" may be referenced but it is not defined in the Environment." class)))
    ;; obtain class documentation string
    (mdc-get-file-buffer filename)
    (condition-case nil
	(save-excursion
	  (save-restriction
	    (widen)
	    (goto-char (point-min))
	    (while (progn
		     (re-search-forward class-regexp)
		     (setq docstring 
			   (buffer-substring
			    (match-beginning 0)
			    (match-end 0)))
		     (or (mdc-within-comment)
			 (mdc-within-string))))
	    (setq docstring 
		  (concat docstring
			  "\n(defined in " filename ")\n\n"))
	    ;; append doc string
	    (forward-comment (buffer-size))
	    (if (looking-at "\"\\([^\"]*[^\\]\\)\"")
		(setq docstring
		      (concat docstring
			      (buffer-substring
			       (match-beginning 1)
			       (match-end 1)))))))
      (error nil))
    (mdc-release-file-buffer)
    (if docstring
	docstring
      (concat class " definition not found in Environment."))))

(defun mdc-class-end-pos (name)
  "Return position after class end;
Assumes point to be after a match of mdc-class-def-regexp."
  (setq name (mdc-narrow-name name))
  (save-excursion
    (if (looking-at mdc-short-definition-regexp)
	(while (progn
		 (re-search-forward ";")
		 (or (mdc-within-comment)
		     (mdc-within-string))))
      (condition-case nil
	  (while
	      (progn
		(re-search-forward (concat "end[ \t\n\r]+" name "[ \t\n\r]*;"))
		(or (mdc-within-comment)
		    (mdc-within-string))))
	(error (error (concat "Unended class \"" name "\"")))))
    (point)))

(defun mdc-narrow-name (full-name)
  "Strip package names from FULL-NAME in dot notation"
  (let (index (len (length full-name)))
    (cond
     ((and (>= len 1)
	   (equal (substring full-name 0 1) "\["))
      (substring full-name 1 (- len 1)))
     (t
      (setq index (string-match "[^.]+$" full-name))
      (substring full-name index)))))

(defun mdc-outer-names (full-name)
  "Strip innermost class name from FULL-NAME in dot notation"
  (let (index (len (length full-name)))
    (cond
     ((and (>= len 1)
	   (equal (substring full-name 0 1) "\["))
      (substring full-name 1 (- len 1)))
     (t
      (setq index (string-match "\\.[^.]+$" full-name))
      (if index
	  (substring full-name 0 index)
	())))))

(defun mdc-inner-names (full-name)
  "Strip outermost class name from FULL-NAME in dot notation"
  (let (index (len (length full-name)))
    (cond
     ((and (>= len 1)
	   (equal (substring full-name 0 1) "\["))
      (substring full-name 1 (- len 1)))
     (t
      (string-match "^[^.]+\\." full-name)
      (setq index (match-end 0))
      (if index
	  (substring full-name index)
	())))))

(defun mdc-narrow-outer-name (full-name)
  "Strip sub-package names and class name from FULL-NAME in dot notation"
  (let (index (len (length full-name)))
    (cond
     ((and (>= len 1)
	   (equal (substring full-name 0 1) "\["))
      (substring full-name 1 (- len 1)))
     (t
      (string-match "^[^.]+" full-name)
      (setq index (match-end 0))
      (substring full-name 0 index)))))

(defun mdc-store-class-name (class)
  "Store class in class names htable."
  (mdc-store-class-name-as class (mdc-narrow-name class)))
  
(defun mdc-store-class-name-as (class name)
  "Store class under name in class names htable."
  (hash-add class name mdc-class-names-htable))
  
(defun mdc-normalize-class-name (name)
  "Normalize class name by prepending package names that define it."
  (let ((narrow-name (mdc-narrow-name name)) (full-name nil))
    (if (equal name narrow-name)
	;; a narrow class name may find directly in htable
	(setq full-name
	      (hash-lookup name mdc-class-names-htable))
      ;; otherwise htable may contain outer names to prepend
      (let ((narrow-outer-name (mdc-narrow-outer-name name))
	    outer-names)
	(setq outer-names
	      (hash-lookup narrow-outer-name mdc-class-names-htable))
	(if outer-names
	    (setq full-name
		  (concat outer-names "." (mdc-inner-names name))))
	;; additionally check narrow-outer-name with appended "." 
	;; as stored for short class definitions (see mdc-obtain-class-info)
	(setq outer-names (hash-lookup (concat narrow-outer-name ".")
				       mdc-class-names-htable))
	(if outer-names
	    (setq full-name
		  (concat outer-names "." (mdc-inner-names name))))))
    ;; return either found full name or call argument
    (or full-name name)))

(defun mdc-import-classes-from-source (file)
  "Scan FILE and store defined class names in mdc-class-names-htable.
   FILE is searched for in br-sys-search-dirs and br-lib-search-dirs."
  ;; load file into a buffer and call mdc-obtain-class-info
  (let (filename (buffer-bak (current-buffer)))
    (setq filename
	  (if (file-name-absolute-p file)
	      file
	    (mdc-complete-filename
	     file
	     (append br-sys-search-dirs br-lib-search-dirs))))
    (if (not filename)
	(error (concat "(mdc-import): file not found, " file)))
    (let ((package (mdc-obtain-package-info filename)))
      (mdc-get-file-buffer filename)
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  (mdc-obtain-class-info (point-max) nil package nil)))
      (mdc-release-file-buffer))
    (set-buffer buffer-bak)))

(defun mdc-complete-filename (file search-dirs)
  "Search FILE in SEARCH-DIRS and return first occurence with full pathname;
   return nil if file not found." 
  (let ((filename nil) hits)
    (setq hits
	  (mapcar 
	   (function
	    (lambda (dir)
	      (if (or (null dir) (equal dir "")
		      (progn (setq dir (file-name-as-directory dir))
			     (br-skip-dir-p dir)))
		  nil
		(if (and (file-directory-p dir)
			 (file-readable-p dir))
		    (directory-files dir t (concat "^" file "$"))))))
	   search-dirs))
    (while (and hits (not filename))
      (setq
       filename (caar hits)
       hits (cdr hits)))
    filename))

;;; ************************************************************************
;;; Private section for getting files into buffers
;;; ************************************************************************

(defconst mdc-tmp-buffer-name "*mdc-tmp<%d>*"
  "Name of temporary buffer used for parsing source files.")

(defun mdc-get-file-buffer (filename)
  "Return FILENAMEs buffer, create a new temporary buffer if needed.
   Make returned buffer current."
  (let (buffer
	(mdc-view-file-function 'mdc-insert-file-contents))
    (setq buffer (get-file-buffer filename))
    (if buffer
	;; make existing buffer current
	(set-buffer buffer)
      ;; create temporary buffer
      (setq buffer (funcall mdc-view-file-function filename)))
    buffer))

(defun mdc-release-file-buffer (&optional buffer)
  "Kill BUFFER (default: current buffer) if its name is a 
   mdc-tmp-buffer-name."
  (let ((buffer-name (buffer-name buffer))
	(end-pos (string-match "<" mdc-tmp-buffer-name)))
    (if (and
	 (> (length buffer-name) end-pos)
	 (equal (substring buffer-name 0 end-pos)
		(substring mdc-tmp-buffer-name 0 end-pos)))
	(progn (set-buffer-modified-p nil)
	       (kill-buffer buffer)))))

(defun mdc-insert-file-contents (filename)
  "Insert FILENAME contents into a temporary buffer and select buffer.
Does not run any find-file or mode specific hooks.  Marks buffer read-only to
prevent any accidental editing.

Set `mdc-view-file-function' to this function when parsing OO-Browser source
files for fast loading of many files."
  (let ((number 1) buf)
    ;; find an unused temporary buffer
    (while (get-buffer (format mdc-tmp-buffer-name number))
      (setq number (1+ number)))
    ;; create a new temporary buffer
    (setq buf (get-buffer-create (format mdc-tmp-buffer-name number)))
    (switch-to-buffer buf)
    ;; Don't bother saving anything for this temporary buffer
    (buffer-disable-undo buf)
    (setq buffer-auto-save-file-name nil
	  buffer-read-only nil)
    (erase-buffer)
    (insert-file-contents filename t)
    (br-scan-mode)
    (setq buffer-read-only t)
    buf))

;;; ************************************************************************
;;; Internal variables
;;; ************************************************************************

(defvar mdc-docstring ""
  "Documentation string for Modelica.")

(defconst mdc-identifier-chars "_.a-zA-Z0-9"
  "String of chars and char ranges that may be used within a Modelica identifier, including '.' for concatenation of names.")

(defconst mdc-identifier-before
  "\\(\\<\\)"
  "Chars before a Modelica identifier.")

(defconst mdc-identifier-after
  "\\(\\>\\)"
  "Chars after a Modelica identifier.")

(defconst mdc-identifier (concat "\\([_a-zA-Z][" mdc-identifier-chars "]*\\)")
  "Regular expression matching a Modelica identifier.")

(defconst mdc-class-name-before
  (concat mdc-identifier-before 
	  "\\(" mdc-class-modifier-keyword "\\)*" mdc-class-keyword)
  "Regexp preceding the class name in a class definition.")

(defconst mdc-class-def-regexp
  (concat mdc-class-name-before mdc-identifier)
  "Regular expression used to match to class definitions in source text.
Class name identifier is grouping 'mdc-class-def-name-grpn'.")

(defconst mdc-class-def-name-grpn 7)

(defconst mdc-lang-prefix "mdc-"
 "Prefix string that starts \"br-mdc.el\" symbol names.")

(defconst mdc-parent-regexp
  (concat mdc-identifier-before "extends[ \t\n\r]+"
	  mdc-identifier)
  "Parent identifier is group 'mdc-parent-name-grpn'.")

(defconst mdc-parent-name-grpn 2)

(defconst mdc-short-definition-regexp
  (concat "\\([ \t\n\r]*=[ \t\n\r]*\\)"
	  mdc-identifier)
  "Short definition identifier is group 'mdc-short-definition-grpn'.")

(defconst mdc-short-definition-grpn 2)

(defconst mdc-import-regexp
  (concat mdc-identifier-before "import[ \t\n\r]+"
	  mdc-identifier)
  "Imported class identifier is 'mdc-import-name-grpn'.")

(defconst mdc-import-name-grpn 2)

(defconst mdc-import-file-regexp
  (concat mdc-identifier-before "import[ \t\n\r]+\"\\([^\"]+\\)\"")
  "Import file name is group 'mdc-import-file-grpn'.")

(defconst mdc-import-file-grpn 2)

(defvar mdc-class-names-htable nil
  "Hash table of full class names using narrow class names as key.")

(defvar mdc-class-names-htable-backup nil
  "Backup for hash table of full class names using narrow class names as key.")

(defvar mdc-class-names-files nil
  "Set of file names whose contents is known in mdc-class-names-htable")

(defconst mdc-src-file-regexp "[^.]\\.\\(mo\\)$"
  "Regular expression matching a unique part of Modelica source file name and no others.")

(defvar mdc-children-htable nil
  "Htable whose elements are of the form: (LIST-OF-CHILD-CLASSES . CLASS-NAME).
Used to traverse Modelica inheritance graph.  'br-build-children-htable' builds
this list.")
(defvar mdc-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Used to traverse Modelica inheritance graph.  'br-build-parents-htable' builds
this list.")
(defvar mdc-paths-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-NAMES . FILE-PATH).
FILE-PATH gives the location of classes found in LIST-OF-CLASS-NAMES.
'br-build-paths-htable' builds this list.")


(defvar mdc-lib-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Only classes from stable software libraries are used to build the list.")
(defvar mdc-lib-paths-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-NAMES . FILE-PATH).
FILE-PATH gives the location of classes found in LIST-OF-CLASS-NAMES.
Only classes from stable software libraries are used to build the list.")

(defvar mdc-sys-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Only classes from systems that are likely to change are used to build the
list.")
(defvar mdc-sys-paths-htable nil
  "Alist whose elements are of the form: (LIST-OF-CLASS-NAMES . FILE-PATH).
FILE-PATH gives the location of classes found in LIST-OF-CLASS-NAMES.
Only classes from systems that are likely to change are used to build the
list.")

(defvar mdc-lib-prev-search-dirs nil
  "Used to check if 'mdc-lib-classes-htable' must be regenerated.")
(defvar mdc-sys-prev-search-dirs nil
  "Used to check if 'mdc-sys-classes-htable' must be regenerated.")

(defvar mdc-env-spec nil
  "Non-nil value means Environment specification has been given but not yet built.
Nil means current Environment has been built, though it may still require
updating.")

(provide 'br-mdc)
