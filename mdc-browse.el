;;!emacs
;;
;; FILE:         mdc-browse.el
;;               (derived from Bob Weiner's java-brows.el)
;; SUMMARY:      Modelica source code browser.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     continuous systems modeling, oop, tools
;;
;; AUTHOR:       Ruediger Franke
;; ORG:          Modelica Design Group
;;
;; ORIG-DATE:    20-Nov-97
;; LAST-MOD:     03-Apr-01
;;
;; Copyright (C) 1997--2001  Free Software Foundation, Inc.
;; See the file BR-COPY for license information.
;;
;; This file is intended for use with OO-Browser.

(defconst mdc-browse-version "1.2.0")

;; Synched up with OO-Browser 4.07

;;
;; DESCRIPTION:  
;;
;;    Support for inheritance browsing for Modelica.
;;
;; DESCRIP-END.

;; Installation:
;; (1) Put the files
;;        mdc-browse.el
;;        br-mdc.el
;;     to an Emacs lisp directory, e.g. ~/elisp
;;
;; (2) Add the following lines to your ~/.emacs file
;;     in order to make the package known to Emacs and to OO-Browser.
;;
;;  ;; Modelica browsing
;;  (autoload 'mdc-browse "mdc-browse" "Modelica Class Browsing" t)
;;  (autoload 'br-mdc "br-mdc" "Modelica Class Browsing" t)
;;
;;  (defvar br-env-lang-avector
;;    '[
;;      ("C++/C"   . "c++-")
;;      ("Eiffel"  . "eif-")
;;      ("Info"    . "info-")
;;      ("Java"    . "java-")
;;      ("Lisp"    . "clos-")
;;      ("Modelica" . "mdc-")
;;      ("Obj-C"   . "objc-")
;;      ("Python"  . "python-")
;;      ]
;;    "Association vector of elements of OO-Browser languages.")
;;
;; (3) Start OO-Browser as described in its documentation
;;
;; (4) Optionally byte-compile the lisp code
;;
;; (5) Please send comments and suggestions to
;;     Ruediger Franke <rfranke@users.sourceforge.net>

;;; HISTORY:     see ChangeLog

;; ************************************************************************
;; Other required Elisp libraries
;; ************************************************************************

(mapcar 'require '(br-mdc br-start br))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun modelica-browse (&optional env-file no-ui)
  "call mdc-browse"
  (interactive "P")
  (mdc-browse env-file no-ui))

;;;###autoload
(defun mdc-browse (&optional env-file no-ui)
  "Invoke the Modelica OO-Browser.
This allows browsing through Modelica library and system class hierarchies.
With an optional non-nil prefix argument ENV-FILE, prompt for Environment 
file to use. Alternatively, a string value of ENV-FILE is used as the 
Environment file name.  See also the file \"br-help\"."
  (interactive "P")

  ;; widen class list windows to allow for long class names
  ;; (do this here as it is defined using defconst in br.el)
  (setq br-min-width-window 35)

  (let ((same-lang (equal br-lang-prefix mdc-lang-prefix))
	(load-succeeded t)
	same-env)
    (if same-lang
	nil
      ;; Save other language Environment in memory
      (if br-lang-prefix (br-env-copy nil))
      (setq br-lang-prefix mdc-lang-prefix
	    *br-save-wconfig* nil))
    (setq same-env (or (equal mdc-env-file env-file)
		       (and (null env-file)
			    (or mdc-lib-search-dirs mdc-sys-search-dirs))))
    (cond
     ;; Continue browsing an Environment
     ((and same-env same-lang))
     ((and same-env (not same-lang))
      (mdc-browse-setup env-file) (br-env-copy t))
     ;;
     ;; Create default Environment file specification if needed and none
     ;; exists.
     ;;
     (t (or env-file (file-exists-p mdc-env-file)
	    (br-env-create mdc-env-file mdc-lang-prefix))
	(or env-file (setq env-file mdc-env-file))
	;;
	;; Start browsing a new Environment.
	;;
	(mdc-browse-setup env-file)
	(setq load-succeeded (br-env-init env-file same-lang nil))
	(if load-succeeded
	    (setq *br-save-wconfig* nil
		  mdc-env-file load-succeeded
		  mdc-sys-search-dirs br-sys-search-dirs
		  mdc-lib-search-dirs br-lib-search-dirs))))
    (cond (load-succeeded
	   (if no-ui
	       nil
	     (br-browse)
	     (or (and same-lang same-env) (br-refresh))))
	  (no-ui nil)
	  (t (message "(mdc-browse): You must build the Environment to browse it.")))))

;; Don't filter Environment classes when listed.
(defun mdc-class-list-filter (class-list top-only-flag)
  "Return class-list."
  class-list)

(defun mdc-mode-setup ()
  "Load best available Modelica major mode and set 'br-lang-mode' to the function that invokes it."
  (fset 'br-lang-mode
	(cond ((fboundp 'modelica-mode)
	       'modelica-mode)
	      ((load "modelica-mode" 'missing-ok 'nomessage)
	       'modelica-mode)
	      (t (error
		  "(mdc-mode-setup): Can't load major mode for Modelica code.")))))

;;; ************************************************************************
;;; Internal functions
;;; ************************************************************************

(defun mdc-browse-setup (env-file)
  "Setup language dependend functions and constants for OO-Browser."
  (br-setup-functions)
  ;; info facility
  (fset 'br-store-class-info 'mdc-store-class-info)
  (fset 'br-class-info 'br-entry-info)
  (mdc-mode-setup)
  (br-setup-constants env-file))

(provide 'mdc-browse)
