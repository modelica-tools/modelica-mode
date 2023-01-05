# OO-Browser Support for Modelica

The files `mdc-browse.el` and `br-mdc.el` contain a Modelica extension for the
OO-Browser (see https://sourceforge.net/projects/oo-browser).

Note that this code is currently unmaintained; see
https://github.com/modelica-tools/modelica-mode/issues/1#issuecomment-38176132;
this file contains the original installation and usage instructions.


## Installation

Put the files `mdc-browse.el` `br-mdc.el` into an Emacs lisp directory,
e.g. `~/elisp`, and add the following lines to your init file:

```elisp
;; Enable Modelica browsing
(autoload 'mdc-browse "mdc-browse" "Modelica Class Browsing" t)
(autoload 'br-mdc "br-mdc" "Modelica Class Browsing" t)

(defvar br-env-lang-avector
  '[
    ("C++/C"   . "c++-")
    ("Eiffel"  . "eif-")
    ("Info"    . "info-")
    ("Java"    . "java-")
    ("Lisp"    . "clos-")
    ("Modelica" . "mdc-")
    ("Obj-C"   . "objc-")
    ("Python"  . "python-")
    ]
  "Association vector of elements of OO-Browser languages.")

;; Autostart OO-Browser (the installation is assumed under ~/oo-browser)
(setq load-path (append
		 '("~/oo-browser/"
		   "~/oo-browser/hypb/")
		 load-path))
(load "br-start")
(global-set-key "\C-c\C-o" 'oo-browser)
```

## Tutorial introduction into OO-Browser for Modelica

The following steps should be helpful to start investigating
OO-Browser for Modelica without the need to read its documentation
before. Please follow the installation instructions given above first.

Source files to browse can be placed in multiple directories, which
are grouped into library directories (for stable code to be re-used)
and system directories (for code under development). For now we will
just work with one system directory. Copy Modelica code examples there.

Now start Emacs and open one of the files in your directory.

-> select the menu item <OO-Browser -> Create-or-Load-Env>

It is prompted for the environment name.

-> Choose a name, e.g. hello-world.

It is prompted for the file to store class information in.

-> confirm the default name OOBR

Now you must choose a language to work with.

-> select Modelica

Finally specify system and library directories
(specify the current directory "." as the only system directory)

Hit RET to specify the code directories for the Environment

-> Return

Top-level system-specific code dir #1 (RET to end):

-> . Return

Top-level system-specific code dir #2 (RET to end):

-> Return

Top-level reusable code library dir #1 (RET to end):

-> Return

Now OO-Browser is ready to scan your Modelica files

Build Environment 'hello-world now? (y or n)

-> y

Build Environment in the background? (y or n)

-> n

Now all Modelica code files in the specified directories are scanned.
Afterwards a list with all obtained classes appears. Some basic
operations are:

- show ancestors and descendants of a class,
   e.g. click on a listed class name to show its definition,
        hit the "a" key to show ancestors
        (hit the "x" key to return to the list of all classes
         if ancestors were found)
        hit the "d" key to show descendants

- obtain information about classes
   e.g. hit the "i" key to obtain the class location and documentation

- view and edit class implementations
   e.g. hit the "v" key to view the implementation of a class
        hit the "e" key to edit the implementation of a class

- view class definition by clicking the Action Key
  (shift-left mouse button or shift-middle mouse button)
  over a class name in any buffer showing a Modelica file


Have fun!

Ruediger Franke <rfranke@users.sourceforge.net>
