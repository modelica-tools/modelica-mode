# Modelica mode for Emacs

This directory contains extensions for Emacs supporting Modelica.  Modelica is
a unified object-oriented language for physical systems modeling (see
http://www.modelica.org).

This code has been written for use with Emacs and shares its licensing (See
COPYING).

The aim is to provide basic support as known from many programming languages
under Emacs. This includes proper indentation, automated closing of code
blocks, movement by statements and code blocks, support for writing comments,
and syntax highlighting.

# Installation

Put the file `modelica-mode.el` into an Emacs lisp directory,
e.g. `~/elisp/modelica-mode/`, and add the following lines to your init file
(`~/.emacs` or `~/.emacs.d/init.el`):

```elisp
;; Modelica mode
(add-to-list 'load-path "~/elisp/modelica-mode")
(autoload 'modelica-mode "modelica-mode" "Modelica Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.mo\\'" . modelica-mode))
```

## Installation with `use-package`

If you use [use-package](https://elpa.gnu.org/packages/use-package.html), add
the following form to your init file instead:

```elisp
(use-package modelica-mode
  :load-path "~/elisp/modelica-mode/"
  :commands (modelica-mode)
  :mode ("\\.mo\\'" . modelica-mode))
```

# OO-Browser Support for Modelica

The files `mdc-browse.el` and `br-mdc.el` contain code for Modelica support
for [OO-Browser](https://sourceforge.net/projects/oo-browser/).  See the file
`README-oo-browser.md` for instructions.
