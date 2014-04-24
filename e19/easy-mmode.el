;;; easy-mmode.el -- easy definition of minor modes.

;; Copyright (C) 1997  Georges Brun-Cottan

;; Author:  Georges Brun-Cottan <Georges.Brun-Cottan@inria.fr>
;; Maintainer:  Georges Brun-Cottan <Georges.Brun-Cottan@inria.fr>
;; $Revision: 1.5 $

;;; Copyright:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of version 2 of the GNU General Public
;; License as published by the Free Software Foundation.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;; Commentary:
;;
;; Order in which minor modes are installed is important. Please,
;; don't forget that keymap lookup proceeds in a precedence order
;; reverse to the install one.
;;
;; Rationale: minor modes are useful and common.  This package makes
;; defining a minor mode easy, by focusing on the writing of the minor
;; mode functionalities themselves. Moreover, this package enforces a
;; conventional naming of user interface primitives, making things
;; natural for the minor-mode end-users.

;;; Using:
;;
;; For each mode the followings are defined :
;; <mode>      : The minor mode predicate. A buffer-local variable.
;; <mode>-map  : The keymap possibly associated to <mode>.
;; <mode>-hook,<mode>-on-hook,<mode>-off-hook and <mode>-mode:
;;       see `easy-mmode-define-minor-mode' documentation
;;
;; eval
;;  (pp (macroexpand '(easy-mmode-define-minor-mode <your-mode> <doc>)))
;; to check the result before using it.

;;; Example:
;; 
;; Take a look at the eftl1.el package which use easy-mmode.

;;; Change Log:
;; $Id: easy-mmode.el,v 1.5 1997/03/10 09:18:51 bruncott Exp $
;; $Log: easy-mmode.el,v $
; Revision 1.5  1997/03/10  09:18:51  bruncott
; Fixed doc. Added a gripe (reporter).
;
; Revision 1.4  1997/03/08  17:44:10  bruncott
; Fixed doc.
;
; Revision 1.3  1997/03/08  15:13:45  bruncott
; Bug fix.
;
; Revision 1.2  1997/03/08  14:22:25  bruncott
; Better doc. Three hooks now.
;

(defconst easy-mmode-version "$Revision: 1.5 $"
  "Current easy-mmode version")

(defun easy-mmode-define-keymap (keymap-alist)
  "Return a keymap builded from KEYMAP-ALIST.
KEYMAP-ALIST must be a list of (KEYBINDING . BINDING) where
KEYBINDING and BINDINGS are suited as for define-key."
  (let ((keymap (make-sparse-keymap)))
    (mapcar
     (function (lambda (bind)
		 (define-key keymap
		   (car bind) (cdr bind))))
     keymap-alist)
    keymap))


(defmacro easy-mmode-define-toggle (mode &optional doc)
  "Define a one arg toggle mode MODE function and three associated hooks.
electric-MODE-mode is the so defined function that toggle the mode.
optional DOC is its associated documentation.

Hooks are checked for run, each time electric-MODE-mode is called.
they do run under the followings conditions :
electric-MODE-hook: if the mode is toggled.
electric-MODE-on : if the mode is on.
electric-MODE-off : if the mode is off.

When the mode is effectively toggled, two hooks may run.
If so electric-MODE-hook is guaranteed to be the first.

(defmacro easy-mmode-define-toggle (MODE &optional DOC)"
  (let* ((mode-name (symbol-name mode))
	 (hook (intern (concat mode-name "-hook")))
	 (hook-on (intern (concat mode-name "-on-hook")))
	 (hook-off (intern (concat mode-name "-off-hook")))
	 (toggle (intern (concat mode-name "-mode")))
	 (toggle-doc (or doc
			 (format "With no argument, toggle %s mode.
With arg turn mode on.
With zero or negative arg turn mode off"
				 mode-name))))
    `(progn
       (defvar ,hook  nil	     
	 ,(format "Hook called when %s mode is toggled" mode-name))

       (defvar ,hook-on  nil	     
	 ,(format "Hook called when %s mode is turned on" mode-name))

       (defvar ,hook-off nil
	 ,(format "Hook called when %s mode is turned off" mode-name))

       (defun ,toggle (&optional arg)
	 ,toggle-doc
	 (interactive "P")
	 (let ((old-mode ,mode))
	   (setq ,mode
		 (if arg
		     (or (listp arg);; C-u alone
			 (> (prefix-numeric-value arg) 0))
		   (not ,mode)))
	   (and ,hook
		(not (equal old-mode ,mode))
	        (run-hooks ',hook))
	   (and ,hook-on
		,mode
		(run-hooks ',hook-on))
	   (and ,hook-off
		(not ,mode)
		(run-hooks ',hook-off)))))))


(defmacro easy-mmode-define-minor-mode
  (mode doc &optional init-value &optional lighter &optional keymap)
  "Define a new minor mode MODE with associated switch, keymap,
toggle and hooks (see `easy-mmode-define-toggle').
optional DOC is the documentation bound to the mode toggle function.
optional LIGHTER is displayed in the mode-bar when the mode is on.
optional KEYMAP is the default (defvar) keymap bound to the
mode keymap.
 
(defmacro easy-mmode-define-minor-mode
  (MODE DOC &optional INIT-VALUE &optional LIGHTER &optional KEYMAP)" 
  (let* ((mode-name (symbol-name mode))
	 (mode-doc (format "%s mode control switch." mode-name))
	 (keymap-name (concat mode-name "-map"))
	 (key-map (cond ((and keymap (keymapp keymap))
			  keymap)
			((listp keymap)
			 (easy-mmode-define-keymap keymap))))
	 (keymap-doc (format "Keymap activated when %s mode is on." mode-name)))
    `(progn
       ;; define the switch
       (defvar ,mode ,init-value ,mode-doc)
       (make-variable-buffer-local ',mode)

       ;; define the minor-mode keymap
       (defvar ,(intern keymap-name) ',key-map ,keymap-doc)

       ;; define the toggle and the hooks
       ,(macroexpand `(easy-mmode-define-toggle ,mode ,doc)) ; toggle and hooks

       ;; update the mode-bar
       (or (assq ',mode minor-mode-alist)
	   (setq minor-mode-alist
		 (cons (list ',mode ,lighter) minor-mode-alist)))

       ;; update the minor-mode-map
       (or (assq ',mode minor-mode-map-alist)
	   (setq minor-mode-map-alist 
		 (cons (cons ',mode ,(intern keymap-name)) minor-mode-map-alist)))) ))


(defun easy-mmode-gripe ()
  "Send a gripe to the author."
  (interactive)
  (require 'reporter)
  (reporter-submit-bug-report
   "Georges.Brun-Cottan@inria.fr"
   easy-mmode-version
   '(easy-mmode-version)
   (function (lambda ()
	       (save-excursion
		 (mail-position-on-field "subject")
		 (insert "easy-mmode report")))) ))

(provide 'easy-mmode)
;; easy-mmode.el ends here

