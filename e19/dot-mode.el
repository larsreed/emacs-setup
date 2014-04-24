;;
;; Purpose of this package: minor mode to repeat typing or commands
;;
;; Installation instructions
;;
;; Install this file somewhere in your load path, byte-compile it and
;; add this line to your .emacs file (remove the comment delimiters ;-)
;;
;; (autoload 'dot-mode "dot-mode" nil t)	; vi `.' command emulation
;;
;; To turn dot mode on or off type `M-x dot-mode'
;;
;; Usage instructions:
;;
;; `C-.'    is bound to dot-mode-execute, which executes the buffer of
;;	    stored commands as a keyboard macro.
;;
;; `C-M-.'  is bound to dot-mode-override, which will cause dot-mode
;;	    to remember the next keystroke regardless of whether it
;;	    changes the buffer.
;;
;; Known bugs:
;;
;; Doesn't work with all versions of GNU Emacs
;;
;; LCD Archive Entry:
;; Dot-mode|James Gillespie|jim@tame
;; |Emacs version of vi "dot" command
;; Fri Feb  2 16:47:00 1996|1.0||
;;

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to jim@sbil.co.uk) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.

;;; COMMENTARY
;;;
;;; This mode is written to address one argument in the emacs vs. vi
;;; jihad :-)  It emulates the vi `redo' command, repeating the
;;; immediately preceding sequence of commands.  This is done by
;;; recording input commands which change the buffer, i.e. not motion
;;; commands.

;;; DESIGN
;;;
;;; This is my first minor mode, and the deepest I've ever dug into
;;; the internals of XEmacs.  It could probably be faster and do
;;; things more cleanly, but hey, it works :-)
;;;
;;; The heart of this minor mode is a state machine.  The function
;;; dot-mode-after-change is called from after-change-functions and
;;; sets a variable (is there one already?  I couldn't find it) which
;;; is examined by dot-mode-loop, called from from post-command-hook.
;;; This variable, dot-mode-changed, is used in conjunction with
;;; dot-mode-state to move to the next state in the state machine.
;;; The state machine is hard coded into dot-mode-loop in the
;;; interests of speed; it uses three normal states (idle, start and
;;; store) and three corresponding override states to allow the user
;;; to forcibly store commands which do not change the buffer.

;;; I keep release numbers separate from RCS version numbers
;;; $Id: dot-mode.el,v 1.8 1996/02/02 16:54:47 jim Exp $

(defconst dot-mode-version "1.0"
  "Report bugs to: James Gillespie jim@sbil.co.uk")

;;; CHANGE HISTORY
;;;
;;; 1.1
;;; Wrote dot-mode.el
;;;
;;; 1.2
;;; At the suggestion of Scott Evans <gse@ocsystems.com>, added
;;; 'dot-mode-override' to allow the user to force dot mode to store a
;;; motion command
;;;
;;; 1.3
;;; Changed dot-mode-loop to use a state machine instead of several
;;; booleans
;;;
;;; 1.4
;;; Hard coded the state machine into dot-mode-loop in the hope of
;;; speeding it up
;;;
;;; 1.5
;;; Ported to GNU Emacs - nearly: the keymap doesn't seem to install
;;; correctly.

(defvar dot-mode nil
  "Whether dot mode is on or not")
(make-variable-buffer-local 'dot-mode)

(defconst dot-mode-map (make-sparse-keymap)
  "Keymap used in dot mode buffers")

;;; XEmacs vs GNU Emacs code to add a minor mode - installing the
;;; keymap doesn't quite seem to work for GNU Emacs
(if (string-match "XEmacs" emacs-version)
    (progn
      (define-key dot-mode-map [(control ?.)]		'dot-mode-execute)
      (define-key dot-mode-map [(control meta ?.)]	'dot-mode-override)
      (add-minor-mode 'dot-mode " Dot" dot-mode-map))
  (define-key dot-mode-map [4194350]	'dot-mode-execute)
  (define-key dot-mode-map [?\e 4194350]	'dot-mode-override)
  (or (assq 'dot-mode minor-mode-alist)
      (setq minor-mode-alist (cons '(dot-mode " Dot") minor-mode-alist)))
  (or (assq 'dot-mode minor-mode-map-alist)
      (setq minor-mode-map-alist (cons (cons 'dot-mode dot-mode-map)
				       minor-mode-map-alist))))

(defvar dot-mode-changed nil
  "Did last command change buffer?")
(make-variable-buffer-local 'dot-mode-changed)

(defvar dot-mode-cmd-buffer nil
  "Saved commands.")
(make-variable-buffer-local 'dot-mode-cmd-buffer)

(defvar dot-mode-state 0
  "Current state of dot mode.")
(make-variable-buffer-local 'dot-mode-state)

(defun dot-mode-execute ()
  "Execute stored commands."
  (interactive)
  ;; Don't want execution to kick off infinite recursion
  (remove-hook 'post-command-hook 'dot-mode-loop)
  (remove-hook 'after-change-functions 'dot-mode-after-change)
  ;; Do the business
  (execute-kbd-macro dot-mode-cmd-buffer)
  ;; Put the hooks back
  (add-hook 'post-command-hook 'dot-mode-loop)
  (add-hook (make-local-variable 'after-change-functions) 'dot-mode-after-change))

(defun dot-mode-override ()
  "Override standard behaviour and store next keystroke no matter what."
  (interactive)
  (setq dot-mode-state (+ dot-mode-state 3)))

(defun dot-mode-after-change (start end prevlen)
  (setq dot-mode-changed t))

;;; In some emacsen this-command-keys returns a string, and I need a
;;; vector
;(if (vectorp (this-command-keys))
;    (fset 'dot-mode-command-keys 'this-command-keys)

(defun dot-mode-command-keys ()
  (vconcat (this-command-keys)))
;    (character-to-event (this-command-keys))))

(defun dot-mode-loop ()
  "The heart of dot mode."
  (cond ((= dot-mode-state 0)		; idle
	 (if dot-mode-changed
	     (setq dot-mode-state	1
		   dot-mode-changed	nil
		   dot-mode-cmd-buffer	(dot-mode-command-keys))))
	((= dot-mode-state 1)		; start again
	 (if dot-mode-changed
	     (setq dot-mode-state	2
		   dot-mode-changed	nil
		   dot-mode-cmd-buffer	(vconcat dot-mode-cmd-buffer (dot-mode-command-keys)))
	   (setq dot-mode-state	0)))
	((= dot-mode-state 2)		; keep going
	 (if dot-mode-changed
	     (setq dot-mode-changed	nil
		   dot-mode-cmd-buffer	(vconcat dot-mode-cmd-buffer (dot-mode-command-keys)))
	   (setq dot-mode-state	0)))
	(t				; override
	 (setq dot-mode-state	    (- dot-mode-state 3)
	       dot-mode-changed	    t))))

(defun dot-mode (arg)
  "Toggle dot mode.
With arg, turn dot mode on iff arg is positive.

Dot mode mimics the `.' function in vi, repeating sequences of
commands and/or typing delimited by motion events.  Use `C-.' rather
than just `.'."
  (interactive "P")
  (setq dot-mode
	(if (if (null arg) (not dot-mode)
	      (> (prefix-numeric-value arg) 0))
	    t))
  (if (not dot-mode)
      (progn
	(remove-hook 'post-command-hook 'dot-mode-loop)
	(remove-hook 'after-change-functions 'dot-mode-after-change))
    (add-hook 'post-command-hook 'dot-mode-loop)
    (add-hook (make-local-variable 'after-change-functions) 'dot-mode-after-change)
    (setq dot-mode-state	0
	  dot-mode-changed	nil
	  dot-mode-cmd-buffer	nil))
  (set-buffer-modified-p (buffer-modified-p))
  )

(provide 'dot-mode)
