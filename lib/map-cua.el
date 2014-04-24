;;; map-zxcv.el --- map C-{zxcv} for Motif/Windows GUI.

;; Copyright (C) 1997 Kim F. Storm.  All rights reserved!

;; Author: Kim F. Storm <storm@olicom.dk>
;; Keywords: keyboard

;; This file is CURRENTLY NOT part of GNU Emacs, but you can use,
;; modify, and redistribute it according to the GNU GPL:

;; This code is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This code is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides yet another way of emulating the
;; standard Motif/Windows/Mac GUI key bindings for selecting
;; and manipulating the region where S-<movement> is used to
;; highlight & extend the region.
;;
;; This package allow the C-z, C-x, C-c, and C-v keys to be
;; bound appropriately according to the Motif/Windows GUI, i.e.
;;	C-z	-> undo
;;	C-x	-> cut
;;	C-c	-> copy
;;	C-v	-> paste
;;
;; The tricky part is the handling of the C-x and C-c keys which
;; are normally used as prefix keys for most of emacs' built-in
;; commands.
;;
;; With the map-zxcv bindings, the C-x and C-c keys behaves as the
;; normal prefix keys *unless* the buffer's mark is currently active
;; and the transient-mark-mode is enabled, i.e. when there is a
;; highlighted region in the current window!

;;; Code:

(defvar map-zxcv-overriding-prefix-keys
  '((?\C-x "\C-x@\C-x" kill-region)
    (?\C-c "\C-x@\C-c" copy-region-as-kill))
  "List of prefix keys which are remapped via key-translation-map.")

(defun map-zxcv-prefix-override (prompt)
  (let (map)
    (if (and mark-active transient-mark-mode
	     (= (length (this-command-keys)) 1))
	(setq map (assq last-input-char map-zxcv-overriding-prefix-keys)))
    (if map
	(car (cdr map))
      (char-to-string last-input-char))))


; Aux functions.

(defun exchange-point-and-mark-nomark (arg)
  "Exchanges point and mark, but don't activate the mark.
Activates the mark if a prefix argument is given."
  (interactive "P")
  (if arg
      (setq mark-active t)
    (exchange-point-and-mark)
    (setq mark-active nil)))

(defvar map-zxcv-mode nil
  "*Non-nil means Map ZXCV mode is enabled.
In Map ZXCV mode, shifted movement keys highlight the region.
When a region is highlighted, insertion commands first delete
the region and then insert.")


(defun map-zxcv-pre-hook ()
  "Function run prior to command to check for special region handling.
If current command is a movement and the key is shifted, set or expand
the region."
  (if (and map-zxcv-mode transient-mark-mode (symbolp this-command))
      (let ((type (get this-command 'map-zxcv))
	    (ro buffer-read-only) (supersede nil))
	(if (eq type 'move)
	    (if (memq 'shift (event-modifiers (aref (this-single-command-keys) 0)))
		(and (not mark-active) (set-mark-command nil))
	      (setq mark-active nil))
	  (if mark-active
	      (progn
		(if (not ro)
		    (cond ((eq type 'kill)
			   (delete-active-region t))
			  ((eq type 'kill-sup)
			   (setq supersede (delete-active-region t)))
			  ((eq type 'yank)
			   ;; Before a yank command, make sure we don't yank
			   ;; the same region that we are going to delete.
			   ;; That would make yank a no-op.
			   (if (string= (buffer-substring (point) (mark))
					(car kill-ring))
			       (current-kill 1))
			   (delete-active-region nil))
			  ((eq type 'del-sup)
			   (setq supersede (delete-active-region nil)))
			  ((eq type 'del)
			   (delete-active-region nil))
			  ((eq type 'indent)
			   (setq supersede (delete-indent-selection current-prefix-arg nil)))
			  ((eq type 'back-indent)
			   (setq supersede (delete-indent-selection current-prefix-arg t)))
			  (t
			   (setq ro t))))
		(if ro ; or not handled above
		    (cond ((eq type 'copy)
			   (delete-active-region '(t)))
			  ((eq type 'copy-sup)
			   (setq supersede (delete-active-region '(t)))))))))
	(if supersede
	    (setq this-command '(lambda () (interactive)))))))

(add-hook 'pre-command-hook 'map-zxcv-pre-hook)

(defvar map-zxcv-region-commands
  '((del	; delete current region before command
     self-insert-command self-insert-iso insert-register
     newline-and-indent newline open-line)
    (del-sup	; delete current region and ignore command
     delete-backward-char backward-delete-char-untabify delete-char)
    (kill	; kill region before command
     )
    (kill-sup	; kill region and ignore command
     kill-region)
    (copy	; copy region before command
     )
    (copy-sup	; copy region and ignore command
     copy-region-as-kill)
    (yank	; replace region with element on kill ring
     yank clipboard-yank)
    (indent	; indent all lines in region by same amount
     indent-for-tab-command tab-to-tab-stop c-indent-command)
    (back-indent ; uninden all lines in region by same amount
     back-tab-indent)
))

(defvar map-zxcv-movement-keys
  '((forward-char	right)
    (backward-char	left)
    (next-line		down)
    (previous-line	up)
    (forward-word	C-right)
    (backward-word	C-left)
    (end-of-line	end)
    (beginning-of-line	home)
    (end-of-buffer	C-end)
    (beginning-of-buffer C-home)
    (scroll-up		next)
    (scroll-down	prior)
    (forward-paragraph	C-down)
    (backward-paragraph	C-up)))

(defun map-zxcv-mode (arg)
  "Toggle Delete Selection mode.
When ON, typed text replaces the selection if the selection is active.
When OFF, typed text is just inserted at point."
  (interactive "P")
  (setq map-zxcv-mode
	(if (null arg) (not map-zxcv-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if (get 'forward-char 'map-zxcv)
      t
    (let ((list map-zxcv-region-commands) type l)
      (while list
	(setq l (car list)
	      type (car l)
	      l (cdr l)
	      list (cdr list))
	(while l
	  (put (car l) 'map-zxcv type)
	  (setq l (cdr l))))
      (let ((list map-zxcv-movement-keys) cmd l)
	(while list
	  (setq l (car list)
		cmd (car l)
		l (cdr l)
		list (cdr list))
	  (while l
	    (put cmd 'map-zxcv 'move)
	    (define-key global-map (vector (car l)) cmd)
	    (define-key global-map (vector (intern (concat "S-" (symbol-name (car l))))) cmd)
	    (setq l (cdr l))))
	))

    (define-key global-map [S-tab]     'back-tab-indent)

    (define-key global-map [S-insert]  'yank)
    (define-key global-map [M-insert]  'yank-pop)
    (define-key global-map [C-insert]  'copy-region-as-kill)
    (define-key global-map [S-delete]  'kill-region)


    ;; The following bindings are useful on Sun Type 3 keyboards
    ;; They implement the Get-Delete-Put (copy-cut-paste)
    ;; functions from sunview on the L6, L8 and L10 keys
    ;;  (define-key global-map [f16]  'yank)
    ;;  (define-key global-map [f18]  'copy-region-as-kill)
    ;;  (define-key global-map [f20]  'kill-region)

    ;; The following bindings are from Pete Forman.
    ;; I modified them a little to work together with the
    ;; mark functionality I added.

    ;;  (global-set-key [f1] 'help)		; KHelp         F1
    ;;  (global-set-key [f6] 'other-window)	; KNextPane     F6
    ;;  (global-set-key [delete] 'delete-char) ; KDelete       Del
    ;;  (global-set-key [M-backspace] 'undo)	; KUndo         aBS
    (global-set-key [C-delete] 'kill-line) ; KEraseEndLine cDel

  (define-key global-map [?\C-z] 'advertised-undo)
  (define-key global-map [?\C-v] 'yank)
  (define-key ctl-x-map [?\C-x] 'exchange-point-and-mark-nomark)

  (or key-translation-map
      (setq key-translation-map (make-sparse-keymap)))
  (let ((map map-zxcv-overriding-prefix-keys))
    (while map
      (define-key key-translation-map (vector (nth 0 (car map))) 'map-zxcv-prefix-override)
      (define-key global-map (nth 1 (car map)) (nth 2 (car map)))
      (setq map (cdr map))))

  (setq transient-mark-mode t)
  (setq mark-even-if-inactive t)
  (setq highlight-nonselected-windows nil)
))

(defun delete-active-region (&optional killp)
  (if killp
      (if (listp killp)
	  (copy-region-as-kill (point) (mark))
	  (kill-region (point) (mark)))
    (delete-region (point) (mark)))
  (setq mark-active nil)
  (run-hooks 'deactivate-mark-hook)
  t)

(defun delete-indent-selection (arg backw)
  (message "Indenting...")
  (let ((a (point)) (b (mark)) c amount)
    (if (> a b) (setq c a a b b c))
    (save-excursion
      (goto-char a)
      (beginning-of-line)
      (setq a (point)))
    (if (equal arg '(4))
	(indent-region a b nil)
      (setq amount (if arg (prefix-numeric-value arg) tab-width))
      (indent-rigidly a b (if backw (- amount) amount))))
  (setq deactivate-mark t)
)
