From: "Bob Glickstein" <bobg@hiro.zanshin.com>
Subject: live-mode v2.0
Newsgroups: gnu.emacs.sources
Reply-To: bobg@zanshin.com
Organization: Zanshin Inc., Marin County, CA
Message-ID: <01bc8fcc$29f42de0$0cfb47c1@im2u>
X-Newsreader: Microsoft Internet News 4.70.1160

This is an update to my live-mode, posted here a few weeks ago.  The
old version created a "timer" object for every "live" buffer,
triggering them separately at identical intervals.  The new version
contains one "timer" that updates all "live" buffers at once.


;;; live-mode.el --- view/edit a changing file

;; Copyright 1997 Bob Glickstein.      <http://www.zanshin.com/~bobg/>

;; Author: Bob Glickstein <bobg@zanshin.com>
;; Maintainer: Bob Glickstein <bobg@zanshin.com>
;; Version: 2.0

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, send e-mail to
;; this program's maintainer or write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Plug:

;; Check out my book, "Writing GNU Emacs Extensions," from O'Reilly
;; and Associates.  <http://www.ora.com/catalog/gnuext/>

;;; Commentary:

;; `live-mode' is a minor mode that works like the "tail -f" Unix
;; command.  If the file grows (or changes in any other way) on the
;; disk, then the buffer copy is periodically updated to show the new
;; file contents.  This makes `live-mode' ideal for viewing such
;; things as log files.

;; The buffer is only updated if there are no unsaved changes.
;; Updating is done every `live-interval' seconds using
;; `revert-buffer'.

;; This code was inspired by live-find-file (available in the Emacs
;; Lisp archive), whose implementation is now obsolete.

;;; Code:

(require 'timer)
(require 'assoc)

(defun copy-except (obj pred)
  "Make a copy of OBJ, excluding list elements for which PRED is true."
  (cond ((null obj) nil)
	((consp obj) (cond ((funcall pred (car obj))
			    (copy-except (cdr obj) pred))
			   ((funcall pred (cdr obj))
			    (copy-except (car obj) pred))
			   (t (cons (copy-except (car obj) pred)
				    (copy-except (cdr obj) pred)))))
	(t obj)))

(defvar live-mode nil)
(make-variable-buffer-local 'live-mode)

(defvar live-timer nil)
(defvar live-buffers nil)

(defvar live-interval 5
  "*The number of seconds between each live-mode file check.")

(aput 'minor-mode-alist 'live-mode '(" Live"))

(defun live-mode (&optional arg)
  "View/edit a growing or changing file."
  (interactive "P")
  (setq live-mode
	(if (null arg)
	    (not live-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if (timerp live-timer)
      (cancel-timer live-timer))
  (if live-mode
      (let ((first (null live-buffers)))
	(add-to-list 'live-buffers (current-buffer))
	(make-local-hook 'kill-buffer-hook)
	(add-hook 'kill-buffer-hook 'live-mode-remove-buffer nil t)
	(if first
	    (setq live-timer
		  (run-with-timer live-interval live-interval
				  'live-mode-update-buffers))))
    (live-mode-remove-buffer)))

(defun live-mode-remove-buffer ()
  (setq live-buffers (copy-except live-buffers
				  (function
				   (lambda (x)
				     (eq x (current-buffer))))))
  (if (null live-buffers)
      (progn
	(if live-timer
	    (cancel-timer live-timer))
	(setq live-timer nil))))

(defun live-mode-update-buffers ()
  (let ((buffers live-buffers))
    (save-excursion
      (while buffers
	(set-buffer (car buffers))
	(setq buffers (cdr buffers))
	(let ((file (buffer-file-name)))
	  (if (and file
		   (not (buffer-modified-p))
		   (not (verify-visited-file-modtime (current-buffer))))
	      (revert-buffer t t)))))))

(provide 'live-mode)

;;; live-mode.el ends here
