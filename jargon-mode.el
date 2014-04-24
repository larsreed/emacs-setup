;; jargon-mode.el - Major mode for reading the Jargon File
;;
;; Version 0.6
;;
;; Copyright (C) 1985, 1986 Free Software Foundation, Inc.
;; Copyright (C) 1993 Sascha Wildner <swildner@channelz.GUN.de> and others
;;
;;
;; Thanx to the following people for their comments & code:
;;
;; Dong-Ping Deng, Erik C. Ostrom, Timo Rinne
;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; The GNU General Public License is available by anonymous ftp from
;; prep.ai.mit.edu in pub/gnu/COPYING.  Alternately, you can write to
;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
;; USA.
;;
;;
;; To install, copy jargon-mode.el into your lisp directory, byte-compile it
;; (M-x byte-compile-file) and put the following lines into your .emacs file:
;;
;; (autoload 'jargon-mode "jargon-mode"
;;           "Major mode for browsing the Jargon File." t)
;; (setq auto-mode-alist (cons
;;                        '("[jJ]argon[0-9.]*\\.\\(txt\\|ascii\\)" . jargon-mode)
;;                        auto-mode-alist))
;;
;;
;; --- HISTORY ---
;; 19 Feb 93
;;		Created jargon.el
;; 30 Mar 93
;;		Changed name to jargon-mode.el
;;		Added installation information
;; 31 Mar 93
;;		jargon-next-entry now goes ahead when at preceding colon
;;		Rebound '<' to jargon-first-entry and '>' to jargon-last-entry
;;		Added jargon-beginning-of-entry
;;		Added jargon-next-reference and jargon-prev-reference
;;		Added default to jargon-chase-reference
;;		Added prefix argument handling where it makes sense
;;		Buffer is now made read-only
;;		Bound scroll-up and scroll-down to <SPC> and <DEL>
;;		Fixed a bug in jargon-chase-reference (remove TABs and NLs)
;; 06 Apr 93
;;		Bound describe-mode to '?'
;; 11 Apr 93
;;		Fixed a bug in jargon-chase-reference (sort out double refs)
;; 14 Apr 93
;;		Added jargon-output-to-cut-file
;; 16 Apr 93
;;		jargon-chase-reference now catches "" input
;;		Added jargon-quit
;; 17 Apr 93
;;		Rebound '?' to jargon-mini-help
;;		A longer help can now be obtained with describe-mode
;;		jargon-chase-reference now catches pseudo-entries
;;		jargon-mode now runs jargon-mode-hook as the last step
;; 22 Apr 93
;;		case-fold-search is nil now only when necessary
;; 26 Apr 93
;;		Wrote a better documentation string for jargon-mode
;; 21 May 93
;;		jargon-prev-entry at the first entry now moves to the top
;;		jargon-next-entry at the last entry now moves to the bottom
;; 22 May 93
;;		jargon-chase-reference now finds the first reference in a file
;; 27 May 93
;;		Added jargon-chase-reference-blind and
;;		jargon-trace-back-references
;; 31 May 93
;;		Added jargon-prev-keyword and jargon-next-keyword
;;
;;
;; LCD Archive Entry:
;; jargon-mode.el|Sascha Wildner|swildner@channelz.GUN.de|
;; Major mode for reading the Jargon File.|
;; 31-May-93|0.6|~/modes/jargon-mode.el.Z|

(defvar jargon-mode-map nil
  "Local keymap for jargon mode buffers.")

(defvar jargon-entry-regexp nil
  "Regular expression for recognizing entries.")

(defvar jargon-reference-regexp nil
  "Regular expression for recognizing references.")

(defvar jargon-last-cut-file nil)

(defvar jargon-reference-backtrace nil
  "List to trace back reference jumps.")

(if jargon-mode-map
    nil
  (setq jargon-mode-map (make-keymap))
  (suppress-keymap jargon-mode-map)
  (define-key jargon-mode-map " " 'scroll-up)
  (define-key jargon-mode-map "<" 'jargon-first-entry)
  (define-key jargon-mode-map ">" 'jargon-last-entry)
  (define-key jargon-mode-map "a" 'jargon-beginning-of-entry)
  (define-key jargon-mode-map "b" 'jargon-prev-reference)
  (define-key jargon-mode-map "B" 'jargon-prev-keyword)
  (define-key jargon-mode-map "c" 'jargon-chase-reference)
  (define-key jargon-mode-map "C" 'jargon-chase-reference-blind)
  (define-key jargon-mode-map "e" 'jargon-find-entry)
  (define-key jargon-mode-map "f" 'jargon-next-reference)
  (define-key jargon-mode-map "F" 'jargon-next-keyword)
  (define-key jargon-mode-map "n" 'jargon-next-entry)
  (define-key jargon-mode-map "o" 'jargon-output-to-cut-file)
  (define-key jargon-mode-map "p" 'jargon-prev-entry)
  (define-key jargon-mode-map "q" 'jargon-quit)
  (define-key jargon-mode-map "t" 'jargon-trace-back-references)
  (define-key jargon-mode-map "T" 'jargon-clear-trace-back)
  (define-key jargon-mode-map "?" 'jargon-mini-help)
  (define-key jargon-mode-map "\177" 'scroll-down))

(defun jargon-beginning-of-entry ()
  "Go to the beginning of the current entry."
  (interactive)
  (beginning-of-line)
  (if (looking-at jargon-entry-regexp)
      (forward-char)
    (jargon-prev-entry)))

(defun jargon-chase-reference ()
  (interactive)
  (jargon-chase-reference-raw t))

(defun jargon-chase-reference-blind ()
  (interactive)
  (jargon-chase-reference-raw '()))

(defun jargon-chase-reference-raw (ask)
  "Follow a reference from the current entry."
  (let ((completion-ignore-case nil)
	completions str i beg end default)
    (save-excursion
      (setq beg (save-excursion
		  (jargon-beginning-of-entry)
		  (1- (point))))
      (jargon-next-entry)
      (setq end (1- (point)))
      (goto-char beg)
      (while (re-search-forward jargon-reference-regexp end t)
	(setq str (buffer-substring
		   (1+ (match-beginning 0))
		   (1- (point))))
	(setq i 0)
	(while (setq i (string-match "[ \n\t]+" str i))
	  (setq str (concat (substring str 0 i) " "
			    (substring str (match-end 0))))
	  (setq i (1+ i)))
	(setq j 0)
	(setq found nil)
	(while (setq reference (car (nth j completions)))
	  (if (string= reference str)
	      (setq found t))
	  (setq j (1+ j)))
	(if (not found)
	    (setq completions
		  (cons (cons str nil)
			completions)))))
    (if completions
	(progn
	  (setq default (cond ((eq (length completions) 1)
			       (car (car completions)))
			      ((looking-at jargon-reference-regexp)
			       (buffer-substring (1+ (match-beginning 0))
						 (1- (match-end 0))))
			      ((and (save-excursion
				      (search-backward "{" nil t)
				      (looking-at jargon-reference-regexp))
				    (> (match-end 0) (point)))
			       (buffer-substring (1+ (match-beginning 0))
						 (1- (match-end 0))))
			      (t
			       "")))
	  (while (setq i (string-match "[ \n\t]+" default i))
	    (setq default (concat (substring default 0 i) " "
				  (substring default (match-end 0))))
	    (setq i (1+ i)))
	  (setq entry nil)
	  (if (and (not (string= default "")) (not ask))
	      (setq entry default)
	    (while (null entry)
	      (setq entry (completing-read "Chase reference: " completions
					   nil t default))
	      (if (string= entry "")
		  (setq entry nil)))))
      (error "No references in this entry"))
    (save-excursion
      (jargon-beginning-of-entry)
      (beginning-of-line)
      (if (looking-at jargon-entry-regexp)
	  (setq jargon-reference-backtrace
		(cons (regexp-quote
		       (buffer-substring (match-beginning 0) (match-end 0)))
		      jargon-reference-backtrace))
	(setq jargon-reference-backtrace (cons "" jargon-reference-backtrace)))
      (goto-char (point-min))
      (let ((case-fold-search nil))
	(re-search-forward (concat "^:" (regexp-quote entry)
				   ":") nil)))
    (goto-char (match-beginning 0))
    (forward-char)))

(defun jargon-clear-trace-back ()
  (interactive)
  (setq jargon-reference-backtrace nil)
  (message "Reference backtrace cleared."))

(defun jargon-find-entry ()
  "Find a certain entry."
  (interactive
   (let ((completion-ignore-case nil)
	 completions str i)
     (save-excursion
       (goto-char (point-min))
       (while (re-search-forward jargon-entry-regexp nil t)
	 (setq str (buffer-substring
		    (1+ (match-beginning 0))
		    (1- (point))))
	 (setq completions
	       (cons (cons str nil)
		     completions))))
     (if completions
	 (setq str (completing-read "Find entry: " completions nil t))
       (error "No entries in this buffer"))
     (goto-char (point-min))
     (let ((case-fold-search nil))
       (re-search-forward (concat "^:" (regexp-quote str) 
				  ":") nil t))
     (goto-char (match-beginning 0))
     (forward-char))))

(defun jargon-first-entry ()
  "Go to the first entry."
  (interactive)
  (goto-char (point-min))
  (jargon-next-entry))

(defun jargon-last-entry ()
  "Go to the last entry."
  (interactive)
  (goto-char (point-max))
  (jargon-prev-entry))

(defun jargon-mini-help ()
  "Display some commands in the minibuffer."
  (interactive)
  (message
   "find [e]ntry, [n]ext entry, [p]revious entry, [c]hase reference, [q]uit"))

(defun jargon-next-entry (&optional arg)
  "Go to the next entry.  With arg, do it arg times."
  (interactive "p")
  (or arg (setq arg 1))
  (if (looking-at jargon-entry-regexp)
      (forward-char))
  (while (> arg 0)
    (if (re-search-forward jargon-entry-regexp (point-max) 0)
	(progn
	  (beginning-of-line)
	  (forward-char)))
    (setq arg (1- arg))))

(defun jargon-next-keyword (&optional arg)
  "Move to the next reference or entry.  With arg, do it arg times."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((end (save-excursion
	       (jargon-next-entry)
	       (1- (point)))))
    (while (> arg 0)
      (forward-char)
      (if (re-search-forward jargon-reference-regexp end t)
	  (goto-char (match-beginning 0))
	(backward-char)
	(jargon-next-entry))
      (setq arg (1- arg)))))

(defun jargon-next-reference (&optional arg)
  "Move to the next reference in this entry.  With arg, do it arg times."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((end (save-excursion
	       (jargon-next-entry)
	       (1- (point)))))
    (while (> arg 0)
      (forward-char)
      (if (re-search-forward jargon-reference-regexp end t)
	  (goto-char (match-beginning 0))
	(backward-char)
	(error "No more references in this entry"))
      (setq arg (1- arg)))))

(defun jargon-output-to-cut-file (file-name)
  "Append the current entry to a cut file."
  (interactive (list (read-file-name
		      (concat "Append entry to cut file: (default "
			      (file-name-nondirectory jargon-last-cut-file)
			      ") ")
		      (file-name-directory jargon-last-cut-file)
		      jargon-last-cut-file)))
  (setq file-name (expand-file-name file-name))
  (setq jargon-last-cut-file file-name)
  (save-excursion
    (jargon-beginning-of-entry)
    (setq beg (1- (point)))
    (jargon-next-entry)
    (setq end (1- (point)))
    (append-to-file beg end file-name)))

(defun jargon-prev-entry (&optional arg)
  "Go to the previous entry.  With arg, do it arg times."
  (interactive "p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (if (re-search-backward jargon-entry-regexp (point-min) 0)
	(progn
	  (beginning-of-line)
	  (forward-char)))
    (setq arg (1- arg))))

(defun jargon-prev-keyword (&optional arg)
  "Move to the previous reference or entry.  With arg, do it arg times."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((beg (save-excursion
	       (jargon-beginning-of-entry)
	       (1- (point)))))
    (while (> arg 0)
      (save-excursion
	(if (re-search-backward jargon-entry-regexp (point-min) 0)
	    (setq lastentry (match-beginning 0))
	  (setq lastentry 0)))
      (save-excursion
	(if (re-search-backward jargon-reference-regexp (point-min) 0)
	    (setq lastref (match-beginning 0))
	  (setq lastref 0)))
      (if (> lastref lastentry)
	  (goto-char lastref)
	(jargon-prev-entry))
      (setq arg (1- arg)))))

(defun jargon-prev-reference (&optional arg)
  "Move to the previous reference in this entry.  With arg, do it arg times."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((beg (save-excursion
	       (jargon-beginning-of-entry)
	       (1- (point)))))
    (while (> arg 0)
      (if (re-search-backward jargon-reference-regexp beg t)
	  (goto-char (match-beginning 0))
	(error "No previous references in this entry"))
      (setq arg (1- arg)))))

(defun jargon-quit ()
  "Quit reading the Jargon File and select another buffer."
  (interactive)
  (switch-to-buffer (prog1 (other-buffer (current-buffer))
		      (bury-buffer (current-buffer)))))

(defun jargon-trace-back-references ()
  "Trace back reference jumps."
  (interactive)
  (if (not (null jargon-reference-backtrace))
      (progn (save-excursion
	       (goto-char (point-min))
	       (let ((case-fold-search nil))
		 (re-search-forward (car jargon-reference-backtrace) nil)))
	     (goto-char (match-beginning 0))
	     (forward-char)
	     (setq jargon-reference-backtrace 
		   (cdr jargon-reference-backtrace)))
    (error "No further backtrace.")))

(defun jargon-mode ()
  "Major mode for reading the Jargon File.

The Jargon File is a huge collection of hacker slang, humor, and folklore which
is currently maintained by Eric S. Raymond <esr@snark.thyrsus.com>.  It's
available for anonymous ftp on prep.ai.mit.edu in the directory pub/gnu.

The following commands are available:

n  jargon-next-entry		Go to the next entry.
p  jargon-prev-entry		Go to the previous entry.
e  jargon-find-entry		Go to a specific entry.
<  jargon-first-entry		Go to the first entry.
>  jargon-last-entry		Go to the last entry.
a  jargon-beginning-of-entry	Go to the beginning of the current entry.

c  jargon-chase-reference	Chase a reference.
C  jargon-chase-reference-blind	Chase a current reference without confirmation.
f  jargon-next-reference	Go to the next reference in the current entry.
b  jargon-prev-reference	Go to the previous reference.
t  jargon-trace-back-references	Trace back reference jumps.
T  jargon-clear-trace-back	Clear the reference jump backtrace.

F  jargon-next-keyword		Go to the next keyword (entry or reference).
B  jargon-prev-keyword		Go to the previous keyword.

o  jargon-output-to-cut-file	Append the current entry to a cut file.
q  jargon-quit			Quit reading the Jargon File.
?  jargon-mini-help		Display some common commands in the minibuffer.


The following variables influence the behavior of jargon-mode:

jargon-entry-regexp
	The regular expression used for recognizing entries.

jargon-reference-regexp
	The regular expression used for recognizing references.


Turning on jargon-mode calls the value of the variable jargon-mode-hook with
no args, if that value is non-nil."
  (interactive)
  (setq major-mode 'jargon-mode)
  (setq mode-name "Jargon")
  (setq jargon-entry-regexp "^:[^:]+:")
  (setq jargon-reference-regexp "{[^{}]+}")
  (setq jargon-last-cut-file (expand-file-name"~/jargon.cut"))
  (setq buffer-read-only t)
  (use-local-map jargon-mode-map)
  (run-hooks 'jargon-mode-hook))
