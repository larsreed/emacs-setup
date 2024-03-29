;;; sql-mode.el --- SQL mode for emacs

;; Copyright (C) 1993 Rob Riepel.

;; Author: Rob Riepel <riepel@snowflake.stanford.edu>
;; Maintainer: Rob Riepel <riepel@snowflake.stanford.edu>
;; Keywords: sql sybase oracle

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Revision: $Id: sql-mode.el,v 2.0 1993/08/17 23:47:10 riepel Exp $

;;; Commentary:

;;  This file is implements a SQL-mode for emacs.  The code is a hacked up
;;  version of Lynn Slater's sql.el.  Some of the comments were pinched from
;;  Jim Lange's sqlplus.  Both are available from the elisp archives.
;;
;;  Sql-mode is for editing and testing SQL statements in a standard text
;;  buffer.  Sql-mode turns on abbrev-mode with abbreviations for common SQL
;;  keywords.  The most useful feature of sql-mode is sending SQL statements
;;  to a SQL interpretor.
;;
;;  The following commands can be added to a global initialization file or
;;  to any user's .emacs file to conveniently use sql-mode.
;;
;;      (autoload 'sql "sql-mode"
;;        "Start the interactive SQL interpretor in a new buffer." t)
;;
;;      (autoload 'sql-mode "sql-mode"
;;        "Mode for editing SQL files and running a SQL interpetror." t)
;;
;;      (setq auto-mode-alist (cons '("\\.sql$" . sql-mode) auto-mode-alist))
;;
;;  Use describe-mode while in sql-mode for further instructions.

;;; Code:


;;;  Revision Information

(defconst sql-revision "$Revision: 2.0 $")
(defconst sql-tvist-version "1.8")


;;;  Variables -

;; (defconst sql-emacs18-p (string-lessp emacs-version "19"))
(defconst sql-emacs18-p (< (string-to-int emacs-version) (string-to-int "19")))
(defconst sql-emacs19-p (not sql-emacs18-p))

(defvar sql-buffer-name "" "Buffer where SQL commands are run.")

(defvar sql-command "isql" "SQL interpretor program.")
(defvar sql-server "" "SQL server name.")
(defvar sql-username "" "SQL interpretor username.")
(defvar sql-process-name "" "SQL interpretor process name.")

(defvar sql-mode-abbrev-table nil "Abbrev table used in SQL mode buffers.")
(defvar sql-mode-map nil "Keymap used in SQL mode.")

(defvar sql-output-separator "@--"
  "String printed between sets of SQL command output.")

(defvar sql-separator-regexp "^[ \t]*go\\b"       ; For Sybase "isql" program
  ;;                         ";[ \t]*$"   	  ; For Oracle "sqlplus" program
  "Regexp used to seperate groups of SQL statements.")

(defvar sql-end-string "go" "String to insert at end of command")


;;;  Markers -

(defvar sql-buffer-mark (make-marker)
  "Marks the current SQL command in the SQL output buffer.")

(make-variable-buffer-local 'sql-buffer-mark)

(defvar sql-region-beginning-mark (make-marker)
  "Marks the beginning of the region to sent to the SQL process.")
(defvar sql-region-end-mark (make-marker)
  "Marks the end of the region to sent to the SQL process.")


;;;  SQL-mode Keymap -

(if (not sql-mode-map)
    (progn
      (setq sql-mode-map (make-sparse-keymap))
      (define-key sql-mode-map "\C-cg"    'sql-goto-error)
      (define-key sql-mode-map "\C-c\C-e" 'sql-buffer-erase)
      (define-key sql-mode-map "\C-cb"    'sql-buffer-bottom)
      (define-key sql-mode-map "\C-ct"    'sql-buffer-top)
      (define-key sql-mode-map "\C-cp"    'sql-buffer-prev-command)
      (define-key sql-mode-map "\C-cn"    'sql-buffer-next-command)
      (define-key sql-mode-map "\C-c\C-w" 'sql-buffer-display-window)
      (define-key sql-mode-map "\C-c\C-l" 'sql-buffer-redisplay-current)
      (define-key sql-mode-map "\C-c\C-b" 'sql-buffer-scroll-right)
      (define-key sql-mode-map "\C-c\C-f" 'sql-buffer-scroll-left)
      (define-key sql-mode-map "\C-c\C-p" 'sql-buffer-scroll-down)
      (define-key sql-mode-map "\C-c\C-n" 'sql-buffer-scroll-up)
      (define-key sql-mode-map "\C-c\C-i" 'sql-send-interrupt)
      (define-key sql-mode-map "\C-c\C-r" 'sql-send-region)
      (define-key sql-mode-map "\C-c\C-c" 'sql-send-current)
      (define-key sql-mode-map "\C-c\C-u" 'sql-set-user)))


;;;  SQL-mode Abbreviations -

(progn
  (define-abbrev-table 'sql-mode-abbrev-table ())
  (define-abbrev sql-mode-abbrev-table "arc" "archivelog"   nil)
  (define-abbrev sql-mode-abbrev-table "s"   "select"       nil)
  (define-abbrev sql-mode-abbrev-table "f"   "from"         nil)
  (define-abbrev sql-mode-abbrev-table "fr"  "from"         nil)
  (define-abbrev sql-mode-abbrev-table "w"   "where"        nil)
  (define-abbrev sql-mode-abbrev-table "o"   "order by"     nil)
  (define-abbrev sql-mode-abbrev-table "nu"  "number"       nil)
  (define-abbrev sql-mode-abbrev-table "da"  "date"         nil)
  (define-abbrev sql-mode-abbrev-table "co"  "connect"      nil)
  (define-abbrev sql-mode-abbrev-table "be"  "between"      nil)
  (define-abbrev sql-mode-abbrev-table "sy"  "synonym"      nil)
  (define-abbrev sql-mode-abbrev-table "tr"  "trigger"      nil)
  (define-abbrev sql-mode-abbrev-table "up"  "update"       nil)
  (define-abbrev sql-mode-abbrev-table "ins" "insert"       nil)
  (define-abbrev sql-mode-abbrev-table "gr"  "grant"        nil)
  (define-abbrev sql-mode-abbrev-table "gra" "grant all to" nil)
  (define-abbrev sql-mode-abbrev-table "pu"  "public"       nil)
  (define-abbrev sql-mode-abbrev-table "un"  "unique"       nil)
  (define-abbrev sql-mode-abbrev-table "cl"  "cluster"      nil)
  (define-abbrev sql-mode-abbrev-table "we"  "whenever"     nil)
  (define-abbrev sql-mode-abbrev-table "ta"  "table"        nil)
  (define-abbrev sql-mode-abbrev-table "pr"  "privileges"   nil)
  (define-abbrev sql-mode-abbrev-table "dr"  "drop"         nil)
  (define-abbrev sql-mode-abbrev-table "ro"  "rollback"     nil)
  (define-abbrev sql-mode-abbrev-table "rb"  "rollback"     nil)
  (define-abbrev sql-mode-abbrev-table "tr"  "transaction"  nil)
  (define-abbrev sql-mode-abbrev-table "us"  "using"        nil)
  (define-abbrev sql-mode-abbrev-table "u"   "using"        nil))


;;;  SQL-mode

(defun sql-get-going (pfx)
  "Command to open connections and open a scratch window"
  (interactive "P")
  (if (get-buffer sql-buffer-name) ()
    (sql))
  (switch-to-buffer-other-window (get-buffer sql-buffer-name))
  (if pfx
      (other-window -1)
    (switch-to-buffer-other-window (get-buffer-create "*SQL-scratch*"))
    (sql-mode)))


(defun sql-mode nil
  "
Mode for editing SQL files and running a SQL interpretor (such as
'isql' (from Sybase) or 'sqlplus' (from Oracle)).  Entry into this
mode runs the hook 'sql-mode-hook' It also enables abbrev-mode, with
abbreviations for SQL keywords.  Use '\\[list-abbrevs]' for a full
list.

Use 'M-x sql-mode' to invoke sql-mode for the current buffer.

Use 'M-x sql' to start the SQL interpretor.  Or, use '\\[sql-set-user]'
(sql-set-user) which sets the current user for the SQL interpretor
and starts it if necessary.  You can run the SQL interpretor
multiple times (as different users) and switch between them using
'\\[sql-set-user]' (sql-set-user).

To test a SQL statement, position the cursor on or near it, and
press '\\[sql-send-current]' to run it and display the results.

Mode Specific Bindings:
     \\{sql-mode-map} "

  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'sql-mode)
  (setq mode-name "SQL")
  (use-local-map sql-mode-map)
  (setq local-abbrev-table sql-mode-abbrev-table)
  (abbrev-mode 1)
  (setq abbrev-all-caps 1)
  (run-hooks 'sql-mode-hook))


;;;  Utilitities

(defun sql-echo-in-buffer (buffer-name string &optional force-display)
  "Displays string in the named buffer, creating the buffer if needed.
If force-display is true, the buffer will appear if not already shown."
  (let ((buffer (get-buffer-create buffer-name)))
    (if force-display (display-buffer buffer))
    (set-buffer buffer)
    (goto-char (point-max))
    (insert string)
    (if force-display
	(set-window-point (get-buffer-window buffer-name) (point-max)))))

(defun sql-verify-buffer nil
  "Generates reasonable error messages abouut the SQL connection."
  (if (not (get-buffer sql-buffer-name))
      (error "No SQL output buffer!  Use 'M-x sql' to initialize the SQL interpretor."))
  (if (not (get-buffer-process sql-buffer-name))
      (error "Buffer '%s' is not talking to anybody!" sql-buffer-name)))

(defun sql-send-strings (strings)
  "Sends strings to the SQL process.
Also shows the string at the top of the SQL output buffer."
  (sql-verify-buffer)
  (sql-echo-in-buffer sql-buffer-name
		      (concat "\n" sql-output-separator "\n\n"))
  (sql-buffer-bottom)
  (sql-buffer-mark-current)
  (sql-echo-in-buffer sql-buffer-name (apply 'concat strings))
  (sql-echo-in-buffer sql-buffer-name "\n\n")
  (let ((string (apply 'concat strings))
	(process  (get-buffer-process sql-buffer-name)))
    (send-string process (concat string "\n"))
    (if (eq (current-buffer) (process-buffer process))
	(set-marker (process-mark process) (point))))
  (sql-buffer-redisplay-current))

(defun sql-mark-current nil
  "Marks the current SQL for sending to the SQL process.
Marks are placed around a region defined by matching pairs
of the expression listed in 'sql-seperater-regexp."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (cond ((re-search-backward sql-separator-regexp nil t)
	   (goto-char (match-end 0)))
	  ((re-search-backward "^[ \t]*$" nil t)
	   (goto-char (match-end 0)))
	  (t
	   (goto-char (point-min))))
    (skip-chars-forward " \t\n")
    (setq sql-region-beginning-mark (copy-marker (point)))
    (re-search-forward sql-separator-regexp)
    (setq sql-region-end-mark (copy-marker (point)))))


;;;  Transmission Commands

(defun sql-send-region (start end)
  "Send a region to the SQL process."
  (interactive "r")
  (save-excursion
    (sql-send-strings (list (buffer-substring start end)))))

(defun sql-send-current (pfx)
  "Send the current SQL command(s) to the SQL process.
With prefix, insert end marker first."
  (interactive "P")
  (if pfx (save-excursion
	    (skip-chars-backward " \t")
	    (if (/= (current-column) 0)
		(progn
		  (end-of-line)
		  (insert "\n")))
	    (insert sql-end-string "\n")))
  (sql-mark-current)
  (sql-send-region sql-region-beginning-mark sql-region-end-mark))


;;;  SQL-Output Buffer Operations -

(defun sql-show-buffer (&optional fcn &rest args)
  "Makes the SQL output buffer visible in the other window."
  (interactive)
  (sql-verify-buffer)
  (let ((window  (selected-window)))
    (if (not (eq (window-buffer window) (get-buffer sql-buffer-name)))
	(switch-to-buffer-other-window sql-buffer-name))
    (if fcn (condition-case nil (apply fcn args) (error nil)))
    (select-window window)))

(fset 'sql-buffer-display-window 'sql-show-buffer)

(defun sql-buffer-scroll-up nil
  "Scroll-up in the SQL output buffer window."
  (interactive)
  (sql-show-buffer 'scroll-up))

(defun sql-buffer-scroll-down nil
  "Scroll-down in the SQL output buffer window."
  (interactive)
  (sql-show-buffer 'scroll-down))

(defun sql-buffer-scroll-left (num)
  "Scroll-left in the SQL output buffer window."
  (interactive "p")
  (sql-show-buffer 'scroll-left (* num (/ (window-width) 2))))

(defun sql-buffer-scroll-right (num)
  "Scroll-right in the SQL output buffer window."
  (interactive "p")
  (sql-show-buffer 'scroll-right (* num (/ (window-width) 2))))

(defun sql-buffer-mark-current nil
  "Mark the current position in the SQL output window."
  (sql-show-buffer 'sql-buffer-make-mark))

(defun sql-buffer-make-mark nil
  "Set the sql-buffer-marker."
  (setq sql-buffer-mark (copy-marker (point))))

(defun sql-buffer-redisplay-current nil
  "Go to the current sql-buffer-mark."
  (interactive)
  (sql-show-buffer 'sql-goto-mark))

(defun sql-goto-mark nil
  (goto-char sql-buffer-mark)
  (recenter 0))

(defun sql-buffer-top nil
  "Goto the top of the SQL output buffer."
  (interactive)
  (sql-show-buffer 'sql-beginning-of-buffer))

(defun sql-beginning-of-buffer nil (goto-char (point-min)))

(defun sql-buffer-bottom nil
  "Goto the bottom of the SQL output buffer."
  (interactive)
  (sql-show-buffer 'sql-end-of-buffer))

(defun sql-end-of-buffer nil (goto-char (point-max)) (recenter -1))

(defun sql-buffer-erase nil
  "Clear the SQL output buffer."
  (interactive)
  (sql-show-buffer 'erase-buffer))

(defun sql-buffer-next-command nil
  "Search for the next command in the SQL output buffer."
  (interactive)
  (sql-show-buffer 'sql-next-command))

(defun sql-next-command nil
  "Search for the next command in the SQL output buffer."
  (cond ((re-search-forward  sql-output-separator nil t)
	 (forward-line 2)
	 (recenter 0))
	(t (beep) (message "No more commands."))))

(defun sql-buffer-prev-command nil
  "Search for the previous command in the SQL output buffer."
  (interactive)
  (sql-show-buffer 'sql-previous-command))

(defun sql-previous-command nil
  "Search for the previous command in the SQL output buffer."
  (let ((start (point)))
    (re-search-backward  sql-output-separator nil t)
    (cond ((re-search-backward  sql-output-separator nil t)
	   (forward-line 2)
	   (recenter 0))
	  (t
	   (message "No more commands.") (beep)
	   (goto-char start)))))

(defun sql-send-interrupt nil
  "Send an interrupt the the SQL interpretor process."
  (interactive)
  (interrupt-process sql-process-name))


;;;  Miscellaneous

(defun sql-goto-error (n)
  "Moves to the n'th line in the most recently executed SQL."
  (interactive "NLine number of error: ")
  (goto-char sql-region-beginning-mark)
  (forward-line (1- n)))

(defun sql-insert-gos nil
  "Inserts 'go' statements between each apparent block of SQL code."
  (interactive)
  (while (not (eobp))
    (forward-line 1)
    (if (and (looking-at "[a-z]") (not (looking-at "go")))
	(progn (insert "go\n")))))


;;;  SQL Interpertor

(defun sql nil
  "Start the interactive SQL interpretor in a new buffer."
  (interactive)
  (sql-set-server-and-user)
  (get-buffer-create sql-buffer-name)            ; move to the buffer.
  (or (get-buffer-process sql-buffer-name)       ; already got a process?
   (let ((pstring (sql-ange-ftp-read-passwd
		   "Password or blank: ")))	 ; no, start it up!
     (set-buffer sql-buffer-name)
     (start-process sql-process-name sql-buffer-name sql-command "-w"
		    "2048" "-n" "-S" sql-server "-U" sql-username
		    (if (> (length pstring) 0) (concat "-P" pstring) ""))
     (if (> (length pstring) 0) ()
       (process-send-string sql-buffer-name
			    (concat (sql-ange-ftp-read-passwd
				     "Password (for real!): ")
				    "\n")))))
  (set-buffer sql-buffer-name)
  (sql-show-buffer)
  )

(defun sql-set-server-and-user nil
  "Set server, username, process, and buffer for the SQL interpretor."
  (interactive)
  (call-interactively 'sql-get-server)
  ;; skip username query if ``server-user'' was input.
  (let ((stroke (string-match "/" sql-server)))
    (cond (stroke
	   (setq sql-username (substring sql-server (1+ stroke) nil))
	   (setq sql-server (substring sql-server 0 stroke)))
	  (t
	   (call-interactively 'sql-get-username))))
  (setq sql-process-name (concat sql-server "/" sql-username))
  (setq sql-buffer-name
	(concat "*" sql-process-name "*")))

(defun sql-get-server (server)
  "Set the SQL server name."
  (interactive (list (sql-string-prompt "Server: " 'sql-get-server-hist 2)))
  (setq sql-server server))

(defun sql-get-username (username)
  "Set the SQL server username."
  (interactive (list (sql-string-prompt "Username: " 'sql-get-username-hist 2)))
  (setq sql-username username))

(defun sql-string-prompt (prompt &optional symbol offset)
  "Read a string using PROMPT.
With optional HIST-SYMBOL and HIST-OFFSET, read with history."
  (cond
   (sql-emacs19-p
    (cond
     (symbol
      (let ((initial nil) (history nil))
	(if (and offset
		 (setq initial (nth (- offset 1) (symbol-value symbol))))
	    (setq history (cons symbol offset))
	  (setq history symbol))
	(read-from-minibuffer prompt initial nil nil history)))
     (t (read-string prompt))))
   (t
    (if (and symbol (featurep 'gmhist))
	(read-with-history-in symbol prompt)
      (read-string prompt)))))

(defun sql-ange-ftp-read-passwd (prompt &optional default)
  "Read a password from the user. Echos a . for each character typed.
End with RET, LFD, or ESC. DEL or C-h rubs out.  ^U kills line.
Optional DEFAULT is password to start with."
  (let ((pass (if default default ""))
	(c 0)
	(echo-keystrokes 0)
	(cursor-in-echo-area t))
    (while (and (/= c ?\r) (/= c ?\n) (/= c ?\e))
      (message "%s%s"
	       prompt
	       (make-string (length pass) ?.))
      (setq c (read-char))
      (if (= c ?\C-u)
	  (setq pass "")
	(if (and (/= c ?\b) (/= c ?\177))
	    (setq pass (concat pass (char-to-string c)))
	  (if (> (length pass) 0)
	      (setq pass (substring pass 0 -1))))))
    (sql-ange-ftp-repaint-minibuffer)
    (substring pass 0 -1)))

(setq sql-ange-ftp-tmp-keymap (make-sparse-keymap))
(define-key sql-ange-ftp-tmp-keymap "\C-m" 'exit-minibuffer)

(defun sql-ange-ftp-repaint-minibuffer nil
  "Gross hack to set minibuf_message = 0, so that the contents of the
minibuffer will show."
  (if (eq (selected-window) (minibuffer-window))
      (if (fboundp 'allocate-event)
	  ;; lemacs
	  (let ((unread-command-event (character-to-event ?\C-m
							  (allocate-event)))
		(enable-recursive-minibuffers t))
	    (read-from-minibuffer "" nil sql-ange-ftp-tmp-keymap nil))
	;; v18 GNU Emacs
	(let ((unread-command-char ?\C-m)
	      (enable-recursive-minibuffers t))
	  (read-from-minibuffer "" nil sql-ange-ftp-tmp-keymap nil)))))

(fset 'sql-set-user 'sql)


;;;  Histification

(cond
 (sql-emacs18-p

  ;; Emacs version 18, must use gmhist for history -
  ;; See if gmhist is already loaded, otherwise, attempt to load it.
  ;; If it's not available, do nothing.

  (cond
   ((or (featurep 'gmhist) (load "gmhist" t t))
    (put 'sql-get-server-hist 'no-default t)
    (put 'sql-get-server-hist 'cursor-end t)
    (put 'sql-get-server-hist 'backup 2)
    (put 'sql-get-server-hist 'initial-hist
	 (list (or (getenv "DSQUERY") "SYBASE")))

    (put 'sql-get-username-hist 'no-default t)
    (put 'sql-get-username-hist 'cursor-end t)
    (put 'sql-get-username-hist 'backup 2)
    (put 'sql-get-username-hist 'initial-hist (list (user-login-name))))))

 (t

  ;; Emacs version 19, use built-in history -

  (setq sql-get-server-hist (make-list 2 (or (getenv "DSQUERY") "SYBASE")))
  (setq sql-get-username-hist (make-list 2 (user-login-name)))))


(provide 'sql-mode)

;;; sql-mode.el ends here
