;;; lretpl.el  --- generic templates and syntax help for programming

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author:		(c) <Lars.Reed@mesan.no>
;; Last-Modified:	98/04/07
;; Version:		$Id: lretpl.el 2.8 1998/06/08 07:36:40 larsr Exp $
;; Keywords:		programming templates help
;; Adapted-By:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file is NOT part of GNU Emacs.
;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;
;;  History:
;;    $Log: lretpl.el $
;;Revision 2.8  1998/06/08  07:36:40  larsr
;;  - No change...
;;
;;Revision 2.7  1998/06/08  07:36:40  larsr
;;  - Misc.  brushup
;;
;;Revision 2.6  1998/06/07  20:13:18  larsr
;;  - working lretpl-add-tpl...
;;
;;Revision 2.5  1998/06/07  13:03:42  larsr
;;  - added lretpl-add-tpl
;;  - added lretpl-reg-fast
;;  - more Java keywords
;;
;;Revision 2.4  1998/04/17  20:56:00  larsr
;; - New mode-line indicator
;;Revision 2.3  1998/04/17  20:43:42  larsr
;; - History included
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defconst lretpl--mode-version "$Revision: 2.8 $")

(defconst lretpl-mode-version (substring lretpl--mode-version
					 11
					 (- (length lretpl--mode-version) 2))
  "Current version of lretpl mode.")

(eval-and-compile
  (if (and (featurep 'custom)
	   (fboundp 'custom-declare-variable))
      nil
    (defmacro defgroup (&rest args) nil)
    (defmacro defface  (var values doc &rest args) (` (make-face (, var))))
    (defmacro defcustom (var value doc &rest args)
      (` (defvar (, var) (, value) (, doc))))))

;;; Code:
;; ========================================================================

;; User options

(defgroup lretpl nil
  "generic templates and syntax help for programming"
  :group 'tools
  :group 'templates)

(defcustom lretpl-tmpl-help nil
  "* Set to t to give help during template expansion."
  :type 'boolean
  :group 'lretpl)

(defcustom lretpl-no-no-help nil
  "* Set to t to silence \"no help\" messages (just be quiet)."
  :type 'boolean
  :group 'lretpl)

(defcustom lretpl-record-file
  (cond ((fboundp 'convert-standard-filename)
	 (convert-standard-filename "~/lretpl.add"))
	(t "~/lretpl.add"))
  "*Name of the file that records `lretpl-add-tpl' values.
nil to avoid recording."
  :type 'file
  :group 'lretpl)

(defcustom lretpl-date-sep "/"
  "*String to separate day/month/year."
  :type 'string
  :group 'lretpl)

;; Global variables

(defconst lretpl-mode-name " |"
  "Name used in mode-line.")

(defvar lretpl-mode nil
  "Non-nil when command `lretpl-mode' is activated.")

(defvar lretpl-mode-hook nil
  "Functions to call after activating `lretpl-mode'.")

(defvar lretpl-mode-off-hook nil
  "Functions to call after deactivating `lretpl-mode'.")

(defvar lretpl-tmpl-key "|"
  "Key to trigger template expansion \(`lretpl-templates'\)
If this is changed to an unprintable key code, `lretpl-tmpl-undefined-action'
should be changed also.")

(defvar lretpl-tmpl-undefined-action 'insert
  "Action when expansion is unsuccessful.
The default action (given by \'insert) is to insert `lretpl-key'.
The other possible values are nil \(ignore\) and \'error, to either
completely ignore the situation, or report an error.")


(defconst lretpl-date-y (format "%04d"
			       (string-to-int
				(substring (current-time-string) 20 24))))
(defconst lretpl-date-m
  (cdr (assoc (substring (current-time-string) 4 7)
	      '(("Jan" . "01") ("Feb" . "02") ("Mar" . "03")
		("Apr" . "04") ("May" . "05") ("Jun" . "06")
		("Jul" . "07") ("Aug" . "08") ("Sep" . "09")
		("Oct" . "10") ("Nov" . "11") ("Dec" . "12")))))
(defconst lretpl-date-d (format "%02d"
			       (string-to-int
				(substring (current-time-string) 8 10))))

(defun lretpl--conc-date (&optional psep pabbr pseq py pm pd)
  "Create date string based on selected format..."
  (let* ((sep (if psep (if (stringp 'psep) psep lretpl-date-sep) nil))
	 (Y (or py lretpl-date-y))
	 (y (if pabbr (substring Y 2) Y))
	 (m (or pm lretpl-date-m))
	 (d (or pd lretpl-date-d)))
    (if pseq (concat y sep m sep d)
      (concat d sep m sep y))))

(defvar lretpl-tmpl-global-list
  (list '("author" "Lars.Reed@ECsoft.no")
	(list "user" (user-login-name))
	(list "mailto" (concat "mailto:" (or user-mail-address
					     (and (fboundp 'user-mail-address)
						  (user-mail-address))
					     (concat (user-login-name)
						     "@"
						     (system-name)))))
	(list "date"   (lretpl--conc-date))
	(list "fdate"  (lretpl--conc-date t))
	(list "sdate"  (lretpl--conc-date nil t))
	(list "rdate"  (lretpl--conc-date nil nil t))
	(list "rfdate" (lretpl--conc-date t   nil t))
	(list "rsdate" (lretpl--conc-date nil t   t)))
  "Global additions to `lretpl-tmpl-mode-list' (which see).")

(defvar lretpl-help-global-list nil
  "Global additions to `lretpl-help-mode-list' (which see).")

;; Buffer local variables

(defvar lretpl-tmpl-case nil
  "Set to t to have case sensisitve template expansion.")
(make-variable-buffer-local 'lretpl-tmpl-case)

(defvar lretpl-help-key "\C-c?"
  "Key to trigger syntax help.")
(make-variable-buffer-local 'lretpl-tmpl-help-key)

(defvar lretpl-cont-string " \\"
  "Line continuation string.")
(make-variable-buffer-local 'lretpl-cont-string)

(defvar lretpl-tmpl-mode-list ()
  "Abbreviations list. This is a list of the form:
  \(\"<abbreviation>\" <template>\)
where <template> may contain one or more of the following:
   \"<string>\"    - inserted literally (first character \\e means \"always
 .                 preserve case\"\)
   |             - record the position of point, to be set when
                   expansion has finished \(note: only the first point setting
		   during an expansion is kept\)
   /             - new, indented line
   *             - reindent current line
   [<vector>]	 - executed as a keyboard macro
   <n>           - indent <n> spaces, or delete <n> characters backwards,
		   if <n> is negative
   \(\"<string>\" [ . \"<default>\"]\)
                 - a string is prompted for and inserted \(with optional
		  default value\)
   \(: [ . <n>]\)  - insert `lretpl-cont-string' and a newline, then indent
		     to underneath the <n>th \(default first - n=1\) word
		     on the previous line \(with `indent-relative'\)
   \(= . \"<tmpl>\"\)
                 - the <tmpl> is expanded recursively \(note:
		   `lretpl--current-tmpl' is not changed\)
   \(help . \"<string>\"\)
                 - the <string> is sent to `lretpl-do-syntax-help', unless
		   `lretpl-tmpl-help' is nil
   <command>     - the command is executed interactively
   \(<function> [<args>]\)
                 - a function call with the given, evaluated arguments
                   \(if any\)
   anything else - ignored \(but reported\)

E.g:
  \(setq lretpl-tmpl-mode-list
      \'\(\(\"fi\" \"end if\" *\)
         \(\"if\" \"if \" | / \(= . \"fi\"\) (help . \"if\"\)\)
         \(\"wh\" \"while \" | / \"end while\" *\)\)\)
The second entry is expanded when \"if\\[lretpl-templates]\" is given,
and is executed like this:
   insert the string \"if \"
   record position of point
   insert & indent a new line
   expand the first entry recursively, i.e. insert \"end if\" and reindent
   implicitly: return point to the end of the previous line \(after \"if \"\)

The abbreviation must always be in lowercase when defined \(upper and mixed
case may be used for definitions used only with recursion\).
If `lretpl-tmpl-case' is `t', the case used when expanding literal strings
within the templates depends on how the abbreviation is written - e.g.:
       \"if\" ==> \"if \" & \"end if\"
       \"If\" ==> \"If \" & \"End If\"
       \"IF\" ==> \"IF \" & \"END IF\"
To force a string to keep a certain case, start it with \\e.
If `lretpl-tmpl-case' is nil, strings are always inserted verbatim.
")
;; Special for each buffer
(make-variable-buffer-local 'lretpl-tmpl-mode-list)

(defvar lretpl-tmpl-buffer-list nil
  "Buffer specific additions to `lretpl-tmpl-mode-list'.")
(make-variable-buffer-local 'lretpl-tmpl-buffer-list)

(defvar lretpl-tmpl-regexp "[^-+/*a-zA-Z0-9?]+"
  "Expression that matches the first character before an abbrev
when searching backwards.")
(make-variable-buffer-local 'lretpl-tmpl-regexp)

(defvar lretpl-help-mode-list nil
  "List of syntax help strings.
The format of this list is a series of string sublists,
containing a help key (always in lowercase) followed by 1 or more help
strings, i.e.:
   (
    (\"<keyA>\" \"<stringA1>\" [... \"<stringAN>\"])
    ...
    (\"<keyZ>\" \"<stringZ1>\" [ ... \"<stringZN>\"])
   )
For each help string, a reference to another topic may be defined instead.
This is done by inserting a cons cell consisting of an equals sign followed
by the referenced topic name:
     (= . \"topic\")
E.g.:
 (setq lretpl-help-mode-list
       '((\"restart-label\" (= . \"restart\")) ; Alias
         (\"restart from\"  (= . \"restart\")) ; Alias
         (\"restart\"       \"Syntax: RESTART-LABEL <label>\"
			    \"    or: RESTART FROM <label>\"
			    \"where <label> is an integer between 1 and 4999\")
         ))")
(make-variable-buffer-local 'lretpl-help-mode-list)

(defvar lretpl-help-buffer-list nil
  "Buffer specific additions to `lretpl-help-mode-list'.")
(make-variable-buffer-local 'lretpl-help-buffer-list)

(defvar lretpl-mode-list nil
  "List of mode bindings in lretpl.
Each major mode using lretpl should register itself using
`lretpl-register-mode', which in turn updates this list.
The list consists of sublists containing the following elements:
   0. Major mode
   1. Template list, used for `lretpl-tmpl-mode-list'
   2. Help list, used for `lretpl-help-mode-list'
   3. Template regexp, used for `lretpl-tmpl-regexp'
   4. Template case, used for `lretpl-tmpl-case'
   5. Cont. strinc, used for `lretpl-cont-string'
   6. Help key, used for `lretpl-help-key'
Each of these may be 'default, in which case the current (default or locally
set) value is used.")

;; Internal variables

(defvar lretpl--current-tmpl ""
  "A string containing the current abbreviation when expanding templates.
Can be used by your own template-handling functions.")

(defvar lretpl--current-tmpl-point nil
  "Contains the position of point after template insertion.")

(defvar lretpl-tmpl-history nil
  "A list containing replies to template prompts.")

(defvar lretpl-help-history nil
  "A list containing replies to help prompts.")

;; Display name
(or (assq 'lretpl-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons '(lretpl-mode lretpl-mode-name)
                minor-mode-alist)))


; ======================================================================


;; Different GNU & XEmacs versions disagree on these:
(fset 'lretpl-capitalize (if (fboundp 'upcase-initials)
			     'upcase-initials
			   'capitalize))
(fset 'lretpl-buffer-substring (if (fboundp 'buffer-substring-no-properties)
				   'buffer-substring-no-properties
				 'buffer-substring))

(defun lretpl-indent-line ()
  "Reindent current line."
  (if (null indent-line-function)
      (indent-relative-maybe)
    (funcall indent-line-function)))

(defun lretpl-indent-new-line ()
  "Reindent the current line, insert a newline and indent the new line."
  (interactive "*")
  (lretpl-indent-line)
  (end-of-line)
  (newline)
  (lretpl-indent-line))

(defun lretpl-insert-undefined (&rest args)
  "Inserts template key literally, if it is printable,
and limited by `lretpl-tmpl-undefined-action'"
  (message (car args) (cdr args))
  (cond ((and (eq lretpl-tmpl-undefined-action 'insert)
	      (stringp lretpl-tmpl-key))
	 (insert  lretpl-tmpl-key))
	((not lretpl-tmpl-undefined-action)
	 t)
	(t
	 (error (car args) (cdr args)))))


(defun lretpl--describe-ins (&rest s)
  "Insert description strings in help buffer."
  (let ((print-escape-newlines t))
    (insert (mapconcat 'identity s ""))))


(defun lretpl--describe-1 (tmpl)
  "Describes an element TMPL of a template definition.
See the description of `lretpl-tmpl-mode-list' and `lretpl-template-exec'
for further information."
  (cond ((null tmpl) t)
	((and (stringp tmpl)		; String with case
	      (string= (substring tmpl 0 1) "\e"))
	 (lretpl--describe-ins "\"" (substring tmpl 1) "\""))
	((stringp tmpl)			; String
	 (lretpl--describe-ins "\"" tmpl "\""))
	((eq '| tmpl)			; Point
	 (lretpl--describe-ins "|"))
	((eq '/ tmpl)			; Newline
	 (lretpl--describe-ins "\\t\\n\\t"))
	((eq '* tmpl)			; Reindent
	 (lretpl--describe-ins "\\t"))
	((vectorp tmpl)			; Macro
	 (lretpl--describe-ins (prin1-to-string tmpl)))
	((integerp tmpl)		; Indent
	 (lretpl--describe-ins (format "%d" tmpl)))
	((and (listp tmpl)
	      (stringp (car tmpl)))	; Prompt & insert
	 (lretpl--describe-ins "{" (car tmpl))
	 (if (cdr tmpl)
	     (lretpl--describe-ins "?" (cdr tmpl)))
	 (lretpl--describe-ins "}"))
	((and (listp tmpl)
	      (eq ': (car tmpl)))	; Split line
	 (lretpl--describe-ins " :\\n"
			      (format "%d\\t" (if (cdr tmpl) (cdr tmpl) 1))))
	((and (listp tmpl)
	      (eq '= (car tmpl)))	; Recursive...
	 (lretpl--describe-ins "(= " (cdr tmpl) ")"))
	((and (listp tmpl)
	      (eq 'help (car tmpl)))	; Help
	 t)				; ignore
	((and (symbolp tmpl)
	      (commandp tmpl))		; Interactive command
	 (lretpl--describe-ins "\'" (symbol-name tmpl)))
	(t (lretpl--describe-ins (prin1-to-string tmpl) )))
  tmpl)					; The argument is returned as is



(defun lretpl-describe-templates (&optional subfun-p)
  "Overview of lretpl mode template table.
If optional SUBFUN-P is non-nil, behaves as a subfunction of another
help command."
  (interactive)
  (let ((curr-buf (current-buffer))
	(bind-tpl (substitute-command-keys "'\\[lretpl-templates]'"))
	(bind-hlp (substitute-command-keys "'\\[lretpl-describe-templates]'"))
	(bind-syn (substitute-command-keys "'\\[lretpl-syntax-help]'"))
	(mode-ver  lretpl-mode-version)
	(mode-desc (symbol-name major-mode))
	(case-desc (if lretpl-tmpl-case "ON" "OFF"))
	(help-desc (if lretpl-tmpl-help "ON" "OFF"))
	(act-desc  (cond ((eq lretpl-tmpl-undefined-action 'insert)
			  "insert key")
			 ((null lretpl-tmpl-undefined-action)
			  "ignore")
			 (t "error")))
	(tmpl-list (append lretpl-tmpl-buffer-list
			  lretpl-tmpl-mode-list
			  lretpl-tmpl-global-list)))
    (if (null tmpl-list)
	(message "No abbreviations!")
      (pop-to-buffer (get-buffer-create "*Help*"))
      (let ((was-r-o buffer-read-only)) ; Remember if read-only
	(if subfun-p
	    (goto-char (point-max))	; Append
	  (erase-buffer))		; - or empty
	(toggle-read-only 0)		; Not read-only
	(insert (if subfun-p "\n\n" "")
                "lretpl-mode version                    " mode-ver   "\n"
		"Settings for:                          " mode-desc  "\n"
                "Key binding for template expansion:    " bind-tpl   "\n"
                "Key binding for syntax help:           " bind-hlp   "\n"
                "Key binding for mode help:             " bind-syn   "\n"
                "Case sensitivity:                      " case-desc  "\n"
                "Automatic syntax help when expanding:  " help-desc  "\n"
                "Action on undefined template:          " act-desc   "\n"
		"\nThe following abbreviations are defined:\n\n"
		"Abbrev\tContents\n"
		"=======\t"
		"==========================================================="
		"\n")
	(mapcar
	 (function (lambda (elem) ; Insert description of a single element
		     (insert (car elem) "\t")
		     (mapcar 'lretpl--describe-1 (cdr elem))
		     (insert "\n")))
	 tmpl-list)
	(goto-char 1)
	(forward-paragraph 2)
	(if was-r-o (toggle-read-only 1))
	(pop-to-buffer curr-buf)))))

(defun lretpl--tmpl-exec (tmpl)
  "(Recursively) expand a template definition TMPL.
See the description of `lretpl-tmpl-mode-list' for a description of how
arguments are handled."
  (cond ((null tmpl) t)			; Ignore null args
	((stringp tmpl)			; Insert string, according to case
	 (insert (cond ((string= (substring tmpl 0 1) "\e")
			(substring tmpl 1))
		       ((not lretpl-tmpl-case)
			tmpl)
		       ((string= (upcase lretpl--current-tmpl)
				 lretpl--current-tmpl)
			(upcase tmpl))
		       ((string= (downcase lretpl--current-tmpl)
				 lretpl--current-tmpl)
			(downcase tmpl))
		       ((string= (lretpl-capitalize lretpl--current-tmpl)
				 lretpl--current-tmpl)
			(lretpl-capitalize tmpl))
		       (t
			tmpl))))
	((eq '| tmpl)			; Record point
	 (if (null lretpl--current-tmpl-point)
	     (setq lretpl--current-tmpl-point (point-marker))))
	((eq '/ tmpl)			; Newline & indent
	 (lretpl-indent-new-line))
	((eq '* tmpl)			; Reindent
	 (lretpl-indent-line))
	((vectorp tmpl)			; Execute kbd-macro
	 (command-execute tmpl))
	((integerp tmpl)
	 (if (> tmpl 0)
	     (insert-char ?\  tmpl)	; Indent
	   (backward-delete-char-untabify (- tmpl)))) ; Unindent
	((and (listp tmpl)		; List starting with string -
	      (stringp (car tmpl)))	; prompt & insert
	 (insert (read-string (concat (car tmpl) ": ")
			      (cdr tmpl)
			      'lretpl-tmpl-history)))
	((and (listp tmpl)
	      (eq ': (car tmpl)))	; Break line
	 (insert lretpl-cont-string "\n")
	 (if (not (cdr tmpl)) (indent-relative)
	   (let ((iterator (cdr tmpl)))
	     (while (> iterator 0)
	       (setq iterator (1- iterator))
	       (indent-relative)))))
	((and (listp tmpl)
	      (eq '= (car tmpl)))	; Recursive...
	 (mapcar 'lretpl--tmpl-exec
		 (cdr
		  (assoc (cdr tmpl)
			 (append lretpl-tmpl-buffer-list
				 lretpl-tmpl-mode-list
				 lretpl-tmpl-global-list)))))
	((and (listp tmpl)
	      (eq 'help (car tmpl)))	; Help!
	 (if (and lretpl-tmpl-help
		  (or lretpl-help-buffer-list
		      lretpl-help-mode-list
		      lretpl-help-global-list))
	     (lretpl-give-syntax-help (cdr tmpl))))
	((and (symbolp tmpl)
	      (commandp tmpl))		; Run interactive command
	 (call-interactively tmpl))
	((and (listp tmpl)
	      (fboundp (car tmpl)))	; A function (with args)
	 (apply (car tmpl) (cdr tmpl)))
	(t				; Unknown - ignore but report
	 (message "unknown argument -- `%s'" (prin1-to-string tmpl))))
  tmpl)					; The argument is returned as is


(defun lretpl-templates (&optional pfx)
  "Expand lretpl abbreviations."
  (interactive "*P")
  (setq prefix-arg pfx			; Keep for tmpl-command
	lretpl--current-tmpl nil)	; None yet
  (if (and (null lretpl-tmpl-buffer-list)
	   (null lretpl-tmpl-mode-list)
	   (null lretpl-tmpl-global-list))
      (lretpl-insert-undefined "No appropriate templates installed")
    (let ((pt (point))
	  fun)
      (save-excursion
	(if (re-search-backward lretpl-tmpl-regexp nil t nil)
	    (progn			; An abbrev was found
	      (setq lretpl--current-tmpl (lretpl-buffer-substring
					   (match-end 0) pt)
		    fun (cdr (assoc (downcase lretpl--current-tmpl)
				    (append lretpl-tmpl-buffer-list
					    lretpl-tmpl-mode-list
					    lretpl-tmpl-global-list)))))))
      (if (not fun)
	  (lretpl-insert-undefined "no abbreviation for `%s'"
				    lretpl--current-tmpl)
	(setq lretpl--current-tmpl-point nil) ; Default position
	(kill-region (match-end 0) pt)	     ; Remove tmpl name
	(mapcar 'lretpl--tmpl-exec fun)	     ; Expand
	(if lretpl--current-tmpl-point
	    (goto-char lretpl--current-tmpl-point))	 ; Set point
	(setq prefix-arg ())))))

; ======================================================================

(defun lretpl--syntax-help-ins (s)
  "Insert a help string, or expand an alias."
  (cond ((stringp s) (insert s "\n"))
	((and (consp s)
	      (eq '= (car s))
	      (stringp (cdr s)))
	 (lretpl-give-syntax-help (cdr s) t))))


(defun lretpl-give-syntax-help (topic &optional no-heading)
  "Give help on named syntactic construction TOPIC."
  (interactive "*sTopic: ")
  (if (and (null lretpl-help-buffer-list)
	   (null lretpl-help-mode-list)
	   (null lretpl-help-global-list))
      (or lretpl-no-no-help
	  (message "No appropriate help topics are defined"))
    (let ((curr-buf (current-buffer))
	  strings)
      (setq strings (cdr (assoc (downcase topic)
				(append lretpl-help-buffer-list
					lretpl-help-mode-list
					lretpl-help-global-list))))
      (if (null strings)
	  (or lretpl-no-no-help
	      (message "No help for `%s'" topic))
	(if no-heading ()
	    (pop-to-buffer (get-buffer-create "*Help*"))
	    (toggle-read-only 0)
	    (erase-buffer)
	    (insert "*** " topic ":\n\n"))
	(mapcar 'lretpl--syntax-help-ins strings)
	(if no-heading ()
	  (goto-char (point-min))
	  (pop-to-buffer curr-buf))))))


(defun lretpl-syntax-help ()
  "Give help on Lretpl topic.
Tries to guess topic according to cursor position."
  (interactive)
  (let (topic begin)
    (save-excursion
      (skip-syntax-backward "w_")
      (setq begin (point))
      (skip-syntax-forward "w_")
      (setq topic (lretpl-buffer-substring begin (point))))
    (setq topic (read-string "Help topic: "
			     topic
			     'lretpl-help-history))
    (lretpl-give-syntax-help topic)))

; ======================================================================

(defun lretpl--add-record (modesym abbrs)
  "Record addition for later inclusion in .emacs or lretpl.el."
  (if (not lretpl-record-file) ()
    (set-buffer (get-buffer-create " *lretpl records*"))
    (delete-region (point-min) (point-max))
    (if (file-readable-p lretpl-record-file)
	(insert-file-contents lretpl-record-file))
    (insert "MODE:\t"   (symbol-name modesym) "\n"
	    "KEY:\t"    (car abbrs)	    "\n"
	    "ABBREV:\t")
    (prin1 (cdr abbrs) (current-buffer))
    (insert "\n\n")
    (write-file lretpl-record-file)
    (kill-buffer (current-buffer))
    (message "Copy stored in %s !" lretpl-record-file)))


(defun lretpl--add-tpl (modesym key tmpl-list)
  "Install another template for a given major mode.
See `lretpl-tmpl-mode-list' for details."
;;   (interactive (list (read-minibuffer "Mode: " (symbol-name major-mode))
;; 			(read-from-minibuffer "Key: ")
;; 			(read-from-minibuffer "Expansion: " nil nil t)))
  (if (null modesym)
      (error "Missing MODESYM")
    (let ((current (assq modesym lretpl-mode-list)))
      (if (not current)
	  (error "Mode has no templates"))
      (let* ((new-abbrs (list (append (list key) tmpl-list)))
	     (abbrs (append new-abbrs (car (cdr current)))))
	(lretpl--add-record modesym (car new-abbrs))
	(setcar (cdr current) abbrs))
      (lretpl-set-according-to-mode))))

(defun lretpl-add-tpl (key &rest tpl)
  "Add abbreviation KEY to current mode."
  (interactive "sKey: \nXLisp expression (must evaluate to a list): ")
  (lretpl--add-tpl major-mode key (car tpl)))

(defun lretpl-register-mode (modesym
			     &optional t-list h-list regex t-case cont-str
				       h-key)
  "Install lretpl-definitions for major mode MODESYM.
See `lretpl-mode-list' for details."
  (if (null modesym) ()
    (let ((current (assq modesym lretpl-mode-list)))
      (if current
	  (setq lretpl-mode-list (delete current lretpl-mode-list))))
    (setq lretpl-mode-list
	  (append (list (list modesym
			      t-list h-list regex t-case cont-str h-key))
		  lretpl-mode-list))))

(defmacro lretpl---swap-nil-default (sym)
  "Swap nil and default, otherwise leave alone."
  `(if (null ,sym) 'default (if (eq ,sym 'default) nil ,sym)))

(defun lretpl-reg-fast (modesym    t-list
			&optional  h-list regex  t-case cont-str h-key)
  "Like `lretpl-register-mode', but inserts default for nil arguments,
and vice versa for arguments following H-LIST."
  (lretpl-register-mode modesym t-list h-list
			(lretpl---swap-nil-default regex)
			(lretpl---swap-nil-default t-case)
			(lretpl---swap-nil-default cont-str)
			(lretpl---swap-nil-default h-key)))

(defmacro lretpl---set-if (n var)
  "Set VAR to Nth element of v-list, unless equal to `default'."
  `(let ((tmp-val (nth ,n v-list)))
     (if (not (eq tmp-val 'default))
	 (setq ,var tmp-val))))

(defun lretpl-set-according-to-mode ()
  "Set local variables according to major mode."
  (let ((v-list (cdr (assq major-mode lretpl-mode-list))))
    (if (null v-list) ()
      (lretpl---set-if 0 lretpl-tmpl-mode-list)
      (lretpl---set-if 1 lretpl-help-mode-list)
      (lretpl---set-if 2 lretpl-tmpl-regexp)
      (lretpl---set-if 3 lretpl-tmpl-case)
      (lretpl---set-if 4 lretpl-cont-string)
      (lretpl---set-if 5 lretpl-help-key))))


; ======================================================================

;;;###autoload
(defun lretpl-mode (arg)
  "Toggle lretpl minor mode.
With ARG, turn lretpl mode on iff arg is positive.
The mode, whose commands all have prefix \\[lretpl-mode-prefix-key],
...

\\{lretpl-mode-map}"
  (interactive "P")
  (make-local-variable 'lretpl-mode)
  (setq lretpl-mode
	(if (null arg)
	    (not lretpl-mode)
	  (> (prefix-numeric-value arg) 0)))

  (force-mode-line-update)
  (if lretpl-mode
      (progn
	(lretpl-set-according-to-mode)
	(if lretpl-tmpl-key (local-set-key lretpl-tmpl-key
					    'lretpl-templates))
	(if lretpl-help-key (local-set-key lretpl-help-key
					    'lretpl-syntax-help))
	(run-hooks 'lretpl-mode-hook))
    (if lretpl-tmpl-key (local-unset-key lretpl-tmpl-key))
    (if lretpl-help-key (local-unset-key lretpl-help-key))
    (run-hooks 'lretpl-mode-off-hook)))

(require 'lretpl-x)
(provide 'lretpl)

;;; lretpl.el ends here
