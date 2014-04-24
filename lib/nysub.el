;; tplsub.el - templates, abbreviations and syntax help
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author:    Lars Reed <lre@mesan.no>
;; Version:   $Id: tplsub.el 2.12 1998/06/07 12:43:36 larsr Exp $
;; Keywords:  programming templates help abbrev
;; Copyright: (c) Lars Reed
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; History:
;;    $Log: tplsub.el $
;;Revision 2.12  1998/06/07  12:43:36  larsr
;;  - corrected message for eval
;;
;;Revision 2.11  1998/06/07  12:30:22  larsr
;;  - eval-message
;;Revision 2.10  1998/06/07  12:23:50  larsr
;;   - tplsub-buffer-name added
;;Revision 2.9  1998/06/07  12:08:52  larsr
;;   - documentation only
;;Revision 2.8  1998/05/03  21:44:18  larsr
;;   - reformatted history
;;Revision 2.7  1998/05/03  21:41:14  larsr
;;   - brushed up file header
;;   - added tplsub-enable-eval
;;   - added §message
;;Revision 2.6  1998/05/03  21:23:38  larsr
;;   - added asku/askl
;;Revision 2.5  1998/05/03  19:52:42  larsr
;;   - eval-functionality
;;Revision 2.4  1998/04/18  14:17:42  larsr
;;   - bug fix...
;;Revision 2.3  1998/04/17  21:19:40  larsr
;;   - no change(!)
;;Revision 2.2  1998/04/17  21:15:32  larsr
;;   - corrected...
;;Revision 2.1  1998/04/17  21:11:00  larsr
;;   - R2
;;
;; History from lretpl::
;;Revision 2.7  1998/06/08  07:36:40  larsr: Misc. brushup
;;Revision 2.6  1998/06/07  20:13:18  larsr: working tplsub-add-tpl...
;;Revision 2.5  1998/06/07  13:03:42  larsr: - added tplsub-add-tpl
;;					     - added tplsub-reg-fast
;;					     - more Java keywords
;;Revision 2.4  1998/04/17  20:56:00  larsr: - New mode-line indicator
;;Revision 2.3  1998/04/17  20:43:42  larsr: - History included
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file is NOT part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;
;;  Installation:
;;    (autoload 'tplsub "tplsub" "Generate from templates" t)
;;
;;  Todo:
;;   - notation to allow variables at left margin, e.g.
;;           §§text => text
;;   - merge with lretpl?
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:
;; ========================================================================

;; --- User options ---
(defvar tplsub-dir-c "§"
  "* line control prefix for tplsub")

(defvar tplsub-var-c tplsub-dir-c
  "* macro character for tplsub")

(defconst tplsub-mode-name " |"
  "* Name used in mode-line.")

(defvar tplsub-enable-eval enable-local-eval
  "* Control processing of eval-expressions.
Defaults to the current value of `enable-local-eval' - which see for further
documentation.")

(defvar tplsub-tmpl-help nil
  "* Set to t to give help during template expansion")

(defvar tplsub-no-no-help nil
  "* Set to t to silence \"no help\" messages (just be quiet)")

;; --- Setup ---
(defconst tplsub--version "$Revision: 2.12 $")

(defconst tplsub-version (substring tplsub--version 11
				    (- (length tplsub--version) 2)))

(defvar tplsub-save-file
  (cond ((fboundp 'convert-standard-filename)
	 (convert-standard-filename "~/.tplsubhist"))
	((or (eq system-type 'ms-dos)
	     (eq system-type 'windows-nt))
	 "~/tplsub.his")
	(t "~/.tplsubhist"))
  "Name of the file that records `tplsub-default-alist' values.")

(defvar tplsub-record-file
  (cond ((fboundp 'convert-standard-filename)
	 (convert-standard-filename "~/tplsub.add"))
	(t "~/tplsub.add"))
  "Name of the file that records `tplsub-add-tpl' values.
nil to avoid recording.")

(defvar tplsub-tmpl-key "|"
  "Key to trigger template expansion \(`tplsub-templates'\)
If this is changed to an unprintable key code, `tplsub-tmpl-undefined-action'
should be changed also.")

(require 'cl)

(defconst tplsub-xemacs-p (string-match "XEmacs\\|Lucid" (emacs-version)))

;; Different GNU & XEmacs versions disagree on these:
(fset 'tplsub-capitalize (if (fboundp 'upcase-initials)
			     'upcase-initials
			   'capitalize))

(fset 'tplsub-buf-sub
      (if (fboundp 'buffer-substring-no-properties)
	  'buffer-substring-no-properties
	'buffer-substring
	))

(defvar tplsub-buffer-name "*tplsub*"
  "tplsub buffer name")

(defvar tplsub-buffer-name-2 "*tplsub2*"
  "tplsub hidden buffer name")

(defvar tplsub-file-directory (concat (getenv "SPE_HOME") "/maler/")
  "Default directory for template files")

;; --- Long-lived variables ---

(defvar tplsub-alist nil
  "List of currently defined variables and replies")

(defvar tplsub-default-alist nil
  "List of previously defined variables and replies")

(defvar tplsub-hist-list nil
  "Minibuffer history list")

(defvar tplsub-hist-read nil
  "t after defaults are read")

(defvar tplsub-list-keymap nil
  "keymap used in *tplsub* buffer \(to avoid direct editing\).")

(defvar tplsub-mode nil
  "Non-nil when tplsub-mode is activated.")

(defvar tplsub-mode-hook nil
  "Functions to call after activating tplsub-mode.")

(defvar tplsub-mode-off-hook nil
  "Functions to call after deactivating tplsub-mode.")

(defvar tplsub-tmpl-undefined-action 'insert
  "Action when expansion is unsuccessful.
The default action (given by \'insert) is to insert `tplsub-key'.
The other possible values are `nil' \(ignore\) and \'error, to either
completely ignore the situation, or report an error.")

(defvar tplsub-help-global-list nil
  "Global additions to `tplsub-help-mode-list' (which see)")

;; --- Short-lived variables ---
(defvar tplsub-file-list nil
  "List of further files to process")

(defvar tplsub-quit nil
  "Set to `t' to abort processing")

(defvar tplsub-err-count 0
  "Error counter used during expansion")

;; --- Buffer local variables ---

(defvar tplsub-tmpl-case nil
  "Set to `t' to have case sensisitve template expansion")
(make-variable-buffer-local 'tplsub-tmpl-case)

(defvar tplsub-help-key "\C-c?"
  "Key to trigger syntax help.")
(make-variable-buffer-local 'tplsub-tmpl-help-key)

(defvar tplsub-cont-string " \\"
  "Line continuation string")
(make-variable-buffer-local 'tplsub-cont-string)

(defvar tplsub-tmpl-mode-list ()
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
   \(: [ . <n>]\)  - insert `tplsub-cont-string' and a newline, then indent
		     to underneath the <n>th \(default first - n=1\) word
		     on the previous line \(with `indent-relative'\)
   \(= . \"<tmpl>\"\)
                 - the <tmpl> is expanded recursively \(note:
		   `tplsub--current-tmpl' is not changed\)
   \(help . \"<string>\"\)
                 - the <string> is sent to `tplsub-do-syntax-help', unless
		   `tplsub-tmpl-help' is nil
   <command>     - the command is executed interactively
   \(<function> [<args>]\)
                 - a function call with the given, evaluated arguments
                   \(if any\)
   anything else - ignored \(but reported\)

E.g:
  \(setq tplsub-tmpl-mode-list
      \'\(\(\"fi\" \"end if\" *\)
         \(\"if\" \"if \" | / \(= . \"fi\"\) (help . \"if\"\)\)
         \(\"wh\" \"while \" | / \"end while\" *\)\)\)
The second entry is expanded when \"if\\[tplsub-templates]\" is given,
and is executed like this:
   insert the string \"if \"
   record position of point
   insert & indent a new line
   expand the first entry recursively, i.e. insert \"end if\" and reindent
   implicitly: return point to the end of the previous line \(after \"if \"\)

The abbreviation must always be in lowercase when defined \(upper and mixed
case may be used for definitions used only with recursion\).
If `tplsub-tmpl-case' is `t', the case used when expanding literal strings
within the templates depends on how the abbreviation is written - e.g.:
       \"if\" ==> \"if \" & \"end if\"
       \"If\" ==> \"If \" & \"End If\"
       \"IF\" ==> \"IF \" & \"END IF\"
To force a string to keep a certain case, start it with \\e.
If `tplsub-tmpl-case' is nil, strings are always inserted verbatim.
")
;; Special for each buffer
(make-variable-buffer-local 'tplsub-tmpl-mode-list)

(defvar tplsub-tmpl-buffer-list nil
  "Buffer specific additions to `tplsub-tmpl-mode-list'.")
(make-variable-buffer-local 'tplsub-tmpl-buffer-list)

(defvar tplsub-tmpl-regexp "[^-+/*a-zA-Z0-9?]+"
  "Expression that matches the first character before an abbrev
when searching backwards.")
(make-variable-buffer-local 'tplsub-tmpl-regexp)

(defvar tplsub-help-mode-list nil
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
 (setq tplsub-help-mode-list
       '((\"restart-label\" (= . \"restart\")) ; Alias
         (\"restart from\"  (= . \"restart\")) ; Alias
         (\"restart\"       \"Syntax: RESTART-LABEL <label>\"
			    \"    or: RESTART FROM <label>\"
			    \"where <label> is an integer between 1 and 4999\")
         ))
")
(make-variable-buffer-local 'tplsub-help-mode-list)

(defvar tplsub-help-buffer-list nil
  "Buffer specific additions to `tplsub-help-mode-list'.")
(make-variable-buffer-local 'tplsub-help-buffer-list)

(defvar tplsub-mode-list nil
  "List of mode bindings in tplsub.
Each major mode using tplsub should register itself using
`tplsub-register-mode', which in turn updates this list.
The list consists of sublists containing the following elements:
   0. Major mode
   1. Template list, used for `tplsub-tmpl-mode-list'
   2. Help list, used for `tplsub-help-mode-list'
   3. Template regexp, used for `tplsub-tmpl-regexp'
   4. Template case, used for `tplsub-tmpl-case'
   5. Cont. strinc, used for `tplsub-cont-string'
   6. Help key, used for `tplsub-help-key'
Each of these may be 'default, in which case the current (default or locally
set) value is used.")

;; --- Internal variables ---

(defvar tplsub--current-tmpl ""
  "A string containing the current abbreviation when expanding templates.
Can be used by your own template-handling functions.")

(defvar tplsub--current-tmpl-point nil
  "Contains the position of point after template insertion.")

(defvar tplsub-tmpl-history nil
  "A list containing replies to template prompts.")

(defvar tplsub-help-history nil
  "A list containing replies to help prompts.")

;; --- Init ---
(if tplsub-list-keymap ()
  (setq tplsub-list-keymap (copy-keymap global-map))
  (substitute-key-definition 'self-insert-command
			     'tplsub-undefined
			     tplsub-list-keymap))

;; Display name when appropriate
(or (assq 'tplsub-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons '(tplsub-mode tplsub-mode-name)
                minor-mode-alist)))


;;; ======================================================================
;;  Template routines
;;; ======================================================================

;; --- Subroutines ---

(defun tplsub-undefined ()
  "dummy function substituted for self-insert keys in *tplsub* buffer"
  (interactive)
  (momentary-string-display "*** THIS BUFFER IS NOT FOR EDITING! ***"
			    (point) ?\  "Type <space> to continue"))

(defun tplsub-def-p (s)
  ;; Return cons corresponding to s or nil
  (assoc s tplsub-alist))

(defun tplsub-message (mess &optional lf)
  ;; Write message
  (let ((b (current-buffer)))
    (pop-to-buffer (get-buffer-create tplsub-buffer-name))
    (insert mess)
    (if lf (insert "\n"))
    (set-buffer-modified-p nil)
    (pop-to-buffer b)))

(defun tplsub-warning (mess)
  ;; Show message temporarily
  (message "%s" mess)
  (sit-for 3)
  (tplsub-message mess t))

(defun tplsub-error (mess)
  ;; Write message
  (setq tplsub-err-count (1+ tplsub-err-count))
  (tplsub-message (concat "ERROR: " mess) t))

(defun tplsub-add (var def &optional case)
  ;; Add new variable/definition-pair, return definition
  (let ((vdef (cond ((null case) def)
		    ((eq case 'upper) (upcase def))
		    ((eq case 'lower) (downcase def))
		    (t def))))
    (setq tplsub-alist (append (list (cons var vdef)) tplsub-alist))
    (tplsub-message (if (stringp vdef)
			vdef
		      (prin1-to-string vdef)) t)
    vdef))

(defun tplsub-y-or-n (var prompt)
  ;; Obtain Boolean reply
  (let ((is-def (tplsub-def-p var)))
    (if is-def
	(not (not (cdr is-def)))
      (if (or (null prompt)
	      (string= prompt ""))
	  (setq prompt var))
      (tplsub-message (format "%s? " prompt))
      (tplsub-add var (y-or-n-p (concat prompt " "))))))


(defconst tplsub--date-y (format "%04d"
			       (string-to-int
				(substring (current-time-string) 20 24))))
(defconst tplsub--date-m
  (cdr (assoc (substring (current-time-string) 4 7)
	      '(("Jan" . "01") ("Feb" . "02") ("Mar" . "03")
		("Apr" . "04") ("May" . "05") ("Jun" . "06")
		("Jul" . "07") ("Aug" . "08") ("Sep" . "09")
		("Oct" . "10") ("Nov" . "11") ("Dec" . "12")))))
(defconst tplsub--date-d (format "%02d"
			       (string-to-int
				(substring (current-time-string) 8 10))))
(defvar tplsub-date-sep "/" "*String to separate day/month/year")

(defun tplsub--conc-date (&optional psep pabbr pseq py pm pd)
  (let* ((sep (if psep (if (stringp 'psep) psep tplsub-date-sep) nil))
	 (Y (or py tplsub--date-y))
	 (y (if pabbr (substring Y 2) Y))
	 (m (or pm tplsub--date-m))
	 (d (or pd tplsub--date-d)))
    (if pseq (concat y sep m sep d)
      (concat d sep m sep y))))

(defvar tplsub--author "Lars.Reed@ECsoft.no")

(defvar tplsub--mailto (concat "mailto:"
			       (or user-mail-address
				   (and (fboundp 'user-mail-address)
					(user-mail-address))
				   (concat (user-login-name)
					   "@"
					   (system-name)))))

(defvar tplsub-builtin-defaults-alist
  (list (cons "author"  tplsub-builtin-defaults-alist)
	(cons "Iuser"   (user-login-name))
	(cons "Imail"   tplsub--mailto)
	(cons "Idate"   (tplsub--conc-date))
	(cons "Ifdate"  (tplsub--conc-date t))
	(cons "Isdate"  (tplsub--conc-date nil t))
	(cons "Irdate"  (tplsub--conc-date nil nil t))
	(cons "Irfdate" (tplsub--conc-date t   nil t))
	(cons "Irsdate" (tplsub--conc-date nil t   t)))
  "Automatically generated defaults")

(defvar tplsub-tmpl-global-list
  (list (list "author" tplsub--author)
	(list "user"   (user-login-name))
	(list "mailto" tplsub--mailto)
	(list "date"   (tplsub--conc-date))
	(list "fdate"  (tplsub--conc-date t))
	(list "sdate"  (tplsub--conc-date nil t))
	(list "rdate"  (tplsub--conc-date nil nil t))
	(list "rfdate" (tplsub--conc-date t   nil t))
	(list "rsdate" (tplsub--conc-date nil t   t)))
  "Global additions to `tplsub-tmpl-mode-list' (which see)")


(defun tplsub-define (var prompt &optional default case)
  ;; Obtain textual reply
  (let ((is-def (tplsub-def-p var)))
    (if is-def (cdr is-def)
      (if (null default)
	  (or (setq default (cdr (assoc var tplsub-builtin-defaults-alist)))
	      (setq default (cdr (assoc var tplsub-default-alist)))))
      (if (or (null prompt)
	      (string= prompt ""))
	  (setq prompt (format "Define %s" var)))
      (tplsub-message (format "Define %s..." var))
      (tplsub-add var
		  (read-string (concat prompt ": ")
			       (cons (or default "") 0)
			       tplsub-hist-list)
		  case))))

(defun tplsub-setup ()
  ;; Create message buffer, create window configuration
  (delete-other-windows)
  (let ((b (current-buffer)))
    (pop-to-buffer (get-buffer-create tplsub-buffer-name))
    (toggle-read-only 0)
    (erase-buffer)
    (use-local-map tplsub-list-keymap)
    (insert "\n\n\n")
    (insert-char ?= 30)
    (insert " tplsub ")
    (insert-char ?= 30)
    (insert "\n\n")
    (sit-for 1)
    (shrink-window-if-larger-than-buffer)
    (set-buffer-modified-p nil)
    (pop-to-buffer b)))

(defun tplsub-file-name (s &optional must-exist)
  ;; Expand a file name
  (let ((result nil))
    (if (or (null s)
	    (string= "" s))
	(if must-exist (tplsub-error "tomt filnavn!"))
      (setq result (substitute-in-file-name (expand-file-name s)))
      (if must-exist
	  (progn
	    (if (not (file-exists-p result))
		(setq result (concat tplsub-file-directory
				     (file-name-nondirectory result))))
	    (if (not (file-exists-p result))
		(progn
		  (tplsub-error (concat "finner ikke " s))
		  (setq result nil))))))
    result))


(defun tplsub-vars ()
  ;; Substitute variables
  (goto-char (point-min))
  (let (var
	repl
	(regexp (concat (regexp-quote tplsub-var-c)
			"\\([^" (regexp-quote tplsub-var-c) "]*\\)"
			(regexp-quote tplsub-var-c))))
  (while (re-search-forward regexp nil t)
    (setq var (tplsub-buf-sub (match-beginning 1)
			      (match-end 1))
	  repl (if (string= var "")
		   (cons var tplsub-var-c)
		 (tplsub-def-p var)))
    (if (null repl)
	(tplsub-error (format "%s is undefined!" var))
      (kill-region (match-beginning 0) (match-end 0))
      (insert (cdr repl))))))


(defun tplsub-string-replace(str)
  "Perform variable replacement on string, rather than buffer"
  (let ((cur-buf (current-buffer))
	res)
    (set-buffer (get-buffer-create tplsub-buffer-name-2))
    (if tplsub-xemacs-p
	(erase-buffer tplsub-buffer-name-2)
      (erase-buffer))
    (insert str)
    (tplsub-vars)
    (set-buffer-modified-p nil)
    (setq res (tplsub-buf-sub (point-min) (point-max)))
    (set-buffer cur-buf)
    res))


(defun tplsub-rest-of-line (n)
  ;; Return the rest of the line - excluding leading blanks - as a string.
  (save-excursion
    (forward-char n)
    (skip-chars-forward " \t")
    (if (or (looking-at "\\\\ ")
	    (looking-at "\\\\\\\\"))	; Skip backslash preceding
					; backslash or blank
	(forward-char 1))
    (tplsub-buf-sub (point)
		    (progn
		      (end-of-line)
		      (point)))))


(defun tplsub-var-and-prompt (n &optional only-one)
  ;; Parse current line, return (var . text)
  (save-excursion
    (let (ws
	  var
	  txt)
      (forward-char n)
      (skip-chars-forward " \t")
      (setq ws (point))
      (skip-syntax-forward "w_")
      (setq var (tplsub-buf-sub ws (point))
	    txt (if only-one
		    nil
		  (tplsub-string-replace (tplsub-rest-of-line 0))))
      (if (and (stringp txt)
	       (> (length txt) 7)
	       (string= (substring txt 0 5)
			"(eval"))
	  (let* ((val (substring txt 6 (- (length txt) 1)))
		 (oval val))
	    (setq txt (cond ((eq tplsub-enable-eval t)
			     (eval (read val)))
			    ((null tplsub-enable-eval)
			     txt)
			    ((y-or-n-p (concat
					"Evaluate expression \`"
					val "\'? "))
			     (eval (read val)))
			    (t txt)))
	    (tplsub-message (concat "eval: " oval " = " txt) t)))
      (cons var txt))))

(defun tplsub-directives ()
  ;; Read conditionals and input definitions
  (goto-char (point-min))
  (let (line-start
	v-and-p
	buf-sub
	s-point
	n
	i
	(overlay-arrow-position (make-marker))
	(overlay-arrow-string (make-string 75 169))
	(regexp (concat "^" (regexp-quote tplsub-dir-c)))
	(kill-from -1)
	(kill-level -1)
	(nest-level 0))
    (while (and (not tplsub-quit)
		(re-search-forward regexp nil t))
      (save-excursion
	(beginning-of-line)
	(setq line-start (point))
	(set-marker overlay-arrow-position (point) (current-buffer))
	(redraw-display))
      (cond

       ;; ---------------------------------------------------------
       ((looking-at "if")
	(recenter)
	(setq nest-level (1+ nest-level))
	(when (= -1 kill-from)
	  (setq v-and-p (tplsub-var-and-prompt 2))
	  (if (not (tplsub-y-or-n (car v-and-p) (cdr v-and-p)))
	      (setq kill-from line-start
		    kill-level nest-level))))

       ;; ---------------------------------------------------------
       ((looking-at "else")
	(cond ((and (> kill-from -1)
		    (= kill-level nest-level))
	       (setq line-start kill-from
		     kill-from  -1))
	      ((> kill-from -1)
	       t)
	      ((> nest-level 0)
	       (setq kill-from  line-start
		     kill-level nest-level))
	      (t
	       (tplsub-error (format "unmatched else, position %d"
				     line-start)))))

       ;; ---------------------------------------------------------
       ((looking-at "fi")
	(cond ((and (> kill-from -1)
		    (= kill-level nest-level))
	       (setq line-start kill-from
		     kill-from  -1
		     nest-level (1- nest-level)))
	      ((> nest-level 0)
	       (setq nest-level (1- nest-level)))
	      (t
	       (tplsub-error (format "unmatched endif, position %d"
				     line-start)))))

       ;; ---------------------------------------------------------
       ((looking-at "do")
	(if (> kill-from -1)
	    ()
	  ;; Count
	  (recenter)
	  (setq v-and-p (tplsub-var-and-prompt 2)
		n (string-to-number (tplsub-define (car v-and-p)
						   (cdr v-and-p))))
	  ;; Cut the loop
	  (beginning-of-line 2)
	  (setq s-point (point))
	  (re-search-forward (concat regexp "done"))
	  (beginning-of-line)
	  (setq buf-sub (buffer-substring s-point (point)))
	  (beginning-of-line 2)
	  (kill-region s-point (point))
	  ;; Insert
	  (setq i n)
	  (while (> i 0)
	    (save-restriction
	      (goto-char s-point)
	      (insert buf-sub)
	      (narrow-to-region s-point (point))
	      (goto-char (point-min))
	      (replace-regexp (concat (regexp-quote tplsub-var-c)
				      "R"
				      (regexp-quote tplsub-var-c))
			      (format "%d" i)))
	    (setq i (1- i)))
	  ;; Return
	  (goto-char (1- s-point))))

       ;; ---------------------------------------------------------
       ((let ((case-fold-search nil))  ;; Backwards compatibility!!!
	  (looking-at "ASK"))
	(if (> kill-from -1)
	    () ; Ignore inside killed part
	  (recenter)
	  (setq v-and-p (tplsub-var-and-prompt 3 ))
	  (tplsub-define (car v-and-p) (cdr v-and-p) nil 'upper)))

       ;; ---------------------------------------------------------
       ((looking-at "ask[ul]?")
	(let (special no)
	  (cond ((looking-at "asku") (setq special 'upper
					   no	   4))
		((looking-at "askl") (setq special 'lower
					   no	   4))
		(t		     (setq special nil
					   no	   3)))
	  (if (> kill-from -1)
	      () ; Ignore inside killed part
	    (recenter)
	    (setq v-and-p (tplsub-var-and-prompt no))
	    (tplsub-define (car v-and-p) (cdr v-and-p) nil special))))

       ;; ---------------------------------------------------------
       ((looking-at "default")
	(if (> kill-from -1)
	    ()
	  (setq v-and-p (tplsub-var-and-prompt 7)
		tplsub-default-alist (append
				      (list (cons (car v-and-p)
						  (cdr v-and-p)))
				      tplsub-default-alist))))

       ;; ---------------------------------------------------------
       ((looking-at "rem")
	t)

       ;; ---------------------------------------------------------
       ((looking-at "next")
	(if (> kill-from -1)
	    ()
	  (setq buf-sub (tplsub-file-name (tplsub-rest-of-line 4) t))
	  (if buf-sub
	      (setq tplsub-file-list (cons buf-sub tplsub-file-list)))))

       ;; ---------------------------------------------------------
       ((looking-at "include")
	(if (> kill-from -1)
	    ()
	  (setq buf-sub (tplsub-file-name (tplsub-rest-of-line 7) t))
	  (if buf-sub
	      (save-excursion
		(beginning-of-line 2)
		(insert-file-contents buf-sub)))))

       ;; ---------------------------------------------------------
       ((looking-at "name")
	(if (> kill-from -1)
	    ()
	  (setq buf-sub (tplsub-file-name (tplsub-rest-of-line 4)))
	  (if (and buf-sub
		   (not (string= "" buf-sub)))
	      (set-visited-file-name buf-sub))))

       ;; ---------------------------------------------------------
       ((looking-at "quit")
	(if (> kill-from -1)
	    ()
	  (setq buf-sub (tplsub-rest-of-line 4))
	  (if (not (or (null buf-sub)
		       (string= buf-sub "")))
	      (tplsub-error buf-sub))
	  (setq tplsub-file-list nil
		tplsub-quit	 t)))

       ;; ---------------------------------------------------------
       ((looking-at "exit")
	(setq tplsub-quit t))

       ;; ---------------------------------------------------------
       ((looking-at "message")
	(if (> kill-from -1)
	    ()
	  (tplsub-warning (tplsub-rest-of-line 7))))

       ;; ---------------------------------------------------------
       ((looking-at "define")
	(if (> kill-from -1)
	    ()
	  (setq v-and-p (tplsub-var-and-prompt 6))
	  (tplsub-message (format "%s=" (car v-and-p)))
	  (tplsub-add (car v-and-p) (cdr v-and-p))))

       ;; ---------------------------------------------------------
       ((looking-at "append")
	(if (> kill-from -1)
	    ()
	  (setq v-and-p (tplsub-var-and-prompt 6))
	  (tplsub-message (format "%s=" (car v-and-p)))
	  (let ((old-val (cdr (tplsub-def-p (car v-and-p)))))
	    (if old-val
		(tplsub-add (car v-and-p) (concat old-val (cdr v-and-p)))
	      (tplsub-add (car v-and-p) (cdr v-and-p))))))

       ;; ---------------------------------------------------------
       ((looking-at "defbool")
	(if (> kill-from -1)
	    ()
	  (setq v-and-p (tplsub-var-and-prompt 7))
	  (tplsub-message (format "%s=" (car v-and-p)))
	  (if (string= (cdr v-and-p) "nil")
	      (tplsub-add (car v-and-p) nil)
	    (tplsub-add (car v-and-p) t))))

       ;; ---------------------------------------------------------
       (t
	(tplsub-error (format "unknown directive, position %d"
				line-start))))

      ;; ---------------------------------------------------------
      (beginning-of-line 2)
      (kill-region line-start (point))
      (if (not (bobp)) (forward-char -1)))
    (if (> kill-from -1)
	(tplsub-error (format "unmatched if, position %d" kill-from)))))

(defun tplsub-read-defaults ()
  "Read default list from file.
Not ready!"
  (when (not tplsub-hist-read)
    (setq tplsub-hist-read t)
    (let ((file (tplsub-file-name tplsub-save-file)))
      (if (and file
	       (file-readable-p file))
	  (save-excursion
	    (set-buffer (get-buffer-create " *tplsubsave*"))
	    (delete-region (point-min) (point-max))
	    (insert-file-contents file)
	    (goto-char (point-min))
	    (setq tplsub-default-alist
		  (car (read-from-string
			(buffer-substring (point-min) (point-max)))))
	    (kill-buffer (current-buffer))
	    (if (and (> (length tplsub-default-alist) 400)
		     (y-or-n-p "The list of defaults is large.  Erase it? "))
		(setq  tplsub-default-alist nil)))))))

(defun tplsub-batch-ask ()
  ;; Asks the user how to continue
  (let ((c " "))
    (switch-to-buffer (get-buffer-create tplsub-buffer-name))
    (delete-other-windows)
    (insert-char ?\n 5)
    (insert "****** Substitution complete! ******\n\n"
	    "Select    To\n"
	    "-----     -----------------------------------------------"
	    "---------------\n"
	    "  e       continue editing, returning to the script when "
	    "emacs is exited\n"
	    "  s       like e, but save defaults first\n"
	    "  x       save buffers and defaults, and return to the script\n"
	    "  q       like x, but don\'t save defaults\n")
    (while (string= c " ")
      (setq c (read-from-minibuffer "Action: " "x"))
      (cond
       ((string= c "e")
	t)
       ((string= c "s")
	(tplsub-save-defaults))
       ((string= c "x")
	(tplsub-save-defaults)
	(save-buffers-kill-emacs))
       ((string= c "q")
	(save-buffers-kill-emacs))
       (t
	(setq c " "))))
      (bury-buffer)))

(defun tplsub-cleanup-defaults (alist)
  "Remove duplicates and non-string elements from default list"
  (let ((reslist nil))
    (mapcar (function (lambda (e)
			(if (and (not (assoc (car e) reslist))
				 (stringp (cdr e)))
			    (setq reslist (append (list e) reslist)))))
	    alist)
    (reverse reslist)))


(defun tplsub-main ()
  (tplsub-directives)
  (tplsub-vars))


;; --- Callable defuns ---

(defsubst tplsub-buffer-name () (file-name-nondirectory
				 (file-name-sans-extension
				  (buffer-file-name))))

(defun tplsub-reset (&optional pfx)
  "Transfer replies to history list.
With prefix, erase history. "
  (interactive "P")
  (setq tplsub-default-alist
	(if pfx nil
	  (append tplsub-alist tplsub-default-alist))
	tplsub-alist nil))

(defun tplsub-save-defaults (&optional file)
  "Save default replies for next emacs session."
  (interactive "P")
  (if (null file) (setq file (tplsub-file-name tplsub-save-file)))
  (save-excursion
    (set-buffer (get-buffer-create " *tplsubsave*"))
    (delete-region (point-min) (point-max))
    (when (file-readable-p file)
      (insert-file-contents file)
      (delete-region (point-min) (point-max)))
    (let ((deflist (tplsub-cleanup-defaults tplsub-default-alist)))
      (print deflist (current-buffer)))
    (write-file file)
    (kill-buffer (current-buffer))))


(defun tplsub (&optional filename batch)
  "Expand templates!
If a FILENAME is given, that file is first loaded.
Then directives are expanded.  Directives are lines starting with
`tplsub-dir-c' \(default==§\), preferrably followed by one of the known
directives...
Directive lines are removed after interpretation \(a directive line containing
an unknown directive is also removed\).

For directives with arguments, variable expansion is performed on the
last argument \(only\), using the currently known variables.
One or more spaces and tabs between each argument are accepted.
The last argument may itself contain spaces and tabs, the others may not.
Furthermore, if the first letter of the last argument is a backslash,
and the second letter is either backslash or space, the first letter
\(backslash\) is ignored - to allow definition of variables with leading
spaces.

Possible directives:
  §ask <var> [<prompt>] - ask for value of <var> \(if not set\)
  §askl <var> [<prompt>] - like §ask, convert result to lowercase
  §asku <var> [<prompt>] - like §ask, convert result to uppercase
  §ASK <var> [<prompt>] - alias for asku - for compatibility only!
  §default <var> <default-value> - set default value for <var>
  §define <var> <value> - set <var> to <value>
  §append <var> <value> - like §define, but appends to <var>\'s old
                          definition if it already exists
  §defbool <var> <value> - set Boolean <var> to `nil' if <value> is \"nil\",
                           otherwise to `t'
  §next <file> - read <file> after this one \(LIFO\)
  §include <file> - insert the contents of <file> here
  §name <file> - change the file name of this buffer to <file>
  §quit [<message>] - give <message> and quit altogether
  §exit - quit this file, continue with next, if any
  §message <text> - display message in tplsub buffer
  §rem [<text>] - source file comment

  §if <var> [<prompt>]
  <body1>
  [§else
  <body2>]
  §fi - ask for <var>, if t, insert <body1>, otherwise <body2>
        if set

  §do <var> [prompt]
  <body>
  §done - ask for integer <var>, repeat <body> <var> times, replacing
          §R§ with current number

The second argument to §default, §ask (with variations), §define, §append and
§if may be of the form \"\(eval sexp\)\"
After expanding and removing all directives, the file is scanned once more,
this file to replace variables, i.e. strings surrounded by `tplsub-var-c'
\(which also defaults to §\) - two such characters in a row are replaced with
a single occurrence.
The string between the macro characters are replaced by their definition,
as given by §define or §ask-directives.

If §next-directives are used, the first file in `tplsub-file-list' is
loaded, and directive and variable substitutions is performed on this
file, remembering previous values.  This loop continues until no more
files remain.

Later invocations of `tplsub' in the same emacs session will remember
earlier replies, and use them as defaults \(does not apply to §if, which
has no default\).  To erase the default list, invoke `tplsub-reset' with
a numeric prefix.
"
  (interactive)
  (if (not tplsub-hist-read)
      (tplsub-read-defaults))
  (if filename
      (find-file (tplsub-file-name filename)))
  (tplsub-setup)
  (unwind-protect
      (let (file
	    (tplsub-err-count 0)
	    (tplsub-file-list nil)
	    (tplsub-quit      nil))
	(tplsub-main)
	(while tplsub-file-list
	  (setq file (car tplsub-file-list)
		tplsub-quit nil
		tplsub-file-list (cdr tplsub-file-list))
	  (tplsub-message (format "Loading %s..." file) t)
	  (find-file file)
	  (tplsub-main))
	(if (> tplsub-err-count 0)
	    (tplsub-message (format "NOTE! %d errors!" tplsub-err-count) t)))
    (tplsub-reset))
  (let ((t-buf (get-buffer tplsub-buffer-name-2)))
    (if t-buf (kill-buffer t-buf)))
  (if batch
      (tplsub-batch-ask)
    (if (y-or-n-p "Save defaults? ")
	(tplsub-save-defaults)))
  (message ""))


;;; ======================================================================
;;  Abbreviation routines
;;; ======================================================================

(defun tplsub-indent-line ()
  "Reindent current line"
  (if (null indent-line-function)
      (indent-relative-maybe)
    (funcall indent-line-function)))

(defun tplsub-indent-new-line ()
  "Reindent the current line, insert a newline and indent the new line."
  (interactive "*")
  (tplsub-indent-line)
  (end-of-line)
  (newline)
  (tplsub-indent-line))

(defun tplsub-insert-undefined (&rest args)
  "Inserts template key literally, if it is printable, and limited
by `tplsub-tmpl-undefined-action'"
  (message (car args) (cdr args))
  (cond ((and (eq tplsub-tmpl-undefined-action 'insert)
	      (stringp tplsub-tmpl-key))
	 (insert  tplsub-tmpl-key))
	((not tplsub-tmpl-undefined-action)
	 t)
	(t
	 (error (car args) (cdr args)))))


(defun tplsub--describe-ins (&rest s)
  "Insert description strings in help buffer"
  (let ((print-escape-newlines t))
    (insert (mapconcat 'identity s ""))))


(defun tplsub--describe-1 (tmpl)
  "Describes an element of a template definition.
See the description of `tplsub-tmpl-mode-list' and `tplsub-template-exec'
for further information."
  (cond ((null tmpl) t)
	((and (stringp tmpl)		; String with case
	      (string= (substring tmpl 0 1) "\e"))
	 (tplsub--describe-ins "\"" (substring tmpl 1) "\""))
	((stringp tmpl)			; String
	 (tplsub--describe-ins "\"" tmpl "\""))
	((eq '| tmpl)			; Point
	 (tplsub--describe-ins "|"))
	((eq '/ tmpl)			; Newline
	 (tplsub--describe-ins "\\t\\n\\t"))
	((eq '* tmpl)			; Reindent
	 (tplsub--describe-ins "\\t"))
	((vectorp tmpl)			; Macro
	 (tplsub--describe-ins (prin1-to-string tmpl)))
	((integerp tmpl)		; Indent
	 (tplsub--describe-ins (format "%d" tmpl)))
	((and (listp tmpl)
	      (stringp (car tmpl)))	; Prompt & insert
	 (tplsub--describe-ins "{" (car tmpl))
	 (if (cdr tmpl)
	     (tplsub--describe-ins "?" (cdr tmpl)))
	 (tplsub--describe-ins "}"))
	((and (listp tmpl)
	      (eq ': (car tmpl)))	; Split line
	 (tplsub--describe-ins " :\\n"
			      (format "%d\\t" (if (cdr tmpl) (cdr tmpl) 1))))
	((and (listp tmpl)
	      (eq '= (car tmpl)))	; Recursive...
	 (tplsub--describe-ins "(= " (cdr tmpl) ")"))
	((and (listp tmpl)
	      (eq 'help (car tmpl)))	; Help
	 t)				; ignore
	((and (symbolp tmpl)
	      (commandp tmpl))		; Interactive command
	 (tplsub--describe-ins "\'" (symbol-name tmpl)))
	(t (tplsub--describe-ins (prin1-to-string tmpl) )))
  tmpl)					; The argument is returned as is



(defun tplsub-describe-templates (&optional subfun-p)
  "Overview of tplsub mode template table.
If optional SUBFUN-P is non-nil, behaves as a subfunction of another
help command."
  (interactive)
  (let ((curr-buf (current-buffer))
	(bind-tpl (substitute-command-keys "'\\[tplsub-templates]'"))
	(bind-hlp (substitute-command-keys "'\\[tplsub-describe-templates]'"))
	(bind-syn (substitute-command-keys "'\\[tplsub-syntax-help]'"))
	(mode-ver  tplsub-version)
	(mode-desc (symbol-name major-mode))
	(case-desc (if tplsub-tmpl-case "ON" "OFF"))
	(help-desc (if tplsub-tmpl-help "ON" "OFF"))
	(act-desc  (cond ((eq tplsub-tmpl-undefined-action 'insert)
			  "insert key")
			 ((null tplsub-tmpl-undefined-action)
			  "ignore")
			 (t "error")))
	(tmpl-list (append tplsub-tmpl-buffer-list
			  tplsub-tmpl-mode-list
			  tplsub-tmpl-global-list)))
    (if (null tmpl-list)
	(message "No abbreviations!")
      (pop-to-buffer (get-buffer-create "*Help*"))
      (let ((was-r-o buffer-read-only)) ; Remember if read-only
	(if subfun-p
	    (goto-char (point-max))	; Append
	  (erase-buffer))		; - or empty
	(toggle-read-only 0)		; Not read-only
	(insert (if subfun-p "\n\n" "")
                "tplsub-mode version                    " mode-ver   "\n"
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
		     (mapcar 'tplsub--describe-1 (cdr elem))
		     (insert "\n")))
	 tmpl-list)
	(goto-char 1)
	(forward-paragraph 2)
	(if was-r-o (toggle-read-only 1))
	(pop-to-buffer curr-buf)))))

(defun tplsub--tmpl-exec (tmpl)
  "(Recursively) expand a template definition.
See the description of `tplsub-tmpl-mode-list' for a description of how
arguments are handled."
  (cond ((null tmpl) t)			; Ignore null args
	((stringp tmpl)			; Insert string, according to case
	 (insert (cond ((string= (substring tmpl 0 1) "\e")
			(substring tmpl 1))
		       ((not tplsub-tmpl-case)
			tmpl)
		       ((string= (upcase tplsub--current-tmpl)
				 tplsub--current-tmpl)
			(upcase tmpl))
		       ((string= (downcase tplsub--current-tmpl)
				 tplsub--current-tmpl)
			(downcase tmpl))
		       ((string= (tplsub-capitalize tplsub--current-tmpl)
				 tplsub--current-tmpl)
			(tplsub-capitalize tmpl))
		       (t
			tmpl))))
	((eq '| tmpl)			; Record point
	 (if (null tplsub--current-tmpl-point)
	     (setq tplsub--current-tmpl-point (point-marker))))
	((eq '/ tmpl)			; Newline & indent
	 (tplsub-indent-new-line))
	((eq '* tmpl)			; Reindent
	 (tplsub-indent-line))
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
			      'tplsub-tmpl-history)))
	((and (listp tmpl)
	      (eq ': (car tmpl)))	; Break line
	 (insert tplsub-cont-string "\n")
	 (if (not (cdr tmpl)) (indent-relative)
	   (let ((iterator (cdr tmpl)))
	     (while (> iterator 0)
	       (setq iterator (1- iterator))
	       (indent-relative)))))
	((and (listp tmpl)
	      (eq '= (car tmpl)))	; Recursive...
	 (mapcar 'tplsub--tmpl-exec
		 (cdr
		  (assoc (cdr tmpl)
			 (append tplsub-tmpl-buffer-list
				 tplsub-tmpl-mode-list
				 tplsub-tmpl-global-list)))))
	((and (listp tmpl)
	      (eq 'help (car tmpl)))	; Help!
	 (if (and tplsub-tmpl-help
		  (or tplsub-help-buffer-list
		      tplsub-help-mode-list
		      tplsub-help-global-list))
	     (tplsub-give-syntax-help (cdr tmpl))))
	((and (symbolp tmpl)
	      (commandp tmpl))		; Run interactive command
	 (call-interactively tmpl))
	((and (listp tmpl)
	      (fboundp (car tmpl)))	; A function (with args)
	 (apply (car tmpl) (cdr tmpl)))
	(t				; Unknown - ignore but report
	 (message "unknown argument -- `%s'" (prin1-to-string tmpl))))
  tmpl)					; The argument is returned as is


(defun tplsub-templates (&optional pfx)
  "Expand tplsub abbreviations"
  (interactive "*P")
  (setq prefix-arg pfx			; Keep for tmpl-command
	tplsub--current-tmpl nil)	; None yet
  (if (and (null tplsub-tmpl-buffer-list)
	   (null tplsub-tmpl-mode-list)
	   (null tplsub-tmpl-global-list))
      (tplsub-insert-undefined "No appropriate templates installed")
    (let ((pt (point))
	  fun)
      (save-excursion
	(if (re-search-backward tplsub-tmpl-regexp nil t nil)
	    (progn			; An abbrev was found
	      (setq tplsub--current-tmpl (tplsub-buf-sub
					   (match-end 0) pt)
		    fun (cdr (assoc (downcase tplsub--current-tmpl)
				    (append tplsub-tmpl-buffer-list
					    tplsub-tmpl-mode-list
					    tplsub-tmpl-global-list)))))))
      (if (not fun)
	  (tplsub-insert-undefined "no abbreviation for `%s'"
				    tplsub--current-tmpl)
	(setq tplsub--current-tmpl-point nil) ; Default position
	(kill-region (match-end 0) pt)	     ; Remove tmpl name
	(mapcar 'tplsub--tmpl-exec fun)	     ; Expand
	(if tplsub--current-tmpl-point
	    (goto-char tplsub--current-tmpl-point))	 ; Set point
	(setq prefix-arg ())))))

; ======================================================================

(defun tplsub--syntax-help-ins (s)
  "Insert a help string, or expand an alias"
  (cond ((stringp s) (insert s "\n"))
	((and (consp s)
	      (eq '= (car s))
	      (stringp (cdr s)))
	 (tplsub-give-syntax-help (cdr s) t))))


(defun tplsub-give-syntax-help (topic &optional no-heading)
  "Give help on named syntactic construction"
  (interactive "*sTopic: ")
  (if (and (null tplsub-help-buffer-list)
	   (null tplsub-help-mode-list)
	   (null tplsub-help-global-list))
      (or tplsub-no-no-help
	  (message "No appropriate help topics are defined"))
    (let ((curr-buf (current-buffer))
	  strings)
      (setq strings (cdr (assoc (downcase topic)
				(append tplsub-help-buffer-list
					tplsub-help-mode-list
					tplsub-help-global-list))))
      (if (null strings)
	  (or tplsub-no-no-help
	      (message "No help for `%s'" topic))
	(if no-heading ()
	    (pop-to-buffer (get-buffer-create "*Help*"))
	    (toggle-read-only 0)
	    (erase-buffer)
	    (insert "*** " topic ":\n\n"))
	(mapcar 'tplsub--syntax-help-ins strings)
	(if no-heading ()
	  (goto-char (point-min))
	  (pop-to-buffer curr-buf))))))


(defun tplsub-syntax-help ()
  "Give help on Lretpl topic.
Tries to guess topic according to cursor position."
  (interactive)
  (let (topic begin)
    (save-excursion
      (skip-syntax-backward "w_")
      (setq begin (point))
      (skip-syntax-forward "w_")
      (setq topic (tplsub-buf-sub begin (point))))
    (setq topic (read-string "Help topic: "
			     topic
			     'tplsub-help-history))
    (tplsub-give-syntax-help topic)))

; ======================================================================

(defun tplsub--add-record (modesym abbrs)
  "Record addition for later inclusion in .emacs or tplsub.el"
  (if (not tplsub-record-file) ()
    (set-buffer (get-buffer-create " *tplsub records*"))
    (delete-region (point-min) (point-max))
    (if (file-readable-p tplsub-record-file)
	(insert-file-contents tplsub-record-file))
    (insert "MODE:\t"   (symbol-name modesym) "\n"
	    "KEY:\t"    (car abbrs)	    "\n"
	    "ABBREV:\t")
    (prin1 (cdr abbrs) (current-buffer))
    (insert "\n\n")
    (write-file tplsub-record-file)
    (kill-buffer (current-buffer))
    (message "Copy stored in %s !" tplsub-record-file)))


(defun tplsub--add-tpl (modesym key tmpl-list)
  "Install another template for a given major mode.
See `tplsub-tmpl-mode-list' for details."
;;   (interactive (list (read-minibuffer "Mode: " (symbol-name major-mode))
;; 			(read-from-minibuffer "Key: ")
;; 			(read-from-minibuffer "Expansion: " nil nil t)))
  (if (null modesym)
      (error "Missing MODESYM")
    (let ((current (assq modesym tplsub-mode-list)))
      (if (not current)
	  (error "Mode has no templates"))
      (let* ((new-abbrs (list (append (list key) tmpl-list)))
	     (abbrs (append new-abbrs (car (cdr current)))))
	(tplsub--add-record modesym (car new-abbrs))
	(setcar (cdr current) abbrs))
      (tplsub-set-according-to-mode))))

(defun tplsub-add-tpl (key &rest tpl)
  "Add abbreviation to current mode"
  (interactive "sKey: \nXLisp expression (must evaluate to a list): ")
  (tplsub--add-tpl major-mode key (car tpl)))

(defun tplsub-register-mode (modesym
			     &optional t-list h-list regex t-case cont-str
				       h-key)
  "Install tplsub-definitions for major mode.
See `tplsub-mode-list' for details."
  (if (null modesym) ()
    (let ((current (assq modesym tplsub-mode-list)))
      (if current
	  (setq tplsub-mode-list (delete current tplsub-mode-list))))
    (setq tplsub-mode-list
	  (append (list (list modesym
			      t-list h-list regex t-case cont-str h-key))
		  tplsub-mode-list))))

(defmacro tplsub---swap-nil-default (sym)
  "Swap nil and default, otherwise leave alone"
  `(if (null ,sym) 'default (if (eq ,sym 'default) nil ,sym)))

(defun tplsub-reg-fast (modesym    t-list
			&optional  h-list regex  t-case cont-str h-key)
  "Like `tplsub-register-mode', but inserts default for nil arguments
and vice versa for arguments following H-LIST."
  (tplsub-register-mode modesym t-list h-list
			(tplsub---swap-nil-default regex)
			(tplsub---swap-nil-default t-case)
			(tplsub---swap-nil-default cont-str)
			(tplsub---swap-nil-default h-key)))

(defmacro tplsub---set-if (n var)
  `(let ((tmp-val (nth ,n v-list)))
     (if (not (eq tmp-val 'default))
	 (setq ,var tmp-val))))

(defun tplsub-set-according-to-mode ()
  "Set local variables according to major-mode."
  (let ((v-list (cdr (assq major-mode tplsub-mode-list))))
    (if (null v-list) ()
      (tplsub---set-if 0 tplsub-tmpl-mode-list)
      (tplsub---set-if 1 tplsub-help-mode-list)
      (tplsub---set-if 2 tplsub-tmpl-regexp)
      (tplsub---set-if 3 tplsub-tmpl-case)
      (tplsub---set-if 4 tplsub-cont-string)
      (tplsub---set-if 5 tplsub-help-key))))


; ======================================================================

;;;###autoload
(defun tplsub-mode (arg)
  "Toggle tplsub minor mode.
With ARG, turn tplsub mode on iff arg is positive.
The mode, whose commands all have prefix \\[tplsub-mode-prefix-key],
...

\\{tplsub-mode-map}"
  (interactive "P")
  (make-local-variable 'tplsub-mode)
  (setq tplsub-mode
	(if (null arg)
	    (not tplsub-mode)
	  (> (prefix-numeric-value arg) 0)))

  (force-mode-line-update)
  (if tplsub-mode
      (progn
	(tplsub-set-according-to-mode)
	(if tplsub-tmpl-key (local-set-key tplsub-tmpl-key
					    'tplsub-templates))
	(if tplsub-help-key (local-set-key tplsub-help-key
					    'tplsub-syntax-help))
	(run-hooks 'tplsub-mode-hook))
    (if tplsub-tmpl-key (local-unset-key tplsub-tmpl-key))
    (if tplsub-help-key (local-unset-key tplsub-help-key))
    (run-hooks 'tplsub-mode-off-hook)))

(require 'tplsub-x)
(provide 'tplsub)

;;; tplsub.el ends here
