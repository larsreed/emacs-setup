;;;; igrep.el --- An improved interface to `grep`.

;;; Description:
;;;
;;; Define the \[igrep] command, which is like \[grep] except that it
;;; takes three required arguments (PROGRAM, EXPRESSION, and FILES) and
;;; an optional argument (OPTIONS) instead of just one argument (COMMAND).
;;; Also define the analogous \[egrep] and \[fgrep] commands for convenience.
;;;
;;; Define the \[igrep-recursively] command, which is like \[igrep]
;;; except that it uses `find` to recursively `grep` a directory.  Also
;;; define the analogous \[egrep-recursively] and \[fgrep-recursively]
;;; commands for convenience.
;;;
;;; \[igrep] and \[igrep-recursively] (and their analogues) provide
;;; defaults for the required arguments when called interactively and
;;; there are global variables that control the syntax of the `grep` and
;;; `find` shell commands that are executed.
;;;
;;; \[agrep] and \[agrep-recursively] are now defined as convenient
;;; interfaces to the approximate `grep` utility, which is distributed
;;; with the `glimpse' indexing and query tool (available from
;;; http://glimpse.cs.arizona.edu:1994/).
;;;
;;; And now \[grep] itself has been advised to provide the \[igrep]
;;; interface when called interactively (when called programmatically,
;;; it still uses the original argument list).  \[grep-recursively] is
;;; defined as an alias for \[igrep-recursively].

;;; Copyright:
;;;
;;; Copyright (C) 1994,1995,1996 Kevin Rodgers
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; Neither my former nor current employer (Martin Marietta and
;;; Information Handling Services, respectively) has disclaimed any
;;; copyright interest in igrep.el.
;;;
;;; Kevin Rodgers <kevinr@ihs.com>		Project Engineer
;;; Information Handling Services		Electronic Systems Development
;;; 15 Inverness Way East, M/S A201
;;; Englewood CO 80112 USA			(303)397-2807[voice]/-2779[fax]

;;; Installation:
;;;
;;; 1. Put this file in a directory that is a member of load-path, and
;;;    byte-compile it (e.g. with `M-x byte-compile-file') for better
;;;    performance.  You can ignore any warnings about references to free
;;;    variables and "not known to be defined" functions.
;;; 2. Put these forms in default.el or ~/.emacs:
;;;    (autoload (function igrep) "igrep"
;;;       "*Run `grep` PROGRAM to match EXPRESSION in FILES..." t)
;;;    (autoload (function igrep-recursively) "igrep"
;;;       "*Run `grep` recursively..." t)
;;;    (autoload (function dired-do-igrep) "igrep"
;;;       "*Run `grep` on the marked (or next prefix ARG) files." t)
;;;    (autoload (function dired-do-igrep-recursively) "igrep"
;;;       "*Run `grep` recursively on the marked (or next prefix ARG) directories." t)
;;; 2. a. For completeness, you can add these forms as well:
;;;    (autoload (function grep) "igrep"
;;;       "*Run `grep` PROGRAM to match EXPRESSION in FILES..." t)
;;;    (autoload (function egrep) "igrep"
;;;       "*Run `egrep`..." t)
;;;    (autoload (function fgrep) "igrep"
;;;       "*Run `fgrep`..." t)
;;;    (autoload (function agrep) "igrep"
;;;       "*Run `agrep`..." t)
;;;    (autoload (function grep-recursively) "igrep"
;;;       "*Run `grep` recursively..." t)
;;;    (autoload (function egrep-recursively) "igrep"
;;;       "*Run `egrep` recursively..." t)
;;;    (autoload (function fgrep-recursively) "igrep"
;;;       "*Run `fgrep` recursively..." t)
;;;    (autoload (function agrep-recursively) "igrep"
;;;       "*Run `agrep` recursively..." t)
;;;    (autoload (function dired-do-grep) "igrep"
;;;       "*Run `grep` on the marked (or next prefix ARG) files." t)
;;;    (autoload (function dired-do-grep-recursively) "igrep"
;;;       "*Run `grep` recursively on the marked (or next prefix ARG) directories." t)

;;; Usage:
;;;
;;; M-x igrep			M-x igrep-recursively
;;; M-x  grep			M-x  grep-recursively
;;; M-x egrep			M-x egrep-recursively
;;; M-x fgrep			M-x fgrep-recursively
;;; M-x agrep			M-x agrep-recursively
;;; M-x dired-do-igrep		M-x dired-do-igrep-recursively
;;; M-x  dired-do-grep		M-x  dired-do-grep-recursively
;;; (Each of the 10 igrep commands accepts 1, 2, or 3 `C-u' prefix arguments.
;;; The 2 Dired commands interpret a prefix argument like the regular `dired-do'
;;; commands.)
;;; C-x ` (M-x next-error)	C-c C-c (M-x compile-goto-error) [in *igrep*]

;;; Customization examples:
;;;
;;; To ignore case by default:
;;; 	(setq igrep-options "-i")
;;; To search subdirectories by default:
;;; 	(setq igrep-recursively t)
;;; To search compressed files with GNU gzip scripts:
;;; 	(setq igrep-program "zgrep")
;;; To avoid exceeding the shell's limit on command argument length
;;; (this only works when grep'ing files in the current directory):
;;; 	(setq igrep-recursively t
;;; 	      igrep-recursivley-prune-option "\\! -name .")

;;; To do:
;;;
;;; 1. Delete the *-program variables (except for igrep-program) and
;;;    replace igrep-options with a table that maps igrep-program to the
;;;    appropriate options.
;;; 2. Provide support for `glimpse`.

;;; LCD Archive Entry:
;;;
;;; igrep|Kevin Rodgers|kevinr@ihs.com|
;;; An improved interface to grep/egrep/fgrep; plus recursive `find` versions.|
;;; 96/06/14|2.41|~/misc/igrep.el.Z|


;;; Package interface:

(provide 'igrep)

(require 'backquote)			; igrep-with-default-in-history
(require 'compile)			; compile-internal and grep-regexp-
					; alist

(defconst igrep-version "2.41"
  "Version of igrep.el")


;;; User options:

(defvar igrep-options nil
  "*The options passed by \\[igrep] to `igrep-program', or nil.

`-n' will automatically be passed to `igrep-program', to generate the
output expected by \\[next-error] and \\[compile-goto-error].
`-e' will automatically be passed to `igrep-program', if it supports
that option.")

(defvar igrep-read-options nil
  "*If non-nil, `\\[igrep]' always prompts for options;
otherwise, it only prompts when 1 or 3 `C-u's are given as a prefix arg.")

(defvar igrep-read-multiple-files nil
  "*If non-nil, `\\[igrep]' always prompts for multiple-files;
otherwise, it only prompts when 2 or 3 `C-u's are given as a prefix arg.")

(defvar igrep-verbose-prompts t
  "*If t, \\[igrep] prompts for arguments verbosely;
if not t but non-nil, \\[igrep] prompts for arguments semi-verbosely;
if nil, \\[igrep] prompts for arguments tersely.")

(defvar igrep-save-buffers 'query
  "*If t, \\[igrep] first saves each modified file buffer;
if not t but non-nil, \\[igrep] offers to save each modified file buffer.")


;;; User variables:

(defvar igrep-program "grep"
  "The default shell program run by \\[igrep] and \\[igrep-recursively].
It must take a `grep` expression argument and one or more file names.
If nil, \\[igrep] prompts for the program to run.")

(defvar igrep-expression-quote-char
  (if (memq system-type '(ms-dos windows-95 windows-nt))
      ?\"
    ?')
  "The character used to delimit the EXPRESSION argument to \\[igrep],
to protect it from shell filename expansion.")

(defvar igrep-recursively nil
  "If non-nil, \\[igrep] recursively searches files using `find`.
See \\[igrep-recursively].")

(defvar igrep-recursively-prune-options "-name SCCS -o -name RCS"
  "The `find` clause used to prune directories, or nil;
see \\[igrep-recursively].")

(defvar igrep-recursively-file-options "-type f -o -type l"
  "The `find` clause used to filter files passed to `grep`, or nil;
see \\[igrep-recursively].")

(defvar igrep-recursively-use-xargs
  (if (equal (call-process "find" nil nil nil
			   (if (boundp 'grep-null-device)
			       grep-null-device
			     "/dev/null")
			   "-print0")
	     0)
      'gnu)
  "If `gnu', \\[igrep-recursively] executes
	`find ... -print0 | xargs -0 -e grep ...`;
if not `gnu' but non-nil, \\[igrep-recursively] executes
	`find ... -print | xargs -e grep ...`;
if nil, \\[igrep-recursively] executes
	`find ... -exec grep ...`.")

(defvar igrep-program-table
  (let ((exec-directories exec-path)
	(program-obarray (make-vector 11 0)))
    (while exec-directories
      (if (and (car exec-directories)
	       (file-directory-p (car exec-directories)))
	  (let ((grep-programs
		 (directory-files (car exec-directories) nil "grep\\'")))
	    (while grep-programs
	      (intern (car grep-programs) program-obarray)
	      (setq grep-programs (cdr grep-programs)))))
      (setq exec-directories (cdr exec-directories)))
    program-obarray)
  "An obarray of available `grep` programs, passed by `igrep-read-program'
to `completing-read'.")

(defvar igrep-expression-history '()
  "The minibuffer history list for \\[igrep]'s EXPRESSION argument.")

(defvar igrep-files-history '()
  "The minibuffer history list for \\[igrep]'s FILES argument.")


;;; Commands:

(defadvice grep (around igrep-interface first (&rest grep-args) activate)
  "If called interactively, use the \\[igrep] interface instead,
where GREP-ARGS is (PROGRAM EXPRESSION FILES OPTIONS);
if called programmatically, GREP-ARGS is still (COMMAND)."
  (interactive (igrep-read-args))
  (if (interactive-p)
      (apply (function igrep) grep-args)
    ad-do-it))

(defalias 'grep-recursively 'igrep-recursively)

(defun igrep (program expression files &optional options)
  "*Run `grep` PROGRAM to match EXPRESSION in FILES.
The output is displayed in the *igrep* buffer, which \\[next-error] and
\\[compile-goto-error] parse to find each line of matched text.

PROGRAM may be nil, in which case it defaults to `igrep-program'.

EXPRESSION is automatically delimited by `igrep-expression-quote-char'.

FILES is either a file name pattern (expanded by the shell named by
`shell-file-name') or a list of file name patterns.

Optional OPTIONS is also passed to PROGRAM; it defaults to `igrep-options'.

If a prefix argument \
\(\\[universal-argument]\) \
is given when called interactively,
or if `igrep-read-options' is set, OPTIONS is read from the minibuffer.

If two prefix arguments \
\(\\[universal-argument] \\[universal-argument]\) \
are given when called interactively,
or if `igrep-read-multiple-files' is set, FILES is read from the minibuffer
multiple times.

If three prefix arguments \
\(\\[universal-argument] \\[universal-argument] \\[universal-argument]\) \
are given when called interactively,
or if `igrep-read-options' and `igrep-read-multiple-files' are set,
OPTIONS is read and FILES is read multiple times.

If `igrep-recursively' is non-nil, the directory or directories
containing FILES is recursively searched for files whose name matches
the file name component of FILES \(and whose contents match
EXPRESSION\)."
  (interactive
   (igrep-read-args))
  (if (null program)
      (setq program (or igrep-program "grep")))
  (if (null options)
      (setq options igrep-options))
  (if (not (listp files))		; (stringp files)
      (setq files (list files)))
  (if (string-match "^[rj]?sh$" (file-name-nondirectory shell-file-name))
      ;; (restricted, job-control, or standard) Bourne shell doesn't expand ~:
      (setq files
	    (mapcar 'expand-file-name files)))
  (let ((command (format "%s -n %s %s %c%s%c %s %s"
			 program
			 (or options "")
			 (if (or (string= program "agrep")
				 (eq system-type 'berkeley-unix)
				 (save-match-data
				   (string-match "-sco" system-configuration)))
			     "-e"	; BSD
			   (progn	; SVR4
			     (if (save-match-data
				   (string-match "\\`-" expression))
				 (setq expression (concat "\\" expression)))
			     ""))
			 igrep-expression-quote-char
			 expression
			 igrep-expression-quote-char
			 (if igrep-recursively
			     (if igrep-recursively-use-xargs
				 ""
			       "\"{}\"")
			   (mapconcat (function identity) files " "))
			 (if (boundp 'grep-null-device)
			     grep-null-device
			   "/dev/null"))))
    (if igrep-recursively
	(setq command
	      (igrep-format-recursive-command command files)))
    (cond ((eq igrep-save-buffers t) (save-some-buffers t))
	  (igrep-save-buffers (save-some-buffers)))
    (compile-internal command
		      (format "No more %c%s%c matches"
			      igrep-expression-quote-char
			      program
			      igrep-expression-quote-char)
		      "igrep" nil grep-regexp-alist)))

;; Analogue commands:

(defun egrep (&rest igrep-args)
  "*Run `egrep` via \\[igrep].
All arguments \(including prefix arguments, when called interactively\)
are handled by `igrep'."
  (interactive
   (let ((igrep-program "egrep"))
     (igrep-read-args)))
  (apply (function igrep) "egrep" (cdr igrep-args)))

(defun fgrep (&rest igrep-args)
  "*Run `fgrep` via \\[igrep].
All arguments \(including prefix arguments, when called interactively\)
are handled by `igrep'."
  (interactive
   (let ((igrep-program "fgrep"))
     (igrep-read-args)))
  (apply (function igrep) "fgrep" (cdr igrep-args)))

(defun agrep (&rest igrep-args)
  "*Run `agrep` via \\[igrep].
All arguments \(including prefix arguments, when called interactively\)
are handled by `igrep'."
  (interactive
   (let ((igrep-program "agrep"))
     (igrep-read-args)))
  (apply (function igrep) "agrep" (cdr igrep-args)))

;; Recursive (`find`) commands:

(defun igrep-recursively (&rest igrep-args)
  "*Run `grep` recursively; see \\[igrep] and `igrep-recursively'.
All arguments \(including prefix arguments, when called interactively\)
are handled by `igrep'."
  (interactive
   (let ((igrep-recursively t))
     (igrep-read-args)))
  (let ((igrep-recursively t))
    (apply (function igrep) igrep-args)))

;; Analogue recursive (`find`) commands:

(defun egrep-recursively (&rest igrep-args)
  "*Run `egrep` recursively via \\[igrep-recursively].
All arguments \(including prefix arguments, when called interactively\)
are handled by `igrep'."
  (interactive
   (let ((igrep-program "egrep")
	 (igrep-recursively t))
     (igrep-read-args)))
    (apply (function igrep-recursively) "egrep" (cdr igrep-args)))

(defun fgrep-recursively (&rest igrep-args)
  "*Run `fgrep` recursively via \\[igrep-recursively].
All arguments \(including prefix arguments, when called interactively\)
are handled by `igrep'."
  (interactive
   (let ((igrep-program "fgrep")
	 (igrep-recursively t))
     (igrep-read-args)))
  (apply (function igrep-recursively) "fgrep" (cdr igrep-args)))

(defun agrep-recursively (&rest igrep-args)
  "*Run `agrep` recursively via \\[igrep-recursively].
All arguments \(including prefix arguments, when called interactively\)
are handled by `igrep'."
  (interactive
   (let ((igrep-program "agrep")
	 (igrep-recursively t))
     (igrep-read-args)))
  (apply (function igrep-recursively) "agrep" (cdr igrep-args)))

;; Dired commands:

(defun dired-do-igrep (program expression &optional options arg)
  "*Run `grep` PROGRAM to match EXPRESSION (with optional OPTIONS)
on the marked (or next prefix ARG) files."
  (interactive
   (let ((igrep-args (let ((current-prefix-arg nil))
		       (igrep-read-args t))))
     ;; Delete FILES:
     (setcdr (nthcdr 1 igrep-args) (nthcdr 3 igrep-args))
     ;; Append ARG:
     (nconc igrep-args (list current-prefix-arg))))
  (igrep program
	 expression
	 (funcall (cond ((fboundp 'dired-get-marked-files) ; GNU Emacs
			 'dired-get-marked-files)
			((fboundp 'dired-mark-get-files) ; XEmacs
			 'dired-mark-get-files))
		  t arg)
	 options))

(defalias 'dired-do-grep 'dired-do-igrep)

;; Dired recursive (`find`) commands:

(defun dired-do-igrep-recursively (program expression &optional options arg)
  "*Run `grep` PROGRAM to match EXPRESSION (with optional OPTIONS)
on the marked (or next prefix ARG) directories."
  (interactive
   (let ((igrep-args (let ((igrep-recursively t)
			   (current-prefix-arg nil))
		       (igrep-read-args t))))
     ;; Delete FILES:
     (setcdr (nthcdr 1 igrep-args) (nthcdr 3 igrep-args))
     ;; Append ARG:
     (nconc igrep-args (list current-prefix-arg))))
  (let ((igrep-recursively t))
    (dired-do-igrep program expression options arg)))

(defalias 'dired-do-grep-recursively 'dired-do-igrep-recursively)


;;; Utilities:

(defsubst igrep-file-directory (name)
  ;; Return the directory component of NAME, or "." if it has no
  ;; directory component.
  (directory-file-name (or (file-name-directory name)
			   (file-name-as-directory "."))))

(defsubst igrep-file-pattern (name)
  ;; Return the file component of NAME, or "*" if it has no file
  ;; component.
  (let ((pattern (file-name-nondirectory name)))
       (if (string= pattern "")
	   "*"
	 pattern)))

(defun igrep-format-recursive-command (command files)
  ;; Format `grep` COMMAND to be recursively invoked on FILES.
  (let ((directories '())
	(patterns '()))
    (while files
      (let ((dir (igrep-file-directory (car files)))
	    (pat (igrep-file-pattern (car files))))
	(if (and (not (string= dir "."))
		 (file-symlink-p dir))
	    (setq dir (concat dir "/.")))
	(if (not (member dir directories))
	    (setq directories (cons dir directories)))
	(cond ((equal pat "*")
	       (setq patterns t))
	      ((and (listp patterns)
		    (not (member pat patterns)))
	       (setq patterns (cons pat patterns)))))
      (setq files (cdr files)))
    (format (cond ((eq igrep-recursively-use-xargs 'gnu)
		   "find %s %s %s %s -print0 | \\\nxargs -0 -e %s")
		  (igrep-recursively-use-xargs
		   "find %s %s %s %s -print | \\\nxargs -e %s")
		  (t
		   "find %s %s %s %s \\\n\t-exec %s \\;"))
	    (mapconcat (function identity) (nreverse directories)
		       " ")
	    (if igrep-recursively-prune-options
		(format "-type d \\( %s \\) -prune -o"
			igrep-recursively-prune-options)
	      "")
	    (if igrep-recursively-file-options
		(format "\\( %s \\)" igrep-recursively-file-options)
	      "")
	    (if (listp patterns)
		(if (cdr patterns)	; (> (length patterns) 1)
		    (concat "\\( "
			    (mapconcat (function (lambda (pat)
						   (format "-name \"%s\"" pat)))
				       (nreverse patterns)
				       " -o ")
			    " \\)")
		  (format "-name \"%s\"" (car patterns)))
	      "")
	    command)))

(defun igrep-read-args (&optional no-files)
  ;; Read and return a list: (PROGRAM EXPRESSION FILES OPTIONS).
  ;; If NO-FILES is non-nil, then FILES is not read and nil is returned
  ;; in its place.
  (let* ((program (igrep-read-program (if igrep-verbose-prompts
					  (if igrep-recursively
					      "[recursive] "))))
	 (prompt-prefix (if igrep-verbose-prompts
			    (apply (function concat)
				   (if igrep-recursively
				       "[recursive] ")
				   (if (eq igrep-verbose-prompts t)
				       (list program " ")))))
	 (options (igrep-read-options prompt-prefix)))
    (if (eq igrep-verbose-prompts t)
	(setq prompt-prefix
	      (concat prompt-prefix options " ")))
    (list program
	  (igrep-read-expression prompt-prefix)
	  (if (not no-files)
	      (igrep-read-files prompt-prefix))
	  options)))

(defsubst igrep-prefix (prefix string)
  ;; If PREFIX is non-nil, concatenate it and STRING; otherwise, return STRING.
  (if prefix
      (concat prefix string)
    string))

(defun igrep-read-program (&optional prompt-prefix)
  ;; If igrep-program is nil, read and return a program name from the
  ;; minibuffer; otherwise, return igrep-program.
  ;; Optional PROMPT-PREFIX is prepended to the "Program: " prompt.
  (or igrep-program
      (let ((prompt "Program: "))
	(completing-read (igrep-prefix prompt-prefix prompt) igrep-program-table
			 nil t "grep"))))

(defun igrep-read-options (&optional prompt-prefix)
  ;; If current-prefix-arg is '(4) or '(64), read and return an options
  ;; string from the minibuffer; otherwise, return igrep-options.
  ;; Optional PROMPT-PREFIX is prepended to the "Options: " prompt.
  (if (or igrep-read-options
	  (and (consp current-prefix-arg)
	       (memq (prefix-numeric-value current-prefix-arg)
		     '(4 64))))
      (let ((prompt "Options: "))
	(read-string (igrep-prefix prompt-prefix prompt)
		     (or igrep-options "-")))
    igrep-options))

(defun igrep-read-expression (&optional prompt-prefix)
  ;; Read and return a `grep` expression string from the minibuffer.
  ;; Optional PROMPT-PREFIX is prepended to the "Expression: " prompt.
  (let ((default-expression (igrep-default-expression)))
    (if (string= default-expression "")
	(read-from-minibuffer (igrep-prefix prompt-prefix "Expression: ")
			      nil nil nil 'igrep-expression-history)
      (let ((expression
	     (igrep-read-string-with-default-in-history
	      (igrep-prefix prompt-prefix (format "Expression (default %s): "
						  default-expression))
	      default-expression
	      'igrep-expression-history)))
	(if (string= expression "")
	    default-expression
	  expression)))))

(defun igrep-default-expression ()
  (if (eq major-mode 'dired-mode)
      (let ((dired-file (dired-get-filename nil t)))
	(save-excursion
	  (set-buffer (or (and dired-file (get-file-buffer dired-file))
			  (other-buffer (current-buffer) t)))
	  (current-word)))
    (current-word)))

(defsubst igrep-default-key ()
  ;; Return the key bound to `exit-minibuffer', preferably "\r".
  (if (eq (lookup-key minibuffer-local-completion-map "\r")
	  (function exit-minibuffer))
      "\r"
    (where-is-internal (function exit-minibuffer)
		       minibuffer-local-completion-map
		       t)))

(defun igrep-read-files (&optional prompt-prefix)
  ;; Read and return a file name pattern from the minibuffer.  If
  ;; current-prefix-arg is '(16) or '(64), read multiple file name
  ;; patterns and return them in a list.  Optional PROMPT-PREFIX is
  ;; prepended to the "File(s): " prompt.
  (let* ((dired-subdirectory (if (eq major-mode 'dired-mode)
				 (dired-current-directory t)))
	 (default-files (concat dired-subdirectory
				(igrep-default-file-pattern)))
	 (prompt (format "File(s) (default %s): " default-files))
	 (insert-default-directory nil)	; use relative path names
	 (file (igrep-read-file-name-with-default-in-history
		(igrep-prefix prompt-prefix prompt)
		default-files
		nil
		'igrep-files-history)))
    (if (or igrep-read-multiple-files
	    (and (consp current-prefix-arg)
		 (memq (prefix-numeric-value current-prefix-arg)
		       '(16 64))))
	(let ((files (list file)))
	  (setq prompt
		(igrep-prefix prompt-prefix
			      (if igrep-verbose-prompts
				  (format "File(s): [Type `%s' when done] "
					  (key-description (igrep-default-key)))
				"File(s): ")))
	  (while (not (string= (setq file
				     (igrep-read-file-name prompt
							   nil "" nil nil
							   'igrep-files-history))
			       ""))
	    (setq files (cons file files)))
	  (nreverse files))
      file)))

(defmacro igrep-with-default-in-history (default history &rest body)
  ;; Temporarily append DEFAULT to HISTORY, and execute BODY forms.
  (` (progn
       ;; Append default to history:
       (set history
	    (cons (, default)
		  (if (boundp (, history))
		      (symbol-value (, history))
		    '())))
       (unwind-protect			; Make sure the history is restored.
	   ;; Execute body:
	   (progn (,@ body))
	 ;; Delete default from history (undo the append above):
	 (setcdr (symbol-value (, history))
		 (nthcdr 2 (symbol-value (, history))))))))

(defun igrep-read-string-with-default-in-history (prompt default history)
  ;; Read a string from the minibuffer, prompting with string PROMPT.
  ;; DEFAULT can be inserted into the minibuffer with `previous-
  ;; history-element'; HISTORY is a symbol whose value (if bound) is a
  ;; list of previous results, most recent first.
  (let ((string (igrep-with-default-in-history default history
		  (read-from-minibuffer prompt nil nil nil history))))
    ;; Replace empty string in history with default:
    (if (string= string "")
	(setcar (symbol-value history) default))
    string))

(defun igrep-read-file-name-with-default-in-history
  (prompt &optional default initial history)
  ;; Read a file name from the minibuffer, prompting with string PROMPT.
  ;; DEFAULT can be inserted into the minibuffer with `previous-
  ;; history-element'; HISTORY is a symbol whose value (if any) is a
  ;; list of previous results, most recent first.
  (igrep-with-default-in-history default history
    (igrep-read-file-name prompt nil default nil initial history)))

(defun igrep-read-file-name (prompt
  &optional directory default existing initial history)
  ;; Just like read-file-name, but with optional HISTORY.
  ;; Also: convert DIRECTORY to DIRECTORY/* file name pattern.
  (let ((file-name
	 (if history
	     (let ((file-name-history (symbol-value history)))
	       (prog1 (read-file-name prompt directory default existing initial)
		 (set history file-name-history)))
	   (read-file-name prompt directory default existing initial))))
    (if (file-directory-p file-name)
	(expand-file-name "*" file-name)
      file-name)))

(defun igrep-default-file-pattern ()
  ;; Return a shell file name pattern that matches files with the same
  ;; extension as the file being visited in the current buffer.
  ;; (Based on other-possibly-interesting-files in ~/as-is/unix.el, by
  ;; Wolfgang Rupprecht <wolfgang@mgm.mit.edu>.)
  (if (eq major-mode 'dired-mode)
      (cond ((stringp dired-directory)
	     (if (file-directory-p dired-directory)
		 "*"
	       (file-name-nondirectory dired-directory))) ; wildcard
	    ((consp dired-directory)	; (DIR FILE ...)
	     (mapconcat 'identity (cdr dired-directory) " ")))
    (if buffer-file-name
	(let ((file-name (file-name-nondirectory buffer-file-name)))
	  (concat "*"
		  (save-match-data
		    (if (string-match "\\.[^.]+\\(\\.g?[zZ]\\)?$" file-name)
			(substring file-name
				   (match-beginning 0) (match-end 0))))))
      "*")))

;;; Local Variables:
;;; eval: (put 'igrep-with-default-in-history 'lisp-indent-hook 2)
;;; End:

;;;; igrep.el ends here
