;;; fe-mode.el  --- Editing FE-logs

;;; Copyright (c) 1995 Lars Reed

;; Author:		Lars Reed <Lars@kalars.net>
;; Last-Modified:	$Date: 2007/06/19 20:06:54 $
;; Version:		$Id: fe-mode.el,v 1.3 2007/06/19 20:06:54 larsr Exp $
;; Keywords:		SCCS FE SPE
;; Adapted-By:

;;; Description:
;; FE-mode is a special mode for editing "FE-logs", derived from
;; indented-text-mode.  The following features are available:
;;
;;    * Automatic indentation:
;;	fe-mode automatically breaks lines when the right margin is
;;	reached.  It indents like indented-text-mode, i.e. the first
;;	character of the new line is placed directly underneath the
;;	first non-blank character of the previous line.  An added
;;	feature of fe-mode is that it automatically skips past the
;;	leading text when indenting line 2 of a field, see the
;;	description of `fe-indent-relative-maybe' below.
;;	Lines can also be broken and indented, use C-c C-m (or C-c RET)
;;	to achieve this (`newline-and-indent').  To indent after a plain
;;	"return", press C-c C-i (or C-c TAB,
;;	`fe-indent-relative-maybe').
;;
;;    * Cursor movement:
;;	The notion of a paragraph is changed, so that the standard
;;	paragraph movement commands, `backward-paragraph' (M-{) and
;;	`forward-paragraph' (M-}), move to the start of the next leading
;;	text.  The special commands C-c left (`fe-backward-field') and
;;	C-c right (`fe-forward-field') goes a step further and moves
;;	cursor beyond the title, to where the real text should start.
;;	Separator lines are automatically skipped.
;;	The command `fe-goto-field' (C-c g) moves to a named field (use
;;	a C-u prefix if you only want to search forward).
;;	`fe-current-field' (C-c .) tells you the name of the current
;;	field, possibly of limited value...
;;
;;    * True colors...:
;;      By adding an appropriate hook (se "Installation" below), fe-mode
;;      will highlight the following items in different colors:
;;	   - leading texts
;;	   - references to other FE-logs (FE-IDs)
;;	   - separator lines
;;	   - your own name (or another name you assign to the variable
;;	     `fe-sign' *before* opening the file, e.g. in .emacs).
;;
;;    * Getting help:
;;      Invoking `fe-field-help' (C-c h  or C-c ?) will display a short
;;      description of the field in which the cursor is located.  A set
;;      of legal values may be displayed, but it might be outdated...
;;	Another way of getting help: contact the author!
;;
;;    * Adding text:
;;	To set a given field to a given value, use `fe-set-field'
;;	(C-c s).  The command prompts you for a field name and a value.
;;	The text will be prepended to the current value, unless a prefix
;;	is given (type C-u C-c s), in which the case the first line of
;;	the old value will be removed (type C-x u if you regret
;;	it...).
;;	To add a new comment to the end of the log, use `fe-comment'
;;	(on C-c c).  This moves to the end of the file, adds a new
;;	leading text (if necessary) and a signature line, and positions
;;	the cursor ready to type.
;;	The command `fe-fix' (C-c SPACE) removes extraneous lines at the
;;	end of the file, spaces at the end of each line, and converts
;;	TABs to spaces.
;;
;;    * ToDo
;;      - A general `fe-fill-fields' function to automatically set several
;;        fields.
;;	- A `fe-clear-field' to empty a whole field.  Should be used by
;;	  `fe-set-field', too.

;;; Installation:
;;    To use fe-mode, make the following changes to your .emacs file:
;;    1  Place fe-mode.el in a directory mentioned in your load-path,
;;       or modify your load-path to add the the directory where
;;       fe-mode.el resides.
;;    2  Optionally byte-compile fe-mode.el.
;;    3	 Add a line containing this to your emacs:
;;         (autoload 'fe-mode "fe-mode" "Mode for editing FE-logs" t)
;;    4  Add also:
;;	    (setq auto-mode-alist
;;	       (append auto-mode-alist
;;		       (list '("[ef][09][0-9][0-9][0-9][0-9][0-9]\\.txt$"
;;			       . fe-mode))))
;;    5  If you want to use highlit19 to colorize, add
;;          (add-hook 'fe-mode-hook 'fe-highlight)
;;       Or, to use font-lock:
;;          (add-hook 'fe-mode-hook 'fe-font-lock)

;; ....

;;; Code:
;;; ========================================================================


(defconst fe-mode-version "2.3"
  "Version number of FE-mode")

(defconst fe-month-trans '(("Jan" . "01") ("Feb" . "02") ("Mar" . "03")
			   ("Apr" . "04") ("May" . "05") ("Jun" . "06")
			   ("Jul" . "07") ("Aug" . "08") ("Sep" . "09")
			   ("Oct" . "10") ("Nov" . "11") ("Dec" . "12")))
(defconst fe-date
  (concat
   (format "%02d"
	   (string-to-int
	    (substring (current-time-string) 22 24)))
   (cdr (assoc (substring (current-time-string) 4 7)
	       fe-month-trans))
   (format "%02d"
	   (string-to-int
	    (substring (current-time-string) 8 10)))
   )  "Todays date!")

(defvar fe-mode-map nil
  "Keymap used with FE-mode.")

(defconst fe-mode-name "FE"
  "Name used in mode-line.")

(defvar fe-mode-hook nil
  "Functions to call after loading fe-mode.")

(defvar fe-sign (user-login-name)
  "*FE-log signature")

(defvar fe-field-default '(("Sign" fe-sign)
			   ("Org"  "Sysdeco"))
  "Default fields to fill in with fe-fill-fields \(\[fe-fill-fields\]\)")

(defconst fe-field-help-alist
  '(("Type"    . "'f' for error, 'e' for change")
    ("Aar"     . "year, 2-digits")
    ("Nr"      . "ID within year and type (nnnn)")
    ("Kort"    . "Short (max 1 line) description")
    ("Status"  . "blank,OBS,AVKL,AVVIST,KLAR,IMPLEM,TEST or OK")
    ("Frist"   . "latest date of next status change (YYMMDD)")
    ("ExtID"   . "external reference")
    ("Kontakt" . "name of external contact (person)")
    ("Fra"     . "name of internal contact (UNIX ID)")
    ("Org"     . "Fra's organization")
    ("Tlf"     . "Fra's telephone number")
    ("Dato"    . "creation date (YYMMDD)")
    ("Beskr"   . "description of error/change")
    ("Begr"    . "the reason the change is requested")
    ("UNIXid"  . "used when encountering an error")
    ("CCASid"  . "the application login name used when encountering an error")
    ("PROGid"  . "the application in which the error occurrs")
    ("Maskin"  . "the machine (node) used when encountering the error")
    ("Katalog" . "the directory from which the program was run")
    ("DBnavn"  . "the database used")
    ("Trace"   . "name of a relevant application log file")
    ("Data"    . "description of the accessed when the error occurred")
    ("Delsys"  . ("Subsystem - one of:"
		  "*, DM, SPE, MAD, basicdata, ledger, invoice, order,"
		  "subscr, customer, rel2, history, sentaks, sale, reports,"
		  "doc, sysdeco, mach"))
    ("Modul"   . "normally the picture/procedure(s) concerned")
    ("ModID"   . "ID, e.g. SCCS-ID, of the module")
    ("Pri"     . "priority - one of '+', blank or '-'")
    ("Reg"     . "UNIX-ID of the person who *wrote* the first version")
    ("Regdato" . "date (YYMMDD) of first registration.")
    ("Ansv"    . "responsible when 'Status' is AVKL or OBS")
    ("Ref"     . "references to files, archive entries etc")
    ("FEref"   . "references to other FE-logs")
    ("Kopi"    . ("UNIX-IDs of people who need a copy of this log."
		  "This often automagically handled through the"
		  "subscription feature."))
    ("Lev"     . "CCAS 'customer' - B,L,N,P,R,F")
    ("Estimat" . "expected amount of work")
    ("Analyse" . "problem analysis.  E.g. what to do, possible consequences")
    ("Sign"    . ("UNIX-ID of decision maker."
		  "Set when changing 'Status' to AVKL,OBS,AVVIST,OK or KLAR"))
    ("Versjon" . "planned release")
    ("Impl"    . "UNIX-ID of responsible for implementation")
    ("Idato"   . "date implemented (YYMMDD)")
    ("QA"      . "UNIX-ID of responsible for QA")
    ("Qdato"   . "date verified (YYMMDD)")
    ("Test"    . "UNIX-ID of responsible for test")
    ("Tdato"   . "date tested (YYMMDD)")
    ("Dict"    . "changes in the data dictionary")
    ("Dok"     . "changes in documentation (requires dke-file)")
    ("Komm"    . "miscellaneous comments (with signature)")
    ("Lars"    . "that's me!")
    ("..."     . "..."))
  "Explanation of the fields.")

; ======================================================================

(eval-when-compile
  (require 'compile))

(defun fe-find-field (srch &optional no-backup)
  "Search for field."
  (let ((s-string (format "%-8s:" srch)))
    (if (not no-backup) (goto-char (point-min)))
    (search-forward s-string nil t)))

(defun fe-goto-field (field &optional arg)
  "Goto field"
  (interactive "sGoto field: \nP")
  (let ((p (point)))
    (save-excursion
      (if (fe-find-field field arg)
	  (setq p (point))
	  (error "%s not found" field)))
    (goto-char p)))

(defun fe-set-field (fnam txt &optional do-delete no-move no-backup no-error)
  "Set the value of a field named FNAM to TXT.
If DO-DELETE is non-nil (prefix when interactive), delete old field value
\(first line only\).
If optional fourth argument NO-MOVE is non-nil,  keep value of point.
If optional fifth argument NO-BACKUP is non-nil, do not return to
point-min before searching."
  (interactive "*sField: \nsValue: \nP")
  (let ((new-point (point)))
    (save-excursion
      (if (not (fe-find-field fnam no-backup))
	  (if no-error nil
	      (error "Cannot find field %s" fnam))
	  (if do-delete (let ((kill-whole-line nil))
			  (kill-line)))
	  (insert " " txt)
	  (setq new-point (point))))
    (if (not no-move)
	(goto-char new-point))))

(defun fe-check ()
  "Run fesjekk on current buffer"
  (interactive)
  (require 'compile)
  (save-some-buffers)
  (compile-internal (concat "fesjekk -s " (buffer-file-name))
		    "no more errors"))

(defun fe-indent-relative-maybe ()
  "Indent like indent-relative-maybe, but add extra spaces on the second line
of the field.  E.g.:

text:      |Komm    :  bla. bla    |    Bl.a. line 2
i-r-m :    |^ here                 |    ^ here
fe-i-r-m:  |           ^ here      |    ^ here
"
  (interactive "*")
  (indent-relative-maybe)
  (let (more)
    (save-excursion
      (beginning-of-line 0)
      (if (not (or (looking-at "[ \t]")
		   (looking-at "$")))
	  (setq more t)))
    (if more (insert-char ?\  10))))

(defun fe-backward-field (arg)
  "GoTo start of previous field"
  (interactive "p")
  (let ((p (point)))
    (backward-paragraph arg)
    (if (re-search-forward " *: *" nil t)
	(if (and (<= p (point))
		 (not (bobp)))
	    (fe-backward-field 2)))))

(defun fe-forward-field (arg)
  "GoTo start of next field"
  (interactive "p")
  (forward-paragraph arg)
  (if (and (re-search-forward " *: *" nil t)
	   (looking-at paragraph-start))
      (fe-forward-field 1)))

(defun fe-current-field ()
  "Find the name of the current field"
  (interactive)
  (require 'thingatpt)
  (save-excursion
    (beginning-of-line 1)
    (while (and (looking-at "^[ \t]+")
		(not (bobp)))
      (beginning-of-line 0))
    (let ((s (word-at-point)))
      (set-text-properties 0 (length s) nil s)
      (if (interactive-p) (message "%s" s))
      s)))

(defun fe-field-help ()
  "Display field description"
  (interactive)
  (let* ((curr-buf (current-buffer))
	 (this-f (fe-current-field))
	 (t-s (cdr-safe (assoc this-f fe-field-help-alist))))
    (pop-to-buffer (get-buffer-create "*FE*"))
    (goto-char (point-min))
    (save-excursion
      (if this-f (insert (format "`%s': " this-f)))
      (cond
       ((not t-s)
	(insert "not a known field...\n"))
       ((listp t-s)
	(insert (car t-s) "\n")
	(mapcar (function
		 (lambda (string)
		   (insert (format "     %s\n" string))))
		(cdr t-s)))
       (t
	(insert t-s "\n"))))
    (shrink-window-if-larger-than-buffer)
    (set-buffer-modified-p nil)
    (pop-to-buffer curr-buf)))

;; Emacs pre 19.30(?)...
(if (not (fboundp 'buffer-substring-no-properties))
    (fset 'buffer-substring-no-properties 'buffer-substring))

(defun fe-field-fill ()
  "Fill in fields according to \`fe-field-default'"
  (interactive "*")
  (let ((f-list fe-field-default)
	fe-cat
	field-name
	field-val
	field-cat
	field-del)
    (save-excursion
      (goto-char (point-min))
      (end-of-line)
      (setq fe-cat (buffer-substring-no-properties
		    (1- (point)) (point))))
    (while f-list
      (setq field-name (nth 0 (car f-list))
	    field-val  (nth 1 (car f-list))
	    field-cat  (nth 2 (car f-list))
	    field-del  (nth 3 (car f-list))
	    f-list     (cdr f-list))
      (if (and field-cat
	       (not (equal field-cat fe-cat)))
	  nil
	  (fe-set-field field-name field-val field-del nil nil t)))))

(defun fe-fix (&optional no-delete)
  "Whitespace fixup..."
  (interactive "*")
  (let (pp)
    (save-excursion
      (untabify (point-min) (point-max))
      (goto-char (point-min))
      (replace-regexp "\r$" "")
      (if no-delete ()
	  (goto-char (point-min))
	  (replace-regexp "[ \t]+$" ""))
      (goto-char (1- (point-max)))
      (setq pp (point))
      (while (looking-at "\n")
	(forward-char -1))
      (forward-char 1)
      (if (> pp (point))
	  (delete-char (- pp (point)))))))

(defun fe-comment ()
  "Add a comment!"
  (interactive "*")
  (fe-fix 1)
  (goto-char (point-max))
  (beginning-of-line 0)
  (if (looking-at "Komm *: *$")
      (progn
	(end-of-line)
	(insert " "))
      (beginning-of-line 2)
      (insert (format "%-8s: " "Komm")))
  (save-excursion
    (insert (format "\n%10s--- %s, %s\n" " " fe-sign fe-date))
    (fe-fix 1)))

(defun fe-insert-me ()
  "Insert fe-sign \(my user-ID\)"
  (interactive "*")
  (insert fe-sign))

(defun fe-insert-date ()
  "Insert current date"
  (interactive "*")
  (insert fe-date))

; ======================================================================

;; Set up main map
(if fe-mode-map ()
  (let ((newmap (make-sparse-keymap)))
    (define-key newmap "\C-cs"       'fe-set-field)
    (define-key newmap "\C-c?"       'fe-field-help)
    (define-key newmap "\C-ch"       'fe-field-help)
;;  (define-key newmap "\C-cf"       'fe-field-fill)
    (define-key newmap "\C-c "       'fe-fix)
    (define-key newmap "\C-c."       'fe-current-field)
    (define-key newmap "\C-cg"       'fe-goto-field)
    (define-key newmap "\C-cc"       'fe-comment)
    (define-key newmap "\C-cd"       'fe-insert-date)
    (define-key newmap "\C-cm"       'fe-insert-me)
    (define-key newmap [?\C-c left]  'fe-backward-field)
    (define-key newmap [?\C-c right] 'fe-forward-field)
    (define-key newmap [?\C-c ?\C-p] 'fe-backward-field)
    (define-key newmap [?\C-c ?\C-n] 'fe-forward-field)
    (define-key newmap [?\C-c ?\C-c] 'fe-check)
    (define-key newmap [?\C-c ?\C-i] 'fe-indent-relative-maybe)
    (define-key newmap [?\C-c ?\C-m] 'newline-and-indent)
    (setq fe-mode-map (nconc newmap 
			     (if (boundp 'indented-text-mode-map)
				 indented-text-mode-map
			       text-mode-map)))))

; hilights

(defconst fe-rexp-1 "^---.*")
(defconst fe-rexp-2 "\\<[ef][0-9][0-9][0-9][0-9][0-9][0-9]\\>")
(defconst fe-rexp-3 "^[a-zA-Z0-9]+[ \t]*:")

(defvar fe-font-lock-keywords
  (list (cons fe-sign    'font-lock-keyword-face)
	(cons fe-rexp-1  'font-lock-comment-face)
	(cons fe-rexp-2  (cond ((boundp 'font-lock-reference-face)
				'font-lock-reference-face)
			       ((boundp 'font-lock-constant-face)
				'font-lock-constant-face)
			       (t font-lock-keyword-face)))
	(cons fe-rexp-3  'font-lock-function-name-face)))

(defun fe-highlight ()
  (cond (window-system
	 (require 'hilit19)
	 (hilit-set-mode-patterns
	  'fe-mode
	  (list (list fe-sign     nil 'keyword)
		(list fe-rexp-1   nil 'comment)
		(list fe-rexp-2   nil 'crossref)
		(list fe-rexp-3   nil 'formula)
		)))))

(defun fe-font-lock ()
  (require 'font-lock)
  (turn-on-font-lock))

; ======================================================================

;;;###autoload
(defun fe-mode ()
  "FE-mode for editing FE-logs.
This is indented-text-mode with the following additions:
   - colorization of leading texts, comment lines and FE-refs,
     using hilit19 or font-lock.
     To use hilit19:  add \(add-hook \'fe-mode-hook \'fe-highlight\)
                      to your .emacs.
     To use font-lock, replace fe-highlight with fe-font-lock in the
     previous statement.
   - \\[forward-paragraph] and \\[backward-paragraph] moves from field
     to field
   - auto-indent indents past leading text
   - special key bindings:
\\{fe-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map fe-mode-map)
  (define-abbrev-table 'text-mode-abbrev-table ())
  (make-local-variable 'indent-line-function)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (set-syntax-table text-mode-syntax-table)
  (setq local-abbrev-table text-mode-abbrev-table
	indent-line-function 'fe-indent-relative-maybe
	comment-start "^--"
	comment-end   ""
	paragraph-start "^[-A-Za-z0-9][^\n]*:[^\n]*$"
	paragraph-separate "^-[^\n]*+$"
	font-lock-defaults '(fe-font-lock-keywords)
	fill-column 72
	major-mode 'fe-mode
	mode-name fe-mode-name)
  (auto-fill-mode t)
  (run-hooks 'text-mode-hook
	     'indented-text-mode-hook
	     'fe-mode-hook))

(provide 'fe-mode)

;;; fe-mode.el ends here
