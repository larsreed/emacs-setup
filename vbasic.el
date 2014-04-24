;; vbasic.el --- A mode for editing Visual Basic programs.

;; Copyright (C) 1996, Fred White <fwhite@world.std.com>

;; Author: Fred White <fwhite@world.std.com>
;; Version: 1.0 (April 18, 1996)
;; Keywords: languages basic

;; LCD Archive Entry:
;; vbasic|Fred White|fwhite@world.std.com|
;; A mode for editing Visual Basic programs.|
;; 18-Apr-96|1.0|~/modes/vbasic.el.Z|

;; This file is NOT part of GNU Emacs but the same permissions apply.
;;
;; GNU Emacs  is free software;  you can redistribute it and/or modify
;; it under the terms of  the GNU General  Public License as published
;; by  the Free Software  Foundation;  either version  2, or (at  your
;; option) any later version.
;;
;; GNU  Emacs is distributed  in the hope that  it will be useful, but
;; WITHOUT    ANY  WARRANTY;  without even the     implied warranty of
;; MERCHANTABILITY or FITNESS FOR A  PARTICULAR PURPOSE.  See the  GNU
;; General Public License for more details.
;;
;; You should have received  a copy of  the GNU General Public License
;; along with GNU Emacs; see  the file COPYING.  If  not, write to the
;; Free Software Foundation, 675  Mass Ave, Cambridge, MA 02139,  USA.
;; This  program  is free  software;  you  can  redistribute it and/or
;; modify it  under  the terms of the  GNU  General Public License  as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.


;; Purpose of this package:
;;  This is a mode for editing programs written in The World's Most
;;  Successful Programming Language.  It features automatic
;;  indentation, font locking, keyword capitalization, and some minor
;;  convenience functions.

;; Installation instructions
;;  Put vbasic.el somewhere in your path, compile it, and add the
;;  following to your init file:

;;  (autoload 'vbasic-mode "vbasic" "Vbasic mode." t)
;;  (setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\)$" .
;;                                  vbasic-mode)) auto-mode-alist))

;; Of course, under Windows 3.1, you'll have to name this file
;; something shorter than vbasic.el



;; Known bugs:
;;  Doesn't know about ":" separated stmts
;;  Doesn't know about single-line IF stmts


;; todo:
;;  fwd/back-compound-statement
;;  completion over OCX methods and properties.
;;  ensure Then at the end of IF statements.
;;  IDE integration
;;  etc.


(provide 'vbasic)

(defvar vbasic-xemacs-p (string-match "XEmacs\\|Lucid" (emacs-version)))
(defvar vbasic-winemacs-p (string-match "Win-Emacs" (emacs-version)))

;; Variables you may want to customize.
(defvar vbasic-mode-indent 2 "*Default indentation per nesting level")
(defvar vbasic-fontify-p t "*Whether to fontify Basic buffers.")
(defvar vbasic-capitalize-keywords-p t
  "*Whether to capitalize BASIC keywords.")
(defvar vbasic-wild-files "*.frm *.bas *.cls"
  "*Wildcard pattern for BASIC source files")
(defvar vbasic-ide-pathname nil
  "*The full pathname of your Visual Basic exe file, if any.")


(defvar vbasic-keywords-to-highlight
  '("Dim" "If" "Then" "Else" "ElseIf" "End If")
  "*A list of keywords to highlight in Basic mode, or T, meaning all keywords")

(defvar vbasic-defn-templates
  (list "Public Sub ()\nEnd Sub\n\n"
	"Public Function () As Variant\nEnd Function\n\n"
	"Public Property Get ()\nEnd Property\n\n")
  "*List of function templates though which vbasic-new-sub cycles.")



(defvar vbasic-mode-syntax-table nil)
(if vbasic-mode-syntax-table
    ()
  (setq vbasic-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\' "\<" vbasic-mode-syntax-table) ; Comment starter
  (modify-syntax-entry ?\n ">" vbasic-mode-syntax-table)
  (modify-syntax-entry ?\\ "w" vbasic-mode-syntax-table)
  (modify-syntax-entry ?_ "w" vbasic-mode-syntax-table))


(defvar vbasic-mode-map nil)
(if vbasic-mode-map
    ()
  (setq vbasic-mode-map (make-sparse-keymap))
  (define-key vbasic-mode-map "\t" 'vbasic-indent-line)
  (define-key vbasic-mode-map "\r" 'vbasic-newline-and-indent)
  (define-key vbasic-mode-map "\M-\C-a" 'vbasic-beginning-of-defun)
  (define-key vbasic-mode-map "\M-\C-e" 'vbasic-end-of-defun)
  (define-key vbasic-mode-map "\M-\C-h" 'vbasic-mark-defun)
  (define-key vbasic-mode-map "\M-\C-\\" 'vbasic-indent-region)
  (define-key vbasic-mode-map "\M-q" 'vbasic-fill-or-indent)
  (if vbasic-xemacs-p
      (progn
	(if vbasic-winemacs-p
	    (define-key vbasic-mode-map '(control C) 'vbasic-start-ide))
	(define-key vbasic-mode-map "\M-G" 'vbasic-grep)
	(define-key vbasic-mode-map '(meta backspace) 'backward-kill-word)
	(define-key vbasic-mode-map '(control meta /) 'vbasic-new-sub))))


;; These abbrevs are valid only in a code context.
(defvar vbasic-mode-abbrev-table nil)

(defvar vbasic-mode-hook ())


;; Is there a way to case-fold all regexp matches?

(defconst vbasic-defun-start-regexp
  (concat
   "^[ \t]*\\([Pp]ublic \\|[Pp]rivate \\|[Ss]tatic \\)*"
   "\\([Ss]ub\\|[Ff]unction\\|[Pp]roperty +[GgSsLl]et\\|[Tt]ype\\)"
   "[ \t]+\\(\\w+\\)[ \t]*(?"))

(defconst vbasic-defun-end-regexp
  "^[ \t]*[Ee]nd \\([Ss]ub\\|[Ff]unction\\|[Pp]roperty\\|[Tt]ype\\)")


;; Includes the compile-time #if variation.
(defconst vbasic-if-regexp "^[ \t]*#?[Ii]f")
(defconst vbasic-else-regexp "^[ \t]*#?[Ee]lse\\([Ii]f\\)?")
(defconst vbasic-endif-regexp "[ \t]*#?[Ee]nd[ \t]*[Ii]f")

(defconst vbasic-continuation-regexp "^.*\\_[ \t]*$")
(defconst vbasic-label-regexp "^[ \t]*[a-zA-Z0-9_]+:$")

(defconst vbasic-select-regexp "^[ \t]*[Ss]elect[ \t]+[Cc]ase")
(defconst vbasic-case-regexp "^[ \t]*[Cc]ase")
(defconst vbasic-select-end-regexp "^[ \t]*[Ee]nd[ \t]+[Ss]elect")

(defconst vbasic-for-regexp "^[ \t]*[Ff]or")
(defconst vbasic-next-regexp "^[ \t]*[Nn]ext")

(defconst vbasic-do-regexp "^[ \t]*[Dd]o")
(defconst vbasic-loop-regexp "^[ \t]*[Ll]oop")

(defconst vbasic-while-regexp "^[ \t]*[Ww]hile")
(defconst vbasic-wend-regexp "^[ \t]*[Ww]end")

(defconst vbasic-with-regexp "^[ \t]*[Ww]ith")
(defconst vbasic-end-with-regexp "^[ \t]*[Ee]nd[ \t]+[Ww]ith")

(defconst vbasic-blank-regexp "^[ \t]*$")
(defconst vbasic-comment-regexp "^[ \t]*\\s<.*$")


;; This is some approximation of the set of reserved words in Visual Vbasic.
(defconst vbasic-all-keywords
  '("Aggregate" "And" "App" "AppActivate" "Application" "Array" "As"
    "Asc" "AscB" "Atn" "Beep" "BeginTrans" "ByVal" "CBool" "CByte" "CCur"
    "CDate" "CDbl" "CInt" "CLng" "CSng" "CStr" "CVErr" "CVar" "Call"
    "Case" "ChDir" "ChDrive" "Character" "Choose" "Chr" "ChrB"
    "ClassModule" "Clipboard" "Close" "Collection" "Column" "Columns"
    "Command" "CommitTrans" "CompactDatabase" "Component" "Components"
    "Const" "Container" "Containers" "Cos" "CreateDatabase" "CreateObject"
    "CurDir" "Currency" "DBEngine" "DDB" "Data" "Database" "Databases"
    "Date" "DateAdd" "DateDiff" "DatePart" "DateSerial" "DateValue" "Day"
    "Debug" "Declare" "Deftype" "DeleteSetting" "Dim" "Dir" "Do" "Domain"
    "Double" "Dynaset" "EOF" "Each" "Else" "End" "Environ" "Erase" "Err"
    "Error" "Exit" "Exp" "FV" "False" "Field" "Fields" "FileAttr"
    "FileCopy" "FileDateTime" "FileLen" "Fix" "Font" "For" "Form"
    "FormTemplate" "Format" "Forms" "FreeFile" "FreeLocks" "Function"
    "Get" "GetAllSettings" "GetAttr" "GetObject" "GetSetting" "GoSub"
    "GoTo" "Group" "Groups" "Hex" "Hour" "IIf" "IMEStatus" "IPmt" "IRR"
    "If" "InStr" "Input" "Int" "Integer" "Is" "IsArray" "IsDate" "IsEmpty"
    "IsError" "IsMissing" "IsNull" "IsNumeric" "IsObject" "Kill" "LBound"
    "LCase" "LOF" "LSet" "LTrim" "Left" "Len" "Let" "Like" "Line" "Load"
    "LoadPicture" "LoadResData" "LoadResPicture" "LoadResString" "Loc"
    "Lock" "Log" "Long" "Loop" "MDIForm" "MIRR" "Me" "MenuItems"
    "MenuLine" "Mid" "Minute" "MkDir" "Month" "MsgBox" "NPV" "NPer" "Name"
    "New" "Next" "Now" "Oct" "On" "Open" "OpenDatabase" "Operator"
    "Option" "PPmt" "PV" "Parameter" "Parameters" "Partition" "Picture"
    "Pmt" "Print" "Printer" "Printers" "Private" "ProjectTemplate"
    "Properties" "Public" "Put" "QBColor" "QueryDef" "QueryDefs" "RGB"
    "RSet" "RTrim" "Randomize" "Rate" "ReDim" "Recordset" "Recordsets"
    "RegisterDatabase" "Relation" "Relations" "Rem" "RepairDatabase"
    "Reset" "Resume" "Return" "Right" "RmDir" "Rnd" "Rollback" "RowBuffer"
    "SLN" "SYD" "SavePicture" "SaveSetting" "Screen" "Second" "Seek"
    "SelBookmarks" "Select" "SelectedComponents" "SendKeys" "Set"
    "SetAttr" "SetDataAccessOption" "SetDefaultWorkspace" "Sgn" "Shell"
    "Sin" "Single" "Snapshot" "Space" "Spc" "Sqr" "Static" "Stop" "Str"
    "StrComp" "StrConv" "String" "Sub" "SubMenu" "Switch" "Tab" "Table"
    "TableDef" "TableDefs" "Tan" "Then" "Time" "TimeSerial" "TimeValue"
    "Timer" "To" "Trim" "True" "Type" "TypeName" "UBound" "UCase" "Unload"
    "Unlock" "Val" "VarType" "Verb" "Weekday" "Wend"
    "While" "Width" "With" "Workspace" "Workspaces" "Write" "Year"))


(defun vbasic-word-list-regexp (keys)
  (let ((re "\\b\\(")
	(key nil))
    (while keys
      (setq key (car keys)
	    keys (cdr keys))
      (setq re (concat re key (if keys "\\|" ""))))
    (concat re "\\)\\b")))

(defun vbasic-keywords-to-highlight ()
  (if (eq vbasic-keywords-to-highlight t)
      vbasic-all-keywords
    vbasic-keywords-to-highlight))


(defvar vbasic-font-lock-keywords
  (list
   ;; Names of functions.
   (list vbasic-defun-start-regexp 3 'font-lock-function-name-face)

   ;; Statement labels
   (cons vbasic-label-regexp 'font-lock-keyword-face)

   ;; Case values
   ;; String-valued cases get font-lock-string-face regardless.
   (list "^[ \t]*[Cc]ase[ \t]+\\([^'\n]+\\)" 1 'font-lock-keyword-face t)

   ;; Any keywords you like.
   (cons (vbasic-word-list-regexp (vbasic-keywords-to-highlight))
	 'font-lock-keyword-face)))



(defun vbasic-mode ()
  "A mode for editing Microsoft Visual Basic programs.
Features automatic  indentation, font locking, keyword capitalization,
and some minor convenience functions.
Commands:
\\{vbasic-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map vbasic-mode-map)
  (setq major-mode 'vbasic-mode)
  (setq mode-name "Vbasic")
  (set-syntax-table vbasic-mode-syntax-table)

  (add-hook 'write-file-hooks 'vbasic-untabify)

  (setq local-abbrev-table vbasic-mode-abbrev-table)
  (if vbasic-capitalize-keywords-p
      (progn
	(make-local-variable 'pre-abbrev-expand-hook)
	(add-hook 'pre-abbrev-expand-hook 'vbasic-pre-abbrev-expand-hook)
	(abbrev-mode 1)))


  (make-local-variable 'comment-start)
  (setq comment-start "' ")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "'+ *")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-end)
  (setq comment-end "")

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'vbasic-indent-line)

  (make-local-variable 'font-lock-keywords)
  (setq font-lock-keywords vbasic-font-lock-keywords)

  (if vbasic-fontify-p
      (font-lock-mode 1))

  (run-hooks 'vbasic-mode-hook))


(defun vbasic-construct-keyword-abbrev-table ()
  (if vbasic-mode-abbrev-table
      nil
    (let ((words vbasic-all-keywords)
	  (word nil)
	  (list nil))
      (while words
	(setq word (car words)
	      words (cdr words))
	(setq list (cons (list (downcase word) word) list)))

      (define-abbrev-table 'vbasic-mode-abbrev-table list))))

(vbasic-construct-keyword-abbrev-table)


(defun vbasic-in-code-context-p ()
  (if (fboundp 'buffer-syntactic-context) ; XEmacs function.
      (null (buffer-syntactic-context))
    ;; Attempt to simulate buffer-syntactic-context
    ;; I don't know how reliable this is.
    (let* ((beg (save-excursion
		  (beginning-of-line)
		  (point)))
	   (list
	    (parse-partial-sexp beg (point))))
      (and (null (nth 3 list))		; inside string.
	   (null (nth 4 list))))))	; inside cocmment

(defun vbasic-pre-abbrev-expand-hook ()
  ;; Allow our abbrevs only in a code context.
  (setq local-abbrev-table
	(if (vbasic-in-code-context-p)
	    vbasic-mode-abbrev-table)))
	
	

(defun vbasic-newline-and-indent (&optional count)
  "Insert a newline, updating indentation."
  (interactive)
  (expand-abbrev)
  (vbasic-indent-line)
  (call-interactively 'newline-and-indent))

(defun vbasic-beginning-of-defun ()
  (interactive)
  (re-search-backward vbasic-defun-start-regexp))

(defun vbasic-end-of-defun ()
  (interactive)
  (re-search-forward vbasic-defun-end-regexp))

(defun vbasic-mark-defun ()
  (interactive)
  (beginning-of-line)
  (vbasic-end-of-defun)
  (set-mark (point))
  (vbasic-beginning-of-defun)
  (if vbasic-xemacs-p
      (zmacs-activate-region)))

(defun vbasic-indent-defun ()
  (interactive)
  (save-excursion
    (vbasic-mark-defun)
    (call-interactively 'vbasic-indent-region)))


(defun vbasic-fill-long-comment ()
  "Fills block of comment lines around point."
  ;; Derived from code in ilisp-ext.el.
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((comment-re "^[ \t]*\\s<+[ \t]*"))
      (if (looking-at comment-re)
	  (let ((fill-prefix
		 (buffer-substring
		  (progn (beginning-of-line) (point))
		  (match-end 0))))

	    (while (and (not (bobp))
			(looking-at vbasic-comment-regexp))
	      (forward-line -1))
	    (if (not (bobp)) (forward-line 1))

	    (let ((start (point)))

	      ;; Make all the line prefixes the same.
	      (while (and (not (eobp))
			  (looking-at comment-re))
		(replace-match fill-prefix)
		(forward-line 1))

	      (if (not (eobp))
		  (beginning-of-line))

	      ;; Fill using fill-prefix
	      (fill-region-as-paragraph start (point))))))))


(defun vbasic-fill-or-indent ()
  "Fill long comment around point, if any, else indent current definition."
  (interactive)
  (cond ((save-excursion
	   (beginning-of-line)
	   (looking-at vbasic-comment-regexp))
	 (vbasic-fill-long-comment))
	(t
	 (vbasic-indent-defun))))


(defun vbasic-new-sub ()
  "Insert template for a new subroutine. Repeat to cycle through alternatives."
  (interactive)
  (beginning-of-line)
  (let ((templates (cons vbasic-blank-regexp
			 vbasic-defn-templates))
	(tem nil)
	(bound (point)))
    (while templates
      (setq tem (car templates)
	    templates (cdr templates))
      (cond ((looking-at tem)
	     (replace-match (or (car templates)
				""))
	     (setq templates nil))))

    (search-backward "()" bound t)))


(defun vbasic-untabify ()
  "Do not allow any tabs into the file"
  (if (eq major-mode 'vbasic-mode)
      (untabify (point-min) (point-max)))
  nil)

(defun vbasic-default-tag ()
  (if (and (not (bobp))
	   (save-excursion
	     (backward-char 1)
	     (looking-at "\\w")))
      (backward-word 1))
  (let ((s (point))
	(e (save-excursion
	     (forward-word 1)
	     (point))))
    (buffer-substring s e)))

(defun vbasic-grep (tag)
  "Search BASIC source files in current directory for tag."
  (interactive
   (list (let* ((def (vbasic-default-tag))
		(tag (read-string
		      (format "Grep for [%s]: " def))))
	   (if (string= tag "") def tag))))

  (grep (format "grep -n %s %s" tag vbasic-wild-files)))



(defun vbasic-start-ide ()
  "Start Visual Basic (or your favorite IDE, (after Emacs, of course))
on the project file in the current directory.
Note: it's not a good idea to leave Visual Basic running while you
are editing in emacs, since Visual Basic has no provision for reloading
changed files."
  (interactive)
  (let (file)
    (cond ((not (fboundp 'win-exec))
	   (error "Not available"))
	  ((null vbasic-ide-pathname)
	   (error "No pathname set for Visual Basic. See vbasic-ide-pathname"))
	  ((setq file (car (directory-files (pwd) t "\\.vbp")))
	   (iconify-emacs)
	   (win-exec vbasic-ide-pathname 'win-show-normal file))
	  (t
	   (error "No project file found.")))))



;;; Indentation-related stuff.

(defun vbasic-indent-region (start end)
  "Perform vbasic-indent-line on each line in region."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    (while (and (not (eobp))
		(< (point) end))
      (if (not (looking-at vbasic-blank-regexp))
	  (vbasic-indent-line))
      (forward-line 1)))

  (cond ((fboundp 'zmacs-deactivate-region)
	 (zmacs-deactivate-region))
	((fboundp 'deactivate-mark)
	 (deactivate-mark))))



(defun vbasic-previous-line-of-code ()
  (if (not (bobp))
      (forward-line -1))	; previous-line depends on goal column
  (while (and (not (bobp))
	      (or (looking-at vbasic-blank-regexp)
		  (looking-at vbasic-comment-regexp)))
    (forward-line -1)))


(defun vbasic-find-original-statement ()
  ;; If the current line is a continuation from the previous, move
  ;; back to the original stmt.
  (let ((here (point)))
    (vbasic-previous-line-of-code)
    (while (and (not (bobp))
		(looking-at vbasic-continuation-regexp))
      (setq here (point))
      (vbasic-previous-line-of-code))
    (goto-char here)))

(defun vbasic-find-matching-stmt (open-regexp close-regexp)
  ;; Searching backwards
  (let ((level 0))
    (while (and (>= level 0) (not (bobp)))
      (vbasic-previous-line-of-code)
      (vbasic-find-original-statement)
      (cond ((looking-at close-regexp)
	     (setq level (+ level 1)))
	    ((looking-at open-regexp)
	     (setq level (- level 1)))))))

(defun vbasic-find-matching-if ()
  (vbasic-find-matching-stmt vbasic-if-regexp vbasic-endif-regexp))

(defun vbasic-find-matching-select ()
  (vbasic-find-matching-stmt vbasic-select-regexp vbasic-select-end-regexp))

(defun vbasic-find-matching-for ()
  (vbasic-find-matching-stmt vbasic-for-regexp vbasic-next-regexp))

(defun vbasic-find-matching-do ()
  (vbasic-find-matching-stmt vbasic-do-regexp vbasic-loop-regexp))

(defun vbasic-find-matching-while ()
  (vbasic-find-matching-stmt vbasic-while-regexp vbasic-wend-regexp))

(defun vbasic-find-matching-with ()
  (vbasic-find-matching-stmt vbasic-with-regexp vbasic-end-with-regexp))


(defun vbasic-calculate-indent ()
  (let ((original-point (point)))
    (save-excursion
      (beginning-of-line)
      ;; Some cases depend only on where we are now.
      (cond ((or (looking-at vbasic-defun-start-regexp)
		 (looking-at vbasic-label-regexp)
		 (looking-at vbasic-defun-end-regexp))
	     0)

	    ;; The outdenting stmts, which simply match their original.
	    ((or (looking-at vbasic-else-regexp)
		 (looking-at vbasic-endif-regexp))
	     (vbasic-find-matching-if)
	     (current-indentation))

	    ;; All the other matching pairs act alike.
	    ((looking-at vbasic-next-regexp) ; for/next
	     (vbasic-find-matching-for)
	     (current-indentation))

	    ((looking-at vbasic-loop-regexp) ; do/loop
	     (vbasic-find-matching-do)
	     (current-indentation))

	    ((looking-at vbasic-wend-regexp) ; while/wend
	     (vbasic-find-matching-while)
	     (current-indentation))

	    ((looking-at vbasic-with-regexp) ; with/end with
	     (vbasic-find-matching-with)
	     (current-indentation))

	    ((looking-at vbasic-select-end-regexp) ; select case/end select
	     (vbasic-find-matching-select)
	     (current-indentation))

	    ;; A case of a select is somewhat special.
	    ((looking-at vbasic-case-regexp)
	     (vbasic-find-matching-select)
	     (+ (current-indentation) vbasic-mode-indent))

	    (t
	     ;; Other cases which depend on the previous line.
	     (vbasic-previous-line-of-code)

	     ;; Skip over label lines, which always have 0 indent.
	     (while (looking-at vbasic-label-regexp)
	       (vbasic-previous-line-of-code))

	     (cond
	      ((looking-at vbasic-continuation-regexp)
	       (vbasic-find-original-statement)
	       ;; Indent continuation line under matching open paren,
	       ;; or else one word in.
	       (let* ((orig-stmt (point))
		      (matching-open-paren
		       (condition-case ()
			   (save-excursion
			     (goto-char original-point)
			     (beginning-of-line)
			     (backward-up-list 1)
			     ;; Only if point is now w/in cont. block.
			     (if (<= orig-stmt (point))
				 (current-column)))
			 (error nil))))
		 (cond (matching-open-paren
			(1+ matching-open-paren))
		       (t
			;; Else, after first word on original line.
			(back-to-indentation)
			(forward-word 1)
			(while (looking-at "[ \t]")
			  (forward-char 1))
			(current-column)))))
	      (t
	       (vbasic-find-original-statement)
	       (let ((indent (current-indentation)))
		 ;; All the various +indent regexps.
		 (cond ((looking-at vbasic-defun-start-regexp)
			(+ indent vbasic-mode-indent))

		       ((or (looking-at vbasic-if-regexp)
			    (looking-at vbasic-else-regexp))
			(+ indent vbasic-mode-indent))

		       ((or (looking-at vbasic-select-regexp)
			    (looking-at vbasic-case-regexp))
			(+ indent vbasic-mode-indent))
			
		       ((or (looking-at vbasic-do-regexp)
			    (looking-at vbasic-for-regexp)
			    (looking-at vbasic-while-regexp)
			    (looking-at vbasic-with-regexp))
			(+ indent vbasic-mode-indent))

		       (t
			;; By default, just copy indent from prev line.
			indent))))))))))

(defun vbasic-indent-to-column (col)
  (let* ((bol (save-excursion
		(beginning-of-line)
		(point)))
	 (point-in-whitespace
	  (<= (point) (+ bol (current-indentation))))
	 (blank-line-p
	  (save-excursion
	    (beginning-of-line)
	    (looking-at vbasic-blank-regexp))))

    (cond ((/= col (current-indentation))
	   (save-excursion
	     (beginning-of-line)
	     (back-to-indentation)
	     (delete-region bol (point))
	     (indent-to col))))

    ;; If point was in the whitespace, move back-to-indentation.
    (cond (blank-line-p
	   (end-of-line))
	  (point-in-whitespace
	   (back-to-indentation)))))


(defun vbasic-indent-line ()
  "Indent current line for VBASIC"
  (interactive)
  (vbasic-indent-to-column (vbasic-calculate-indent)))
