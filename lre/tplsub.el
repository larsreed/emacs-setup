;; -- lexical-binding: t;--
;; tplsub.el - templates, abbreviations and syntax help
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author:              Lars Reed <Lars@kalars.net>
;; Version:             1.8
;; Keywords:  programming templates help abbrev
;; Copyright: (c) Lars Reed
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;    (autoload 'tplsub-mode "tplsub" "General abbreviations" t)
;;
;;  Todo:
;;   - notation to allow variables at left margin, e.g.
;;           §§text => text
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:


(require 'cl)

;; ---AF---
(defgroup tplsub nil
  "tplsub setup variables"
  :prefix "tplsub-")

;; --F--
(defcustom tplsub-dir-c "§"
  "* line control prefix for tplsub"
  :group 'tplsub :type 'string)

(defcustom tplsub-var-c tplsub-dir-c
  "* macro/variable character for tplsub"
  :group 'tplsub :type 'string)

(defcustom tplsub-enable-eval enable-local-eval
  "* Control processing of eval-expressions.
Defaults to the current value of `enable-local-eval' - which see for further
documentation."
  :group 'tplsub :type 'boolean)

(defcustom tplsub-default-high-limit 500
  "* When it is time to truncate the list, set to nil to avoid truncation"
  :group 'tplsub :type 'integer)

(defcustom tplsub-default-low-limit (round (/ tplsub-default-high-limit 2))
  "* How short to make the list when truncating.
Be sure it is smaller than the high limit..."
  :group 'tplsub :type 'integer)

(defcustom tplsub-save-file
  (cond ((fboundp 'convert-standard-filename)
         (convert-standard-filename "~/.tplsubhist"))
        ((or (eq system-type 'ms-dos)
             (eq system-type 'windows-nt))
         "~/tplsub.his")
        (t "~/.tplsubhist"))
  "Name of the file that records `tplsub-default-alist' values."
  :group 'tplsub :type 'file)

;; --A--
(defcustom tplsub-mode-name "|"
  "* Name used in mode-line."
  :group 'tplsub :type 'string)

(defcustom tplsub-tmpl-help nil
  "* Set to t to give help during template expansion"
  :group 'tplsub :type 'boolean)
(make-variable-buffer-local 'tplsub-tmpl-help)

(defcustom tplsub-no-no-help nil
  "* Set to t to silence \"no help\" messages \\=(just be quiet\\=)"
  :group 'tplsub :type 'boolean)

(defcustom tplsub-std-indent 4
  "* Standard indentation \\=(spaces\\=), or \\='tab for TAB"
  :group 'tplsub :type 'integer)
(make-variable-buffer-local 'tplsub-std-indent)

(defcustom tplsub-tab-indent tab-width
  "* Normal indentation for tab stops."
  :group 'tplsub :type 'integer)
(make-variable-buffer-local 'tplsub-tab-indent)

(defcustom tplsub-tmpl-undefined-action 'insert
  "* Action when expansion is unsuccessful.
The default action (given by \\='insert) is to insert `tplsub-key'.
The other possible values are `nil' \\=(ignore\\=) and \\='error, to either
completely ignore the situation, or report an error."
  :group 'tplsub
  :type '(choice :tag "Action"
                 (const :tag "Insert" 'insert)
                 (const :tag "Ignore" nil)
                 (const :tag "Error"  'error)))

(defcustom tplsub-tmpl-case nil
  "*Set to `t' to have case sensisitve template expansion"
  :group 'tplsub :type 'boolean)
(make-variable-buffer-local 'tplsub-tmpl-case)

(defcustom tplsub-record-file
  (cond ((fboundp 'convert-standard-filename)
         (convert-standard-filename "~/tplsub.add"))
        (t "~/tplsub.add"))
  "Name of the file that records `tplsub-add-tpl' values.
nil to avoid recording."
  :group 'tplsub :type 'file)

(defcustom tplsub-tmpl-key "|"
  "Key to trigger template expansion \\=(`tplsub-templates'\\=)
If this is changed to an unprintable key code, `tplsub-tmpl-undefined-action'
should be changed also."
  :group 'tplsub :type 'string)

(defcustom tplsub-next-key nil
  "Key to jump to next point in expanded text."
  :group 'tplsub :type 'string)


;; --- Setup --AF-- ---

;; Different GNU & XEmacs versions disagree on these:
(fset 'tplsub-capitalize (if (fboundp 'upcase-initials)
                             'upcase-initials
                           'capitalize))

(fset 'tplsub-buf-sub
      (if (fboundp 'buffer-substring-no-properties)
          'buffer-substring-no-properties
        'buffer-substring
        ))

(or (fboundp 'display-message-or-buffer)
    (defsubst display-message-or-buffer (msg &optional
                                             buffer-name not-this-window frame)
      (message "%s" msg)))

;; --- Long-lived variables ---

;; --AF--

(defvar tplsub-buffer-name-2 "*tplsub2*"
  "tplsub hidden buffer name")

(defvar tplsub-buffer-name-3 "*tplsub template*"
  "tplsub hidden buffer name")

(defconst tplsub-version "%I"
  "Version of tplsub")

(defvar tplsub-hist-list nil
  "Minibuffer history list")

;; --F--
(defcustom tplsub-file-directory "."
  "Default directory for template files"
  :group 'tplsub
  :type '(file :tag "Path"))

(defvar tplsub-hist-read nil
  "t after defaults are read")

(defvar tplsub-buffer-name "*tplsub*"
  "tplsub buffer name")

(defvar tplsub-list-keymap nil
  "keymap used in *tplsub* buffer \(to avoid direct editing\).")

;; --A--
(defvar tplsub-builtin-alist (list (cons "at" "@")
                                   (cons "para" "§"))
  "List of predefined variables and replies")

(defvar tplsub-alist tplsub-builtin-alist
  "List of currently defined variables and replies")

(defvar tplsub-default-alist nil
  "List of previously defined variables and replies")

(defvar tplsub-resort-alist nil
  "Last resort for defaults...")

(defvar tplsub-mode nil
  "Non-nil when tplsub-mode is activated.")

(defvar tplsub-mode-hook nil
  "Functions to call after activating tplsub-mode.")

(defvar tplsub-mode-off-hook nil
  "Functions to call after deactivating tplsub-mode.")

(defvar tplsub-help-global-list nil
  "Global additions to `tplsub-help-mode-list' (which see)")

;; --- Short-lived variables ---

;; --F--
(defvar tplsub-file-list nil
  "List of further files to process")

(defvar tplsub-quit nil
  "Set to `t' to abort processing")

(defvar tplsub-err-count 0
  "Error counter used during expansion")

;; --- Buffer local variables ---

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
   \"<string>\"    - inserted literally \(first character \\e means \"always
                     preserve case\"\)
   |             - record the position of point, to be set when
                   expansion has finished \(in a list\)
   n             - newline
   /             - new, indented line
   o             - if point is at EOL - do nothing, otherwise split line
   *             - reindent current line
   +             - indent standard indentation \(a TAB if `tplsub-std-indent'
                     is the literal `tab', otherwise spaces\)
   -             - outdent standard indentation
   ^             - goto beginning of line
   $             - goto end of line
   [<vector>]    - executed as a keyboard macro
   <n>           - indent <n> spaces, or delete <n> characters backwards,
                   if <n> is negative
   (<n>)         - indent n words (from the previous line)
   \(\"<string>\" [ . \"<default>\"]\)
                 - a string is prompted for and inserted \(with optional
                  default value\) - this is short for:
   \(q \"<string>\" &optional \"<default>\" ask-only volatile case-fix\)
                 - like the previous; if ask-only=non-nil, do not insert;
                   if volatile=non-nil, do not remember the answer;
                   case-fix may be any function that takes a string as an
                   argument and returns a string
   \(: [ . <n>]\)  - insert `tplsub-cont-string' and a newline, then indent
                   to underneath the <n>th \(default first - n=1\) word
                   on the previous line \(with `indent-relative'\)
   \(= . \"<tmpl>\"\)
                 - the <tmpl> is expanded recursively \(note:
                   `tplsub--current-tmpl' is not changed\)
   \(var . <name>\) - insert the value of the lisp variable
   \(yn \"<prompt>\" \(<YES-template>\) [ \(<NO-template>\) ]\)
                 - prompt for yes/no, exec. YES-list if true, otherwise NO-list
                   \(if set\)
   \(msg \"str\"\) - like `message' (but preformatted)
   \(opt \"title\" \(\(\"letter1\" [. \"string1\"]\) ...\)
                 - select an option, insert corresponding string
   \(for <expr> \(<template>\)\)
                 - repeat the template for each item in <expr>, without
                   remembering replies.  <expr> may be a string \(with ? as the
                   first char to prompt for values, otherwise split into
                   values\), an integer \(to repeat n times\),or a list of
                   strings \(to execute for each item\);
                   during the expansion, the variable F is set to the
                   current value of expr.
   \(tmpl \"<template>\"\)
                 - perform template substitution \(see `tplsub'\) on the named
                   file template, and insert the result
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

(defvar tplsub-tmpl-regexp "[^-+/*a-zA-Z0-9?]+\\|\\`"
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
  "A string containing the current top-level abbreviation when expanding
templates.  Can be used by your own template-handling functions.")

(defvar tplsub--current-tmpl-point nil
  "A list of \"important\" points registered during template expansion.")

(defvar tplsub--current-tmpl-defs nil
  "Contains the questions and answers from the current template.")

;; --- Init ---
(unless tplsub-list-keymap
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

(defsubst tplsub-int-indent()
  "Amount of indentation"
  (if (eq tplsub-std-indent 'tab)
      tplsub-tab-indent
    tplsub-std-indent))

(defsubst tplsub-buffer-name ()
  (file-name-nondirectory
   (file-name-sans-extension
    (buffer-file-name))))

(when (>= emacs-major-version 27)
  (defalias 'incf 'cl-incf)
  (defalias 'decf 'cl-decf)
  (defalias 'copy-list 'cl-copylist)
  (defalias 'pushnew 'cl-pushnew))


(defun tplsub-replace-in-string (s from &optional to)
  "Replace all occurences in the string S of the regexp FROM to the string TO."
  (if (fboundp 'replace-regexp-in-string)
      (let ((to-s (or to "")))
        (replace-regexp-in-string from to-s s))
    (if (and s
             from
             (> (length s) 0)
             (> (length from) 0))
        (save-match-data
          (let (p)
            (while (string-match from s (and p
                                             (+ (match-beginning 0)
                                                (length to))))
              (setq p t
                    s (concat (substring s 0 (match-beginning 0))
                              to
                              (substring s (match-end 0))))))))
    s))

(defun tplsub-read-char-list (prompt v-list)
  "Read char from minibuffer until member of given list.
The list should contain cells \(\"chr\" . \"description\"\)."
  (let (resp
        (pr prompt)
        (opts (concat "?" (mapconcat 'car v-list "")))
        ok)
    (while (not ok)
      (setq resp (char-to-string
                  (read-char-exclusive (concat pr " [" opts "]: ")))
            ok (assoc resp v-list))
      (cond ((string= resp "?")
             (display-message-or-buffer
              (mapconcat (function (lambda (e)
                                     (concat (car e) "=" (cdr e))))
                         v-list
                         "    ") "*Select Options*")
             (sit-for 3)
             (setq ok nil))
            ((and (consp ok) (stringp (cdr ok))
                  (string= (cdr ok) "\"\""))
             (setq ok '(x . "")))
            ((not ok)
             (setq pr (concat "Invalid char! " prompt))
             (beep))))
    (or (cdr ok) (car ok))))

(defun tplsub-undefined ()
  "dummy function substituted for self-insert keys in *tplsub* buffer"
  (interactive)
  (momentary-string-display "*** THIS BUFFER IS NOT FOR EDITING! ***"
                            (point) ?\  "Type <space> to continue"))

(defsubst tplsub-definition (s)
  "Return cons corresponding to s or nil"
  (if (and (stringp s) (string= s "_")) (cons "_" " ")
    (assoc s tplsub-alist)))

(defsubst tplsub-get-default (var)
  "Get default value for VAR, if any"
  (or (cdr (assoc var tplsub-builtin-defaults-alist))
      (cdr (assoc var tplsub-default-alist))
      (cdr (assoc var tplsub-resort-alist))))

(defsubst tplsub-set-default (var val)
  "Remember default value for VAR"
  (setq tplsub-default-alist
        (append (list (cons var val)) tplsub-default-alist)))

(defsubst tplsub-set-resort (var val)
  "Remember last resort default value for VAR"
  (setq tplsub-resort-alist
        (append (list (cons var val)) tplsub-resort-alist)))

(defun tplsub-message (mess &optional lf)
  "Write message"
  (let ((b (current-buffer)))
    (pop-to-buffer (get-buffer-create tplsub-buffer-name))
    (insert mess)
    (if lf (insert "\n"))
    (set-buffer-modified-p nil)
    (pop-to-buffer b)))

(defun tplsub-warning (mess)
  "Show message temporarily"
  (message "%s" mess)
  (sit-for 3)
  (tplsub-message mess t))

(defun tplsub-error (mess)
  "Write message"
  (incf tplsub-err-count)
  (tplsub-warning (concat "ERROR: " mess)))

(defun tplsub-add (var def &optional case)
  "Add new variable/definition-pair, return definition"
  (let ((vdef (cond ((null case) def)
                    ((eq case 'upper)  (upcase def))
                    ((eq case 'caps)   (tplsub-capitalize def))
                    ((eq case 'lower)  (downcase def))
                    ((eq case 'number) (number-to-string
                                        (string-to-number def)))
                    (t def))))
    (setq tplsub-alist (append (list (cons var vdef)) tplsub-alist))
    (tplsub-message (if (stringp vdef)
                        vdef
                      (prin1-to-string vdef)) t)
    vdef))

(defun tplsub-y-or-n (var prompt)
  "Obtain Boolean reply for variable VAR giving prompt PROMPT"
  (let ((is-def (tplsub-definition var)))
    (if is-def
        (not (not (cdr is-def)))
      (if (or (null prompt)
              (string= prompt ""))
          (setq prompt var))
      (recenter)
      (tplsub-message (format "%s? " prompt))
      (tplsub-add var (y-or-n-p (concat prompt " "))))))

(defun tplsub-define (var prompt &optional default case optlist)
  "Obtain textual reply"
  (let ((is-def (tplsub-definition var)))
    (if is-def
        (cdr is-def)
      (if (null default)
          (setq default (tplsub-get-default var)))
      (if (or (null prompt)
              (string= prompt ""))
          (setq prompt (format "Define %s" var)))
      (recenter)
      (tplsub-message (format "Define %s..." var))
      (tplsub-add var
                  (if optlist
                      (tplsub-read-char-list prompt optlist)
                    (read-string (concat prompt ": ")
                                 (cons (or default "") 0)
                                 'tplsub-hist-list))
                  case))))


(defconst tplsub--date-y (format "%04d"
                               (string-to-number
                                (substring (current-time-string) 20 24))))
(defconst tplsub--date-m
  (cdr (assoc (substring (current-time-string) 4 7)
              '(("Jan" . "01") ("Feb" . "02") ("Mar" . "03")
                ("Apr" . "04") ("May" . "05") ("Jun" . "06")
                ("Jul" . "07") ("Aug" . "08") ("Sep" . "09")
                ("Oct" . "10") ("Nov" . "11") ("Dec" . "12")))))
(defconst tplsub--date-d (format "%02d"
                               (string-to-number
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

(defconst tplsub--author "Lars@kalars.net")

(defvar tplsub-user (user-login-name) "User signature")

(defvar tplsub--mailto (concat "mailto:"
                               (or user-mail-address
                                   (and (fboundp 'user-mail-address)
                                        (user-mail-address))
                                   (concat (user-login-name)
                                           "@"
                                           (system-name)))))

(defvar tplsub-builtin-defaults-alist
  (list (cons "author"  tplsub--author)
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
        (list "para" "§")
        (list "cb"     "{" '| "}")
        (list "sb"     "[" '| "]")
        (list "user"   tplsub-user)
        (list "mailto" tplsub--mailto)
        (list "date"   (tplsub--conc-date))
        (list "fdate"  (tplsub--conc-date t))
        (list "sdate"  (tplsub--conc-date nil t))
        (list "rdate"  (tplsub--conc-date nil nil t))
        (list "rfdate" (tplsub--conc-date t   nil t))
        (list "rsdate" (tplsub--conc-date nil t   t)))
  "Global additions to `tplsub-tmpl-mode-list' (which see)")


(defun tplsub-setup ()
  "Create message buffer, create window configuration"
  (delete-other-windows)
  (let ((b (current-buffer)))
    (pop-to-buffer (get-buffer-create tplsub-buffer-name))
    (toggle-read-only 0)
    (erase-buffer)
    (use-local-map tplsub-list-keymap)
    (insert "\n\n\n"
            (make-string 30 ?=)
            " tplsub "
            (make-string 30 ?=)
            "\n\n")
    (sit-for 1)
    (shrink-window-if-larger-than-buffer)
    (set-buffer-modified-p nil)
    (pop-to-buffer b)))

(defun tplsub-file-name (tmpl-name &optional must-exist)
  "Search for TMPL-NAME in `tplsub-file-directory'."
  (let ((result nil)
        flist
        fbase)
    (if (or (null tmpl-name)
            (string= "" tmpl-name))
        (if must-exist (tplsub-error "empty file name!"))
      (setq result (substitute-in-file-name (expand-file-name tmpl-name)))
      (when (and must-exist
                 (not (file-exists-p result)))
        (setq fbase (file-name-nondirectory result)
              flist (if (stringp tplsub-file-directory)
                        (list (concat tplsub-file-directory fbase))
                      (mapcar (function (lambda (dir)
                                          (concat dir "/" fbase)))
                              tplsub-file-directory)))
        (while (and flist
                    (not (file-exists-p result)))
            (setq result (car flist)
                  flist (cdr flist)))
        (unless (file-exists-p result)
          (tplsub-error (concat tmpl-name " not found!"))
          (setq result nil))))
    result))


(defun tplsub-vars (&optional main)
  "Substitute variables"
  (save-mark-and-excursion
    (goto-char (point-min))
    (let* (var
           (after-change-functions nil)
           (f-o-on (and main
                        (fboundp 'font-lock-mode)
                        font-lock-mode))
           repl
           (r--q (regexp-quote tplsub-var-c))
           (regexp (concat r--q "\\([^" r--q "]*\\)" r--q)))
      (if f-o-on (font-lock-mode -1))
      (while (re-search-forward regexp nil t)
        (setq var (tplsub-buf-sub (match-beginning 1)
                                  (match-end 1))
              repl (if (string= var "")
                       (cons var tplsub-var-c)
                     (tplsub-definition var)))
        (if (null repl)
            (tplsub-error (format "%s is undefined!" var))
          (delete-region (match-beginning 0) (match-end 0))
          (insert (cdr repl))))
      (if f-o-on (font-lock-mode 1)))))


(defun tplsub-string-vars(str)
  "Perform variable replacement on string, rather than buffer"
  (let ((cur-buf (current-buffer))
        res)
    (set-buffer (get-buffer-create tplsub-buffer-name-2))
    (erase-buffer)
    (insert str)
    (tplsub-vars)
    (set-buffer-modified-p nil)
    (setq res (tplsub-buf-sub (point-min) (point-max)))
    (set-buffer cur-buf)
    res))


(defun tplsub-rest-of-line (n)
  "Return the rest of the line - excluding leading blanks - as a string."
  (save-mark-and-excursion
    (forward-char n)
    (skip-chars-forward " \t")
    (if (or (looking-at "\\\\ ")
            (looking-at "\\\\\\\\"))    ; Skip backslash preceding
                                        ; backslash or blank
        (forward-char 1))
    (tplsub-buf-sub (point)
                    (progn
                      (end-of-line)
                      (point)))))


(defun tplsub-var-and-prompt (n &optional only-one)
  "Parse current line, return (var . text)"
  (save-mark-and-excursion
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
                  (tplsub-string-vars (tplsub-rest-of-line 0))))
      (if (and (stringp txt)
               (> (length txt) 7)
               (string= (substring txt 0 5)
                        "(eval"))
          (let* ((val (substring txt 6 (- (length txt) 1)))
                 (oval val))
            (setq txt ;; Trengs prin1-to-string her??
                   (prin1-to-string
                   (cond ((eq tplsub-enable-eval t)
                          (eval (read val)))
                         ((null tplsub-enable-eval)
                          txt)
                         ((y-or-n-p (concat
                                     "Evaluate expression \`"
                                     val "\'? "))
                          (eval (read val)))
                         (t txt))))
            (tplsub-message (concat "eval: " oval " = " txt) t)))
      (cons var txt))))

(defun tplsub-directives ()
  "Read conditionals and input definitions"
  (goto-char (point-min))
  (let (line-start
        v-and-p
        buf-sub
        s-point
        n
        i
        repl-string
        repl-short-string
        string-list
        del-count
        (overlay-arrow-position (make-marker))
        (overlay-arrow-string (make-string 75 169))
        (regexp (concat "^" (regexp-quote tplsub-dir-c)))
        (kill-from -1)
        (kill-level -1)
        (nest-level 0))
    (while (and (not tplsub-quit)
                (re-search-forward regexp nil t))
      (setq repl-string nil
            del-count nil
            repl-short-string nil)
      (save-mark-and-excursion
        (beginning-of-line)
        (setq line-start (point))
        (if (looking-at "§\\(do\\|ask\\|if\\|select\\)")
            (set-marker overlay-arrow-position (point) (current-buffer)))
        ;; (redraw-display)
        )
      (cond

       ;; ---------------------------------------------------------
       ((looking-at "if") ;; or "if(n)def"
        (let ((is-ifdef nil)
              (nochars 2)
              (rev nil)
              keep-it
              actdef)
          (cond ((looking-at "ifndef")
                 (setq rev      t
                       nochars  6
                       is-ifdef t))
                ((looking-at "ifdef")
                 (setq nochars 5
                       is-ifdef t)))
          (incf nest-level)
          (when (= -1 kill-from)
            (setq v-and-p (tplsub-var-and-prompt nochars is-ifdef)
                  actdef (cdr-safe (tplsub-definition (car v-and-p)))
                  keep-it (if is-ifdef
                              (and actdef
                                   (or (eq actdef t)
                                       (not (or (string= actdef "")
                                                (string= actdef "0")))))
                            (tplsub-y-or-n (car v-and-p) (cdr v-and-p))))
            (if rev (setq keep-it (not keep-it)))
            (if (not keep-it)
                (setq kill-from line-start
                      kill-level nest-level)))))

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
               (decf nest-level))
              (t
               (tplsub-error (format "unmatched endif, position %d"
                                     line-start)))))

       ;; ---------------------------------------------------------
       ((looking-at "do[A-Z]?")
        (unless (> kill-from -1)
          (let ((no 2)
                (R "R")
                (ends "done"))
            (if (looking-at "do[A-Z]")
                (setq no 3
                      R  (tplsub-buf-sub (+ (point) 2) (+ (point) 3))
                      ends (concat "done" R)))
            ;; Count
            (setq v-and-p (tplsub-var-and-prompt no)
                  n (string-to-number (tplsub-define (car v-and-p)
                                                     (cdr v-and-p)
                                                     nil
                                                     'number)))
            ;; Cut the loop
            (beginning-of-line 2)
            (setq s-point (point))
            (re-search-forward (concat regexp ends))
            (beginning-of-line)
            (setq buf-sub (buffer-substring s-point (point)))
            (beginning-of-line 2)
            (delete-region s-point (point))
            ;; Insert
            (setq i n)
            (while (> i 0)
              (save-restriction
                (goto-char s-point)
                (insert buf-sub)
                (narrow-to-region s-point (point))
                (goto-char (point-min))
                (replace-regexp (concat (regexp-quote tplsub-var-c)
                                        R
                                        (regexp-quote tplsub-var-c))
                                (format "%d" i)))
              (decf i))
            ;; Return
            (goto-char (1- s-point)))))

       ;; ---------------------------------------------------------
       ((looking-at "done") t)

       ;; ---------------------------------------------------------
       ((looking-at "for")
        (unless (> kill-from -1)
          ;; Count
          (setq v-and-p (tplsub-var-and-prompt 3)
                string-list (split-string (cdr v-and-p)))
          ;; Cut the loop
          (beginning-of-line 2)
          (setq s-point (point))
          (re-search-forward (concat regexp "endfor *" (car v-and-p)))
          (beginning-of-line)
          (setq buf-sub (buffer-substring s-point (point)))
          (beginning-of-line 2)
          (delete-region s-point (point))
          ;; Insert
          (setq i (length string-list))
          (while (> i 0)
            (save-restriction
              (goto-char s-point)
              (insert buf-sub)
              (narrow-to-region s-point (point))
              (goto-char (point-min))
              (replace-regexp (concat (regexp-quote tplsub-var-c)
                                      (car v-and-p)
                                      (regexp-quote tplsub-var-c))
                              (nth (1- i) string-list)))
            (decf i))
          ;; Return
          (goto-char (1- s-point))))

       ;; ---------------------------------------------------------
       ((looking-at "endfor") t)

       ;; ---------------------------------------------------------
       ((let ((case-fold-search nil))  ;; Backwards compatibility!!!
          (looking-at "ASK"))
        (unless (> kill-from -1)
          (setq v-and-p (tplsub-var-and-prompt 3))
          (tplsub-define (car v-and-p) (cdr v-and-p) nil 'upper)))
; !!! TODO !!! Remove!

       ;; ---------------------------------------------------------
       ((looking-at "select")
        (unless (> kill-from -1)
          (setq v-and-p (tplsub-var-and-prompt 6))
          (let ((args (split-string (cdr v-and-p)))
                (optlist (list t)))
            (while args
              (setq optlist (append optlist
                                    (list (cons (car args) (cadr args))))
                    args (cddr args)))
            (tplsub-define (car v-and-p) (concat "Select " (car v-and-p))
                           nil nil (cdr optlist)))))

       ;; ---------------------------------------------------------
       ((looking-at "ask[ulci]?")
        (let (special (no 4))
          (cond ((looking-at "asku") (setq special 'upper))
                ((looking-at "askl") (setq special 'lower))
                ((looking-at "askc") (setq special 'caps))
                ((looking-at "aski") (setq special 'number))
                (t                   (setq no      3)))
          (unless (> kill-from -1)
            (setq v-and-p (tplsub-var-and-prompt no))
            (tplsub-define (car v-and-p) (cdr v-and-p) nil special))))

       ;; ---------------------------------------------------------
       ((looking-at "default")
        (unless (> kill-from -1)
          (setq v-and-p (tplsub-var-and-prompt 7))
          (tplsub-set-default (car v-and-p)
                              (cdr v-and-p))))

       ;; ---------------------------------------------------------
       ((looking-at "def1")
        (unless (> kill-from -1)
          (setq v-and-p (tplsub-var-and-prompt 4))
          (tplsub-set-resort (car v-and-p)
                             (cdr v-and-p))))

       ;; ---------------------------------------------------------
       ((looking-at "§")
        (setq repl-string (concat "\n"
                                  (tplsub-string-vars
                                   (tplsub-rest-of-line 1)))))

       ;; ---------------------------------------------------------
       ((looking-at "rem")
        t)

       ;; ---------------------------------------------------------
       ((looking-at "next")
        (unless (> kill-from -1)
          (setq buf-sub (tplsub-file-name (tplsub-rest-of-line 4) t))
          (if buf-sub
              (setq tplsub-file-list (cons buf-sub tplsub-file-list)))))

       ;; ---------------------------------------------------------
       ((looking-at "include")
        (unless (> kill-from -1)
          (setq buf-sub (tplsub-file-name
                         (tplsub-string-vars (tplsub-rest-of-line 7)) t))
          (if buf-sub
              (save-mark-and-excursion
                (beginning-of-line 2)
                (condition-case err-symb
                    (insert-file-contents buf-sub)
                  ((file-error error)
                   (tplsub-error (format "Cannot open include file %s"
                                         buf-sub))))))))

       ;; ---------------------------------------------------------
       ((looking-at "name")
        (unless (> kill-from -1)
          (setq buf-sub (tplsub-file-name (tplsub-rest-of-line 4)))
          (if (and buf-sub
                   (not (string= "" buf-sub)))
              (set-visited-file-name buf-sub))))

       ;; ---------------------------------------------------------
       ((looking-at "quit")
        (unless (> kill-from -1)
          (setq buf-sub (tplsub-rest-of-line 4))
          (if (not (or (null buf-sub)
                       (string= buf-sub "")))
              (tplsub-error buf-sub))
          (setq tplsub-file-list nil
                tplsub-quit      t)))

       ;; ---------------------------------------------------------
       ((looking-at "exit")
        (unless (> kill-from -1)
          (setq tplsub-quit t)))

       ;; ---------------------------------------------------------
       ((looking-at "message")
        (unless (> kill-from -1)
          (tplsub-warning (tplsub-rest-of-line 7))))

       ;; ---------------------------------------------------------
       ((looking-at "define[ulc]?")
        (let (special (no 7))
          (cond ((looking-at "defineu") (setq special 'upper))
                ((looking-at "definel") (setq special 'lower))
                ((looking-at "definec") (setq special 'caps))
                (t                      (setq no      6)))
          (unless (> kill-from -1)
            (setq v-and-p (tplsub-var-and-prompt no))
            (tplsub-message (format "%s=" (car v-and-p)))
            (tplsub-add (car v-and-p) (cdr v-and-p) special))))

       ;; ---------------------------------------------------------
       ((looking-at "append")
        (unless (> kill-from -1)
          (setq v-and-p (tplsub-var-and-prompt 6))
          (tplsub-message (format "%s=" (car v-and-p)))
          (let ((old-val (cdr (tplsub-definition (car v-and-p)))))
            (if old-val
                (tplsub-add (car v-and-p) (concat old-val (cdr v-and-p)))
              (tplsub-add (car v-and-p) (cdr v-and-p))))))

       ;; ---------------------------------------------------------
       ((looking-at "defbool")
        (unless (> kill-from -1)
          (setq v-and-p (tplsub-var-and-prompt 7))
          (tplsub-message (format "%s=" (car v-and-p)))
          (if (string= (cdr v-and-p) "nil")
              (tplsub-add (car v-and-p) nil)
            (tplsub-add (car v-and-p) t))))

       ;; ---------------------------------------------------------
       ((looking-at "backspace")
        (unless (> kill-from -1)
          (setq v-and-p (tplsub-var-and-prompt 9))
          (setq del-count (string-to-number (car v-and-p)))
          (setq repl-short-string (cdr v-and-p))))

; !!! TODO !!! §run-kommando
       ;; ---------------------------------------------------------
       (t
        (tplsub-error (format "unknown directive, position %d -- %s"
                              line-start (tplsub-rest-of-line 0)))))

      ;; ---------------------------------------------------------
      (beginning-of-line 2)
      (delete-region line-start (point))
      (when (not (bobp))
        (forward-char -1)
        (if del-count
            (delete-char (- del-count))))
      (if repl-short-string (insert repl-short-string))
      (if repl-string (insert repl-string "\n"))
      )
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
          (save-mark-and-excursion
            (set-buffer (get-buffer-create " *tplsubsave*"))
            (delete-region (point-min) (point-max))
            (insert-file-contents file)
            (goto-char (point-min))
            (setq tplsub-default-alist
                  (car (read-from-string
                        (buffer-substring (point-min) (point-max)))))
            (kill-buffer (current-buffer))
            (when (and (numberp tplsub-default-high-limit)
                       (numberp tplsub-default-low-limit)
                       (> (length tplsub-default-alist)
                          tplsub-default-high-limit)
                       (> tplsub-default-high-limit
                          tplsub-default-low-limit))
              (message "Truncating default list...")
              (setq tplsub-default-alist
                    (nreverse
                     (member
                      (nth tplsub-default-low-limit tplsub-default-alist)
                      (nreverse tplsub-default-alist))))))))))

(defun tplsub-batch-ask ()
  "Asks the user how to continue"
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
    (mapc (function (lambda (e)
                      (if (and (not (assoc (car e) reslist))
                               (stringp (cdr e)))
                          (setq reslist (append (list e) reslist)))))
          alist)
    (reverse reslist)))


(defun tplsub-main ()
  (tplsub-directives)
  (tplsub-vars t))


;; --- Callable defuns ---

(defun tplsub-reset (&optional pfx)
  "Transfer replies to history list. With prefix, erase history."
  (interactive "P")
  (setq tplsub-default-alist
        (if pfx tplsub-builtin-alist
          (append tplsub-alist tplsub-default-alist))
        tplsub-alist tplsub-builtin-alist))

(defun tplsub-save-defaults (&optional file)
  "Save default replies for next emacs session."
  (interactive "P")
  (if (null file) (setq file (tplsub-file-name tplsub-save-file)))
  (save-mark-and-excursion
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
`tplsub-dir-c' \(default==§, assumed hereafter\), preferrably followed by
one of the known directives...
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
     askl/asku/askc/aski are the same, except that the result is converted to
     lower/upper/caps/number
  §default <var> <default-value> - set default value for <var>
  §def1 <var> <default-value> - set default value for <var> if no other default
                                is available
  §define <var> <value> - set <var> to <value>
     definel/defineu/definec are the same, except that the result is converted
     to lower/upper/caps
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
  §select <var> l1 var1 ... lN varN [lM] - choose <var> between a series of
          varX, each with option letter lX, the last may be empty
  §run <command args ...> - not implemented!

  §if <var> [<prompt>]
  <body1>
  [§else
  <body2>]
  §fi - ask for <var>, if t, insert <body1>, otherwise <body2> if set

  §ifdef <var>
  ... Like §if, but checks whether <var> is currently defined as either `t' or
  a non-empty string <> \"0\"
  §ifndef <var> is the inverse.

  §do[X] <var> [prompt]
  <body>
  §done[X] - ask for integer <var>, repeat <body> <var> times, replacing
          §R§ with current number. A capital letter may be attached to do and
          done, and is then used instead of R within the loop, to allow nesting.

  §for X value1 ... valueN
  <body>
  §endforX - repeat <body> N times, replacing §X§ with successive values
             - endfor must end with the variable name to allow nesting
  §backspace N [string] - remove N characters backwards from the end of the
             last line, optionally insert new text
     new text - used to clean up after loops

The second argument to §default, §ask (with variations), §define, §append and
§if may be of the form \"\(eval sexp\)\".
After expanding and removing all directives, the file is scanned once more,
this time to replace variables, i.e. strings surrounded by `tplsub-var-c'
\(which also defaults to §\) - two such characters in a row are replaced with
a single occurrence.
The string between the macro characters are replaced by their definition,
as given by §define or §ask-directives.

If §next-directives are used, the first file in `tplsub-file-list' is loaded,
and directive and variable substitutions are performed on this file, remembering
previous values.  This loop continues until no more files remain.

Later invocations of `tplsub' in the same emacs session will remember earlier
replies, and use them as defaults \(does not apply to §if, which has no default\).
To erase the default list, invoke `tplsub-reset' with a numeric prefix.
The default list is also kept for later emacs sessions.

If a line is to start with a §, start it with §§.  It may not contain variables.
The special variable §_§ represents a space character.
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


(defsubst tplsub--buffer-subst (tmpl-file buf-name)
  "Insert template TMPL-FILE in buffer BUF-NAME and perform substitution.
Both the template and the file must already exist."
  (switch-to-buffer (get-buffer buf-name))
  (insert-file-contents tmpl-file)
  (tplsub))


(defun tplsub-buffer-subst (tmpl-file buf-name)
  "Insert template TMPL-FILE in buffer BUF-NAME and perform substitution.
The file is searched for in `tplsub-file-directory' if necessary.
The buffer is created and/or emptied as appropriate."
  (switch-to-buffer (get-buffer-create buf-name))
  (erase-buffer)
  (tplsub--buffer-subst (tplsub-file-name tmpl-file t) buf-name))

(defun tplsub-tmpl-from-file (tmpl)
  "Perform substitution on file template TMPL, return result as string."
  (save-window-excursion
    (tplsub-buffer-subst tmpl tplsub-buffer-name-3)
    (tplsub-buf-sub (point-min) (point-max))))


;;; ======================================================================
;;  Abbreviation routines
;;; ======================================================================

(defun tplsub-indent-line ()
  "Reindent current line"
  (if (null indent-line-function)
      (indent-relative-maybe)
    (funcall indent-line-function)))

(defun tplsub-open-line()
  "Break current line if not at end."
  (or (looking-at "\\s-*$")
      (split-line)))

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
  (if args (message (car args) (cdr args)))
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
        ((and (stringp tmpl)            ; String with case
              (string= (substring tmpl 0 1) "\e"))
         (tplsub--describe-ins "\"" (substring tmpl 1) "\""))
        ((stringp tmpl)                 ; String
         (tplsub--describe-ins "\"" tmpl "\""))
        ((eq '| tmpl)                   ; Point
         (tplsub--describe-ins "|"))
        ((eq 'o tmpl)                   ; Open line
         (tplsub--describe-ins "//"))
        ((eq '/ tmpl)                   ; Newline
         (tplsub--describe-ins "\\n\\t"))
        ((eq '* tmpl)                   ; Reindent
         (tplsub--describe-ins "[\\t]"))
        ((eq '+ tmpl)                   ; Indent
         (tplsub--describe-ins (format "%d" (tplsub-int-indent))))
        ((eq '- tmpl)                   ; Unindent
         (tplsub--describe-ins (format "-%d" (tplsub-int-indent))))
        ((vectorp tmpl)                 ; Macro
         (tplsub--describe-ins (prin1-to-string tmpl)))
        ((integerp tmpl)                ; Indent
         (tplsub--describe-ins (format "%d" tmpl)))
        ((and (listp tmpl)
              (stringp (car tmpl)))     ; Prompt & insert
         (tplsub--describe-ins "{" (car tmpl))
         (if (cdr tmpl)
             (tplsub--describe-ins "?" (cdr tmpl)))
         (tplsub--describe-ins "}"))
        ((and (listp tmpl)
              (eq 'q (car tmpl)))       ; Long version
         (tplsub--describe-ins "{" (nth 1 tmpl))
         (when (nth 2 tmpl)
           (tplsub--describe-ins "?" (nth 2 tmpl))
           (if (cddr tmpl)
               (tplsub--describe-ins "..." (prin1-to-string (cddr tmpl)))))
         (tplsub--describe-ins "}"))
        ((and (listp tmpl)
              (integerp (car tmpl)))
         (tplsub--describe-ins (format "%d\\t" (car tmpl))))
        ((and (listp tmpl)
              (eq ': (car tmpl)))       ; Split line
         (tplsub--describe-ins " :\\n"
                              (format "%d\\t" (if (cdr tmpl) (cdr tmpl) 1))))
        ((and (listp tmpl)
              (eq 'opt (car tmpl)))     ; Option list
         (tplsub--describe-ins (cadr tmpl) "["
                               (mapconcat
                                (function
                                 (lambda (cell)
                                   (concat (car cell)
                                           "="
                                           (or (cdr cell) (car cell)))))
                                (caddr tmpl) " ")
                               "]"))
        ((and (listp tmpl)
              (eq '= (car tmpl)))       ; Recursive...
         (tplsub--describe-ins "(= " (cdr tmpl) ")"))
        ((and (listp tmpl)
              (eq 'yn (car tmpl)))      ; Yes/no
         (tplsub--describe-ins "(" (nth 1 tmpl) ")? ")
         (mapc 'tplsub--describe-1 (nth 2 tmpl))
         (tplsub--describe-ins " : ")
         (mapc 'tplsub--describe-1 (nth 3 tmpl)))
        ((and (listp tmpl)
              (eq 'for (car tmpl)))     ; For-list
         (tplsub--describe-ins "for[" (prin1-to-string (nth 1 tmpl)) "]:(")
         (mapc 'tplsub--describe-1 (nth 2 tmpl))
         (tplsub--describe-ins ")"))
        ((and (listp tmpl)
              (eq 'var (car tmpl)))     ; Variable
         (tplsub--describe-ins "'" (symbol-name (cdr tmpl)) " "))
        ((and (listp tmpl)
              (eq 'help (car tmpl)))    ; Help
         t)                             ; ignore
        ((and (listp tmpl)
              (eq 'tmpl (car tmpl)))    ; Variable
         (tplsub--describe-ins "<file " (cdr tmpl) ">"))
        ((and (symbolp tmpl)
              (commandp tmpl))          ; Interactive command
         (tplsub--describe-ins "\'" (symbol-name tmpl)))
        (t (tplsub--describe-ins (prin1-to-string tmpl) )))
  tmpl)                                 ; The argument is returned as is



(defun tplsub-describe-templates (&optional search)
  "Overview of tplsub mode template table.
If optional SEARCH is non-nil (i.e. prefix is given), asks for a string to
search for."
  (interactive "P")
  (let ((match-it (if search (read-string "Search for regexp ") ".+"))
        (curr-buf (current-buffer))
        (bind-tpl (concat (substitute-command-keys "'\\[tplsub-templates]'")
                          " and/or "
                          (if (stringp tplsub-tmpl-key) tplsub-tmpl-key
                            (prin1-to-string tplsub-tmpl-key))))
        (bind-hlp (substitute-command-keys "'\\[tplsub-describe-templates]'"))
        (bind-syn (substitute-command-keys "'\\[tplsub-syntax-help]'"))
        (bind-nxt (substitute-command-keys "'\\[tplsub-next-position]'"))
        (mode-ver tplsub-version)
        (newcol   t)
        (mode-desc (symbol-name major-mode))
        (case-desc (if tplsub-tmpl-case "ON" "OFF"))
        (help-desc (if tplsub-tmpl-help "ON" "OFF"))
        (act-desc  (cond ((eq tplsub-tmpl-undefined-action 'insert)
                          "insert key")
                         ((null tplsub-tmpl-undefined-action)
                          "ignore")
                         (t "error")))
        (tmpl-list (sort (append tplsub-tmpl-buffer-list
                                 tplsub-tmpl-mode-list
                                 (copy-list tplsub-tmpl-global-list))
                         (function (lambda (e1 e2)
                                     (string< (car e1) (car e2))))))
        (hlp-list (sort (append tplsub-help-buffer-list
                                tplsub-help-mode-list
                                (copy-list tplsub-help-global-list))
                         (function (lambda (e1 e2)
                                     (string< (car e1) (car e2))))))
        )
    (if (null tmpl-list)
        (message "No abbreviations!")
      (pop-to-buffer (get-buffer-create "*tplsub abbrevs*"))
      (setq truncate-lines t)           ; Truncate long abbrevs
      (if nil
          (goto-char (point-max))       ; Append
        (erase-buffer))                 ; - or empty
      (toggle-read-only 0)              ; Not read-only
      (unless search
        (insert "tplsub-mode version                    " mode-ver   "\n"
                "Settings for:                          " mode-desc  "\n"
                "Key binding for template expansion:    " bind-tpl   "\n"
                "Key binding for syntax help:           " bind-hlp   "\n"
                "Key binding for mode help:             " bind-syn   "\n"
                "Key binding for next position:         " bind-nxt   "\n"
                "Case sensitivity:                      " case-desc  "\n"
                "Automatic syntax help when expanding:  " help-desc  "\n"
                "Action on undefined template:          " act-desc   "\n"
                "\nThe following abbreviations are defined:\n\n"
                "Abbrev\tContents\n"
                "=======\t"
                "==========================================================="
                "\n"))
      (mapc
       (function (lambda (elem) ; Insert description of a single element
                   (unless (and search
                            (not (string-match match-it
                                               (prin1-to-string elem))))
                     (insert (car elem) "\t")
                     (mapc 'tplsub--describe-1 (cdr elem))
                     (insert "\n"))))
       tmpl-list)
      (unless search
        (insert "\n=============="
                "===========================================================\n"
                "Help topics:"
                "\n=============="
                "===========================================================\n"
                ))
      (mapc (function (lambda (elem) ; Insert help topic name
                        (unless (and search
                                     (not (string-match match-it
                                                        (prin1-to-string elem))))
                          (insert (if search "help: " "")
                                  (car elem) (if newcol "\t\t" "\n"))
                          (setq newcol (not newcol)))))
            hlp-list)
      (unless search
        (insert "\n\nSee the documentation of `tplsub-tmpl-mode-list'"
                " for information on how to define abbrevs."))
      (goto-char 1)
      (forward-paragraph 2)
      (set-buffer-modified-p nil)
      (pop-to-buffer curr-buf))))


(defun tplsub--tmpl-ask (prmp dfl &optional nostore fixcase)
  (let* ((old (cdr (assoc prmp tplsub--current-tmpl-defs)))
         (res (or old (read-string (concat prmp ": ")
                                   dfl
                                   'tplsub-hist-list))))
    (or nostore
        (setq tplsub--current-tmpl-defs
              (append (list (cons prmp res)) tplsub--current-tmpl-defs)))
    (cond ((null fixcase)           res)
          ((functionp fixcase)      (funcall fixcase res))
          (t                        res))))


(defun tplsub--tmpl-exec (tmpl)
  "(Recursively) expand a template definition.
See the description of `tplsub-tmpl-mode-list' for a description of how
arguments are handled."
  (let ((p1 (if (listp tmpl) (car tmpl)))
        (p2 (if (listp tmpl) (cdr tmpl))))
    (cond ((null tmpl) t)               ; Ignore null args
          ((and (stringp tmpl) (string= tmpl "")) t)
          ((stringp tmpl)           ; Insert string, according to case
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
          ((eq '| tmpl)                 ; Record point
           (pushnew (point-marker) tplsub--current-tmpl-point))
          ((eq 'o tmpl)                 ; Open if necessary
           (tplsub-open-line))
          ((eq '/ tmpl)                 ; Newline & indent
           (tplsub-indent-new-line))
          ((eq '* tmpl)                 ; Reindent
           (tplsub-indent-line))
          ((eq '+ tmpl)                 ; Indent
           (if (eq tplsub-std-indent 'tab) (insert-char ?\t 1)
             (insert-char ?\  tplsub-std-indent)))
          ((eq '- tmpl)                 ; Unindent
           (backward-delete-char-untabify (tplsub-int-indent)))
          ((eq '^ tmpl)                 ; BOL
           (beginning-of-line))
          ((eq '$ tmpl)                 ; EOL
           (end-of-line))
          ((eq 'n tmpl)                 ; newline
           (insert "\n"))
          ((vectorp tmpl)                       ; Execute kbd-macro
           (command-execute tmpl))
          ((and (listp tmpl)
                (eq 'var p1))   ; Insert variable
           (insert (format "%s" (eval p2))))
          ((and (listp tmpl)
                (eq 'yn p1))    ; Yes or no
           (let* ((old (cdr (assoc (nth 1 tmpl) tplsub--current-tmpl-defs)))
                  (res (or old (if (y-or-n-p (nth 1 tmpl)) ?y ?n)))
                  (yez (= res ?y)))
             (mapc 'tplsub--tmpl-exec (nth (if yez 2 3) tmpl))
             (setq tplsub--current-tmpl-defs
                   (append (list (cons (nth 1 tmpl) res))
                           tplsub--current-tmpl-defs))))
          ((integerp tmpl)
           (if (> tmpl 0)
               (insert-char ?\  tmpl)   ; Indent
             (backward-delete-char-untabify (- tmpl)))) ; Unindent
          ((and (listp tmpl)            ; List starting with string -
                (stringp p1))           ; prompt & insert
           (insert (tplsub--tmpl-ask p1 p2)))
          ((and (listp tmpl)
                (eq 'q p1))             ;; Prompt++
           (let ((prom (nth 1 tmpl))
                 (dflt (nth 2 tmpl))
                 (only (nth 3 tmpl))
                 (nost (nth 4 tmpl))
                 (cfix (nth 5 tmpl))
                 res)
             (setq res (tplsub--tmpl-ask prom dflt nost cfix))
             (or only (insert res))))
          ((and (listp tmpl)
                (integerp p1))  ; Indent
           (let ((iterator p1))
             (while (> iterator 0)
               (decf iterator)
               (indent-relative))))
          ((and (listp tmpl)
                (eq ': p1))     ; Break line
           (insert tplsub-cont-string "\n")
           (if (not p2) (indent-relative)
             (let ((iterator p2))
               (while (> iterator 0)
                 (decf iterator)
                 (indent-relative)))))
          ((and (listp tmpl)
                (eq 'opt (car tmpl)))   ; Option list
           (insert (tplsub-read-char-list (cadr tmpl) (caddr tmpl))))
          ((and (listp tmpl)
                (eq '= p1))     ; Recursive...
           (mapc 'tplsub--tmpl-exec
                 (cdr
                  (assoc p2
                         (append tplsub-tmpl-buffer-list
                                 tplsub-tmpl-mode-list
                                 tplsub-tmpl-global-list)))))
          ((and (listp tmpl)
                (eq 'for p1))   ; Loop
           (let ((c-def tplsub--current-tmpl-defs)
                 (i 0)
                 maps
                 j
                 prom
                 (cnt (nth 1 tmpl)))
             (cond ((integerp cnt)
                    (dotimes (j cnt)
                      (set 'maps
                           (append maps (list (int-to-string (1+ j)))))))
                   ((and (stringp cnt)
                         (string= (substring cnt 0 1) "?"))
                    (setq prom (substring cnt 1)
                          cnt (tplsub--tmpl-ask prom nil))
                    (unless (string= cnt "")
                      (setq c-def (append (list (cons prom cnt)) c-def)
                            cnt (string-to-int cnt))
                      (dotimes (j cnt)
                        (set 'maps
                             (append maps (list (int-to-string (1+ j))))))))
                   ((stringp cnt)
                    (setq maps
                          (split-string (tplsub--tmpl-ask cnt nil))))
                   ((listp cnt)
                    (setq maps cnt)))
             (while maps
               (let ((tplsub--current-tmpl-defs (append
                                                 (list (cons "F" (car maps)))
                                                 c-def)))
                 (setq maps (cdr maps))
                 (mapc 'tplsub--tmpl-exec (nth 2 tmpl))))))
          ((and (listp tmpl)
                (eq 'tmpl p1))  ; Template from file
           (insert (tplsub-tmpl-from-file (car p2))))
          ((and (listp tmpl)
                (eq 'help p1))  ; Help!
           (if (and tplsub-tmpl-help
                    (or tplsub-help-buffer-list
                        tplsub-help-mode-list
                        tplsub-help-global-list))
               (tplsub-give-syntax-help (or p2
                                            tplsub--current-tmpl))))
          ((and (symbolp tmpl)
                (commandp tmpl))                ; Run interactive command
           (call-interactively tmpl))
          ((and (listp tmpl)
                (fboundp p1))   ; A function (with args)
           (apply p1 (cdr tmpl)))
          (t                            ; Unknown - ignore but report
           (message "unknown argument -- `%s'" (prin1-to-string tmpl)))))
  tmpl                                  ; The argument is returned as is
  )


(defun tplsub-templates (&optional pfx)
  "Expand tplsub abbreviations"
  (interactive "*P")
  (setq prefix-arg pfx                  ; Keep for tmpl-command
        tplsub--current-tmpl nil        ; None yet
        tplsub--current-tmpl-point nil) ; None yet
  (if (and (null tplsub-tmpl-buffer-list)
           (null tplsub-tmpl-mode-list)
           (null tplsub-tmpl-global-list))
      (tplsub-insert-undefined "No appropriate templates installed")
    (let ((pt (point))
          fun)
      (save-mark-and-excursion
        (when (re-search-backward tplsub-tmpl-regexp nil t nil)
          (setq tplsub--current-tmpl (tplsub-buf-sub
                                      (match-end 0) pt)
                fun (cdr (assoc (downcase tplsub--current-tmpl)
                                (append tplsub-tmpl-buffer-list
                                        tplsub-tmpl-mode-list
                                        tplsub-tmpl-global-list))))))
      (if (not fun)
          (if (or (null tplsub--current-tmpl)
                  (string= "" tplsub--current-tmpl))
              (tplsub-insert-undefined)
            (tplsub-insert-undefined
             "no abbreviation for `%s'" tplsub--current-tmpl))
        (setq tplsub--current-tmpl-point nil
              tplsub--current-tmpl-defs  nil) ; Defaults
        (delete-region (match-end 0) pt)              ; Remove tmpl name
        (mapc 'tplsub--tmpl-exec fun)         ; Expand
        (when tplsub--current-tmpl-point
          (setq tplsub--current-tmpl-point
                (nreverse (pushnew (point-marker)
                                   tplsub--current-tmpl-point)))
          (tplsub-next-position))
        (setq prefix-arg ())))))

; ======================================================================

(defun tplsub--syntax-help-ins (s)
  "Insert a help string, or expand an alias"
  (cond ((stringp s)
         (setq tplsub-give-syntax-help--reslist
               (concat (or tplsub-give-syntax-help--reslist "")
                       (if tplsub-give-syntax-help--reslist "\n" "")
                       s)))
        ((and (consp s)
              (eq '= (car s))
              (stringp (cdr s)))
         (tplsub-give-syntax-help (cdr s) t))))

(defvar tplsub-give-syntax-help--templist nil)
(defvar tplsub-give-syntax-help--reslist nil)

(defun tplsub-give-syntax-help (topic &optional no-heading)
  "Give help on named syntactic construction"
  (interactive "*sTopic: ")
  (unless no-heading
    (setq tplsub-give-syntax-help--reslist nil
          tplsub-give-syntax-help--templist
          (append tplsub-help-buffer-list
                  tplsub-help-mode-list
                  (copy-list tplsub-help-global-list))))
  (if (= (length tplsub-give-syntax-help--templist) 0)
      (or tplsub-no-no-help
          (message (format "No help topics defined")))
    (let ((curr-buf (current-buffer))
          (slen (- (window-width (minibuffer-window)) 2))
          strings)
      (setq strings (cdr (assoc (if no-heading topic (downcase topic))
                                tplsub-give-syntax-help--templist)))
      (if (null strings)
          (or tplsub-no-no-help
              (message "No help for `%s'" topic))
        (mapc 'tplsub--syntax-help-ins strings)
        (unless no-heading
          (if (string-match "\n" tplsub-give-syntax-help--reslist)
              (progn
                (pop-to-buffer (get-buffer-create "*Syntax Help*"))
                (toggle-read-only 0)
                (erase-buffer)
                (insert "\t\t\t*** "
                        topic
                        " ***\n\n"
                        tplsub-give-syntax-help--reslist)
                (set-buffer-modified-p nil)
                (enlarge-window (- 6 (window-height)))
                (shrink-window-if-larger-than-buffer)
                (goto-char (point-min))
                (pop-to-buffer curr-buf))
            (setq tplsub-give-syntax-help--reslist
                  (concat topic ": " tplsub-give-syntax-help--reslist))
            (if (> (length tplsub-give-syntax-help--reslist)
                   slen)
                (setq tplsub-give-syntax-help--reslist
                      (substring tplsub-give-syntax-help--reslist 0 slen)))
            (message "%s" tplsub-give-syntax-help--reslist)))))))

(defun tplsub-syntax-help (&optional topic)
  "Give help on topic.
Tries to guess topic according to cursor position."
  (interactive)
  (let (begin)
    (unless topic
      (save-mark-and-excursion
        (skip-syntax-backward "w_")
        (setq begin (point))
        (skip-syntax-forward "w_")
        (setq topic (tplsub-buf-sub begin (point))))
      (setq topic (read-string "Help topic: "
                               topic
                               'tplsub-hist-list)))
    (tplsub-give-syntax-help topic)))

; ======================================================================

(defun tplsub-next-position ()
  "Jump to next position defined during last template expansion"
  (interactive)
  (if tplsub--current-tmpl-point
      (let ((mrk (car tplsub--current-tmpl-point)))
        (setq tplsub--current-tmpl-point
              (append (cdr tplsub--current-tmpl-point)
                      (list mrk)))
        (goto-char mrk)
        (if (looking-at "\\s-+$")
            (end-of-line 1)))))

(defun tplsub--add-record (modesym abbrs)
  "Record addition for later inclusion in .emacs or tplsub.el"
  (unless (not tplsub-record-file)
    (set-buffer (get-buffer-create " *tplsub records*"))
    (delete-region (point-min) (point-max))
    (if (file-readable-p tplsub-record-file)
        (insert-file-contents tplsub-record-file))
    (insert "MODE:\t"   (symbol-name modesym) "\n"
            "KEY:\t"    (car abbrs)         "\n"
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
;;                      (read-from-minibuffer "Key: ")
;;                      (read-from-minibuffer "Expansion: " nil nil t)))
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
  "Install tplsub definitions for major mode.
See `tplsub-mode-list' for details."
  (unless (null modesym)
    (let ((current (assq modesym tplsub-mode-list)))
      (if current
          (setq tplsub-mode-list (delete current tplsub-mode-list))))
    (setq tplsub-mode-list
          (append (list (list modesym
                              t-list h-list regex t-case cont-str h-key))
                  tplsub-mode-list))))

(defun tplsub-unregister-mode (modesym)
  "Uninstall mode definitions - not implemented!"
  (error "!!! TODO !!! tplsub-unregister-mode"))

(defsubst tplsub-reregister-mode (modesym &optional t-list h-list regex t-case
                                                    cont-str h-key)
  (tplsub-unregister-mode modesym)
  (tplsub-register-mode modesym t-list h-list regex t-case cont-str h-key))

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
    (unless (null v-list)
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
See `tplsub-tmpl-mode-list' for a description of templates,
and `tplsub-describe-templates' (\\[tplsub-describe templates]) for even
more help..."
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
        (if tplsub-next-key (local-set-key tplsub-next-key
                                            'tplsub-next-position))
        (if tplsub-help-key (local-set-key tplsub-help-key
                                            'tplsub-syntax-help))
        (run-hooks 'tplsub-mode-hook))
    (if tplsub-tmpl-key (local-unset-key tplsub-tmpl-key))
    (if tplsub-help-key (local-unset-key tplsub-help-key))
    (if tplsub-next-key (local-unset-key tplsub-next-key))
    (run-hooks 'tplsub-mode-off-hook)))

;; Load tplsub-x if it exists on the load-path
(condition-case err
    (require 'tplsub-x)
  (error nil))
(provide 'tplsub)

;;; tplsub.el ends here
