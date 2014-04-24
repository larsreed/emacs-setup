;;; generic-extras.el --- Extra Modes for generic-mode

;; Author:  Peter Breton <pbreton@i-kinetics.com>
;; Created: Tue Oct 08 1996
;; Version: $Id: generic-extras.el,v 1.3 1996/10/19 12:22:07 peter Exp $
;; Keywords: 
;; Time-stamp: <96/10/19 08:20:40 peter>

;;; Commentary:
;;
;; This file contains some pre-defined generic-modes.
;; 
;; INSTALLATION:
;;
;; Add this line to your .emacs file:
;;
;;   (require 'generic-extras)
;;
;; You can decide which modes to load by setting the variable
;; 'generic-extras-enable-list'. Some platform-specific modes are
;; affected by the variables 'generic-define-mswindows-modes' and
;; 'generic-define-unix-modes' (which see).
;;
;; ALTERING THESE MODES:
;;
;; To alter the definition of these modes, use the 'alter-generic-mode-'
;; convenience functions defined in generic-mode.el. Each of these functions
;; takes an optional how-to-alter argument, which can be one of the following
;; symbols: 'overwrite, 'append, 'prepend.
;; 
;; You can also send me new modes (I'll accept ones for file types which are
;; reasonably common) or patches to these ones.

;;; Change log:
;; $Log: generic-extras.el,v $
;; Revision 1.3  1996/10/19 12:22:07  peter
;; Added new versions of rc and rul modes
;; Regexp patches for generic-bat-mode
;;
;; Revision 1.2  1996/10/17 01:02:41  peter
;; Improved samba and apache modes
;; Added fvwm and x-resource modes
;;

;;; Code:

(require 'generic-mode)

(defvar generic-extras-enable-list nil
  "*List of generic modes to enable by default.
Each entry in the list should be a symbol.
The variables 'generic-define-mswindows-modes' and 'generic-define-unix-modes'
also affect which generic modes are defined")

(defvar generic-define-mswindows-modes 
  (memq system-type (list 'windows-nt 'ms-dos))
  "*If non-nil, some MS-Windows specific generic modes will be defined")

(defvar generic-define-unix-modes
  (not generic-define-mswindows-modes)
  "*If non-nil, some Unix specific generic modes will be defined")

(if generic-define-mswindows-modes
    (setq generic-extras-enable-list
	  (append (list 'bat 'ini 'inf 'rc 'reg 'rul)
		  generic-extras-enable-list)))

(if generic-define-unix-modes
    (setq generic-extras-enable-list
	  (append (list 'apache 'samba 'hosts 'fvwm 'x-resource)
		  generic-extras-enable-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic-modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (not (memq 'apache generic-extras-enable-list))
    nil
  (define-generic-mode 'apache
    (list ?#)  
    nil 
    '(("^\\(<.*>\\)"       1 'font-lock-reference-face)
      ("^\\(\\sw+\\)\\s-"  1 'font-lock-variable-name-face))    
    (list "srm\\.conf$" "httpd\\.conf$" "access\\.conf$")
    nil 
    "Generic mode for Apache or HTTP configuration files"))
 
(if (not (memq 'samba generic-extras-enable-list))
    nil
  (define-generic-mode 'samba
    (list ?\;)
    nil
    '(("^\\(\\[.*\\]\\)"   1 'font-lock-reference-face))
    (list "smb\\.conf$")
    (list 'generic-bracket-support)
    "Generic mode for Samba configuration files"))

;; This is pretty basic. Also, modes for other window managers could
;; be defined as well.
(if (not (memq 'fvwm generic-extras-enable-list))
    nil
  (define-generic-mode 'fvwm
    (list ?#)
    (list "Style" "Function" "EndFunction" "Popup" "EndPopup")
    nil
    (list "\\.fvwmrc")
    nil
    "Generic mode for FVWM configuration files"))

;; I'm pretty sure I've seen an actual mode to do this, but I don't
;; think it's standard with Emacs
(if (not (memq 'x-resource generic-extras-enable-list))
    nil
  (define-generic-mode 'x-resource
    (list ?!)
    nil
    '(("^\\([^:\n]+:\\)" 1 'font-lock-variable-name-face))
    (list "\\.Xdefaults" "\\.Xresources")
    nil
    "Generic mode for X Resource configuration files"))

(if (not (memq 'hosts generic-extras-enable-list))
    nil
  (define-generic-mode 'hosts
    (list ?#)
    (list "localhost")
    '(("\\([0-9]+\.[0-9]+\.[0-9]+\.[0-9]+\\)" 1 'font-lock-reference-face))
    (list "[hH][oO][sS][tT][sS]$")
    nil
    "Generic mode for HOSTS files"))

(if (not (memq 'inf generic-extras-enable-list))
    nil
  (define-generic-mode 'inf
    (list ?\;)
    nil 
    '(("^\\(\\[.*\\]\\)"   1 'font-lock-reference-face))
    (list "\\.[iI][nN][fF]")
    (list 'generic-bracket-support)
    "Generic mode for MS-Windows INF files"))

;; Should define escape character as well!
(if (not (memq 'ini generic-extras-enable-list))
    nil
  (define-generic-mode 'ini
    (list ?\;)
    nil
    '(("^\\(\\[.*\\]\\)"   1 'font-lock-reference-face)
      ("^\\(.*\\)="        1 'font-lock-variable-name-face))
    (list "\\.[iI][nN][iI]$")
    (list 'generic-bracket-support)
    "Generic mode for MS-Windows INI files"))

(if (not (memq 'reg generic-extras-enable-list))
    nil
  (define-generic-mode 'reg
    '(?\;)
    '("key" "classes_root")
    '(("\\(\\[.*]\\)" 1 'font-lock-reference-face))
    '("\\.[rR][eE][gG]$")
    nil
    "Generic mode for MS-Windows Registry files"))

(if (not (memq 'bat generic-extras-enable-list))
    nil
  (define-generic-mode 'bat
    nil
    nil
    (list
     ;; Make this one first in the list, otherwise comments will
     ;; be over-written by other variables
     (list "^[@ \t]*\\([rR][eE][mM].*\\)" 1 'font-lock-comment-face t)
     (list "^[ \t]*\\(::-.*\\)"	    1 'font-lock-comment-face t)
     ;; These keywords appear as the first word on a line
     (list
      (concat "^[@ \t]*\\(\\<"
	      (mapconcat 
	       'identity
	       '(
		 "[cC][aA][lL][lL]"
		 "[eE][cC][hH][oO]"
		 "[fF][oO][rR]"
		 "[iI][fF]"
		 "[pP][aA][tT][hH]"
		 "[pP][aA][uU][sS][eE]"
		 "[pP][rR][oO][mM][pP][tT]"
		 "[sS][eE][tT]"
		 "[sS][tT][aA][rR][tT]"
		 )
	       "\\>\\|\\<")
	      "\\>\\)") 1 'font-lock-keyword-face)
     ;; These keywords can be anywhere on a line
     (list
      (concat "\\(\\<"
	      (mapconcat
	       'identity
	       '(
		   "[eE][xX][iI][sS][tT]"
		   "[eE][rR][rR][oO][rR][lL][eE][vV][eE][lL]"
		   "[gG][oO][tT][oO]"
		   "[nN][oO][tT]"
		   )
	       "\\>\\|\\<")
	      "\\>\\)") 1 'font-lock-keyword-face)
     (list "^[ \t]*\\(:\\sw+\\)"         1 'font-lock-function-name-face t)
     (list "\\(%\\sw+%\\)"		 1 'font-lock-reference-face)
     (list "\\(%[0-9]\\)"		 1 'font-lock-reference-face)
     (list "\\(/[^/ \"\t\n]+\\)"	 1 'font-lock-type-face)
     (list "[\t ]+\\([+-][^\t\n\" ]+\\)" 1 'font-lock-type-face)
     (list "\\<\\([gG][oO][tT][oO]\\)\\>[ \t]*\\(\\sw+\\)?" 
	   '(1 font-lock-keyword-face)
	   '(2 font-lock-function-name-face nil t))
     
     )
    (list "\\.[bB][aA][tT]$" "CONFIG\\." "AUTOEXEC\\." )
    (list 'generic-bat-mode-setup-function)
    "Generic mode for MS-Windows BAT files")

  (defvar bat-generic-mode-syntax-table nil
    "Syntax table in use in bat-generic-mode buffers.")
  
  ;; Make underscores count as words
  (if bat-generic-mode-syntax-table
      nil
    (setq bat-generic-mode-syntax-table (make-syntax-table))
    (modify-syntax-entry ?_  "w"  bat-generic-mode-syntax-table))
  
  ;; BAT-generic-mode doesn't use the comment functionality of generic-mode
  ;; because it has a three-letter comment-string, so we do it
  ;; here manually instead
  (defun generic-bat-mode-setup-function ()
    (make-local-variable	     'parse-sexp-ignore-comments)
    (make-local-variable	     'comment-start)
    (make-local-variable	     'comment-start-skip)
    (make-local-variable	     'comment-end)
    (setq imenu-generic-expression  '((nil "^:\\(\\sw+\\)" 1))
	  parse-sexp-ignore-comments t
	  comment-end                ""
	  comment-start		     "[Rr][Ee][Mm] "
	  comment-start-skip	     "[Rr][Ee][Mm] *"
	  )
    (set-syntax-table	      bat-generic-mode-syntax-table)
    )
  )

;; Contributed by ACorreir@pervasive-sw.com (Alfred Correira)
(if (not (memq 'rc generic-extras-enable-list))
    nil
  (define-generic-mode 'rc
    (list ?\/)
  '("ACCELERATORS"
    "AUTO3STATE"
    "AUTOCHECKBOX"
    "AUTORADIOBUTTON"
    "BITMAP"
    "CAPTION"
    "CHARACTERISTICS"
    "CHECKBOX"
    "CLASS"
    "COMBOBOX"
    "CONTROL"
    "CTEXT"
    "CURSOR"
    "DEFPUSHBUTTON"
    "DIALOG"
    "EDITTEXT"
    "EXSTYLE"
    "FONT"
    "GROUPBOX"
    "ICON"
    "LANGUAGE"
    "LISTBOX"
    "LTEXT"
    "MENUITEM SEPARATOR" 
    "MENUITEM" 
    "MENU"
    "POPUP"
    "PUSHBOX"
    "PUSHBUTTON"
    "RADIOBUTTON"
    "RCDATA"
    "RTEXT"
    "SCROLLBAR"
    "SEPARATOR"
    "STATE3"
    "STRINGTABLE"
    "STYLE"
    "VERSIONINFO"
    "VERSION"
   )
  ;; the choice of what tokens go where is somewhat arbitrary,
  ;; as is the choice of which value tokens are included, as
  ;; the choice of face for each token group
  (list
   (cons
    (concat
     "\\(\\<"
     (mapconcat
      'identity
      '("FILEFLAGSMASK"
        "FILEFLAGS"
        "FILEOS"
        "FILESUBTYPE"
        "FILETYPE"
        "FILEVERSION"
        "PRODUCTVERSION"
       )
      "\\>\\|\\<")
     "\\>\\)")
    'font-lock-type-face)
   (cons
    (concat
     "\\(\\<"
     (mapconcat
      'identity
      '("BEGIN"
        "BLOCK"
        "END"
        "VALUE"
       )
      "\\>\\|\\<")
     "\\>\\)")
    'font-lock-function-name-face)
   '("^#[ \t]*include[ \t]+\\(<[^>\"\n]+>\\)" 1 font-lock-string-face)
   '("^#[ \t]*define[ \t]+\\(\\sw+\\)(" 1 font-lock-function-name-face)
   '("^#[ \t]*\\(elif\\|if\\)\\>"
     ("\\<\\(defined\\)\\>[ \t]*(?\\(\\sw+\\)?" nil nil
      (1 font-lock-reference-face) (2 font-lock-variable-name-face nil t)))
   '("^#[ \t]*\\(\\sw+\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 font-lock-reference-face) (2 font-lock-variable-name-face nil t)))
    (list "\\.[rR][cC]$")
    nil
    "Generic mode for MS-Windows Resource files"))

;; Contributed by ACorreir@pervasive-sw.com (Alfred Correira)
(if (not (memq 'rul generic-extras-enable-list))
    nil
  (define-generic-mode 'rul 
    (list ?\/)
  '("begin"
    "call"
    "case"
    "declare"
    "default"
    "downto"
    "elseif"
    "else"
    "endfor"
    "endif"
    "endswitch"
    "endwhile"
    "end"
    "exit"
    "external"
    "for"
    "function"
    "if"
    "program"
    "prototype"
    "repeat"
    "return"
    "step"
    "switch"
    "then"
    "to"
    "typedef"
    "until"
    "void"
    "while")
  (list
   ;; preprocessor constructs
   '("#[ \t]*include[ \t]+\\(<[^>\"\n]+>\\)"
     1 font-lock-string-face)
   '("#[ \t]*\\(\\sw+\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 font-lock-reference-face)
     (2 font-lock-variable-name-face nil t))
   ;; gotos
   '("[ \t]*\\(\\sw+:\\)" 1 font-lock-function-name-face)
   '("\\<\\(goto\\)\\>[ \t]*\\(\\sw+\\)?" 
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face nil t))
   ;; type keywords
   (cons
    (concat
     "\\(\\<"
     (mapconcat
      'identity
      '("BOOL"
        "BYREF"
        "CHAR"
        "HIWORD"
        "HWND"
        "INT"
        "LIST"
        "LONG"
        "LOWORD"
        "NUMBER"
        "POINTER"
        "QUAD"
        "RGB"
        "SHORT"
        "STRINGLIST"
        "STRING")
      "\\>\\|\\<")
     "\\>\\)")
    'font-lock-type-face)
   ;;; system variables
   (cons
    (concat
     "\\(\\<"
     (mapconcat
      'identity
      '("CMDLINE"
        "ERRORFILENAME"
        "INFOFILENAME"
        "ISRES"
        "ISUSER"
        "ISVERSION"
        "SRCDIR"
        "SRCDISK"
        "SUPPORTDIR"
        "TARGETDIR"
        "TARGETDISK"
        "WINDIR"
        "WINDISK"
        "WINSYSDIR"
        "WINSYSDISK")
      "\\>\\|\\<")
     "\\>\\)")
    'font-lock-variable-name-face)
   ;; pre-defined constants (not exhaustive -- just my favorites)
   (cons
    (concat
     "\\(\\<"
     (mapconcat
      'identity
      '("AFTER"
        "APPEND"
        "BACKGROUNDCAPTION"
        "BACKGROUND"
        "BACK"
        "BEFORE"
        "BK_BLUE"
        "BK_GREEN"
        "BK_RED"
        "CANCEL"
        "COMMANDEX"
        "COMMAND"
        "CONTINUE"
        "DEFWINDOWMODE"
        "DISABLE"
        "DLG_ERR"
        "ENABLE"
        "EXCLUSIVE"
        "EXISTS"
        "EXIT"
        "FAILIFEXISTS"
        "FALSE"
        "FULL"
        "INDVFILESTATUS"
        "INFORMATION"
        "LIST_NULL"
        "NEXT"
        "NONEXCLUSIVE"
        "NOSET"
        "NO"
        "OFF"
        "ON"
        "PARTIAL"
        "REPLACE_ITEM"
        "REPLACE"
        "RESET"
        "RESTART"
        "SET"
        "SEVERE"
        "SRCTARGETDIR"
        "STATUS"
        "TRUE"
        "YES"
        "WARNING")
      "\\>\\|\\<")
     "\\>\\)")
    'font-lock-variable-name-face))     ; is this face the best choice?
    (list "\\.[rR][uU][lL]$")
    nil
    "Generic mode for InstallShield RUL files"))

(provide 'generic-extras)

;;; generic-extras.el ends here

