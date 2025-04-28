;; lresetup.el --- Lars Reed - Emacs init file

;; Copyright (C) 1993-2014 Lars Reed
;; Author: Lars Reed <Lars@kalars.net>
;; Maintainer: Lars Reed
;; Keywords: Emacs setup

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
; Hi-lock: (("^;;; lre.*\\.el[^\n]*" (0 (quote hi-black-hb) t)))
; Hi-lock: (("Lars.*R[e]ed" (0 (quote hi-blue) t)))
; Hi-lock: (("^;;; [^\n]+" (0 (quote hi-green) t)))

;; Todo: sjekk combine-and-quote-strings, format-seconds, read-shell-command,
;  locate-user-emacs-file, looking-at-p, string-match-p


;;; --------------------------------------------------------------
;;; Loading libraries
;;;
(message "Setup...")

(require 'cl) ; Always!

;;; Commentary:
;;  This file is my personal emacs setup file.  It has grown complex over the
;;  years, so the lesser used parts have been moved to separate
;;  lre-xx.el-files.


;;; History:
;; An endless number of changes!
;; Current version: $Id: lresetup.el,v 1.18 2007/06/19 20:06:54 larsr Exp $

;;; TODO
;; - printer win32
;; - konfig csdiff i 00setup
;; - lpr-page-header-program



;;; Code:

;; Config...
(defsubst lre-memb (&rest args)
  "Decide if ANY of the given symbols are present in the current config."
  (not (null (intersection args lre-cfg))))
(defsubst lre-not-memb (&rest args)
  "Decide if NONE of the given symbols are present in the current config."
  (null (intersection args lre-cfg)))
(defsubst lre-memb-all (&rest args)
  "Decide if ALL of the given symbols are present in the current config."
  (subsetp args lre-cfg))
(defsubst lre-add-memb (sym)
  "Add a symbol to the current configuration"
  (or (lre-memb sym)
      (setq lre-cfg (cons sym lre-cfg))))
(defsubst lre-del-memb (sym)
  "Remove a symbol from the current configuration"
  (setq lre-cfg (remove sym lre-cfg)))

(defsubst lre-fixed (sym)
  "Fixed string named n"
  (cdr (assoc sym lre-fixed-paths)))

;; A require that does not fail because of a missing file

(defun lre-safe-require (symbol &optional fname)
  (let ((ok t))
    (condition-case err-symb
        (setq ok (require symbol fname t))
      ((file-error error)
       (message "REQUIRE: %s -- %s"
                (prin1-to-string  symbol)
                (prin1-to-string  err-symb))
       (setq ok nil))
      )
    ok))

(defsubst lre-debug (&rest args) (if nil (apply 'message args)))

(defun lre-file-name (fname)
  "Return a standarized file name"
  (abbreviate-file-name (file-truename fname)))

(unless (lre-memb 'e21+)
  (lre-safe-require 'advice)
  (lre-safe-require 'custom)
  (lre-safe-require 'sgml-mode))
(lre-safe-require 'regexp-opt)

(eval-when-compile
  (lre-safe-require 'advice)
  (lre-safe-require 'custom)
  (lre-safe-require 'sgml-mode)
  (lre-safe-require 'cc-mode)
  (lre-safe-require 'cf-mode)
  (lre-safe-require 'font-lock)
  ;; (lre-safe-require 'fe-mode)
  (lre-safe-require 'imenu)
  (lre-safe-require 'ksh-mode)
  (lre-safe-require 'man)
  (lre-safe-require 'ps-print)
  (lre-safe-require 'generic)
  (if (lre-memb 'e24+) ;;; Sjekk her for strengfunksjoner
      (lre-safe-require 's)))

(eval-and-compile
  (lre-safe-require 'expand-region)
  (lre-safe-require 'tplsub))

;;; Implicit config
(if (lre-memb 'dos 'win32) (lre-add-memb 'pc))


;;; For customization
(eval-and-compile
  (cond ((and (featurep 'custom)
              (fboundp 'custom-declare-variable)) t)
        ((<= (+ (* 1000 emacs-major-version) emacs-minor-version) 19029) nil)
        (t
         (defmacro defgroup (&rest args) nil)
         (defmacro defface  (var values doc &rest args) `(make-face ,var))
         (defmacro defcustom (var value doc &rest args)
           `(defvar ,var ,value ,doc)))))

;;; ---------------------------------------------------------------------
;;; /// Variables to control loading ///

(defgroup lresetup nil "LRE setup variables" :prefix "lre-" :group 'custom)

(defvar lre-non-graph (or (null window-system)
                          (eq window-system 'ms-dos))
  "t if not in a graphical environment")

;; (defcustom lre-inhibit-fume (lre-not-memb 'xemacs)
;;   "* if non-nil, func-menu is not run."
;;   :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-eshell nil
  "* if non-nil, eshell is unavailable."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-welcome nil
  "* if nil, no(t) welcome..."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-cc-lobotomize t
  "* if non-nil, cc-lobotomy is not loaded."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-imenu (or lre-non-graph
                                 (lre-not-memb 'gnunix 'win32))
  "* if non-nil, func-menu is not run."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-server (lre-not-memb 'win32 'gnunix)
  "* if non-nil, gnuserv/`server-start' is not run."
  :group 'lresetup  :type  'boolean)
(defcustom lre-gnuserv-server t
  "* set to nil to use emacslient rather than gnuclient."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-term (lre-memb 'dos 'win32 'gnunix)
  "* if non-nil, default terminal bindings are not loaded."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-toolbar (or (lre-not-memb 'e21+)
                                   (and (lre-not-memb 'e22+)
                                        (lre-memb 'win32)))
  "* if non-nil, toolbar settings are not made."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-iso nil
  "* if non-nil, iso terminal bindings are not loaded."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-mouse (or lre-non-graph (lre-memb 'dos))
  "* if non-nil, mouse bindings are not loaded."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-paren (or lre-non-graph (lre-memb 'dos))
  "* if non-nil, flashy parentheses are not loaded."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-hilit (or lre-non-graph (lre-memb 'e21+))
  "* if non-nil, the hilit library is not loaded."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-flock (lre-not-memb 'gnunix 'win32)
  "* if non-nil, the font-lock library is not loaded."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-keys nil
  "* Set to non-nil to avoid changing setup of some standard keys."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-keymap nil
  "* Set to non-nil to avoid creating special keymap."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-sccs (lre-memb 'pc)
  "* Set to non-nil to avoid loading of SPE SCCS with `sysdul-mode'."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-scripts (lre-memb 'dos)
  "* Set to non-nil to avoid searching for `#!' in scripts."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-inet (lre-not-memb 'e22+)
  "* Set to non-nil to avoid preparing Gnus & vm."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-ido (not (lre-memb-all 'e22+ 'personal))
  "* Set to non-nil to avoid preparing ido-...."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-pkg nil
  "* Set to non-nil to avoiding adding many non-standard packages."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-ispell t
  "* Set to non-nil to avoiding using Ispell."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-banner (lre-memb 'pc)
  "* Set to non-nil to avoiding banners when printing."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-printer nil
  "* Set to non-nil to avoid setting up printer."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-sysdul nil
  "* Set to non-nil to avoid adjusting `sysdul-mode'."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-color-change nil
  "* Set to non-nil to avoid changing colors."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-modeline nil
  "* Set to non-nil to avoid changing the mode line."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-loadpath nil
  "* Set to non-nil to avoid changing the load path."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-tpl nil
  "* Set to non-nil to avoid setting up templates."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-braces (or (lre-memb 'e22+)
                                  (lre-not-memb 'personal))
  "* Set to non-nil to avoid setting up brace handling."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-lazy nil
  "* Set to non-nil to avoid setting up lazy font-locking."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-sql nil
  "* Set to non-nil to avoid setting up SQL-mode."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-tabbar (not (lre-memb-all 'personal 'e21+ 'win32))
  "* Set to non-nil to avoid loading tabbar."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-ruler (lre-not-memb 'e22+)
  "* Set to non-nil to ignore ruler."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-user nil
  "* Set to non-nil to avoid setting up user info."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-nxml-hack (lre-not-memb 'e22+)
  "* Set to non-nil to avoid hacking fonts in nxml-mode (Emacs 22)."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-cursor-color nil
  "* Set to non-nil to avoid changing cursor colors."
  :group 'lresetup  :type  'boolean)
(defcustom lre-inhibit-scala (not lre-scala-pkg)
  "* Set to non-nil to avoid loading Scala support."
  :group 'lresetup  :type  'boolean)

;; Special variables
(defcustom lre-user-sign "LRE"
  "*User signature for history records."
  :group 'lresetup :type 'string)
(defcustom lre-user-org "Mesan"
  "* User organization for history records."
  :group 'lresetup :type 'string)
(defcustom lre-full-name "Lars Reed"
  "* Full user name."
  :group 'lresetup :type 'string)
(defcustom lre-full-org "Mesan"
  "* Full organization name."
  :group 'lresetup :type 'string)
(defcustom lre-fe-org lre-user-org
  "* Org. for FE-mode."
  :group 'lresetup :type 'string)
(defcustom lre-fe-user (cond ((lre-memb 'ccas) "x06lre")
                             ((lre-memb 'tvist) "LRE")
                             (t lre-user-sign))
  "* Org. for FE-mode."
  :group 'lresetup :type 'string)
(defcustom lre-std-domain "kalars.net"
  "* Domain for misc. servers."
  :group 'lresetup :type 'string)
(defcustom lre-mail-user "lars"
  "* User name for mail servers."
  :group 'lresetup :type 'string)

(defcustom lre-mail-address "Lars@kalars.net"
  "* Sender address."
  :group 'lresetup :type 'string)
(defcustom lre-smtp-domain (concat "smtp." lre-std-domain)
  "* SMTP server."
  :group 'lresetup :type 'string)
(defcustom lre-pop-domain (concat "pop." lre-std-domain)
  "* POP3 server."
  :group 'lresetup :type 'string)
(defcustom lre-nntp-domain (concat "nntp." lre-std-domain)
  "* NNTP server."
  :group 'lresetup :type 'string)

(defcustom lre-long-line 75
  "* This is a long line..."
  :group 'lresetup :type 'integer)
(make-variable-buffer-local 'lre-long-line)
(put 'lre-long-line 'permanent-local t)

(defcustom lre-menu-lines (cond ((lre-memb 'lreibm)
                                 56)
                                ((lre-memb 'lredell 'lredigital)
                                 40)
                                ((lre-memb 'gnunix)
                                 37)
                                ((lre-memb 'pc) 31)
                                (t 42))
  "* No of lines in a menu"
  :group 'lresetup :type 'integer)

(defcustom lre-vip-files (if (and (lre-memb 'personal) user-init-file)
                             (list
                              (concat
                               (file-name-sans-extension
                                lre-ini) ".el")
                              lre-00
                              (lre-fixed :lre-start)
                              user-init-file)
                           (if (lre-memb 'personal)
                             (list
                              (concat
                               (file-name-sans-extension
                                lre-ini) ".el"))
                             (list user-init-file)))
  "* Important files..."
  :group 'lresetup :type '(repeat file))

(defcustom lre-load-all-dir (concat (if (boundp 'user-emacs-directory)
				      user-emacs-directory
				      "~/.emacs.d") "/loadall/")
  "* Directory containing local files to autoload at startup"
  :group 'lresetup :type 'file)


(defcustom lre-fignore-re '("^/tmp/" "\\.tmp$" "~$"   "\\.lsd$"
                            "\\.bak$" "/x\\..$" "/..$"
                            "/spe/.*src/new/"
                            "/....9505/ci/"
                            "/..../ci/"
                            "/\\.emacs\\.bmk$"
                            "/\\.emacs-places$"
                            "/\\.recentf$"
                            "/\\.elc$"
                            "/\\.cvsignore$"
                            "\\.prn$"
                            "newsrc.eld$"
                            "COMMIT_EDITMSG$"
                            "/hg-editor-[^/]+\\.txt"
                            "bzr_log\\.[^/]+"
                            "/VMBOX$")
  "* Unimportant files..."
  :group 'lresetup :type '(repeat file))

(defcustom lre-fignore-2  '("/emacs/.*/lisp/"
                            "\\.sd$")
  "* Files excluded from the recent menu"
  :group 'lresetup :type '(repeat file))


(defcustom lre-imenu-general
  '("///" "[/]\\{3,\\}\\s-+\\([^\n]+\\)\\s-+[/]\\{3,\\}" 1)
  "* Expression to add to the imenu"
  :type 'sexp
  :group 'lresetup)

(defcustom lre-regsave-file
  (expand-file-name
   (convert-standard-filename
    (if (lre-memb-all 'win32 'tvist)
        "~/_emacsregs"
      "~/.emacsregs")))
  "*File in which to save register contents"
  :group 'lresetup  :type 'file)

(defcustom lre-generic-mode-list
  nil
  "generic-modes"
  :group 'lresetup
  :type 'list)

(unless (or (lre-memb 'personal)
            lre-inhibit-user)
  (setq lre-user-sign    (upcase
                          (if (lre-memb 'tvist)
                              (let ((user (getenv "USER")))
                                (cond ((null user)
                                       "unknown")
                                      ((string-match "9505$" user)
                                       (substring user 0 (- (length user) 4)))
                                      (t
                                       user)))
                              user-login-name))
        lre-user-org     (if (lre-memb 'tvist) "tvist" "")
        lre-full-name    user-full-name
        lre-full-org     lre-user-org
        lre-std-domain   (if (lre-memb 'tvist) "toll.no" "")
        lre-mail-user    user-login-name
        lre-mail-address (concat lre-mail-user "@" lre-std-domain)
        lre-fe-user      (if (lre-memb 'tvist) (substring lre-user-sign 0 4)
                           lre-user-sign)
        lre-smtp-domain  (concat "smtp." lre-std-domain)
        lre-pop-domain   (concat "pop." lre-std-domain)
        lre-nntp-domain  (concat "nntp." lre-std-domain)))

(unless lre-inhibit-user
  (if (= 0 (length user-full-name)) (setq user-full-name lre-full-name))
  (if (= 0 (length mail-host-address)) (setq mail-host-address lre-std-domain))
  (if (= 0 (length user-mail-address)) (setq user-mail-address lre-mail-address)))

(defcustom sccs-prefix-string (concat "@@(#)"
                                   (cond ((lre-memb 'tvist) "TVIST")
                                         (t "LRE")))
  "* Prefix in SCCS version string."
  :group 'lresetup :type 'string)
(defconst sccs-suffix-string (concat "%M" "% " "%I" "% " "%E" "%>")
  "Suffix in SCCS version string.")
(defvar lre-last-hist "^; "
  "Current history line prefix.")

(defcustom lre-std-indent (if (lre-memb 'tvist) 3 4)
  "Preferred indentation for most modes."
  :group 'lresetup :type 'integer)


;;; ---------------------------------------------------------------------
;;; /// Printer ///

(defgroup lre-ps-group nil
  "Remember print settings"
  :tag "Standard print settings"
  :prefix "lre-ps-"
  :group 'lresetup)

(defcustom lre-ps-use-spool nil
  "*Non-nil to spool rather than print.  Use `ps-despool' to print."
  :type 'boolean
  :group 'lre-ps-group)

(defcustom lre-ps-use-faces t
  "*Non-nil to print with different fonts"
  :type 'boolean
  :group 'lre-ps-group)

(defcustom lre-ps-ignore-region nil
  "*Non-nil to always print whole buffer"
  :type 'boolean
  :group 'lre-ps-group)

(defcustom lre-ps-use-landscape nil
  "*Non-nil to print in landscape format (otherwise portrait)"
  :type 'boolean
  :group 'lre-ps-group)

(defcustom lre-ps-use-header t
  "*Non-nil to include page header."
  :type 'boolean
  :group 'lre-ps-group)

(defcustom lre-ps-use-duplex nil
  "*Non-nil to print duplex"
  :type 'boolean
  :group 'lre-ps-group)

(defcustom lre-ps-use-zebra (if (lre-memb 'personal) 3
                              nil)
  "*Nil to avoid zebra stripes, otherwise number of lines in each stripe."
  :type '(choice :tag "Zebra"
                 (const :tag "None" nil)
                 (integer :tag "No. of lines"))
  :group 'lre-ps-group)

(defcustom lre-ps-use-n-up 1
  "*No. of pages per sheet"
  :type '(integer
          :tag "N Up Printing"
          :validate
          (lambda (wid)
            (if (and (< 0 (widget-value wid))
                     (<= (widget-value wid) 100))
                nil
              (widget-put
               wid :error
               "Number of pages per sheet paper must be between 1 and 100.")
              wid)))
  :group 'lre-ps-group)

(defcustom lre-ps-use-line-no 10
  "*Use line numbers?

nil: no line numbers
zebra: first in each zebra stripe
number: each N line"
  :type '(choice :tag "Line numbers"
                 (const :tag "None" nil)
                 (const :tag "Zebra" 'zebra)
                 (integer :tag "Each n. line"))
  :group 'lre-ps-group)


(defcustom lre-printer-lp (cond ((lre-memb-all 'win32 'tvist)
                                 (lre-fixed :printer-path))
                                ((lre-memb 'win32) (lre-fixed :text-print))
                                ((lre-memb 'tvist) (lre-fixed :printer1))
                                (t ""))
  "* Text printer"
  :type 'string :group 'lresetup)
(defcustom lre-ps-printer-name (cond ((lre-memb-all 'win32 'tvist)
                                      (lre-fixed :printer-path))
                                     ((lre-memb 'lreibm) (lre-fixed :ps-print))
                                     ((lre-memb 'win32) (lre-fixed :ps-print))
                                     ((lre-memb 'tvist) (lre-fixed :printer2))
                                     (t ""))
  "* Postscript printer"
  :type 'string :group 'lre-ps-group)
(defcustom lre-printer-cmd-lp (cond ((lre-memb 'win32) "")
                                    ((lre-memb 'tvist) "lpr")
                                    (t "lp"))
  "* Text print command"
  :type 'string :group 'lresetup)
(defcustom lre-ps-print-cmd lre-printer-cmd-lp
  "* PS printer command"
  :type 'string :group 'lre-ps-group)
(defcustom lre-printer-opt-lp (cond ((lre-memb-all 'win32)
                                     nil)
                                    ((lre-memb 'tvist)
                                     (list "-P" lre-printer-lp
                                           (if lre-inhibit-banner "-h" "")))
                                    (t nil))
  "* Text printer command options"
  :type 'string :group 'lresetup)
(defcustom lre-ps-print-cmd-opt (cond ((lre-memb-all 'tvist 'gnunix)
                                       (if lre-inhibit-banner '("-h")))
                                      (t nil))
  "* PS printer command options"
  :type '(repeat string) :group 'lre-ps-group)

(defvar lre-duplex-switch "")

;;; -------------------------------------------------------------------------


;; Abandon lre-inhibit-xxx - use lre-cfg
(or lre-inhibit-banner (lre-add-memb 'banner))
(or lre-inhibit-braces (lre-add-memb 'braces))
(or lre-inhibit-cc-lobotomize (lre-add-memb 'cc-lobo))
(or lre-inhibit-color-change (lre-add-memb 'col-chg))
(or lre-inhibit-flock (lre-add-memb 'flock))
(or lre-inhibit-hilit (lre-add-memb 'hilit))
(or lre-inhibit-ido (lre-add-memb 'ido))
(or lre-inhibit-imenu (lre-add-memb 'imenu))
(or lre-inhibit-inet (lre-add-memb 'inet))
(or lre-inhibit-iso (lre-add-memb 'iso))
(or lre-inhibit-ispell (lre-add-memb 'ispell))
(or lre-inhibit-keymap (lre-add-memb 'keymap))
(or lre-inhibit-keys (lre-add-memb 'keys))
(or lre-inhibit-lazy (lre-add-memb 'lazy))
(or lre-inhibit-loadpath (lre-add-memb 'loadpath))
(or lre-inhibit-modeline (lre-add-memb 'modeline))
(or lre-inhibit-mouse (lre-add-memb 'mouse))
(or lre-inhibit-paren (lre-add-memb 'paren))
(or lre-inhibit-pkg (lre-add-memb 'pkg))
(or lre-inhibit-printer (lre-add-memb 'printer))
(or lre-inhibit-ruler (lre-add-memb 'ruler))
(or lre-inhibit-sccs (lre-add-memb 'sccs))
(or lre-inhibit-scripts (lre-add-memb 'scripts))
(or lre-inhibit-server (lre-add-memb 'server))
(or lre-inhibit-sql (lre-add-memb 'sql))
(or lre-inhibit-sysdul (lre-add-memb 'sysdul))
(or lre-inhibit-tabbar (lre-add-memb 'tabbar))
(or lre-inhibit-term (lre-add-memb 'term))
(or lre-inhibit-toolbar (lre-add-memb 'toolbar))
(or lre-inhibit-tpl (lre-add-memb 'tpl))
(or lre-inhibit-user (lre-add-memb 'user))
(or lre-inhibit-welcome (lre-add-memb 'welcome))
(or lre-inhibit-nxml-hack (lre-add-memb 'nxml-hack))
(or lre-inhibit-cursor-color (lre-add-memb 'cursor-color))
(and (lre-memb 'tadntp4) (lre-add-memb 'TAD))

(setq load-path
      (let ((lp (list (car lre-lisp))))
	(mapc
	 (function (lambda (d)
		     (if (and d
			      (eq (car-safe (file-attributes d)) t)
			      (not (and lre-inhibit-inet
					(string-match "/gnus$" d)))
			      (not (string-match "/obsolete$" d))
			      (or (not lre-inhibit-eshell)
                                  (not (string-match "/eshell$" d)))
			      (or (file-expand-wildcards (concat d "/*.el"))
				  (file-expand-wildcards (concat d "/*.elc")))
			      )
			 (add-to-list 'lp d))))
	 load-path)
	(nreverse lp)))


;; Seem to need these here???
(eval-and-compile
  (lre-safe-require 'vc)
  (when (lre-memb 'inet)
    (lre-safe-require 'gnus)
    (lre-safe-require 'vm)))

;;; --------------------------------------------------------------
;;; /// Different versions... ///
;;;

(or (fboundp 'set-frame-font)
    (defalias 'set-frame-font 'set-default-font))

(or (fboundp 'read-only-mode)
    (defsubst read-only-mode (&optional arg)
      (toggle-read-only arg)))

(if (lre-memb 'e22+)
    (defalias 'To-int 'string-to-number )
  (defsubst To-int(s) (string-to-int s)))

(or (fboundp 'display-message-or-buffer)
    (defsubst display-message-or-buffer (msg &optional
                                             buffer-name not-this-window frame)
      (message "%s" msg)))

(or (fboundp 'with-output-to-string)
    (defmacro with-output-to-string (&rest body)
      `(save-excursion
         (set-buffer (get-buffer-create " *string-output*"))
         (setq buffer-read-only nil)
         (buffer-disable-undo (current-buffer))
         (erase-buffer)
         (let ((standard-output (current-buffer)))
             ,@body)
           (buffer-string))))

(or (fboundp 'duplicate-line)
    (defun duplicate-line (arg)
      "Duplicate current line, leaving point in lower line."
      (interactive "*p")

      ;; save the point for undo
      (setq buffer-undo-list (cons (point) buffer-undo-list))

      ;; local variables for start and end of line
      (let ((bol (save-excursion (beginning-of-line) (point)))
            eol)
        (save-excursion
          ;; don't use forward-line for this, because you would have
          ;; to check whether you are at the end of the buffer
          (end-of-line)
          (setq eol (point))
          ;; store the line and disable the recording of undo information
          (let ((line (buffer-substring bol eol))
                (buffer-undo-list t)
                (count arg))
            ;; insert the line arg times
            (while (> count 0)
              (newline)         ;; because there is no newline in 'line'
              (insert line)
              (setq count (1- count)))
            )
          ;; create the undo information
          (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list))))
      ;; put the point in the lowest line and return
      (next-line arg)))

(or (fboundp 'kbd)
    (defmacro kbd (keys)
      "Convert KEYS to Emacs key representation.
KEYS must be in keyboard macro notation."
      (read-kbd-macro keys)))

(or (fboundp 'line-beginning-position)
    (defsubst line-beginning-position (&optional n)
      "Return point at beginning of current line"
      (let ((p (point)))
        (beginning-of-line n)
        (prog1 (point)
          (goto-char p)))))

(or (fboundp 'line-end-position)
    (defsubst line-end-position (&optional n)
      "Return point at end of current line"
      (let ((p (point)))
        (end-of-line n)
        (prog1 (point)
          (goto-char p)))))

(or (fboundp 'line-length)
    (defsubst line-length (&optional n)
      "Return length of current line"
      (let ((p (point))
            pmax)
        (end-of-line n)
        (prog1
            (- (point)
               (progn
                 (beginning-of-line)
                 (point)))
            (goto-char p)))))

(or (fboundp 'looking-back)
    (defun looking-back (regexp &optional limit greedy)
      "Return non-nil if text before point matches regular expression REGEXP.
Like `looking-at' except matches before point, and is slower.
LIMIT if non-nil speeds up the search by specifying how far back the
match can start.

If GREEDY is non-nil, extend the match backwards as far as possible,
stopping when a single additional previous character cannot be part
of a match for REGEXP."
      (let ((start (point))
            (pos
             (save-excursion
               (and (re-search-backward (concat "\\(?:" regexp "\\)\\=") limit t)
                    (point)))))
        (if (and greedy pos)
            (save-restriction
              (narrow-to-region (point-min) start)
              (while (and (> pos (point-min))
                          (save-excursion
                            (goto-char pos)
                            (backward-char 1)
                            (looking-at (concat "\\(?:"  regexp "\\)\\'"))))
                (setq pos (1- pos)))
              (save-excursion
                (goto-char pos)
                (looking-at (concat "\\(?:"  regexp "\\)\\'")))))
        (not (null pos)))))

(or (fboundp 'printenv)
    (defun printenv(var)
      "Returns value of environment variable"
      (interactive
       (list
        (progn
          (require 'env)
          (read-envvar-name "Enviroment variable: "))))
      (let ((var-name (getenv var)))
        (if (null var-name)
            (error "%s does not exist" var)
          (message var-name)))))

(fset 'lre-regexp-opt
      (if (fboundp 'regexp-opt)
          'regexp-opt
        'make-regexp))


;; with-temp-message was introduced in e20.4
(or (fboundp 'with-temp-message)
    (defmacro with-temp-message (message &rest body)
      "Display MESSAGE temporarily if non-nil while BODY is evaluated.
The original message is restored to the echo area after BODY has finished.
The value returned is the value of the last form in BODY.
MESSAGE is written to the message log buffer if `message-log-max' is non-nil.
If MESSAGE is nil, the echo area and message log buffer are unchanged.
Use a MESSAGE of \"\" to temporarily clear the echo area."
      (let ((current-message (make-symbol "current-message"))
            (temp-message (make-symbol "with-temp-message")))
        `(let ((,temp-message ,message)
               (,current-message))
           (unwind-protect
               (progn
                 (when ,temp-message
                   (setq ,current-message (current-message))
                   (message "%s" ,temp-message))
                 ,@body)
             (and ,temp-message ,current-message
                  (message "%s" ,current-message)))))))

(or (fboundp 'symbol-near-point)
    (defun symbol-near-point ()
      "Return the first textual item to the nearest point."
      (interactive)
                                        ;alg stolen from etag.el
      (save-excursion
        (if (not (memq (char-syntax (preceding-char)) '(?w ?_)))
            (while (not (looking-at "\\sw\\|\\s_\\|\\'"))
              (forward-char 1)))
        (while (looking-at "\\sw\\|\\s_")
          (forward-char 1))
        (if (re-search-backward "\\sw\\|\\s_" nil t)
            (regexp-quote
             (progn (forward-char 1)
                    (buffer-substring (point)
                                      (progn (forward-sexp -1)
                                             (while (looking-at "\\s'")
                                               (forward-char 1))
                                             (point)))))
          nil))))

(defalias 'Report-whitespace (if (lre-memb 'e23+)
                                 'whitespace-report
                               'whitespace-buffer))

(defun lre-e20-undo (p)
  "See `undo'!"
  (interactive "P")
  (if p
      (save-excursion
        (goto-char (point-max))
        (set-mark (point-min))
        (undo p))
    (undo)))

;; From Charles Curley
(defadvice recenter (after lre-recenter disable) ;; !!!!
  "Fontify block if needed."
  (if (lre-memb 'flock)
      (font-lock-fontify-block (window-height))))

(or (fboundp 'htmlize-region)
    (defun htmlize-region (rbeg rend)
      "Like `htmlize-buffer', but on region only."
      (interactive "r")
      (save-restriction
        (narrow-to-region rbeg rend)
        (htmlize-buffer))))

(or (fboundp 'shell-execute-url)
    (defun shell-execute-url (url &optional new-window)
      "Invoke the shell-execute-helper program to call ShellExecute
and launch or redirect a browser to the specified URL."
      (interactive "sURL: ")
      (start-process "shellex" nil shell-execute-helper url)))
;;  (call-process shell-execute-helper nil nil nil url))


(defun lre-replace-in-string (s from &optional to)
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

(or (fboundp 'imenu--flatten-index-alist)
    (defun imenu--flatten-index-alist (index-alist &optional concat-names prefix)
      ;; Takes a nested INDEX-ALIST and returns a flat index alist.
      ;; If optional CONCAT-NAMES is non-nil, then a nested index has its
      ;; name and a space concatenated to the names of the children.
      ;; Third argument PREFIX is for internal use only.
      (mapcan
       (lambda (item)
         (let* ((name (car item))
                (pos (cdr item))
                (new-prefix (and concat-names
                                 (if prefix
                                     (concat prefix imenu-level-separator name)
                                   name))))
           (cond
            ((or (markerp pos) (numberp pos))
             (list (cons new-prefix pos)))
            (t
             (imenu--flatten-index-alist pos new-prefix)))))
       index-alist)))

(or (fboundp 'try-complete-sysdul-symbol)
    (defun try-complete-sysdul-symbol (old)
      "Blir overstyrt" nil))


;;; -------------------------------------------------------------------------
;;;  /// main-setq ///
;;;

(setq
      add-log-keep-changes-together   t
      add-log-full-name               lre-full-name
      apropos-do-all                  t
      apropos-sort-by-scores          t
      auto-mode-case-fold             (lre-memb 'pc)
      auto-raise-tool-bar-buttons     t
      backup-by-copying               nil
      backup-by-copying-when-mismatch (not (or (eq LRE-this-cfg 'tadnt)
                                               (eq LRE-this-cfg 'tadntp4)))
      backup-by-copying-when-linked   (not (or (eq LRE-this-cfg 'tadnt)
                                               (eq LRE-this-cfg 'tadntp4)))
      bookmark-save-flag              1
      bookmark-version-control        'never
      bookmark-automatically-show-annotations nil
      browse-url-new-window-flag      t
      browse-url-new-window-p         t
      buffers-menu-max-size           lre-menu-lines
      change-log-default-name         "ChangeLog"
      change-log-version-info-enabled t
      color-theme-is-cumulative       nil
      comment-mode-alist              '((t         ?#    1      " ")
                                        (awk-mode  ?#    1      " ")
                                        (c++-mode  "// ")
                                        (c-mode    " * " "/* "  "\n */")
                                        (cf-mode  " * " "/* "  "\n */")
                                        (emacs-lisp-mode ?\; 2 " ")
                                        (fe-mode   "\t"   "Komm:")
                                        (html-mode " " "<!-- " " --!>")
                                        (indented-text-mode ?|  1 " ")
                                        (java-mode "// ")
                                        (lisp-interaction-mode ?\; 2 " ")
                                        (lisp-mode ?\; 2 " ")
                                        (mail-mode ?> 1 " ")
                                        (message-mode ?> 1 " ")
                                        (nroff-mode ".\\\ ")
                                        (psgml-mode " " "<!-- " " --!>")
                                        (pxml-mode " " "<!-- " " --!>")
                                        (sgml-mode " " "<!-- " " --!>")
                                        (sql-mode  "-- " "/* " " */")
                                        (sysdul-mode ?\; 1 " ")
                                        (text-mode ?|  1 " ")
                                        (vm-mode ?> 1 " ")
                                        (vsq-mode  "-- " "/* " " */")
                                        (wml-mode " " "<!-- " " --!>")
                                        (nxml-mode " " "<!-- " " --!>")
                                        (xml-mode " " "<!-- " " --!>")
                                        )
      compilation-context-lines       5
      completion-ignored-extensions   (append completion-ignored-extensions
                                              '(".sd" ".f" ".shx"))
      ; completion-styles -- below
      complex-buffers-menu-p          t
      compilation-scroll-output       t
      cvs-highlight                   t
      csdiff-program                  (lre-fixed :csdiff)
      dabbrev-abbrev-char-regexp      "\\sw\\|\\s_"
      default-frame-alist             (list (cond ((lre-memb 'lreibm)
                                                   '(height . 60))
                                                  ((lre-memb 'lredell
                                                             'lredigital)
                                                   '(height . 39))
                                                  ((lre-memb-all 'tvist
                                                                 'win32
                                                                 'personal)
                                                   '(height . 50))
                                                  ((lre-memb-all 'gnunix
                                                                 'personal)
                                                   '(height . 46))
                                                  ((lre-memb 'gnunix)
                                                   '(height . 40))
                                                  (t
                                                   '(height . 31)))
                                            (cond ((lre-memb 'lreibm)
                                                   '(top . 1))
                                                  ((lre-memb-all 'personal 'tadntp4 'win32)
                                                   '(top . 24)))
                                            (cond ((lre-memb 'lreibm)
                                                   '(left . 150)))
                                            (if (lre-memb 'toolbar)
                                                '(tool-bar-lines . 1)
                                              '(tool-bar-lines . 0))
                                            '(width . 81)
                                            '(background-color . "white")
                                            '(foreground-color . "black")
                                            '(vertical-scroll-bars . right)
                                            '(menu-bar-lines . 1))
      default-major-mode              'text-mode
      delete-by-moving-to-trash       t
      delete-selection-mode           nil
      delete-trailing-lines           t
      diff-switches                   ""
      eldoc-idle-delay                1.75
      eldoc-minor-mode-string         " doc"
      enable-local-eval               'maybe
      enable-recursive-minibuffers    nil
      ext-cmd-w32-office-path         lre-office-path
      ext-menu-path                   '("lre")
      ext-menu-before                 "Printing"
      ext-discard-buffer              (if (lre-memb 'personal)
                                          "*ext-discard*"
                                        nil)
      fe-sign                         lre-fe-user
      ffap-file-finder                'find-file-other-frame
      filladapt-mode-line-string      "/FA"
      find-file-existing-other-name   t
      find-file-use-truenames         nil
      find-file-compare-truenames     t
;     fume-display-in-modeline-p      nil
;     generic-define-mswindows-modes  (lre-memb 'win32)
;     generic-define-unix-modes       (lre-memb 'unix)
      goal-column                     nil
      global-visual-line-mode         nil
      grep-highlight-matches          (cond ((lre-memb 'personal)
                                             'auto-detect)
                                            ((lre-memb 'tadntp4)
                                             nil)
                                            (t 'auto-detect))
      grep-tree-files-aliases         '(
                                        ("ch" . "*.[ch]")
                                        ("c" .  "*.c")
                                        ("h" .  "*.h")
                                        ("make" . "[Mm]akefile*")
                                        ("all" . "*")
                                        ("el" . "*.el")
                                        ("sd" . "*.vsd *.svp")
                                        ("java" . "*.java")
                                        )
      grep-files-aliases              grep-tree-files-aliases
      highlight-changes-mode          nil
      highlight-nonselected-windows   nil
      highlight-changes-active-string " h+"
      highlight-changes-passive-string " h-"
      hilit-inhibit-rebinding         (not (lre-memb 'hilit))
      hilit-inhibit-hooks             (not (lre-memb 'hilit))
      hippie-expand-try-functions-list '(
                                         try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-list
                                         try-expand-line
                                         try-complete-sysdul-symbol
                                         try-expand-dabbrev-from-kill
                                         try-expand-whole-kill
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol
                                         try-expand-all-abbrevs
                                         )
      history-length                  50
      history-delete-duplicates       t
      html-helper-mode-uses-JDE       t
      html-helper-mode-uses-bold-italic t
      inhibit-startup-message         t
      inhibit-startup-buffer-menu     t
      imenu-sort-function             'imenu--sort-by-name
      imenu-auto-rescan-maxout        (if (lre-memb 'e21+) 20000
                                        100000)
      imenu-auto-rescan               (lre-memb 'e21+)
      imenu-max-items                 lre-menu-lines
      imenu-generic-skip-comments-and-strings nil
      indent-tabs-mode                nil
;;      initial-frame-alist             (list
;;                                       (if (lre-memb 'personal)
;;                                           (if (lre-memb 'TAD)
;;                                               '(left . 100)
;;                                             '(left . 1))))
      initial-scratch-message         (not (lre-memb 'personal))
      ispell-highlight-p              t
      ispell-look-p                   t
      jit-lock-chunk-size             1000
      jit-lock-stealth-time           5
      js-indent-level                 2
      kill-do-not-save-duplicates     t
      kill-whole-line                 t
      kmacro-call-mouse-event         nil
      lazy-highlight-cleanup          (not (lre-memb 'personal))
      lazy-lock-defer-on-scrolling    nil
      lazy-lock-defer-on-the-fly      nil
      line-number-display-limit-width 400
      lisp-code-directory             (concat (nth 0 lre-lisp) "/LCD-datafile")
      local-use-mark-paren            t
      longlines-show-hard-newlines    t
      lpr-switches                    lre-printer-opt-lp
      lpr-command                     lre-printer-cmd-lp
      lpr-headers-switch              nil
      magic-mode-alist                nil
      mail-archive-file-name          "~/Mail/outbox.VM"
      mail-yank-prefix                "> "
      max-lisp-eval-depth             1000
      max-specpdl-size                2000
      message-log-max                 500
      minibuffer-allow-text-properties nil
      mouse-wheel-follow-mouse        t
      mouse-wheel-progressive-speed   nil
      mouse-wheel-scroll-amount       '(1 ((shift . 3)) ((control)))
      next-line-add-newlines          nil
      nxml-syntax-highlight-flag      (lre-not-memb 'nxml-hack)
      outline-regexp                  nil
      printer-name                    lre-printer-lp
      ps-lpr-command                  lre-ps-print-cmd
      ps-lpr-switches                 lre-ps-print-cmd-opt
      ps-paper-type                   'a4
      ps-font-size                    8
      ps-line-height                  (* (/ ps-font-size 10.0) 11.29)
      ps-avg-char-width               (* (/ ps-font-size 10.0) 5.6)
      ps-space-width                  ps-avg-char-width
      ps-print-color-p                (if (lre-memb 'e21+) 'black-white nil)
      ps-print-header-frame           t
      ps-print-header                 t
      ps-auto-font-detect             (lre-memb-all 'e22+ 'personal nil)
      ps-use-face-background          t
      ps-zebra-stripe-follow          'full
      ps-printer-name                 lre-ps-printer-name
      ps-line-number-step             1
      psgml-balanced-tag-edit         t
      psgml-tag-region-if-active      t
      psgml-indent-step               lre-std-indent
      psgml-indent-data               t
;;      psgml-markup-faces            '((start-tag . font-lock-function-name-face)
;;                                      (end-tag . font-lock-function-name-face)
;;                                      (comment . font-lock-comment-face)
;;                                      (pi . font-lock-type-face)
;;                                      (doctype . bold)
;;                                      (entity . font-lock-constant-face)
;;                                      (shortref . font-lock-constant-face)
;;                                      )
      psgml-auto-insert-required-elements  t
      psgml-insert-missing-element-comment t
      query-replace-highlight         t
      read-file-name-completion-ignore-case t
      read-quoted-char-radix          10
      recentf-max-saved-items         400
      recentf-always-list             lre-vip-files
      recentf-exclude                 (append lre-fignore-2 lre-fignore-re)
      recentf-filename-handler        'lre-file-name
      recentf-menu-path               (if (lre-memb 'e22+) '("file")
                                        '("files"))
      recentf-menu-before             (if (lre-memb 'e22+) '("new-file")
                                        "recover")
      recentf-menu-title              (if (lre-memb 'e22+) "Recent"
                                        "Open Recent")
      recentf-max-menu-items          (* lre-menu-lines 5)
      recentf-max-submenu-items       lre-menu-lines
      recentf-menu-filter             (if (lre-memb 'personal)
                                          'recentf-filter-changer
                                        'recentf-arrange-by-rule)
      recentf-menu-action             'recentf-find-file-other-frame
      recentf-save-file               (expand-file-name
                                       (convert-standard-filename
                                        (if (lre-memb-all 'win32 'tvist)
                                            (lre-fixed :recentf1)
                                          (lre-fixed :recentf2))))
      recentf-arrange-rules           (list
                                       (cons (format "Special (%d)"
                                                     (length lre-vip-files))
                                             (mapcar
                                              (lambda (s)
                                                (if s (regexp-quote s) nil))
                                              lre-vip-files))
                                       '("Elisp (%d)" ".\\.el$" "\\.emacs$")
                                       '("Java (%d)"  ".\\.j\\(a[vd]a\\|sp\\)$")
                                       '("Tmpl (%d)"  ".\\.\\(vpl\\|tpl\\|vcf\\)$")
                                       '("xut (%d)" "\\.xut$")
                                       '("XSL (%d)" "\\.xsl$")
                                       '("wiki (%d)" "\\.wiki$")
                                       '("*ML Schema (%d)" "\\.\\(dtd\\|xsd\\|rng\\|rnc\\|mod\\)$")
                                       '("*ML (%d)" "\\.\\(x\\|sg\\|w\\)ml$" "\\.x?htm\\(l\\)?$" "\\.\\(tld\\)$")
                                       '("Sysdul (%d)" "\\.\\(\\(v\\)?sd\\|svp\\|cf\\)$")
                                       '("C/C++ (%d)" "\\.[ch]\\(pp\\)?$")
                                       '("SQL (%d)" "\\.\\(vsq\\|sql\\|hql\\|inx\\|dba\\)$")
                                       '("Scripts+Make (%d)" "\\<[Mm]ake\\(file\\)?\\>" "\\.[kcv]?sh$")
    )
      recentf-arrange-by-rule-subfilter (if (lre-memb 'personal)
                                            'recentf-show-basenames-ascending
                                          'recentf-show-basenames)
      recentf-arrange-by-rule-others  "Others (%d)"
      replace-lax-whitespace          t
      require-final-newline           'ask
      save-interprogram-paste-before-kill t
      save-place-mode                 t
      save-place-keep-eof             t
      save-place-use-file-menu        (not (lre-memb 'e21+))
      save-place-version-control      nil
      save-place-limit                500
      save-place-recent-no            (if (lre-memb 'e21+) nil
                                        lre-menu-lines)
      save-place-file                 (expand-file-name
                                       (convert-standard-filename
                                        (if (lre-memb-all 'win32 'tvist 'tvust-nix)
                                            "~/_emacs_places"
                                         "~/.emacs-places")))
      save-place-ignore               lre-fignore-re ;; pre-emacs 24
      save-place-ignore-files-regexp  (concat "\\("
                                              (mapconcat 'identity
                                                         lre-fignore-re
                                                         "\\)\\|\\(")
                                              "\\)") ;; emacs 24.1+
      save-place-masters              lre-vip-files
      scroll-margin                   0
      scroll-preserve-screen-position t
      scroll-step                     5
      search-exit-char                1
      search-highlight                t
      semantic-load-turn-everything-on nil
      sh-indentation                  lre-std-indent
      sh-shell-file                   (or (and (lre-memb 'win32)
                                               "/bin/ksh")
                                          (getenv "SHELL")
                                          "/bin/sh")
      sh-imenu-generic-expression
 `((sh . ((nil "^\\s-*function\\s-+\\([A-Za-z_0-9.]+\\)" 1)
    (nil "^\\s-*\\([A-Za-z_][A-Za-z_0-9]*\\s-*()\\)" 1)
    ("*variables*"
     "^\\s-*\\(typeset\\||integer\\|readonly\\)\\s-+\\([A-Za-z_0-9.]+\\)"
     2)
    ("*misc*" "^\\s-*\\(\\(trap\\|exit\\|return\\)\\(\\s-+.*\\)\\)" 1))))
      shell-prompt-pattern            "[^])>]+[])>%] +"
      show-paren-style                'mixed
      show-paren-ring-bell-on-mismatch t
      smooth-scroll-margin            2
      special-display-buffer-names    '("*Compile-Log*" "*Shadows*" "*Faces*")
      speedbar-use-tool-tips-flag     t
      split-height-threshold          12
      sql-user                        (if (lre-memb 'personal) "sa"
                                        user-login-name)
      sql-server                      (or (getenv "DSQUERY") "SYBASE")
      sql-password                    (if (lre-memb 'personal)
                                          (getenv "DEF_SA_PASS"))
      sql-database                    ""
      sql-electric-stuff              nil
      sql-input-ring-file-name        "~/.emacs_sqlhist"
      sql-input-ring-separator        "^\\s-*go\\s-*$"
      sql-product                     'sybase
      sql-sybase-options              '("-w2048")
      sysdul-inhibit-templates        (not (lre-memb 'tpl))
      sysdul-abbrev-lib               nil
      sysdul-local-lib                nil
      sysdul-imenu-levels             (if (lre-memb 'e21+) '(1 3 4)
                                        '(1 2))
      sysdul-he-files                 (if (and (lre-memb-all 'tvist 'e21+ 'unix)
                                               (getenv "SPE_HOME"))
                                          (substitute-in-file-name
                                           "$SPE_HOME/data/repository.el"))
      sysdul-he-modes                 (if (lre-memb-all 'tvist 'e21+ 'unix)
                                          '(sysdul-mode cf-mode vsq-mode
                                            sql-mode sql-interactive-mode)
                                        '(sysdul-mode))
      sysdul-std-indent               lre-std-indent
      sysdul-fill-column              79
      sysdul-continuation-indent      8
      tab-always-indent               'complete
      table-command-prefix            [(control c) (control t)]
      table-html-th-rows              1
      teach-extended-commands-p       t
      tempo-interactive               t
      tool-bar-mode                   (lre-memb 'toolbar)
      tplsub-enable-eval              (if (lre-memb 'tvist) t
                                        'maybe)
      tplsub-file-directory           (list (concat lre-slisp "/tmpl")
                                            (concat (getenv "SPE_HOME")
                                                    "/maler"))
      tplsub-std-indent               lre-std-indent
      tplsub-next-key                 (if (lre-memb 'e21+)
                                          [(control |)])
      tplsub-save-file                (expand-file-name
                                       (convert-standard-filename
                                        (if (lre-memb-all 'win32 'tvist)
                                            "~/_tplsubhist"
                                         "~/.tplsubhist")))
      track-eol                       t
      undelete-frame-mode             t
      uniquify-buffer-name-style      'forward
      vc-command-messages             t
      vc-static-header-alist          '(("\\.[ch]$" .
                                         "\n#if !( defined(lint) || defined(NO_ID) )\n   static char sccsid[]= \"%s\";\n#endif\n")
                                        ("\\.vsd" . "   WSCCSID = \'%s\'\n")
                                        ("\\.svp" . "   WSCCSID = \'%s\'\n"))
      version-control                 nil
      visible-bell                    (not (lre-memb-all 'personal 'gnunix))
      visual-line-fringe-indicators   '(left-curly-arrow right-curly-arrow)
      which-key-mode                  t
      woman-always-color-faces        t
      woman-fill-frame                t
      woman-ignore                    (not (lre-memb 'personal))
      which-func-unknown              "?"
      which-func-format               '("[" which-func-current "]")
      xml-lite-indent-offset          4 ;; lre-std-indent
      xml-lite-indent-comment-offset  (+ 2 lre-std-indent)
      xsl-comment-max-column          75
      xsl-element-indent-step         lre-std-indent
      )
(if (boundp 'completion-styles)
    (add-to-list 'completion-styles 'initials t))
(if (lre-memb 'e22+)
    (setq vc-sccs-header              (list (concat sccs-prefix-string
                                                    " " sccs-suffix-string))
          vc-rcs-header               (list "\$Id\$")
          vc-cvs-header               (list "\$Id\$")
          )
  (setq vc-header-alist               (list
                                       (list
                                        'SCCS (concat sccs-prefix-string
                                                      " " sccs-suffix-string))
                                       (list 'RCS  "\$Id\$")
                                       (list 'CVS  "\$Id\$"))
        isearch-lazy-highlight-cleanup (not (lre-memb 'personal))
        ))

(setq ext-cmd-list
      (list
       (list "ls" "ls" 'source (lre-memb 'gnunix 'personal))
       (list "ls -l" "ls" 'source (lre-memb 'gnunix 'personal) nil nil "-l")
;;       (list "du" "du" 'source (lre-memb 'gnunix 'personal) nil nil "-k")
       (list "Notepad" "notepad.exe" 'editor
             (eq window-system 'w32))
       (list "WinWord" "winword.exe" 'editor
             (eq window-system 'w32)
             (list (list 'path ext-cmd-w32-office-path)))
       (list "Excel" "excel.exe" 'editor
             (eq window-system 'w32)
             (list (list 'path ext-cmd-w32-office-path))
             nil
             "%w")
       (list "IE" "iexplore.exe" 'editor
             (eq window-system 'w32)
             (list (list 'path lre-ie-path)))
       (list "WinZip" "winzip32.exe" 'free
             (eq window-system 'w32)
             (list (list 'path (lre-fixed :winzip))))
       (list "CharMap" "charmap.exe" 'free
             (eq window-system 'w32))
       (list "Calculator" "calc.exe" 'free
             (and (eq window-system 'w32)
                  (not (lre-memb 'personal)))
             nil "Start Windows calculator")
       (list "PowerCalc" "powercalc.exe" 'free
             (and (eq window-system 'w32)
                  (lre-memb 'personal))
             nil "Start Windows Power calculator")
       (list "XML Spy" (lre-fixed :xmlspy) 'editor
             (and (eq window-system 'w32)
                  (lre-memb 'personal)))
       (list "WinCVS" "wincvs.exe" 'editor
             (and (eq window-system 'w32)
                  (lre-memb 'personal))
             (list (list 'path (lre-fixed :wincvs))))
       (list "Explorer" "explorer.exe" 'free
             (eq window-system 'w32)
             nil "Open Explorer window" "/e,/select,%W")
       ))

;; Uttrekk kan lages omtrent slik (pass på å vise flest mulig fonter først
;; (sort (delete-duplicates (mapcar (function (lambda(f) (if
;;    (font-italic-p f) f))) (face-list))) (function (lambda (a b) (cond
;;    ((null a) t) ((null b) nil) (t (string< (face-name a) (face-name b)))))))


(or ps-auto-font-detect
    (setq ps-bold-faces       (append
                               '(font-lock-warning-face
                                 font-lock-builtin-face)
                               (if (lre-memb 'eeeeeeeeeeeeeee21+)
                                   '(Info-title-1-face Info-title-2-face
                                     Info-title-4-face change-log-file-face
                                     custom-face-tag-face custom-group-tag-face
                                     custom-group-tag-face-1
                                     custom-invalid-face
                                     custom-modified-face
                                     custom-variable-button-face
                                     custom-variable-tag-face
                                     diff-file-header-face diff-index-face
                                     diff-nonexistent-face hi-black-b
                                     hi-black-hb hi-blue hi-blue-b hi-green
                                     hi-green-b hi-pink hi-red-b hi-yellow
                                     info-menu-header info-node info-xref
                                     widget-button-face woman-bold-face)
                                 nil)
                               '(font-lock-function-name-face
                                 font-lock-keyword-face
                                 font-lock-type-face
                                 bold
                                 bold-italic))
          ps-italic-faces     (append
                               '(font-lock-constant-face)
                               (if (lre-memb 'eeeeeeeeeeeeeeeeeeeeee21+)
                                   ;; FIXME
                                   '(Info-title-1-face Info-title-3-face
                                     change-log-date-face custom-invalid-face
                                     custom-modified-face custom-rogue-face
                                     info-header-node
                                     info-node widget-inactive-face
                                     woman-addition-face woman-italic-face
                                     woman-italic-face-no-ul)
                                 nil)
                               '(font-lock-function-name-face
                                 font-lock-string-face
                                 font-lock-comment-face
                                 italic
                                 bold-italic))
          ps-underlined-faces (append
                               (if (lre-memb 'eeeeeeeeeeeeeee21+)
                                   ;; FIXME
                                   '(Info-title-1-face custom-saved-face
                                   custom-variable-button-face
                                   speedbar-selected-face woman-italic-face
                                   woman-unknown-face)
                                 nil)
                               '(underline))))


(defun lre-swap-font (pfx)
  "Swap fonts..."
  (interactive "P")
  (if pfx
      (set-frame-font
       "-outline-Courier New-normal-r-normal-normal-13-97-96-96-c-*-iso8859-1")
;;    (set-default-font
;;     "-outline-Bitstream Vera Sans Mono-normal-r-normal-normal-13-97-96-96-c-*-iso8859-1"))
    (set-frame-font "-outline-Consolas-normal-r-normal-normal-14-112-96-96-c-*-iso8859-1")
    ))


;;; main-setq-default
(setq-default save-place                   t
              case-fold-search             t
              default-indicate-empty-lines t
              show-trailing-whitespace     t
              sgml-set-face                t
              indent-tabs-mode             nil)
;; (setq-default enable-multibyte-characters  nil)

(if (fboundp 'temp-buffer-resize-mode)
    (temp-buffer-resize-mode t))

(when (or (lre-memb 'personal) (lre-memb 'e21+))
  (setq imenu-scanning-message nil)
  (if (lre-memb 'lreibm)
      (setq xsl-initial-stylesheet-file (lre-fixed :def-xsl)
            xsl-initial-stylesheet-initial-point 177))
  (or (lre-memb 'win32)
      (lre-not-memb 'e21+)
      (tooltip-mode t)))

;;; -----------------------------------------------------------------------
;;; /// per configuration ///
(if (functionp 'set-scroll-bar-mode)
    (set-scroll-bar-mode 'right))
(and (lre-memb-all 'gnunix 'tvist)
     (lre-safe-require 'mwheel)
     (mouse-wheel-mode t))
(if (lre-memb 'dos)
    (setq change-log-default-name "ChangeLo"))
(if (>= emacs-major-version 20)
    (if (or (> emacs-major-version 20)
            (>= emacs-minor-version 4))
        (setq eol-mnemonic-dos                "\\"
              eol-menmonic-mac                ":"
              eol-mnemonic-undecided          "-"
              eol-mnemonic-unix               "/")
      (setq eol-mnemonic-dos                  ?\\
            eol-menmonic-mac                  ?:
            eol-mnemonic-undecided            ?-
            eol-mnemonic-unix                 ?/)))
(when (lre-memb 'win32)
        ;;    MKS:
        ;; win32-quote-process-args   t
        ;; shell-command-switch       "-c"
  (setq
        backup-by-copying             (not (or (eq LRE-this-cfg 'tadnt)
                                               (eq LRE-this-cfg 'tadntp4)))
;;      browse-url-browser-function   'shell-execute-url ; nt-url-browse-url
        compile-command               '("nmake -f .mak " . 10)
;;      gnuserv-frame                 (selected-frame)
        lpr-add-switches              t
        lpr-page-header-program       (lre-fixed :pr-exe)
        lpr-page-header-switches      '("-l60")
        lpr-spool-file                (lre-fixed :temp)
        shell-execute-helper          (lre-fixed :shell-ex)
        sysdul-tool-version           5
        sysdul-lang                   ?e
        vc-consult-headers            nil
        vc-mistrust-permissions       t
        w32-enable-italics            t
        w32-enable-synthesized-fonts  t
        w32-grab-focus-on-raise       t
        w32-pass-multimedia-buttons-to-system t
        w32-scroll-lock-modifier      nil
        w32-swap-mouse-buttons        (lre-not-memb 'e22+)
        win32-enable-italics          t
        win32-grab-focus-on-raise     t
        win32-swap-mouse-buttons      t
        woman-imenu                   t
        woman-imenu-title             "Index"
        )
  (set-face-background 'highlight "Gold")
  (make-face-bold 'secondary-selection nil t)
  (if (not vc-handled-backends)
      (remove-hook 'find-file-hooks 'vc-find-file-hook)))

;; NB: musmapping (define-key function-key-map [(ctrl mouse-1)] [f1])

(when (lre-memb 'win32 'gnunix)
  (setq frame-title-format '("%b" " - emacs"
                             (server-buffer-clients " server"))
        icon-title-format frame-title-format)
  (defun lre-ps-time-string ()
    (if (and (not (lre-memb 'e23+))
             (fboundp 'time-stamp-yyyy-mm-dd))
        (concat (time-stamp-yyyy-mm-dd) " " (time-stamp-hh:mm:ss))
      (concat (format-time-string "%Y-%m-%d") " "
              (format-time-string "%H:%M:%S"))
    ))
  (setq ps-right-header (list "/pagenumberstring load"
                              'lre-ps-time-string))
  (if (lre-memb 'modeline)
      (if (lre-memb 'e21+)
          (progn
            (setq-default
             mode-line-buffer-identification
             (propertized-buffer-identification "%b"))
            (setq-default
             mode-line-format
             (let* ((help-echo
                     "mouse-1: select (drag to resize), mouse-2: delete others, mouse-3: delete ...")
                    (dashes (propertize "-" 'help-echo help-echo)))
               (cond
                ((lre-memb 'e23+)
                 (list
                  "%e"
                  (propertize "-" 'help-echo help-echo)
                  'mode-line-mule-info
                  'mode-line-client
                  'mode-line-modified
                  'mode-line-remote
                  'mode-line-frame-identification
                  'mode-line-buffer-identification
                  (propertize " " 'help-echo help-echo)
                  'mode-line-position
                  `(vc-mode vc-mode)
                  'mode-line-modes
                  (propertize "%n" 'help-echo "mouse-2: widen"
                              'local-map (make-mode-line-mouse-map
                                          'mouse-2 #'mode-line-widen))
                  `(global-mode-string (,dashes global-mode-string));;'global-mode-string
                  `(which-func-mode ("" which-func-format ,dashes))
                  `(-9 . ,(propertize "%P %I" 'help-echo help-echo))
                  (propertize "-%-" 'help-echo help-echo)))
                ((lre-memb 'e22+)
                 (list
                  (propertize "-" 'help-echo help-echo)
                  'mode-line-mule-info
                  'mode-line-modified
                  'mode-line-buffer-identification
                  (propertize " " 'help-echo help-echo)
                  `(line-number-mode
                    (,(propertize "L%l/" 'help-echo help-echo)))
                  `(column-number-mode
                    (,(propertize "C%c" 'help-echo help-echo)))
                  `(vc-mode vc-mode)
                  'mode-line-modes
                  (propertize "%n" 'help-echo "mouse-2: widen"
                              'local-map (make-mode-line-mouse-map
                                          'mouse-2 #'mode-line-widen))
                  `(global-mode-string (,dashes global-mode-string));;'global-mode-string
                  `(which-func-mode ("" which-func-format ,dashes))
                  `(-9 . ,(propertize "%P %I" 'help-echo help-echo))
                  (propertize "-%-" 'help-echo help-echo)))
                (t
                 (list
                  (propertize "-" 'help-echo help-echo)
                  'mode-line-mule-info
                  'mode-line-modified
                  'mode-line-buffer-identification
                  (propertize " " 'help-echo help-echo)
                  `(line-number-mode
                    (,(propertize "L%l/" 'help-echo help-echo)))
                  `(column-number-mode
                    (,(propertize "C%c" 'help-echo help-echo)))
                  (propertize " %[(" 'help-echo help-echo)
                  "%t:"
                  '(:eval (mode-line-mode-name))
                  'mode-line-process
                  'minor-mode-alist
                  (propertize "%n" 'help-echo "mouse-2: widen"
                              'local-map (make-mode-line-mouse-map
                                          'mouse-2 #'mode-line-widen))
                  (propertize ")%]-" 'help-echo help-echo)
                  'global-mode-string
                  `(which-func-mode ("" which-func-format ,dashes))
                  `(-3 . ,(propertize "%p" 'help-echo help-echo))
                  (propertize "-%-" 'help-echo help-echo))))))
            (setq jde-mode-line-format
                  (mapcar
                   (function
                    (lambda (elt)
                      (if (and (listp elt)
                               (eq (car elt) 'which-func-mode))
                          '(jde-which-method-mode
                            ("-" jde-which-method-format "-"))
                        elt))) mode-line-format)))
        (setq-default mode-line-buffer-identification '("%b")
                      mode-line-format
                      (list ""
                            (if (boundp 'mode-line-mule-info)
                                '("" mode-line-mule-info))
                            'mode-line-modified
                            " "
                            'mode-line-buffer-identification
                            " "
                            'global-mode-string
                            " %[("
                            "%t:"
                            'mode-name
                            'mode-line-process
                            'minor-mode-alist
                            "%n"
                            ")%]-"
                            '(line-number-mode "L%l/")
                            '(column-number-mode "C%c=")
                            '(-3 . "%p")
                            (list 'which-func-mode '("-" which-func-format))
                            "-%-"))
        )))


(when (lre-memb-all 'tvist 'unix)
  (setq ediff-diff-program "gdiff"
        ediff-diff3-program "gdiff3"
        emerge-diff3-program "gdiff3"
        emerge-diff-program "gdiff")
)

;;; -------------------------------------------------------------------------
;;; /// General setup ///
;;;

;; Styr backup til egen katalog
(when (lre-memb 'personal 'win32)
  (setq auto-save-list-file-prefix "~/bup/saves-")
  (if (lre-memb 'e21+)
      (setq backup-directory-alist '(("." . "~/bup")))
    (defun make-backup-file-name (file)
      (concat "~/bup/" (file-name-nondirectory file) "~"))))

(and (lre-memb 'e22+)
     (savehist-mode t))

(defmacro lre-setup-lib (lib &rest setup)
  `(if (lre-not-memb 'e21+)
       (lre-safe-require ,lib)
     (eval-when-compile (lre-safe-require ,lib))
     ,@setup))

(put 'lre-setup-lib 'lisp-indent-function 1)

(defmacro lre-set-local (var value)
  "Macro that converts a variable to a local variable and sets it."
  `(progn
     (make-local-variable (quote ,var))
     (setq ,var ,value)))

(defun lre--double-keyfun (pfx lsymb schar achar &optional run-func)
  "Insert SCHAR, or if used twice in a row, insert ACHAR.
If PFX is-non-nil, always insert SCHAR.
LSYMB is the name of the calling function.
If RUN-FUNC is given - run that function, rather than inserting SCHAR"
  (if (or pfx
          (not (eq last-command lsymb)))
      (insert schar)
    (delete-char -1)
    (insert achar)))

(defun lre--double-keyfunfun (pfx lsymb schar afunc)
  "Run sfunc, or if used twice in a row, run AFUNC.
If PFX is -1, always insert SCHAR.
LSYMB is the name of the calling function."
  (if (or (= pfx -1)
          (not (eq last-command lsymb)))
      (insert schar)
    (delete-char -1)
    (call-interactively afunc)))

(defsubst lre-buffer-name ()
  (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))


(defun lre-read-opt (prompt v-list &optional help-t)
  "Read char from minibuffer until member of list"
  ;;; TODO: hjelp virker ikke
  (let (resp
        (pr prompt)
        (opts (mapconcat 'car v-list ""))
        (ok nil))
    (while (not ok)
      (setq resp (char-to-string
                  (read-char-exclusive (concat pr " [" opts "]: ")))
            pr (concat "Invalid selection! " prompt)
            ok (assoc resp v-list))
      (cond (ok t)
            ((and help-t (string= resp "?"))
             (setq pr prompt)
             (display-message-or-buffer help-t))
            (t (beep))))
    (or (cdr ok) (car ok))))

(defun lre-y-or-n-p (prompt)
  "Ask user a \"y or n\" question.  Return t if answer is \"y\".
Takes one argument, which is the string to display to ask the question.
It should end in a space; `lre-y-or-n-p' adds `(y or n) ' to it.
The user must confirm the answer with RET,
and can edit it until it has been confirmed."
  (let ((full-prompt (concat prompt "(y or n) "))
        (reply-ok nil)
        reply
        retval)
    (while (not reply-ok)
      (setq reply (substring (upcase (read-string full-prompt nil nil " "))
                             0 1)
            reply-ok (cond ((or (string= reply "Y") (string= reply "J"))
                            (setq retval t))
                           ((string= reply "N")
                            (setq retval nil
                                  reply-ok t))
                           (t nil))
            full-prompt (concat prompt "(please answer Y or N!) ")))
    retval))

(defun lre--imenu-add ()
  "Imenu setup for current buffer"
  (if (and (lre-memb-all 'e21+ 'imenu)
           imenu-generic-expression
           lre-imenu-general)
      (pushnew lre-imenu-general imenu-generic-expression))
  (when (lre-memb 'imenu)
    (if (fboundp 'imenu-add-menubar-index)
        (imenu-add-menubar-index)
      (imenu-add-to-menubar "Index"))
    (when (featurep 'which-func)
      (or (eq which-func-modes t)
          (member major-mode which-func-modes)
          (setq which-func-modes (cons major-mode which-func-modes)))
      (if (lre-memb 'e24+)
          (which-function-mode t)
        (which-func-mode t)))))

(when (lre-memb 'personal)
  (put 'upcase-region   'disabled nil)  ; Allow commands
  (put 'downcase-region 'disabled nil)
  (put 'eval-expression 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  ;; (fset 'csh-mode 'indented-text-mode) ; csh-mode is so-so
  (fset 'yes-or-no-p 'lre-y-or-n-p)     ; Y or N + enter
  )

(defsubst lre-toggle-debug ()
  "Toggle debug flag."
  (interactive)
  (lre-toggle-variable 'debug-on-error))

(defsubst lre-toggle-trunc ()
  "Toggle truncate-lines"
  (interactive)
  (lre-toggle-variable 'truncate-lines))

(defun lre-newline-and-indent ()
  "Create new line, insert TAB."
  (interactive)
  (newline)
  (insert "\t"))

(defun lre--setup-ac ()
  "Klargjør for autocomplete"
  (when nil
    (setq ac-auto-start nil)
    (define-key ac-mode-map (kbd "M-.") 'auto-complete))
  (setq ac-auto-start 2
        ac-auto-show-menu 0.8)
  ;; ac-trigger-key (kbd "TAB")
  ;; ac-trigger-commands nil
  (require 'auto-complete)
  (add-to-list 'ac-dictionary-directories
               (concat (nth 0 (delq nil (mapcar
                          (lambda(p)
                            (if (string-match "auto-complete" p) p))
                          load-path))) "/dict"))
  (require 'auto-complete-config)
;;   (add-to-list 'ac-modes 'wiki-mode)
  (ac-config-default))

(defun lre--setup-mc ()
  "Klargjør for multiple-cursors"
  (require 'multiple-cursors)
  (define-key lre-keymap "\\" 'mc/mark-all-like-this-dwim)
  (define-key region-bindings-mode-map (kbd "C-m <")  'mc/mark-sgml-tag-pair)
  (define-key region-bindings-mode-map (kbd "C-m a")  'mc/mark-all-like-this)
  (define-key region-bindings-mode-map (kbd "C-m e")  'mc/edit-lines)
  (define-key region-bindings-mode-map (kbd "C-m in") 'mc/insert-numbers)
  (define-key region-bindings-mode-map (kbd "C-m n")  'mc/mark-next-like-this)
  (define-key region-bindings-mode-map (kbd "C-m p")  'mc/mark-previous-like-this)
  (define-key region-bindings-mode-map (kbd "C-m s")  'mc/sort-regions)
  )


;;; ---------------------------------------------------------------------
;;; /// Autoloads ///

(autoload 'align "align" "Align columns in text" t)
(autoload 'align-regexp "align" "Align regexps in text" t)
(autoload 'apply-operator "radix"
  "Apply OPERATOR, returning in radix RADIX, to NUMBERS." t)
(autoload 'artist-mode "artist" "Enter artist-mode" t)
(when (lre-memb 'win32)
  (autoload 'bill-help-on-topic "bill-g" "Get Windows help" t)
  (autoload 'bill-maximize      "bill-g"                    t)
  (autoload 'bill-minimize      "bill-g"                    t)
  (autoload 'bill-restore       "bill-g"                    t)
  (autoload 'bill-menubar       "bill-g"                    t)
  (autoload 'bill-screensaver   "bill-g"                    t))
(autoload 'bison-mode "bison" "Mode for editing YACC code" t)
(autoload 'box-for "box-for" "Print text in box" t)
(autoload 'brace-mode "braces" "Toggles æøå/{|}-settings" t)
(autoload 'brace-swap-norwegian "brace-mode" "Swaps æøå/{|}-settings" t)
(autoload 'brace-swap-norwegian-rev "brace-mode" "Swaps æøå/{|}-settings" t)
(autoload 'brace-swap-norwegian-html "brace-mode" "Swaps æøå/{|}-settings" t)
(autoload 'brace-swap-norwegian-html-rev "brace-mode" "Swaps æøå/{|}-settings" t)
(autoload 'brace-swap-ml "brace-mode" "Swaps <&>settings" t)
(autoload 'brace-swap-ml-rev "brace-mode" "Swaps <&>settings" t)
(if (lre-memb 'inet)
    (autoload 'browse-url-browser-function "browse-url"
      "Ask a WWW browser to show a URL." t))
(autoload 'cancel-debug-on-condition "dbfrobs" "selective debug" t)
(if (lre-memb 'e21+)
    (autoload 'color-theme-select "color-theme" "Select display colors" t))
(autoload 'clojure-mode "clojure-mode" "Clojure major-mode" t)
(autoload 'comment-out-region "comment" nil t)
(autoload 'csdiff-files "csdiff" "Compare files using csdiff" t)
(autoload 'csdiff-revisions "csdiff" "Compare file revisions using csdiff" t)
(autoload 'csh-mode "csh-mode2" "Major mode for editing csh scripts" t)
(autoload 'css-mode "css-mode")
(autoload 'csv-mode "csv-mode" "Edit CSV-files" t)
(autoload 'debug-on-condition "dbfrobs" "selective debug" t)
(autoload 'debug-on-interesting-errors "dbfrobs" "selective debug" t)
(autoload 'diff-mode "diff-mode" "Diff major mode" t)
(autoload 'diminish "diminish" "Diminish mode-line" t)
(autoload 'dockerfile-mode "dockerfilemode" "Edit Dockerfiles" t)
(unless (commandp 'repeat)
  (autoload 'dot-mode "dot-mode" nil t))
(autoload 'elint-initialize "elint" nil t)
(autoload 'elint-defun "elint" "eLint the function at point" t)
(autoload 'elint-current-buffer "elint" "eLint the current buffer" t)
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
;; (autoload 'fe-mode "fe-mode" nil t)
(autoload 'find-function "find-func" nil t)
(autoload 'find-function-on-key "find-func" nil t)
(autoload 'format-lisp-code-directory "lispdir" nil t)
(autoload 'gfm-mode "markdown-mode" "Mode for editing GitHub markdown files" t)
(when (lre-memb 'inet)
  (autoload 'gnus "gnus" "Read network news." t)
  (autoload 'gnus-post-news "gnuspost" "Post a news." t))
(autoload 'hex-to-string "radix"
  "Convert arg HEX ascii to a one-character string." t)
(autoload 'hexl-find-file "hexl" "Edit file FILENAME in hexl-mode." t)
(autoload 'highline-on "highline"
  "Turn on highlighting of current line locally" t)
(autoload 'highline-mode-on "highline"
  "Turn on highlighting of current line globally" t)
(autoload 'html-helper-mode "html-helper-mode" "HTML major mode." t)
(autoload 'htmlize-buffer "htmlize" "Create HTML from colored buffer" t)
(autoload 'htmlize-file "htmlize" "Create HTML from file" t)
(autoload 'htmlize-many-files "htmlize" "Create HTML from many files" t)
(autoload 'htmlize-region "htmlize" "Create HTML from region" t)
(when (lre-memb 'ispell)
  (autoload 'ispell-word "ispell"  "Check the spelling of word in buffer." t)
  (autoload 'ispell-region "ispell"  "Check the spelling of region." t)
  (autoload 'ispell-buffer "ispell"  "Check the spelling of buffer." t)
  (autoload 'ispell-change-dictionary "ispell"  "Change ispell dictionary." t)
  (autoload 'ispell-message "ispell"
    "Check spelling of mail message or news post.")
  (autoload 'ispell-complete-word "ispell"
    "Look up current word in dictionary and try to complete it." t))
(autoload 'jargon-mode "jargon-mode" "Jargon File major mode." t)
(autoload 'kotlin-mode "kotlin-mode" "Major mode for editing Kotlin files" t)
(autoload 'ksh-mode "ksh-mode" "Major mode for editing ksh scripts" t)
(autoload 'lisp-dir-apropos "lispdir" nil t)
(autoload 'lisp-dir-retrieve "lispdir" nil t)
(autoload 'lisp-dir-verify "lispdir" nil t)
(autoload 'lorem-ipsum-insert-sentences "lorem-ipsum" "Insert lorem ipsum" t)
(autoload 'make-regexp "make-regexp" "Create regexp" t)
(autoload 'make-regexps "make-regexp" "Create regexp" t)
(if (lre-memb 'e24+)
    (autoload 'markdown-mode "markdown-mode" "Mode for editing markdown files" t))
(if (lre-memb-all 'inet 'win32)
    (autoload 'nt-url-browse-url "nt-url"
      "Browse a URL using Windows web browser" t))
(autoload 'number-to-number "radix"
  "Convert NUMBER in radix RADIX1 to string in radix RADIX2." t)
(autoload 'outline-mode "outline-mode" t)
(autoload 'perl-mode "perl-mode" "Major mode for editing perl code" t)
(autoload 'ps-header-dirpart "ps-print")
(autoload 'ps-get-buffer-name "ps-print")
(autoload 'psgml-mode "psgml" "Major mode for editing SGML" t)
(autoload 'pxml-mode "psgml" "Major mode for editing XML" t)
(if (lre-memb 'e21+)
    (autoload 're-builder "re-builder" "Build regexps" t))
(autoload 'register-list "register-list" "Display a list of registers" t)
(autoload 'sc-cite-original "sc" "Supercite 2.3" t)
(autoload 'select-buffer "buffer-select" nil t)
(autoload 'sgml-mode "sgml-mode" "Major mode to edit SGML files" t nil)
(autoload 'spe-sccs-mode "spe-sccs" "SPE SCCS-script interface" t nil)
(autoload 'speedbar "speedbar" "Popup a speedbar frame" t)
(autoload 'speedbar-frame-mode "speedbar" "Popup a speedbar frame" t)
(autoload 'speedbar-get-focus "speedbar" "Jump to speedbar frame" t)
(when (lre-memb 'sql)
  (if (lre-memb 'e21+)
      nil
    (autoload 'sql "sql-mode"
      "Start the interactive SQL interpretor in a new buffer." t)
    (autoload 'sql-mode "sql-mode"
      "Mode for editing SQL files and running a SQL interpretor." t)
    (autoload 'sql-get-going "sql-mode" "Start SQL mode++" t)))
(autoload 'string-to-hex "radix"
  "Convert arg STRING to hexadecimal ascii." t)
(autoload 'sysdul-comment-box "sysdul" "Create boxed comment" t)
(autoload 'sysdul-fix-white   "sysdul" "Remove unnecessary blanks" t)
(autoload 'sysdul-mode "sysdul" "SYSDUL mode" t nil)
(autoload 'tar-mode "tar-mode" t)
(autoload 'time-stamp-yyyy-mm-dd "time-stamp")
(autoload 'time-stamp-hh:mm:ss "time-stamp")
(autoload 'toggle-debug-on-error "dbfrobs" "selective debug" t)
(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(if (lre-memb 'lazy)
    (autoload 'turn-on-lazy-lock "lazy-lock"
      "Unconditionally turn on Lazy Lock mode."))
(autoload 'vbasic-mode "vbasic" "Visual Basic mode" t)
(when (lre-memb 'personal)
  (autoload 'va-mode "va-mode" "SPE/XML-mode" t))
(when (lre-memb 'inet)
  (autoload 'vm "vm" "Start VM on your primary inbox." t)
  (autoload 'vm-visit-folder "vm" "Start VM on an arbitrary folder." t)
  (autoload 'vm-visit-virtual-folder "vm" "Visit a VM virtual folder." t)
  (autoload 'vm-mode "vm" "Run VM major mode on a buffer" t)
  (autoload 'vm-mail "vm" "Send a mail message using VM." t)
  (autoload 'vm-submit-bug-report "vm" "Send a bug report about VM." t))
(autoload 'vsq-mode "vsq-mode" "Mode for editing vsq files" t)
(autoload 'whitespace-cleanup "whitespace" "Cleanup whitespace" t)
(autoload 'whitespace-report "whitespace" "Report whitespace problems" t)
(autoload 'wiki-mode "wiki-mode" "wiki-mode" t nil)
(autoload 'wiki-scratch "wiki-mode" "Create a temp.buffer in wiki mode" t nil)
(autoload 'wml-mode "wml-mode" "wml-mode" t nil)
(autoload 'woman "woman" "Decode and browse a UN*X man page." t)
(autoload 'woman-find-file "woman"
  "Find, decode and browse a specific UN*X man-page file." t)
(autoload 'woman-dired-find-file "woman"
  "In dired, run the WoMan browser on this file." t)
(autoload 'xml-mode "xml-mode" "Major mode for XML files" t)
(autoload 'xml-parse-file "xml")
(autoload 'xslt-process-mode "xslt-process" "Emacs XSLT processing" t)
(autoload 'xsl-mode "xslide" "Major mode for XSL style sheets" t)
(autoload 'zone "zone" "idle display hacks" t)

;; (autoload 'smalltalk-mode "st" "Major mode for editing Smalltalk code" t)
;; (autoload 'sml-mode "sml-mode" "Major mode for editing SML." t)
;; (autoload 'isabelle-mode "sml-mode" "Major mode for editing Isabelle." t)
;; (autoload 'objc-mode "cc-mode" "Objective-C mode" t)


;;; ----------------------------------------------------------------------
;;; /// loads and requires go here ///

(message "loading 1...")
(lre-setup-lib 'apropos)
(lre-setup-lib 'help)                           ; Help is always needed!
(unless (lre-memb 'e21+)
  (lre-safe-require 'overlay-fix))
(when (lre-memb 'ido)
  (lre-setup-lib 'ido)
  (ido-mode 'both))
(lre-safe-require 'thingatpt)           ; Needed by several other functions
(when (lre-memb 'pkg)
  (if (lre-memb 'braces)
      (lre-safe-require 'brace-mode))   ; {|} ÆØÅ
  (if (lre-memb 'tvist)
      (lre-safe-require 'cf-mode)
    (lre-setup-lib 'vsq-mode
      (autoload 'vsq-mode "vsq-mode" "Mode for editing vsq files" t))
    (lre-setup-lib 'cf-mode
      (autoload 'cf-mode "cf-mode" "Extension of C-mode for cf-files" t)
      (autoload 'cf-grapeify-region "cf-mode" "Sysdul->Grape" t)
      (autoload 'cf-ungrapeify-region "cf-mode" "Grape->Sysdul" t)
      ))                        ; grape
  (lre-setup-lib 'commbox
    (autoload 'commbox "commbox" "Comment boxes" t)
    (autoload 'commbox-quick "commbox" "Comment boxes" t)
    )
;;  (lre-safe-require 'complete)        ; do I really want this???
  (lre-safe-require 'filladapt)         ; adaptive fill - must-have
  (lre-setup-lib 'hippie-exp)           ; advanced dabbrevs
  (load "ps-print")                     ; printing PostScript
  (lre-safe-require 'time-stamp)
  (lre-safe-require 'saveplace)         ; save buffer position
  (lre-setup-lib 'speedbar)             ; speedbar
  (if (lre-memb 'tpl)
      (progn
        (lre-safe-require 'tplsub))
    (autoload 'tplsub "tplsub" "Generate template files"  t))
  (when (lre-memb 'e21+)
    (lre-safe-require 'recentf)
    (lre-safe-require 'swbuff)
    (lre-safe-require 'hi-lock)
    (let ((x (assq 'hi-lock-mode minor-mode-alist)))
      (if x (setq minor-mode-alist (delq x minor-mode-alist))))
    (if (functionp 'global-hi-lock-mode)
            (global-hi-lock-mode 1)
          (hi-lock-mode 1))             ; Highlight menu
    (recentf-mode 1))                   ; recent file menu
  (lre-safe-require 'which-func)      ; show function in mode-line
  (if (and (lre-memb 'e21+)
           nil ;;; Not yet!
           (fboundp 'imenu--flatten-index-alist))
      (defadvice which-function (around lre-which-func-fix first
                                        activate compile)
        (let ((imenu--index-alist
               (if imenu--index-alist
                   (imenu--flatten-index-alist imenu--index-alist t))))
          ad-do-it)))
;;      (lre-safe-require 'delsel)      ; typing replaces selection
;;      (delete-selection-mode)
  (if (lre-memb 'imenu)
      (lre-safe-require 'imenu))        ; imenu for GNU emacs
  ;; (if (lre-memb 'fume)
  ;;   (require 'func-menu))            ; and XEmacs
  (lre-safe-require 's-region)          ; allows shift+arrow-key
  (lre-safe-require 'uniquify)          ; better file names
;;  (lre-safe-require 'scroll-in-place) ; keep buffer position when returning
;; TODO  (lre-safe-require 'smooth-scrolling)  ; keep buffer position when returning
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (if (lre-memb 'e24+)
      (add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode))
  (when (lre-memb 'win32)  ; newer emacs on Win32
    (or (lre-memb 'e21+)
        (load "occurmod" t t))          ; ???
;;     (if (lre-memb 'e20+)
;;      (lre-safe-require 'generic-x)
;;       (lre-safe-require 'generic-mode)       ; misc "small modes"
;;       (lre-safe-require 'generic-extras))
    (if (and (lre-memb 'win32)
             (boundp 'generic-mode-list))
        (setq lre-generic-mode-list
              (if (lre-memb 'e21+)
                  (mapcar (function (lambda (e) (intern (car e))))
                          generic-mode-list)
                (mapcar (function (lambda (e) (car e)))
                        generic-mode-alist)))))
  (if (lre-memb 'gnunix)
    (require 'generic-x))
  (if (lre-memb 'tabbar)
      (if (lre-safe-require 'tabbar)
          (tabbar-mode)))
  (if (lre-memb 'e21+)
      (progn
        (defvar whitespace-modes)
        (lre-safe-require 'minibuf-eldef)
        (lre-setup-lib 'whitespace)
        (eval-after-load "whitespace"
          '(progn
             ;; (if (fboundp 'whitespace-global-mode) (whitespace-global-mode 1))
             (or (boundp 'whitespace-modes) (setq whitespace-modes nil))
             (setq dummy-var whitespace-modes)
             (setq whitespace-modes (append '(sysdul-mode cf-mode bat-mode
                                                          vsq-mode
                                                          fe-mode wml-mode)
                                            whitespace-modes)))))
    (autoload 'resize-minibuffer-mode "rsz-mini" nil t)
    (resize-minibuffer-mode)))

(eval-after-load "speedbar"
  '(progn (speedbar-add-supported-extension ".vsd")
          (speedbar-add-supported-extension ".vsq")
          (speedbar-add-supported-extension ".bnf")
          (speedbar-add-supported-extension ".cf")
          (speedbar-add-supported-extension ".htm")
          (speedbar-add-supported-extension ".dtd")
          (speedbar-add-supported-extension ".mod")
          (speedbar-add-supported-extension ".xsd")
          (speedbar-add-supported-extension ".xsl")
          (speedbar-add-supported-extension ".rng")
          (speedbar-add-supported-extension ".xml")
          (speedbar-add-supported-extension ".xut")
          (speedbar-add-supported-extension ".jsp")
          (speedbar-add-supported-extension ".sql")))

(when (lre-memb 'iso)
  (lre-safe-require 'iso-insert)
  (lre-safe-require 'latin-1)
  (lre-safe-require 'disp-table))

(message "loading 2...")
(if (lre-memb 'term)
    (require 'term-setup))
(column-number-mode 1)

(when (lre-memb 'iso)
  (set-input-mode t nil '8bit)
  (or (lre-memb 'e22+) (standard-display-european 1))
  (and (lre-memb 'win32)
       (standard-display-8bit ?\223 ?\224)))

(cond ((not (lre-memb-all 'paren 'pkg))
       t)
      ((lre-memb 'e21+)
       (lre-safe-require 'paren)
       (show-paren-mode t))
      (t (setq paren-message-offscreen t)
         (lre-safe-require 'stig-paren)
         (setq blink-matching-paren nil)
         (when (lre-memb 'keys)
           (global-set-key [?\C-\(] 'stig-paren-toggle-dingaling-mode)
           (global-set-key [?\C-\)] 'stig-paren-toggle-sexp-mode))))

;; Also save registers...

(defun lre--outvar (var)
  "Return a setq statement for variable VAR."
  (let ((hadit (member 'desktop-kill kill-emacs-hook)))
    (require 'desktop)
    (or hadit
        (remove-hook 'kill-emacs-hook 'desktop-kill)))
  (if (boundp var)
      (concat "(setq "
              (symbol-name var)
              " "
              (desktop-value-to-string (symbol-value var))
              ")\n")))

(defun lre-save-registers()
  "Saves values in registers"
  (save-mark-and-excursion
    (set-buffer (get-buffer-create "*saveregs*"))
    (erase-buffer)
    (insert (lre--outvar 'register-alist))
    (if (file-exists-p lre-regsave-file)
        (delete-file lre-regsave-file))
    (write-region (point-min) (point-max) lre-regsave-file nil 'nomessage)))

(defun lre-load-registers()
  "Restores values in registers"
  (load lre-regsave-file t t t))

(when (lre-memb 'personal)
  (lre-load-registers)
  (add-hook 'kill-emacs-hook 'lre-save-registers))

(defun lre--insert-regs(from-char to-char)
  "Sett inn neste register"
  (when (<= from-char to-char)
    (let ((txt (get-register from-char)))
      (when txt
          (princ (format "%c=\t%s\n"
                           from-char (if (stringp txt)
                                         (substring-no-properties txt)
                                       txt)))))
    (lre--insert-regs (1+ from-char) to-char)))

(defun lre-insert-all-regs()
  "Lag tomt buffer med innholdet i alle registre"
  (interactive)
  (with-output-to-temp-buffer "*register-contents*"
    (lre--insert-regs ?a ?z)
    (lre--insert-regs ?A ?Z)
    (lre--insert-regs ?0 ?9)))



;;; ------------------------------------------------------------------
;;; /// File name/interpreter/auto-modes/templates ///

(defun lre-find-mode-for-filename (sym &optional nix)
  (let (mode
        (name (if nix sym (concat "xx." sym)))
        (remote-id nil))
    ;; Remove backup-suffixes from file name.
    (setq name (file-name-sans-versions name))
    (while name
      ;; Find first matching alist entry.
      (setq mode
            (if (memq system-type '(windows-nt cygwin))
                ;; System is case-insensitive.
                (let ((case-fold-search t))
                  (assoc-default name auto-mode-alist
                                 'string-match))
              ;; System is case-sensitive.
              (or
               ;; First match case-sensitively.
               (let ((case-fold-search nil))
                 (assoc-default name auto-mode-alist
                                'string-match))
               ;; Fallback to case-insensitive match.
               (and auto-mode-case-fold
                    (let ((case-fold-search t))
                      (assoc-default name auto-mode-alist
                                     'string-match))))))
      (if (and mode
               (consp mode)
               (cadr mode))
          (setq mode (car mode)
                name (substring name 0 (match-beginning 0)))
        (setq name nil)))
    mode))

(defconst lre-auto-mode-0
  '(
    ("[ef][09][0-9][0-9][0-9][0-9][0-9]\\.txt$" . fe-mode)
     ("\\.m$"    . math-mode)
     ("\\.m$"    . objc-mode)
     ("\\.st$"   . smalltalk-mode)
     ("\\.sim$"  . simula-mode)
     ("\\.mod$"  . modula-2-mode)
     ("\\.def$"  . modula-2-mode)
     ("\\.sml$"  . sml-mode)
     ("\\.ML$"   . isabelle-mode)
     ("\\.h$"    . objective-C-mode)
     ("\\.m$"    . objective-C-mode)
     ("[jJ]argon[0-9.]*\\.\\(txt\\|ascii\\)" . jargon-mode)
    )
  "Not used")
(defconst lre-auto-mode-21
  (if (lre-memb 'e21+)
      '(
        ("\\.vsq\\'" . vsq-mode)
        ("\\.hql\\'" . vsq-mode)
        ("\\.\\([aj]sp\\|phtml\\)\\'" . html-helper-mode)
        )
    "Emacs 21+ only"))
(defconst lre-auto-mode-all
  '(
    ("\\.\\(frm\\|bas\\|cls\\|vbs\\)$" . vbasic-mode)
    ("\\.\\(vsq\\|hql\\)\\'" . vsq-mode)
    ("\\.\\([aj]sp\\|phtml\\)\\'" . html-helper-mode)
    ("\\.clj$" . clojure-mode)
    ("\\.wml$" . wml-mode)
    ("\\.wiki$" . wiki-mode)
    ("\\.\\(xsl\\|fo\\)$" . xsl-mode)
    ("\\.\\(svp\\|[vl]?sd\\|vpl\\)$" . sysdul-mode)
    ("\\.v?cf$" . cf-mode)
    ("\\.txt$" . indented-text-mode)
    ("\\.csh\\(rc\\)?$" . csh-mode)
    ("\\.\\(.*profile\\|ksh\\(rc\\)?\\)$" . ksh-mode)
    ("\\.\\(inx\\|utl\\|dba\\)$" . sql-mode)
    ("\\.ja[dv]a$" . lre-choose-java-mode)
    ("\\.1l$" . nroff-mode)
    ("[Mm]ake\\(file\\).*" . makefile-mode)
    ("\\.htt$" . html-mode)
    ("\\.markdown$" . markdown-mode)
    ("\\.md$" . gfm-mode)
    ("\\.scala$" . lre-scala-mode)
    ("\\.\\(wsf\\|sgml?\\)$" . lre-choose-sgml-mode)
    ("\\.\\(dtd\\|mod\\)$" . lre-choose-dtd-mode)
    ("\\.\\(xml\\|tld\\|xut\\|xsd\\|rng\\|xhtml\\)$" . lre-choose-xml-mode)
    ("\\.g\\(d[imop]\\|rd\\|os\\|ca\\|bi\\|mp\\)x$" . lre-choose-xml-mode)
    ("\\.\\(cmd\\|bat\\)$" . bat-generic-mode)
    ("\\.csv\\'" . csv-mode)
    ("\\.dockerfile\\'" . dockerfile-mode)
    ("\\.kts?\\'" . kotlin-mode)
    )
  "Always")
(setq auto-mode-alist
      (append lre-auto-mode-all auto-mode-alist))
(when (lre-memb 'e21+)
  (setq auto-mode-alist
        (append lre-auto-mode-21 auto-mode-alist)))
(unless (lre-memb 'e22+)
  (delq (rassq 'awk-mode auto-mode-alist)
                auto-mode-alist))

(setq interpreter-mode-alist
      (if (lre-memb 'scripts)
          '(("sh" . sh-mode)
            ("perl" . perl-mode)
            ("ksh" . ksh-mode)
            ("zsh" . sh-mode)
            ("csh" . csh-mode)
            ("bash" . ksh-mode)
            ("perl" . perl-mode)
            ("perl5" . perl-mode)
            ("miniperl" . perl-mode)
            ("wish" . tcl-mode)
            ("wishx" . tcl-mode)
            ("tcl" . tcl-mode)
            ("tclsh" . tcl-mode)
            ("mawk" . awk-mode)
            ("nawk" . awk-mode)
            ("gawk" . awk-mode)
            ("scm" . scheme-mode)
            ("ash" . sh-mode)
            ("bash" . sh-mode)
            ("dtksh" . sh-mode)
            ("es" . sh-mode)
            ("itcsh" . sh-mode)
            ("jsh" . sh-mode)
            ("oash" . sh-mode)
            ("pdksh" . ksh-mode)
            ("rc" . sh-mode)
            ("sh5" . ksh-mode)
            ("tcsh" . csh-mode)
            ("wksh" . sh-mode)
            ("wsh" . sh-mode)
            ("tail" . text-mode)
            ("more" . text-mode)
            ("less" . text-mode)
            ("pg" . text-mode))
        nil))

(eval-after-load "speedbar"
  '(setq speedbar-obj-alist (append speedbar-obj-alist
                                   '(("\\.v?sd" . ".o")
                                     ("\\.vsq"  . ".sql")
                                     ("\\.vsh"  . "")))
        speedbar-ignored-path-expressions
        (append speedbar-ignored-path-expressions
                '("/usr/lpp" "/dev"))))

(when (lre-memb 'tpl)
  (defsubst lre--reg-xml (sym)
    "Register XML-variant"
    (tplsub-register-mode sym tplsub-Xml-tmpl-list  tplsub-Xml-help-list
                          'default 'default "" 'default))
  (tplsub-reg-fast 'makefile-mode tplsub-makefile-list)
  (tplsub-reg-fast 'ksh-mode    tplsub-ksh-list)
  (tplsub-reg-fast 'sh-mode     tplsub-ksh-list)
  (tplsub-reg-fast 'wml-mode    tplsub-wml-tmpl-list  tplsub-wml-help-list)
  (lre--reg-xml 'html-mode)
  (lre--reg-xml 'mhtml-mode)
  (lre--reg-xml 'xml-mode)
  (lre--reg-xml 'nxml-mode)
  (lre--reg-xml 'pxml-mode)
  (lre--reg-xml 'sgml-mode)
  (lre--reg-xml 'psgml-mode)
  (lre--reg-xml 'xsl-mode)
  (tplsub-reg-fast 'sql-mode    tplsub-sql-tmpl-list)
  (tplsub-register-mode 'cf-mode
                        tplsub-sysdul-tmpl-list
                        tplsub-sysdul-help-list
                        'default                ; template regexp
                        'default                ; case sens.
                        " : @@\\"               ; cont string
                        'default                ; expansion key
                        ))

;;; ------------------------------------------------------------------
;;; /// Key maps/menus/toolbars ///

(message "Keymaps...")

(defvar lre-keymap nil
  "Personal keymap")

(defvar lre-tvist-key (if (lre-memb 'tvist) [f12] nil)
  "* Tast som inneholder spesialbindinger.")

(defvar lre-tvist-key-description
  '("? - Beskriv bindinger")
  "Beskrivelse av lokale tastebindinger.")

(defvar lre-tvist-keymap (make-sparse-keymap)
  "Spesielle TVIST tastebindinger.
Gi kommando \"\\[lre-tvist-key-description]\" for beskrivelse")

(defvar lre-tvist-sysdul-submap (make-sparse-keymap)
  "Tilleggsdefinisjoner i `sysdul-mode'.")

(defvar lre-tvist-first-sysdul t)
(defvar lre-tvist-first-cf t)

(when (lre-memb 'tvist)
  (if lre-tvist-key
      (define-key global-map lre-tvist-key lre-tvist-keymap))
  (define-key lre-tvist-keymap "?" 'lre-tvist-key-description)

  (defun lre-tvist-key-description ()
    "Beskriv spesielle TVIST tastaturbindinger."
    (interactive)
    (setq lre-tvist-key-description (sort lre-tvist-key-description 'string<))
    (with-output-to-temp-buffer "*TVISTATUR*"
      (princ "Spesielle TVIST tastebindinger.")
      (terpri)
      (princ "\(Disse ligger under en egen tast, default F12\).")
      (terpri)
      (let ((str-list lre-tvist-key-description))
        (while str-list
          (princ (car str-list))
          (terpri)
          (setq str-list (cdr str-list)))))))

;; (global-set-key [apps] 'find-file)

;; /// lre-main-menu-base ///

(defmacro lre--men-reg ()
  `(and mark-active (not buffer-read-only)))

(defvar lre-main-menu-base
  '("lre"
    [ "Printer setup" lre-print-setup (lre-memb 'e21+)
      :help "Tilpass printeroppsett" ]
    [ "Print" lre-print (lre-memb 'e21+)
      :help "Skriv ut ihht oppsett" ]
    "---"
    ("Edit"
     [ "Repeat" repeat t ]
     "---"
     [ "Toggle wrapping (trunc)" lre-toggle-trunc t
       :help "Toggle line wrapping" ]
     [ "Automatic hor. scrolling" hscroll-mode t
       :help "Toggle automatic scrolling" ]
     [ "Next long line" lre-next-long-line t :help "Goto next long line" ]
     [ "Soft wrap long lines" (if (fboundp 'visual-line-mode) visual-line-mode
                                longlines-mode) (lre-memb 'e22+)
       :help "Soft wrapping of long lines" ]
     "---"
     [ "Conv NOR 8-7" brace-swap-norwegian (not buffer-read-only)
       :help "Convert 8-bit to 7-bit norwegian" ]
     [ "Conv NOR 7-8" brace-swap-norwegian-rev (not buffer-read-only)
       :help "Convert 7-bit to 8-bit norwegian" ]
     [ "Conv NOR to HTML" brace-swap-norwegian-html (not buffer-read-only)
       :help "Convert norwegian to HTML ent" ]
     [ "Conv HTML to NOR" brace-swap-norwegian-html-rev (not buffer-read-only)
       :help "Convert norwegian from HTML ent" ]
     [ "Conv to *ML" brace-swap-ml (not buffer-read-only)
       :help "Convert standard entities TO *ML" ]
     [ "Conv from *ML" brace-swap-ml-rev (not buffer-read-only)
       :help "Convert standard entities FROM *ML" ]
     [ "Conv 7-8*ML" lre-script-conv (not buffer-read-only)
       :help "7-8 + *ML + name" ]
     "---"
     [ "Goto line" goto-line t ]
     [ "Goto percent" lre-goto-percent t ]
     [ "Resize interactively" lre-resize-window (not (one-window-p)) ]
     [ "Which function?" lre-which-function t ]
     "---"
     [ "Sort lines" sort-lines (lre--men-reg) ]
     [ "Sort case-insensitive" lre-sort-lines-nocase (not buffer-read-only) ]
     [ "Copy line to other win" lre-copy-line-other-window t]
     [ "Delete duplicate lines" delete-duplicate-lines (not buffer-read-only) ]
     [ "Duplicate line" duplicate-line (not buffer-read-only) ]
     [ "Kill this line" lre-kill-this-line (not buffer-read-only) ]
     [ "Replace selection with kill" lre-kill-yank (lre--men-reg) ]
     )
    ("Find/replace"
     [ "List matching" list-matching-lines t
       :help "List lines matching regexp" ]
     [ "Delete matching" delete-matching-lines (not buffer-read-only)
       :help "Delete lines matching regexp" ]
     [ "Q/R regexp" query-replace-regexp (not buffer-read-only)
       :help "Query/replace" ]
     "---"
     [ "Grep" grep t ]
     [ "Find+grep" grep-find (lre-memb 'e22+) ]
     [ "Treegrep" grep-tree (lre-memb 'e22+) ]
     "---"
     [ "Multibuffer isearch regexp" multi-isearch-buffers-regexp
       (lre-memb 'e23+) ]
     )
    ("Fonts"
     [ "Highlight regexp" highlight-regexp (functionp 'highlight-regexp) ]
     [ "Highlight current line" highline-on t ]
     ("Highlight changes"
      [ "Highlight changes" highlight-changes-mode t ]
      [ "Remove highlights" highlight-changes-remove-highlight
        (and highlight-changes-mode mark-active) ]
      [ "Rotate faces" highlight-changes-rotate-faces highlight-changes-mode ]
      [ "Next change" highlight-changes-next-change highlight-changes-mode ]
      [ "Previous change" highlight-changes-previous-change
        highlight-changes-mode ]
      [ "Compare with file" highlight-compare-with-file t ]
      )
     "---"
     [ "Fontify" font-lock-fontify-buffer (lre-memb 'flock)
       :help "Refontify buffer" ]
     [ "Refontify" font-lock-fontify-block (lre-memb-all 'flock 'e22+)
       :help "Refontify block" ]
     [ "Remove lazy isearch" isearch-lazy-highlight-cleanup
       (and (fboundp 'isearch-lazy-highlight-cleanup)
            lazy-highlight-cleanup)
       :help "Remove highlight from last search" ]
     [ "Swap fonts" lre-swap-font (lre-memb-all 'e22+ 'win32 'personal)
       :help "Switch between Courier and Vera" ]
     "---"
     [ "Htmlize" lre-htmlize-buffer t
       :help "HTML with colors in separate buffer" ]
     [ "Htmlize region" htmlize-region mark-active ]
     [ "Htmlfontify buffer" htmlfontify-buffer (lre-memb 'e23+) ]
     )
    ("Space + align"
     [ "Trim blanks" lre-trim (not buffer-read-only)
       :help "Remove unnecessary whitespace" ]
     [ "Untabify" lre-untabify (lre--men-reg) :help "Expand tabs" ]
     [ "Tabify" tabify (lre--men-reg) :help "Replace spaces with tabs" ]
     [ "Check whitespace" Report-whitespace (lre-memb 'e21+)
       :help "Check for WS errors" ]
     [ "Fix whitespace" whitespace-cleanup (and (lre-memb 'e21+)
                                                (not buffer-read-only))
       :help "Correct WS errors" ]
     [ "Whitespace mode" whitespace-mode (lre-memb 'e23+)
       :help "Show whitespace in buffer" ]
     "---"
     [ "Realign delimiters (mode)" lre-realign-delim (lre--men-reg)
       :help "Realign cont. separators" ]
     [ "Realign delimiters (general)" lre-realign-sep-in-region (lre--men-reg)
       :help "Realign cont. separators" ]
     [ "Align regexp" align-regexp (lre--men-reg)
       :help "Align characters" ]
     [ "Align" align (and mark-active (not buffer-read-only)) ]
     [ "Indent rigidly" indent-code-rigidly (lre--men-reg)
       :help "Indent all lines" ]
     [ "Realign last word" lre-adjust-separator (not buffer-read-only)
       :help "Indent last separator (beta)" ]
     "---"
     [ "Max line length" lre-max-length-in-region
       mark-active :help "Find max line length in region" ]
     [ "Enable tables" lre-enable-table (lre-memb 'e22+)
       :help "Enable table handling in text files" ]
     )
    ("Insert"
     [ "Date" lre-todays-date (not buffer-read-only) ]
     [ "Signature" lre-insert-sign (not buffer-read-only) ]
     [ "Line numbers" lre-number-lines (not buffer-read-only) ]
     "---"
     [ "History rec" lre-insert-hist-rec (not buffer-read-only) ]
     [ "History in place" lre-history-insert (not buffer-read-only) ]
     [ "Goto history" lre-history-point t ]
     [ "Version" lre-wversion (not buffer-read-only) ]
     "---"
     ("Comments"
      [ "Comment box 1" commbox  (not buffer-read-only) ]
      [ "Comment box 2" comment-box  (lre--men-reg) ]
      [ "Comment DWIM" comment-dwim  (not buffer-read-only)
        :help "Comment Do What I Mean!" ]
      [ "Comment region" comment-region  (lre--men-reg) ]
      [ "Comment out region" comment-out-region  (lre--men-reg) ]
      )
     "---"
     [ "Code doc (experimental)" lre-insert-code-doc (not buffer-read-only) ]
     )
    ("BufWinFrameFile..."
     [ "Open file at point" find-file-at-point t ]
     [ "Create from template" lre-subst-tmpl t]
     "---"
     [ "Rename buffer file" lre-rename-current-buffer-file (buffer-file-name) ]
     [ "Delete buffer file" lre-delete-current-buffer-file (buffer-file-name) ]
     "---"
     [ "Kill buffer" lre-kill-save-buf t ]
     [ "Kill buf + win" lre-kill-buff-win t ]
     [ "Kill buf + frame" lre-kill-buf-frame t ]
     "---"
     [ "Ask to kill all" lre-kill-save-all t ]
     [ "Kill all other buffers" lre-kill-others t ]
     [ "Kill other non-file windows" lre-kill-some-windows t]
     "---"
     [ "Toggle read only" toggle-read-only t ]
     [ "Toggle brace mode" brace-mode (lre-memb 'braces) ]
     "---"
     [ "Balance windows" balance-windows-area (fboundp 'balance-windows-area) ]
     [ "Bury buffer" bury-buffer t ]
     [ "Show in new frame" lre-switch-to-new-frame t ]
     "---"
     [ "Hexedit" hexl-find-file t ]
     [ "Wiki scratchpad" wiki-scratch t ]
     [ "View other win" view-file-other-window t ]
     [ "Switch other win" switch-to-buffer-other-window t ]
     [ "Open w/o conversion" find-file-literally t ]
     [ "Iswitch buffer" iswitchb-buffer t ]
     "---"
     [ "Chmod" chmod (fboundp 'chmod) ]
     [ "Recover file" recover-file t ]
     [ "Reload current" lre-alternate-file (not (buffer-modified-p)) ]
     )
    ("Tools"
     [ "Timings" lre-show-timers (lre-memb 'e23+) ]
     [ "Toggle SpeedBar" speedbar t ]
     [ "SQL" lre-sql t ]
     [ "Template subst" tplsub (not buffer-read-only) ]
     [ "Template descriptions" tplsub-describe-templates t ]
     "---"
     [ "Grep" grep t ]
     [ "Find+grep" grep-find (lre-memb 'e22+) ]
     [ "Treegrep" grep-tree (lre-memb 'e22+) ]
     "---"
     [ "Display macro" lre-disp-macro t ]
     [ "Compile" lre-compile t ]
     [ "RegExp builder" re-builder (commandp 're-builder)  ]
     "---"
     [ "Check parentheses" lre-check-parentheses t ]
     [ "Show char class" lre-char-class t]
     [ "Show env.var." printenv t ]
     [ "Ascii chart" lre-ascii-chart t ]
     [ "Show clipboard" lre-temp-clip t ]
     [ "Show directory info" lre-disp-dir-info t ]
     [ "Show register contents" lre-insert-all-regs t ]
     "---"
     [ "Exec kbd-macro" call-last-kbd-macro t ]
     [ "Edit kbd-macro" edit-kbd-macro t ]
     [ "Edit last kbd-macro" edit-last-kbd-macro t ]
     [ "Insert kbd-macro" insert-kbd-macro t ]
     "---"
     [ "Make CVS script" lre-make-cvs-script t ]
     )
    ("Printing"
     [ "Buffer with faces" ps-print-buffer-with-faces t ]
     [ "Buffer with faces - spool" ps-spool-buffer-with-faces t ]
     [ "Buffer with faces - 2up" lre-print-2up t ]
     [ "Region with faces" ps-print-region-with-faces t ]
     [ "Plain" lpr-buffer t ]
     [ "Grape p1 / buffer" lre-print-greip t ]
     [ "Print" lre-print (lre-memb 'e21+)
       :help "Skriv ut ihht oppsett" ]
     "---"
     [ "Fix PS-file" lre-fix-ps (not buffer-read-only) ]
     )
    "---"
    "MODES"
    ("Change"
     [ "Sysdul"   sysdul-mode     t ]
     [ "C"        c-mode          t ]
     [ "cf"       cf-mode         t ]
     [ "text"     text-mode       t ]
     [ "SQL"      sql-mode        t ]
     [ "vsq"      vsq-mode        t ]
     [ "html"     html-mode       t ]
     [ "html-helper" html-helper-mode t ]
     [ "sgml"     sgml-mode       t ]
     [ "xml"      xml-mode        t ]
     [ "nxml"     nxml-mode       (lre-memb 'nxml) ]
     [ "psgml"    psgml-mode      (lre-memb 'psgml) ]
     [ "pxml"     pxml-mode       (lre-memb 'psgml) ]
     [ "wml"      wml-mode        t ]
     [ "lisp"     lisp-mode       t ]
     [ "elisp"    emacs-lisp-mode t ]
     [ "makefile" makefile-mode   t ]
     [ "ksh"      ksh-mode        t ]
     [ "csh"      csh-mode        t ]
     [ "diff"     diff-mode       t ]
      )
    ("*ml"
     [ "Create A from URL" lre-html-dup-url (not buffer-read-only) ]
     [ "Insert tag" lre-sgml-tag  (not buffer-read-only) ]
     [ "Insert ent.ref." lre-sgml-string  (not buffer-read-only) ]
     [ "Insert marked section" lre-sgml-marked (not buffer-read-only) ]
     [ "Insert nbsp" lre-sgml-space (not buffer-read-only) ]
     [ "Insert comment" lre-sgml-comment (not buffer-read-only) ]
     [ "Insert table" lre-html-ins-table (not buffer-read-only) ]
     [ "html-helper-mode" html-helper-mode t ]
     [ "XUT: Convert to AWK function" lre-xut-awkfunc (and (featurep 'lre-doc)
                                                      (not buffer-read-only)) ]
     [ "XUT: use global" lre-xut-globuse (and (featurep 'lre-doc)
                                              (not buffer-read-only)) ]
     )
    ("Sysdul/Grape"
     [ "Mark procedure" mark-sysdul-subprogram (featurep 'sysdul) ]
     [ "Grapeify region" cf-grapeify-region (lre--men-reg) ]
     [ "Ungrapeify region" cf-ungrapeify-region (lre--men-reg) ]
     [ "Procify..." lre-grape-procify (not buffer-read-only) ]
     [ "Hack DDF..." lre-hack-ddf t ]
     "---"
      ("Adm"
       ["Prepare checkin" lre-grape-ready-file t]
       ["Remove part 2" lre-grape-kill-part-2 t]
       ["Remove part 3" lre-grape-kill-part-3 t]
       ["Remove part 4" lre-grape-kill-part-4 t]
       "---"
       ["Open non-diff" lre-grape-open-non-diff t]
       ["Non-diff new frame" lre-grape-open-non-diff-new-frame t]
       "---"
       ["Update event-handler" lre-grape-update-events t]
       ["Update main part 2" lre-grape-update-main t]
       )
      "---"
      [ "Trash buffer - raw Sysdul..." lre-raw-sysdul t ]
      )
    ("Java"
     [ "PMD buffer" pmd-current-buffer t ]
     [ "PMD directory" pmd-current-directory t ])
    ("Lisp"
     [ "Open library" lre-open-library t ]
     [ "List libraries" lre-list-libs t ]
     [ "Search for defun" lre-search-defun t ]
     [ "Goto definition" elisp-slime-nav-find-elisp-thing-at-point t ]
     "---"
     [ "Byte compile file" lre-byte-compile-file t ]
     [ "Recompile directory" byte-recompile-directory t ]
     "---"
     [ "Toggle debug" lre-toggle-debug t ]
     [ "Toggle variable" lre-toggle-variable t ]
     "---"
     [ "Lint buffer" elint-current-buffer t ]
     [ "Check documentation" checkdoc t ]
     "---"
     [ "Clojure" lre-run-clojure t ]
     )
    )
  "My own menu...")

(when (lre-memb-all 'keys 'toolbar)
  (defun lre-toolbar()
    "Define toolbar"
    (setq tool-bar-map nil)
    (setq tool-bar-map (make-sparse-keymap))
    (tool-bar-add-item (if (find-image
                            (list
                             (list :type 'pbm :file "left-arrow.pbm")))
                           "left-arrow" "left_arrow")
                       (lambda ()
                                      (interactive)
                                      (popup-menu
                                       (lookup-key global-map
                                                   (vector
                                                    'menu-bar
                                                    (intern
                                                     (car recentf-menu-path))
                                                    (intern
                                                     recentf-menu-title)))))
                       'open-recent
                       :help "Open recent files menu")
    (tool-bar-add-item-from-menu (if (commandp 'menu-find-file-existing)
                                     'menu-find-file-existing
                                   (if (commandp 'find-file-existing)
                                       'find-file-existing
                                     'find-file)) "open")
    (tool-bar-add-item-from-menu 'save-buffer "save" nil
                                 :visible '(or buffer-file-name
                                               (not (eq 'special
                                                        (get major-mode
                                                             'mode-class)))))
;;    (if (lre-memb 'personal)
;;        (tool-bar-add-item  "contact" 'wiki-scratch 'wiki-scratch))
    (tool-bar-add-item  "saveas" 'write-file 'write-file
                        :visible '(or buffer-file-name
                                      (not (eq 'special
                                               (get major-mode
                                                    'mode-class)))))
    (tool-bar-add-item "close" 'lre-kill-save-buf 'lre-kill-save-buf
                       :help "Kill buffer after saving changes")
;;    (tool-bar-add-item-from-menu 'write-file "saveas" nil
;;                               :visible '(or buffer-file-name
;;                                             (not (eq 'special
;;                                                      (get major-mode
;;                                                           'mode-class)))))
    (tool-bar-add-item-from-menu 'undo "undo" nil
                                 :visible '(not (eq 'special (get major-mode
                                                                  'mode-class))))
    (tool-bar-add-item "cut" 'kill-region  'kill-region
                       :help "Cut"
                       :visible '(not (eq 'special (get major-mode
                                                        'mode-class))))
    (tool-bar-add-item "copy" 'kill-ring-save 'menu-bar-kill-ring-save
                       :help "Copy")
    (tool-bar-add-item "paste" 'yank  'yank
                       :help "Paste"
                       :visible '(not (eq 'special (get major-mode
                                                        'mode-class))))
    (tool-bar-add-item-from-menu 'nonincremental-search-forward "search")
    (tool-bar-add-item "print" 'lre-print 'lre-print
                       :help "Print according to taste")
;;    (when (lre-memb 'toolbar 'welcome)
;;      (tool-bar-add-item "jump_to" 'lre-welcome 'lre-welcome))
;;     (tool-bar-add-item "preferences" 'customize 'customize
;;                     :help "Edit preferences (customize)")
;;    (tool-bar-add-item "cancel" 'tool-bar-mode 'tool-bar-mode
;;                     :help "Turn off tool bar")
    (unless (lre-memb 'personal)
      (tool-bar-add-item "help" (lambda ()
                                  (interactive)
                                  (popup-menu menu-bar-help-menu))
                         'help
                         :help "Pop up the Help menu"))
    (tool-bar-add-item-from-menu 'tool-bar-mode "cancel")
    (tool-bar-add-item-from-menu 'save-buffers-kill-emacs "exit" nil
                                 :help "Exit Emacs after saving changes"))
  (lre-toolbar)
  (define-key-after menu-bar-tools-menu [tool-bar]
    '("Display Toolbar" . tool-bar-mode) 'speedbar )
  (if (lre-memb 'ruler)
      (define-key-after menu-bar-tools-menu [ruler-bar]
        '("Display Ruler" . ruler-mode) 'tool-bar))
  )


(when (lre-memb-all 'mouse 'keys)
    (setq mouse-sel-default-bindings 'interprogram-cut-paste)
    (lre-safe-require 'mouse-sel)
    (transient-mark-mode 1)
    (setq mouse-yank-at-point t)
    (global-set-key [?\e mouse-1] 'lre-occ-at-point)
    (global-set-key [?\e C-mouse-1] 'find-file-at-point)
    (global-set-key [C-mouse-2]   'lre-mouse-open)
;;  (global-set-key [mouse-2] 'lre-mouse-click-ins)
    (global-set-key [S-mouse-2] 'mouse-insert-selection))

(when (lre-memb 'keymap)
  (setq lre-keymap (make-sparse-keymap))
  (let ((k-bind (key-binding "\C-z")))
    (global-unset-key "\C-z")
    (global-set-key   "\C-z" lre-keymap)
    (define-key lre-keymap "\C-z" k-bind))
  (if (fboundp 'easy-menu-define)
      (easy-menu-define lre-main-menu global-map "LRE menu"
                        lre-main-menu-base))
  (if (fboundp 'kill-matching-buffers)
      (define-key lre-keymap [C-backspace] 'kill-matching-buffers))
  (define-key lre-keymap [backspace] 'delete-matching-lines)
  (define-key lre-keymap " "      'lre-trim)
  (define-key lre-keymap ":"      'lre-realign-delim)
  (define-key lre-keymap "="      'lre-which-function)
  (define-key lre-keymap "+"      'er/expand-region)
  (define-key lre-keymap "-"      nil) ;; Prefiks, se lre--setup-mc
  (define-key lre-keymap "*"      'lre-insert-code-doc)
  (define-key lre-keymap "%"      'multi-isearch-buffers-regexp)
  (define-key lre-keymap "<"      'balance-windows-area)
  (define-key lre-keymap "("      'lre-char-class)
  (define-key lre-keymap "?"      'list-matching-lines)
  (define-key lre-keymap "0"      'lre-kill-some-windows)
  (define-key lre-keymap "2p"     'lre-print-2up)
  (define-key lre-keymap "C"      'c-mode)
  (define-key lre-keymap "D"      'lre-toggle-debug)
  (if (fboundp 'grep-tree) (define-key
              lre-keymap "G"      'grep-tree))
  (define-key lre-keymap "J"      'lre-join-line-inverse)
  (define-key lre-keymap "P"      'ps-print-region-with-faces)
  (define-key lre-keymap "Q"      'lre-sql)
  (define-key lre-keymap "S"      'sysdul-mode)
  (define-key lre-keymap "U"      'delete-duplicate-lines)
  (define-key lre-keymap "\t"     'indent-code-rigidly)
  (define-key lre-keymap "a"      'align-regexp)
  (define-key lre-keymap "b"      'commbox-quick)
  (define-key lre-keymap "c"      'commbox)
  (define-key lre-keymap "d"      'lre-todays-date)
  (define-key lre-keymap "f"      'find-file-at-point)
  (define-key lre-keymap "g"      'grep)
  (define-key lre-keymap "i"      'indented-text-mode)
  (define-key lre-keymap "j"      'join-line)
  (define-key lre-keymap "k"      'lre-kill-buff-win)
  (define-key lre-keymap "m"      'tplsub)
  (define-key lre-keymap "o"      'lre-copy-line-other-window)
  (define-key lre-keymap "p"      'ps-print-buffer-with-faces)
  (define-key lre-keymap "q"      'query-replace-regexp)
  (define-key lre-keymap "r"      'toggle-read-only)
  (define-key lre-keymap "s"      'lre-insert-sign)
  (define-key lre-keymap "u"      'lre-untabify)
  (define-key lre-keymap "w"      'wiki-scratch)
  (define-key lre-keymap "\C-a"   'lre-copy-from-above)
  (define-key lre-keymap "\C-b"   'bury-buffer)
  (define-key lre-keymap "\C-c"   'comment-out-region)
  (define-key lre-keymap "\C-d"   'duplicate-line)
  (define-key lre-keymap "\C-k"   'kmacro-keymap)
  (define-key lre-keymap "\C-m"   'lre-disp-macro)
  (define-key lre-keymap "\C-n"   'lre-subst-tmpl)
  (define-key lre-keymap "\C-o"   'lre-open-library)
  (define-key lre-keymap "\C-p"   'lpr-buffer)
  (define-key lre-keymap "\C-r"   'rename-uniquely)
  (define-key lre-keymap "\C-s"   'sort-lines)
  (define-key lre-keymap "\C-t"   'tplsub-describe-templates)
  (define-key lre-keymap "\C-x"   'hexl-find-file)
  (define-key lre-keymap "\M-p"   'ps-spool-buffer-with-faces)
  (define-key lre-keymap "\M-d"   'lre-kill-this-line)
  (if (fboundp 'multi-occur)
      (define-key lre-keymap [?\C-+] 'multi-occur)
    (if (fboundp 'multi-occur-by-filename-regexp)
        (define-key lre-keymap [?\C-+] 'multi-occur-by-filename-regexp)))
  (if (lre-memb 'flock)
      (define-key lre-keymap "\C-l" 'font-lock-fontify-buffer))
  (when (lre-memb 'win32)
    (define-key lre-keymap "\C-f" 'find-file)  ; if needed...
    (define-key ctl-x-map  "\C-f" 'lre-find-file)
    (define-key ctl-x-map  "/"    'edit-kbd-macro)))

(when (lre-memb 'keys)
  (lre-safe-require 'region-bindings-mode)
  (if (fboundp 'region-bindings-mode-enable)
      (region-bindings-mode-enable))
  (when (lre-memb 'e21+)
    (define-key lre-keymap "h" 'highlight-regexp)
    (lre-safe-require 'ext-cmd)
    (ext-global-menu))
  (or (lre-memb 'hilit)
      (lre-memb 'e23+)
      (define-key global-map [C-l] 'recenter))
  (global-set-key [?\C-<]       'lre-indent-or-expand)
  (global-set-key "\C-a"        'mark-whole-buffer)
  (global-set-key [f4]          'call-last-kbd-macro)
  (global-set-key [M-f5]        'imenu)
  (global-set-key [f6]          'other-window)
  (global-set-key [f9]          'lre-next-long-line)
  (global-set-key [S-f9]        'lre-adjust-separator)
  (global-set-key [S-insert]    'lre-kill-yank)
  (global-set-key [print]       'lre-print)
  (global-set-key (kbd "<C-S-down>") 'lre-move-line-down)
  (global-set-key (kbd "<C-S-up>") 'lre-move-line-up)

  (when (lre-memb 'e22+)
    (global-set-key "\C-z\C-k" 'kmacro-keymap)
    (global-set-key [S-f4] 'kmacro-start-macro-or-insert-counter)
    (global-set-key [f4] 'kmacro-end-or-call-macro))

  (when (lre-memb-all 'eieio 'e21+)
    (lre-setup-lib 'linemark
      (autoload 'viss-bookmark-toggle "linemark"
        "Toggle a bookmark on the current line." t)
      (autoload 'viss-bookmark-next-buffer "linemark"
        "Move to the next bookmark in this buffer." t)
      (autoload 'viss-bookmark-prev-buffer "linemark"
        "Move to the next bookmark in this buffer." t)
      (autoload 'viss-bookmark-clear-all-buffer "linemark"
        "Clear all bookmarks in this buffer." t))
    (define-key global-map [(f2)] 'viss-bookmark-toggle)
    (define-key global-map [(shift f2)] 'viss-bookmark-prev-buffer)
    (define-key global-map [(control f2)] 'viss-bookmark-next-buffer)
    (define-key global-map [(control shift f2)] 'viss-bookmark-clear-all-buffer)
    (define-key-after menu-bar-bookmark-map [speed-sep] '(menu-item "---"))
    (define-key-after menu-bar-bookmark-map [toggle]
        '(menu-item "Toggle speedmark"  viss-bookmark-toggle))
    (define-key-after menu-bar-bookmark-map [prev]
        '(menu-item "Prev. speedmark" viss-bookmark-prev-buffer))
    (define-key-after menu-bar-bookmark-map [next]
        '(menu-item "Next speedmark" viss-bookmark-next-buffer))
    (define-key-after menu-bar-bookmark-map [clear-all]
        '(menu-item "Clear all speedmarks" viss-bookmark-clear-all-buffer))
    )

  (if (lre-memb 'e21+) (global-set-key [M-f2] 'bookmark-map))

  (define-key esc-map "_"         'lre-q-prev-word)
  (define-key esc-map "*"         'lre-q-prev-word)
  (define-key esc-map "\""        'lre-q-prev-word)
  (define-key esc-map ":"         'eval-expression)
  (define-key esc-map [f1]        'eval-expression)
  (if (lre-memb 'ispell)
      (define-key esc-map "$"   'ispell-word))
  (if (fboundp 'cycle-spacing)
      (define-key esc-map " "     'cycle-spacing))

  (define-key ctl-x-4-map "v"   'view-file-other-window)
  (define-key ctl-x-4-map "g"   'lre-grape-open-non-diff)
  (define-key ctl-x-4-map "b"   'switch-to-buffer-other-window)
  (define-key ctl-x-5-map "k"   'lre-kill-buf-frame)
  (define-key ctl-x-5-map "n"   'lre-switch-to-new-frame)
  (define-key ctl-x-5-map "g"   'lre-grape-open-non-diff-new-frame)
  (define-key ctl-x-map "\C-k"  'lre-kill-save-buf)
  (define-key ctl-x-map "u"     'lre-e20-undo)
  ;;    (define-key ctl-x-map  "o"    'iswitchb-buffer)
  (define-key ctl-x-map  "rd"   'delete-rectangle)
  (define-key ctl-x-map "vv"      (if (and (lre-memb 'pc)
                                           (not (lre-memb 'personal)))
                                      'lre-vc-no-action
                                    'lre-vc-next-action))
  (define-key ctl-x-map "\C-hi" 'lre-history-insert)
  (define-key ctl-x-map "\C-hp" 'lre-history-point)
  (define-key ctl-x-map "\C-hv" 'lre-wversion)
  (define-key ctl-x-map "\C-v"  'lre-alternate-file)

  (define-key help-map "AA" 'apropos)
  (define-key help-map "Aa" 'apropos-command)
  (define-key help-map "Ad" 'apropos-documentation)
  (if (fboundp 'apropos-library)
      (define-key help-map "Al" 'apropos-library))
  (define-key help-map "Au" 'apropos-user-option)
  (define-key help-map "Av" 'apropos-variable)
  (define-key help-map "Aw" 'apropos-value)
  (define-key help-map "d"  'list-load-path-shadows)
  (define-key help-map "E"  'describe-face)
  (define-key help-map "M"  'man)
  (define-key help-map "Gl" 'locate-library)
  (define-key help-map "Gf" 'find-function)
  (define-key help-map "Gk" 'find-function-on-key)
  (define-key help-map "O"  'bookmark-bmenu-list)
  (define-key help-map "o"  'describe-symbol)
  (define-key help-map "P"  'lre-show-save-places)
  (define-key help-map "y"  'describe-syntax)
  (define-key help-map "W"  'woman)
  (define-key help-map "\M-w" 'bill-help-on-topic)

  (if (fboundp 'help-for-help-doc)
      (defadvice help-for-help-doc (before lre-more-docs activate)
        "Additional bindings:
AA    Apropos symbol
Aa    Apropos (like a)
Ad    Apropos documentation
Au    Apropos value
Av    Apropos variable
d     List shadows \(duplicates\) in `load-path'
E     Describe face
Gl    Locate library
Gf    Find function
Gk    Find function on key
M     Run `man'
O     Show bookmark list
P     Show save-places
y     Describe syntax
W     Run `woman'
M-w   Show Windows Help
V     Show Sun keyboard (maybe...)"))
  (if (fboundp 'help-for-help-internal-doc)
      (defadvice help-for-help-internal-doc (before lre-more-docs activate)
        "Additional bindings:
AA    Apropos symbol
Aa    Apropos (like a)
Ad    Apropos documentation
Al    Apropos library
Au    Apropos value
Av    Apropos variable
d     List shadows \(duplicates\) in `load-path'
E     Describe face
Gl    Locate library
Gf    Find function
Gk    Find function on key
M     Run `man'
o     Show bookmark list
S     Show save-places
y     Describe syntax
W     Run `woman'
M-w   Show Windows Help
V     Show Sun keyboard (maybe...)"))

  (define-key isearch-mode-map "\C-e" 'isearch-edit-string)

  ;; (global-set-key [backspace] 'backward-delete-char-untabify)

  (define-key global-map (if (lre-memb 'e22+) [menu-bar file recover]
                           [menu-bar files recover])
    '("Recover file" . recover-file))

  (define-key menu-bar-edit-menu [separator-realign] '("--"))
  (define-key menu-bar-edit-menu [mode-realign]
    '("Realign separators (mode-specific)" . lre-realign-delim))
  (define-key menu-bar-edit-menu [gen-realign]
    '("Realign separators" . lre-realign-sep-in-region))

  (if (commandp 'repeat)
      (global-set-key  (kbd "C-.") 'repeat)
    (global-set-key (kbd "C-.")   'dot-mode))
  (global-set-key (kbd "C-,")     'hippie-expand)
  ;; (global-set-key "%"          'lre-match-paren)
  (global-set-key [C-tab]         'indent-relative)
  (global-set-key [?\e backspace] 'undo)
  (unless (lre-memb 'e21+)
    (global-set-key [end]         'end-of-line)
    (global-set-key [home]        'beginning-of-line)
    (global-set-key [C-end]       'end-of-buffer)
    (global-set-key [C-home]      'beginning-of-buffer))
  (global-set-key [delete]        'delete-char)
  (global-set-key [insert]        'overwrite-mode)
  (global-set-key [C-backspace]   'backward-kill-word)
  (global-set-key [C-right]       'forward-word)
  (global-set-key [C-left]        'backward-word)
  (global-set-key [C-down]        'forward-paragraph)
  (global-set-key [C-up]          'backward-paragraph)
  (global-set-key [C-next]        'forward-sexp)
  (global-set-key [C-prior]       'backward-sexp)
  (global-set-key [C-M-next]      '(lambda ()
                                    (interactive)
                                    (ignore-errors (next-line 5))))
  (global-set-key [C-M-prior]     (lambda ()
                                    (interactive)
                                    (ignore-errors (previous-line 5))))

  (global-set-key [C-return]      'newline-and-indent)
  (global-set-key [M-:]           'eval-expression)
  (global-set-key [S-backspace]   'lre-kill-back-line)

  (global-set-key [M-right]       'lre-scroll-right)
  (global-set-key [M-left]        'lre-scroll-left)
  (global-set-key [f1]            'help-command)
  (global-set-key [f3]            'tplsub-next-position)
  (global-set-key [M-f4]          'save-buffers-kill-emacs)
  (global-set-key [C-f4]          'lre-kill-save-buf)
  (global-set-key [f5]            'goto-line)
  (global-set-key [C-f6]          'other-window)
  (global-set-key [f8]            'lre-compile)
  (global-set-key [f12]           'dabbrev-expand)
  (when (lre-memb 'dos)
    (if (lre-memb 'braces)
        (global-set-key [f23]     'brace-mode))
    (global-set-key "\177"        'backward-delete-char-untabify) ; <Del>
    (global-set-key "\C-^g"       'goto-line)
    (global-set-key "\C-^z"       'suspend-emacs)
    (global-set-key "\C-^?"       'help-command)
    (global-set-key [S-mouse-3]   'mouse-choose-completion)
    (emacs-pc-enable-mouse)
    (define-key ctl-x-map "b"     'select-buffer)
    (define-key ctl-x-4-map "b"   'select-buffer-other-window)
    (if (lre-memb 'iso)
        (set-input-mode (car (current-input-mode))
                        (nth 1 (current-input-mode))
                        0)))
  (when (lre-memb 'win32)
    (define-key-after
      ;; Repeated calls do not seem to matter!
      (if (lre-memb 'e21+) menu-bar-manuals-menu
        (lookup-key global-map [menu-bar help-menu]))
      [woman]
      (if (lre-memb 'e21+)
          '(menu-item "WoMan..." woman :help "Manpage without man...")
        '("WoMan..." . woman-find-file))
      (if (lre-memb 'e21+)
          t 'man)))
  (when (and (lre-memb 'braces)
             (lre-memb 'win32 'gnunix))
    (global-set-key [pause]  'brace-mode)))

(when (lre-memb-all 'tvist 'keys)
  (define-key lre-tvist-sysdul-submap "p" 'lre-print-greip)
  (define-key lre-tvist-sysdul-submap "C" 'cf-mode)
  (define-key lre-tvist-keymap "a" 'tplsub-describe-templates)
  (define-key lre-tvist-keymap "r" 'query-replace-regexp)
  (define-key lre-tvist-keymap "m" 'tplsub)
  (define-key lre-tvist-keymap "q" 'lre-sql)
  (define-key lre-tvist-keymap "\C-m" 'lre-disp-macro)
  (define-key lre-tvist-keymap "d" 'lre-todays-date)
  (define-key lre-tvist-keymap "\C-l" 'font-lock-fontify-buffer)
  (define-key lre-tvist-keymap "\C-c" 'sysdul-comment-box)
  (define-key lre-tvist-keymap " " 'sysdul-fix-white)
  (define-key lre-tvist-keymap "\C-p" 'lpr-buffer)
  (define-key lre-tvist-keymap "p" 'ps-print-buffer-with-faces)
  (define-key lre-tvist-keymap "P" 'ps-print-region-with-faces)
  (define-key lre-tvist-keymap "\M-p" 'ps-spool-buffer-with-faces)
  (setq lre-tvist-key-description
        (append lre-tvist-key-description
                (list "a - list \"abbrevs\" for gjeldende modus"
                      "r - søk erstatt regexp"
                      "m - kjør malsubstituering"
                      "d - sett inn dato, samt signatur hvis prefiks"
                      "C-c - lag \"kommentarboks\""
                      "C-m - vis makrodefinisjon"
                      "C-l - gjenoppfrisk fonter/farger"
                      "C-p - print buffer 'plain'"
                      "p - pretty-print buffer (hvis greip-fil, kun del 1)"
                      "P - pretty-print merket område"
                      "M-p - pretty-print til temp.buffer (skrives ut med M-x ps-despool etterpå)"
                      "q - start SQL-vinduer (prefiks for kun resultatvindu)"
                      "SPACE - fjern overflødige blanke"))))

(message "Misc...")

(when (lre-memb 'tabbar)
  (defun lre-tabbar-buffer-list ()
    "All buffers"
    (delq t
          (mapcar #'(lambda (b)
                      (cond ((buffer-file-name b) b)
                            (b)))
                  (buffer-list))))
  (setq tabbar-buffer-list-function 'lre-tabbar-buffer-list)

  (defun lre-tabbar-buffer-groups (buffer)
    "Return the list of group names BUFFER belongs to.
Return only one group for each buffer."
    (with-current-buffer (get-buffer buffer)
      (cond
       ((or (get-buffer-process (current-buffer))
            (memq major-mode
                  '(comint-mode compilation-mode)))
        '("Process")
        )
       ((member (buffer-name)
                '("*scratch*" "*Messages*"))
        '("Common")
        )
       ((eq major-mode 'dired-mode)
        '("Dired")
        )
       ((memq major-mode
              '(help-mode apropos-mode Info-mode Man-mode))
        '("Help")
        )
       ((memq major-mode
              '(rmail-mode
                rmail-edit-mode vm-summary-mode vm-mode mail-mode
                mh-letter-mode mh-show-mode mh-folder-mode
                gnus-summary-mode message-mode gnus-group-mode
                gnus-article-mode score-mode gnus-browse-killed-mode))
        '("Mail")
        )
       ((char-equal ?\  (aref (buffer-name) 0))
        '("Hidden"))
       (t
        (list
         (if (and (stringp mode-name) (string-match "[^ ]" mode-name))
             mode-name
           (symbol-name major-mode)))
        )
       )))
  (setq tabbar-buffer-groups-function 'lre-tabbar-buffer-groups)
  )


;;; ------------------------------------------------------------------
;;; /// lre-libs ///

(eval-and-compile
  (lre-safe-require 'lrecjava))

(lre-setup-lib 'lre-util
  (autoload 'lre-char-class "lre-util" "Convert `c' to `[cC]'." t)
  (autoload 'lre-split-string "lre-util"
    "Split a string into words, using REGEXP as a delimiter.")
  (autoload 'lre-kill-back-line "lre-util" "Kill to beginning of line." t)
  (autoload 'lre-kill-this-line "lre-util" "Kill the current line." t)
  (autoload 'lre-copy-line-other-window "lre-util"
    "Copy current line(s) to current point in other-window." t)
  (autoload 'lre-inc-num-str "lre-util" "Increment number in string")
  (autoload 'lre-number-lines "lre-util" "Insert line number" t)
  (autoload 'lre-next-long-line "lre-util" "Goto next long line" t)
  (autoload 'lre-disp-dir-info "lre-util" "Show directory info" t)
  (autoload 'lre-adjust-separator "lre-util"
    "Adjust final sequence of this line" t)
;;  (autoload 'lre-fe-mode "lre-util" "Additions to `fe-mode' setup.")
  (autoload 'lre-cal-mode "lre-util" "Additions to `cal-mode'.")
  (autoload 'lre-kill-save-all "lre-util"
    "Ask whether to run `lre-kill-save-buf' on all buffers." t)
  (autoload 'lre-kill-others "lre-util"
    "Kill almost all buffers..." t)
  (autoload 'lre-kill-some-windows "lre-util"
    "Close almost all non-file windows..." t)
  (autoload 'lre-goto-percent "lre-util"
    "Gå til angitt posisjon i bufferet." t)
  (autoload 'lre-occ-at-point "lre-util" "Search for thing at point." t)
  (autoload 'lre-resize-window "lre-util" "Resize window interactively." t)
  (autoload 'lre-touch-file "lre-util" "Touch a file by the given name." t)
  (autoload 'lre-spe-sign  "lre-util" "Add footer to SPE-file." t)
  (autoload 'lre-fixup-spe-dok  "lre-util" nil t)
  (autoload 'lre-fix-ps  "lre-util" "Fix HP prn/ps file." t)
  (autoload 'lre-temp-clip  "lre-util"
    "Show contents of clipboard in temp buffer" t)
  (autoload 'lre-ascii-chart  "lre-util"
    "Displays a character table in the *ascii map * buffer." t)
  (autoload 'lre-welcome  "lre-util"  nil t)
  )

(lre-setup-lib 'lre-doc
  (autoload 'lre-markdown-mode "lre-doc" "Additions to `markdown-mode' setup.")
  (autoload 'lre-nroff-mode "lre-doc" "Additions to `nroff-mode' setup.")
  (autoload 'lre-choose-sgml-mode "lre-doc" "Select the right SGML-mode" t)
  (autoload 'lre-choose-dtd-mode "lre-doc" "Select the right XML-mode for DTDs" t)
  (autoload 'lre-choose-xml-mode "lre-doc" "Select the right XML-mode" t)
  (autoload 'lre-add-quote "lre-doc" "Add quote to quotes.xml" t)
  (autoload 'lre-add-cookie "lre-doc" "Add cookie to quotes.xml" t)
  (autoload 'lre-xml-mode "lre-doc" "XML add-ons")
  (autoload 'lre-nxml-mode "lre-doc" "XML add-ons")
  (autoload 'lre-xsl-mode "lre-doc" "XSlide add-ons")
  (autoload 'lre-css-mode "lre-doc" "CSS add-ons")
  (autoload 'lre-pxml-mode "lre-doc" "XML add-ons")
  (autoload 'lre-html-mode "lre-doc" "HTML add-ons")
  (autoload 'lre-html-dup-url "lre-doc"  "Create A-tag from URL" t)
  (autoload 'lre-html-ins-table "lre-doc" "Insert table tags" t)
  (autoload 'lre-sgml-mode "lre-doc" "Additions to `sgml-mode' setup.")
  (autoload 'lre-sgml-tag "lre-doc" "Create tag")
  (autoload 'lre-psgml-mode "lre-doc" "Additions to `psgml-mode' setup.")
  (autoload 'lre-wml-mode "lre-doc" "Additions to wml-mode")
  (autoload 'lre-htmlize-notab "lre-doc" "Fix htmlize table")
  )

(lre-setup-lib 'lre-prog
  (autoload 'lre-scala-mode "lre-prog" "Scala mode setup" t)
  (autoload 'lre-sql-mode "lre-prog" "Personal SQL definitions.")
  (autoload 'lre-sql-mode-21 "lre-prog" "Personal SQL definitions.")
  (autoload 'lre-sql-mode-21i "lre-prog" "Personal SQL definitions.")
  (autoload 'lre-hack-ddf "lre-prog" "Merge compiled picture." t)
  (autoload 'lre-grape-ready-file "lre-prog" "Klargjør fil for innsjekking." t)
  (autoload 'lre-grape-auto-ready-file "lre-prog")
  (autoload 'lre-grape-update-events "lre-prog" "Change x_mXXX_events.svp." t)
  (autoload 'lre-grape-update-main "lre-prog" "Change mXXX.vsd." t)
  (autoload 'lre-load-sysdul-stereo "lre-prog")
  (autoload 'lre-grape-procify "lre-prog"
    "Konverterer vsd-fil til proc.cf-format" t)
  (autoload 'lre-grape-open-non-diff "lre-prog"
    "Åpne fil svarende til diff-fil, posisjoner cursor ved endring om mulig" t)
  (autoload 'lre-grape-open-non-diff-new-frame "lre-prog"
    "Åpne fil svarende til diff i nytt vindu." t)
  (autoload 'lre-raw-sysdul "lre-prog"
    "Ødelegger koden i bufferet - viser 1 setning per linje...")
  (autoload 'lre-makefile-mode "lre-prog"
    "Personal `makefile-mode' definitions.")
  (autoload 'lre-shell-mode "lre-prog" "Additions to shell setup.")
  (autoload 'lre-ksh-mode "lre-prog" "Additions to `ksh-mode' setup.")
  (autoload 'lre-csh-mode "lre-prog" "Additions to `csh-mode' setup.")
  (autoload 'lre-compile "lre-prog" "Perform mode-dependent compile." t)
  (autoload 'lre-sql "lre-prog" "Start interactive sql." t)
  (autoload 'lre-make-cvs-script "lre-prog" "Create script from cvs update." t)
  (autoload 'lre-choose-java-mode "lre-prog" "Select the right java-mode" t)
  (autoload 'lre-run-clojure "lre-prog" "Run interactive Clojure" t)
  )

(lre-setup-lib 'lre-lisp
  (autoload 'lre-list-libs "lre-lisp"
    "List files in `load-path' matching REGX." t)
  (autoload 'lre-open-library "lre-lisp"
    "Open given lisp library for editing in other window(s)." t)
  (autoload 'lre-byte-compile-file "lre-lisp"
    "Byte compile file - see `byte-compile-file'." t)
  (autoload 'lre-search-defun "lre-lisp"
    "Search for defun in load-path." t)
  (autoload 'lre-lisp-mode "lre-lisp" "Personal Lisp mode definitions.")
  (autoload 'lre-toggle-variable "lre-lisp"
    "Toggle a variable (assumed to be boolean)." t)
  )

(lre-setup-lib 'lre-net
  (autoload 'lre-win32-mail "lre-net" "Win32 mail/news setup." t)
  (autoload 'lre-vm-mode "lre-net" )
  (autoload 'lre-gnus-mode "lre-net" "Personal gnus setup.")
  (autoload 'lre-vm "lre-net" "Start vm.")
  (autoload 'lre-gnus "lre-net" "Start gnus.")
  )

(lre-setup-lib 'lre-git
  (autoload 'lre-git-mergetool-emacsclient-ediff "lre-git" "Git mergetool function." t)
  )

;;; ------------------------------------------------------------------
;;; /// Colors... ///

(defvar lre-cursorcol "black"
  "Current cursor color (for `lre-colorcursor')")

(defvar lre-cursorcol-buffer "*scratch*"
  "Current cursor buffer (for `lre-colorcursor')")

(defun lre-colorcursor()
  "Change cursor color as needed"
  (let ((color (cond (buffer-read-only "blue")
                     (overwrite-mode   "red")
                     (t "black"))))
    (unless (and (string= color lre-cursorcol)
                 (string= (buffer-name) lre-cursorcol-buffer))
      (set-cursor-color (setq lre-cursorcol color))
      (setq lre-cursorcol-buffer (buffer-name)))))

(if (lre-memb 'cursor-color)
    (add-hook 'post-command-hook 'lre-colorcursor))

(defconst lre-colors-set nil)

(defvar lre-x-hilit
  (mapconcat 'identity
             '("[x]06lre" "lare9505" "lare"
               "\\([Mm]esan\\( *[Aa]/?[Ss]\\)?\\)"
               "\\<[L][R][E]\\>"
               "\\<\\(\\(Lars +\\)?[R]eed\\)\\>")
             "\\|")
  "Special highlight patterns...")

(defvar lre-x-hilit-list nil
  "List of modes where special highlights are already added.")

;;
;; (font-lock-add-keywords 'mode-name
;;                         '(("regexp" 0 font-lock-xxx-face t)))
;;

;; virker dårlig!!!   (if (and (lre-memb 'e20+) (not lre-inhibit-flock))
;;                        (setq global-font-lock-mode t))

(defun lre-turn-on-font-lock ()
  (font-lock-mode 1))

(defconst lre-flock-mode-list
  (append
   lre-generic-mode-list
   '(change-log-mode compilation-mode dired-mode       ediff-mode
     makefile-mode   message-mode     outline-mode     sh-mode
     nroff-mode      perl-mode        rmail-mode       sgml-mode
     c-mode          c++-mode         lisp-mode        ada-mode
     pascal-mode     scheme-mode      text-mode        shell-script-mode
     emacs-lisp-mode sysdul-mode      fe-mode          lisp-interaction-mode
     cf-mode         html-mode        latex-mode       java-mode
     wml-mode        ksh-mode         sql-mode         psgml-mode
     pxml-mode       xml-mode         diff-mode        vsq-mode
     css-mode        xsl-mode         nxml-mode        Info-mode
     apropos-mode    awk-mode         simula-mode      markdown-mode
     ))
  "Modes that support font-lock.")

(when (lre-memb 'flock)
  (setq font-lock-maximum-decoration     t
        font-lock-use-maximal-decoration t
        font-lock-auto-fontify           t
        font-lock-use-colors             t
        font-lock-use-fonts              nil
        font-lock-support-mode           (if (lre-memb 'e21+)
                                             'jit-lock-mode
                                           'lazy-lock-mode)))

(defsubst lre-font-lock-ref-face ()
  (cond ((facep 'font-lock-constant-face)  'font-lock-constant-face)
        ((facep 'font-lock-reference-face) 'font-lock-reference-face)
        (t 'font-lock-keyword-face)))

(when (lre-memb 'hilit)
  (setq hilit-mode-enable-list
        (append '(not text-mode)
                (if (lre-memb 'flock) lre-flock-mode-list nil)))
  (setq hilit-background-mode   'light
        hilit-quietly           t
        hilit-inhibit-hooks     nil
        hilit-inhibit-rebinding (lre-memb 'win32)
        query-replace-highlight t)
  (lre-safe-require 'hilit19))
(when (lre-memb 'flock)
  (mapc (function
         (lambda (mod-n)
           (add-hook (intern (concat (symbol-name mod-n) "-hook"))
                     'lre-turn-on-font-lock)
;;           (add-hook (intern (concat (symbol-name mod-n) "-hook"))
;;                     'lre-fontify 'append)
           ))
        lre-flock-mode-list))

(defun lre-add-x-hilit (mode-symb)
  (if (or (not (lre-memb 'hilit))
          (memq mode-symb lre-x-hilit-list))
      nil
      (setq lre-x-hilit-list (append (list mode-symb) lre-x-hilit-list))
      (hilit-add-pattern lre-x-hilit "" 'rule mode-symb t)
      t))

(defvar lre-font-lock-specials
  (append (list '("\t+" (0 'lre-tab-face t))
                '("\r+" (0 'lre-tab-face t)))
          (if (not (lre-memb 'e21+))
              (list '("[ \t\r]+$" (0 'lre-trailing-space-face t))))))

(defmacro lre-define-face (face &optional fg bg ital bold uline inv force)
  "Change face definition"
  `(progn
     (if ,force (make-face (quote ,face)))
     (when (facep (quote ,face))
       (set-face-underline (quote ,face) ,uline)
       (set-face-inverse-video-p (quote ,face) ,inv)
       (if ,fg (set-face-foreground (quote ,face) ,fg))
       (if ,bg (set-face-background (quote ,face) ,bg))
       (if ,ital (make-face-italic (quote ,face))
         (or (and (lre-not-memb 'e21+)
                  (lre-memb 'win32)) ;; ville ikke virke
             (make-face-unitalic  (quote ,face))))
       (if ,bold (make-face-bold (quote ,face))
         (or (and (lre-not-memb 'e21+)
                  (lre-memb 'win32)) ;; ville ikke virke
             (make-face-unbold  (quote ,face))))
       (if ,bold  (pushnew (quote ,face) ps-bold-faces))
       (if ,ital  (pushnew (quote ,face) ps-italic-faces))
       (if ,uline (pushnew (quote ,face) ps-underlined-faces))
       )))


(defun lre-define-font-lock ()
  "Define faces for font-lock."
  (require 'font-lock)
  (when (lre-memb 'col-chg)
    (lre-define-face default "black" "white")
    (lre-define-face font-lock-string-face "black" nil t nil)
    (lre-define-face font-lock-comment-face (if (lre-memb 'e21+)
                                                "firebrick" "dodgerBlue")
                     nil t nil)
    (lre-define-face font-lock-keyword-face (if (lre-memb 'e21+)
                                                "green2" "firebrick"))
    (lre-define-face font-lock-function-name-face (if (lre-memb 'e21+)
                                                      "dodgerBlue" "firebrick")
                     nil nil t)
    (lre-define-face font-lock-type-face (if (lre-memb 'e21+)
                                                      "firebrick" "RoyalBlue"))
    (lre-define-face font-lock-builtin-face "black" "khaki1")
    (lre-define-face font-lock-doc-string-face "coral2")
    (lre-define-face font-lock-preprocessor-face "black" "khaki2")
    (lre-define-face font-lock-variable-name-face (if (lre-memb 'e21+)
                                                      "royalBlue"
                                                    "green2"))
    (lre-define-face font-lock-warning-face "red" "LightGoldenrod1"
                     nil t nil
                     (lre-memb 'e21+))
    (lre-define-face font-lock-constant-face (if (lre-memb 'e21+)
                                                 "royalBlue" "green2")
                     nil t)
    (lre-define-face woman-italic-face "green2" nil t)
    (lre-define-face woman-bold-face "green2" nil nil t)
    (lre-define-face italic nil nil t nil)
    (lre-define-face bold nil nil nil t)
    (lre-define-face trailing-whitespace nil "light blue")
    (lre-define-face bold-italic nil nil t t))
  (if (functionp 'custom-set-faces)
      (progn
        (custom-set-faces
         '(lre-tab-face ((((class color))
                          (:background "LemonChiffon1"))) t)
         '(minibuffer-prompt ((t (:background "LightGoldenrod"
                                  :foreground "dark blue"))))
         '(font-lock-comment-delimiter-face
           ((default (:inherit font-lock-comment-face))
            (((class color) (min-colors 16)) (:slant normal))))
         '(lre-trailing-space-face ((((class color))
                                     (:background "lavender"))) t))
        (add-hook 'font-lock-mode-hook
                  (function
                   (lambda ()
                     (setq font-lock-keywords
                           (append font-lock-keywords
                                   lre-font-lock-specials))))))))

(or (functionp 'hilit-translate)
    (defmacro hilit-translate (&rest args)
      (let (cmdl from to)
        (while args
          (setq from (car args) to (nth 1 args) args (nthcdr 2 args))
          (defvar from)))))


(defun lre-define-hilit ()
;;   "Define faces for font-lock."
;;      (when (lre-safe-require 'hilit19)
;; ;;  (setq hilit-face-check nil)
;;        (hilit-translate
;;         comment  'dodgerBlue-bold
;;         decl     'firebrick-italic
;;         error    'red/yellow-bold
;;         include  'ForestGreen
;;         formula  'white/blue-bold
;;         define   'RoyalBlue
;;         type     'purple
;;         string   'default-italic
;;         crossref 'black-bold
;;         rule     'white/red-bold-italic
;;         keyword  'firebrick))
)

(defun lre-colors ()
"Set highlight colors."
  (when (and (lre-memb 'flock)
             (not  (member 'font-lock lre-colors-set)))
    (lre-define-font-lock)
    (add-to-list 'lre-colors-set 'font-lock))
  (when (and (lre-memb 'hilit)
             (not (member 'hilit lre-colors-set)))
    (lre-define-hilit)
    (add-to-list 'lre-colors-set 'hilit)))

(defun lre-fontify ()
  "Fontify buffer."
  (lre-colors)
  (font-lock-default-fontify-buffer))

(defun lre-htmlize-buffer ()
  "Create HTL - see `htmlize-buffer'."
  (interactive)
  (lre-fontify)
  (htmlize-buffer)
  (lre-htmlize-notab))

(cond ((lre-memb 'e21+) t)
      ((featurep 'custom) (custom-set-faces)))

(defun lre-ask-hilite (patterns)
  (or (and buffer-file-name
           (string-match (regexp-quote (getenv "EMACS_DIR")) buffer-file-name))
      (and (lre-memb 'TAD)
           buffer-file-name
           (string-match (regexp-quote (lre-fixed :site-lisp)) buffer-file-name))
      (y-or-n-p "Add patterns from this buffer to hi-lock? ")))
(if (lre-memb-all 'e22+ 'personal)
    (setq hi-lock-file-patterns-policy 'lre-ask-hilite))

;;; --------------------------------------------------------------
;;; /// Norwegian characters ///

(defun lre-brace-mode (arg)
  "If braces are activated, call `brace-mode'."
  (when (lre-memb 'braces)
      (brace-mode arg)))

(defconst lre--norw-list
  (if (lre-memb 'dos)
      '(("\221" "ae" "AE_L" "aelig")
        ("\233" "oe" "OE_L" "oslash")
        ("\206" "aa" "AA_L" "aring")
        ("\222" "Ae" "AE_S" "Aelig")
        ("\235" "Oe" "OE_S" "Oslash")
        ("\217" "Aa" "AA_S" "Aa"))
    '(("æ" "ae" "AE_L" "aelig")
      ("ø" "oe" "OE_L" "oslash")
      ("å" "aa" "AA_L" "aring")
      ("Æ" "Ae" "AE_S" "Aelig")
      ("Ø" "Oe" "OE_S" "Oslash")
      ("Å" "Aa" "AA_S" "Aa")))
  "Mapping - SGML-binding, ROFF-binding, HTML-binding.")

(defun lre-norw (mapno)
  "\"Insert ?? - for norwegian character.
MAPNO is 1=SGML/?roff, 2=VA, 3=HTML\""
  (insert (or (nth mapno (assoc (char-to-string last-command-event)
                                lre--norw-list))
              "??")))

(defun lre-set-norw (kmap fun)
  "Map norwegian."
  (if (lre-memb 'dos)
      (progn
        (define-key kmap [?\C-c 145]  fun)
        (define-key kmap [?\C-c 155]  fun)
        (define-key kmap [?\C-c 134]  fun)
        (define-key kmap [?\C-c 146]  fun)
        (define-key kmap [?\C-c 157]  fun)
        (define-key kmap [?\C-c 143]  fun))
      (define-key kmap [?\C-c 230]  fun)
      (define-key kmap [?\C-c 248]  fun)
      (define-key kmap [?\C-c 229]  fun)
      (define-key kmap [?\C-c 198]  fun)
      (define-key kmap [?\C-c 216]  fun)
      (define-key kmap [?\C-c 197]  fun)))

;;; ---------------------------------------------------------------
;;; /// Text ///

(defun lre-text-mode ()
"Additions to `text-mode' setup."
  (auto-fill-mode   t)
  (if (or (eq major-mode 'text-mode)
               (eq major-mode 'indented-text-mode))
      (local-set-key [backspace] 'backward-delete-char-untabify))
  (lre-set-local fill-column 72)
  (setq filladapt-mode t))

(defun lre-untabify (r-beg r-end &optional how-many)
  "* Untabify med mulig justering av `tab-width'."
  (interactive "*r\np")
  (let ((tab-width how-many))
    (untabify r-beg r-end)))

(defun lre-text-ins (txt &optional chr wrd)
  "Insert and move cursor."
  (if txt (insert txt))
  (if chr (forward-char chr))
  (if wrd (forward-word wrd)))

(autoload 'lre-how-many "lre-util"
  "Return number of matches for REGEXP following point" t)

(defun lre-q-prev-word (count)
  "Surrounds the previous word with the last-command-char."
  (interactive "*p")
  (backward-word count)
  (insert last-command-event)
  (forward-word count)
  (insert last-command-event))

(defun lre-kill-yank (&optional arg)
  "Swaps current selection with last kill.
Also works for normal
yank even with ARGS (thus it can be mapped to \C-y)"
  (interactive "p")
  (if mark-active
      (let ((s (buffer-substring (point) (mark))))
        (delete-region (point) (mark))
        (if (string= s (current-kill 0 1))
            (let ((str2 (current-kill 1 1)))
              (kill-new str2 t)))))
  (if arg (yank arg)
    (yank)))


(defun lre-trim-trail-cr ()
  "Deletes all CRs found at the end of a line."
  (interactive "*")
  (goto-char (point-min))
  (replace-regexp "\r$" ""))

(defun lre-trim-trail-nl ()
  "Delete superfluous LFs at end of buffer."
  (interactive "*")
  (let (pp)
    (goto-char (1- (point-max)))
    (setq pp (point))
    (while (looking-at "\n")
      (forward-char -1))
    (forward-char 1)
    (if (> pp (point)) (progn
                         (delete-char (- pp (point)))))))

(defun lre-trim-trail-spc ()
  "Delete trailing whitespace in buffer."
  (interactive "*")
  (goto-char (point-min))
  (replace-regexp "[ \t]+$" ""))

(defun lre-trim (pfx)
  "Remove unwanted space"
  (interactive "*P")
  (save-mark-and-excursion
    (if pfx
        (let ((tab-width 4))
          (untabify (point-min) (point-max))))
    (goto-char (point-min))
    (replace-regexp "\\([ \t\r]+$\\)\\|\\([ \t\r\n]+\n\\'\\)" "")
    (goto-char (1- (point-max)))
    (unless (looking-at "\n")
      (goto-char (point-max))
      (insert "\n"))))

(defun lre-join-line-inverse ()
  "join-line to following"
  (interactive)
  (join-line -1))

(defun lre-move-line-down ()
  "move current line down"
  (interactive)
  ;; http://whattheemacsd.com/
  (let ((col (current-column)))
    (save-mark-and-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun lre-move-line-up ()
  "move current line up"
  (interactive)
  ;; http://whattheemacsd.com/
  (let ((col (current-column)))
    (save-mark-and-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

(defun lre-replace-many-regexps (ins rlist)
  "For each (org . replace) in RLIST, perform replacement in INS"
  (let (from
        to
        (res ins)
        (explist rlist))
    (while explist
      (setq from    (caar  explist)
            to      (cadar explist)
            res     (replace-regexp-in-string from to res)
            explist (cdr explist)))
    res))

(defun lre-match-paren (arg)
  "Find matching paren or insert %."
  (interactive "p")
  (cond
   ((eq last-command 'self-insert-command) (self-insert-command arg))
   ((looking-at "[[{(]") (forward-sexp 1) (forward-char -1))
   ((looking-at "[]})]") (forward-char 1) (forward-sexp -1))
   (t (self-insert-command (or arg 1)))))


(defun lre-check-parentheses ()
  "Check the buffer for unbalanced parentheses.
Stops at any that are unbalanced.
By Bob Wiener"
  (interactive)
  (if (lre-memb 'e21+)
      (check-parens)
    (let ((start-point (point)))
      (goto-char (point-min))
      (condition-case e
          (while (/= (point) (point-max))
            (forward-sexp))
        (error
         ;; If this is an extra left paren error, we have to scan backwards to
         ;; find the exact left paren in error
         (cond ((and (eq (car e) 'error)
                     (string-equal (car (cdr e)) "Unbalanced parentheses"))
                ;; left paren error
                (goto-char (point-max))
                (while (/= (point) (point-min))
                  (condition-case e (backward-sexp)
                    (error
                     (error "Probably an extra left parenthesis here")))))
               (t
                (error "Probably an extra right parenthesis here")))))
      (goto-char start-point)
      (message "All parentheses appear balanced."))))

(defun lre-dup-line (pfx)
  "Copy current line to kill-ring.  Without prefix, also insert immediately."
  (interactive "P")
  (save-mark-and-excursion
    (beginning-of-line)
    (save-mark-and-excursion
      (let ((p1 (point)))
        (forward-line 1)
        (copy-region-as-kill p1 (point))))
    (if (not pfx) (yank))))

(defun lre-mouse-click-ins (click)
  "Insert the contents of the selection at mouse click."
  (interactive "*e")
  (mouse-set-point click)
  (if mouse-sel-get-selection-function
      (insert (or (funcall mouse-sel-get-selection-function) ""))))

(defun lre-max-length-in-region (m p)
  "Finn maks linjelengde i merket område."
  (interactive "R")
  (let (pbeg
        pend
        nchars
        (rest 0)
        (maxchars 0))
    (save-restriction
      (narrow-to-region m p)
      (goto-char (point-min))
      (while (= 0 rest)
        (setq pbeg (point))
        (end-of-line)
        (setq nchars (- (point) pbeg))
        (if (> nchars maxchars) (setq maxchars nchars))
        (setq rest (forward-line 1))
        (beginning-of-line)))
    maxchars))

(defun lre-realign-sep-in-region (delim m p &optional intro)
  "Lag rett linje av separatorer."
  (interactive "*sDelimiter: \nr\nsPrefix: ")
  (let (mlen
        ppoint
        (rest 0))
    (save-restriction
      (narrow-to-region m p)
      (goto-char (point-min))
      (while (re-search-forward (concat "[ \t]*" (regexp-quote delim)) nil t)
        (replace-match "" nil nil))
      (setq mlen (lre-max-length-in-region (point-min) (point-max)))
      (goto-char (point-min))
      (setq ppoint (point))
      (while (= 0 rest)
        (end-of-line)
        (if (not (= (point) ppoint))
            (progn
              (while (< (current-column) mlen)
                (insert " "))
              (if intro (insert intro))
              (insert delim)))
        (setq rest (forward-line 1))
        (beginning-of-line)
        (setq ppoint (point))))))

(defun lre-realign-delim (m p)
  (interactive "*r")
  (cond ((eq major-mode 'sysdul-mode)
         (lre-realign-sep-in-region ":" m p " "))
        ((eq major-mode 'cf-mode)
         (lre-realign-sep-in-region "@@\\" m p " "))
        ((member major-mode '(sql-mode vsq-mode))
         (lre-realign-sep-in-region "\§\§\\" m p " "))
        ((member major-mode '(c-mode c++-mode java-mode))
         (lre-realign-sep-in-region "\\" m p " "))
        (t
         (lre-realign-sep-in-region (read-string "Linjemerke: "
                                                   "\\")
                                      m p " "))))



;;; ------------------------------------------------------------------
;;; /// Sysdul ///

(defun lre-is-greip-p ()
  "Bestem om buffer er greip-buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp "^\\*V INCLUDE IF SRCTYPE" nil t)))

(defun lre--grape-kill-part-n (froms tos)
  "Fjerner del n  fra cf-fil."
  (goto-char (point-min))
  (search-forward froms)
  (beginning-of-line 2)
  (let ((p (point)))
    (if tos (search-forward tos)
      (goto-char (point-max)))
    (beginning-of-line 0)
    (or tos (looking-at "\\*V") (error "Feil format"))
    (delete-region p (point))))

(defun lre-grape-kill-part-2 ()
  "Fjerner del 2 (SRCTYPE=GRAPE) fra cf-fil."
  (interactive)
  (lre--grape-kill-part-n "INCLUDE IF SRCTYPE = GRAPE"
                          "INCLUDE IF SRCTYPE = PROC"))

(defun lre-grape-kill-part-3 ()
  "Fjerner del 3 (SRCTYPE=PROC) fra cf-fil."
  (interactive)
  (lre--grape-kill-part-n "INCLUDE IF SRCTYPE = PROC"
                          "INCLUDE IF SRCTYPE = DEF"))

(defun lre-grape-kill-part-4 ()
  "Fjerner del 3 (SRCTYPE=DEF) fra cf-fil."
  (interactive)
  (lre--grape-kill-part-n "INCLUDE IF SRCTYPE = DEF" nil))

(defun lre-sysdul-mode ()
  "Personal sysdul definitions."
  (auto-fill-mode -1)
  (or (lre-memb 'e21+)
      (lre-set-local imenu-scanning-message
                     "Index... (%3d%%)"))
  (if (lre-memb 'tvist)
      (sysdul-auto-fill-mode -1))
  (lre-colors)
  (if (listp 'align-sq-string-modes)
      (add-to-list 'align-sq-string-modes 'sysdul-mode))
  (if (listp 'align-open-comment-modes)
      (add-to-list 'align-open-comment-modes 'sysdul-mode))
  (when (lre-memb 'keys)
    (define-key sysdul-mode-map "\C-zp" 'lre-print-greip)
    (define-key sysdul-mode-map "\C-cp" 'ps-print-buffer-with-faces)
    (define-key sysdul-mode-map "\C-zC" 'cf-mode)
    (define-key sysdul-mode-map "\C-c\C-c" 'commbox)
    (when (lre-memb 'tvist)
      (define-key sysdul-mode-map lre-tvist-key lre-tvist-sysdul-submap)
      (if lre-tvist-first-sysdul
          (setq lre-tvist-first-sysdul nil
                lre-tvist-key-description
                (append lre-tvist-key-description
                        (list "C - kun i sysdul-mode: bytt til cf-mode")))))
    (define-key sysdul-mode-map "\C-cg" 'cf-grapeify-region)
    (easy-menu-define sysdul-admin-menu sysdul-mode-map "Sysdul admin menu"
                        '("Grape"
                          ["Grapeify region" cf-grapeify-region (mark)]
                          ["Ungrapeify region" cf-ungrapeify-region (mark)]
                          ["Fixup (experimental)" lre-grape-procify
                           (not buffer-read-only)]
                          "---"
                          ("Adm"
                           ["Prepare checkin" lre-grape-ready-file t]
                           ["Remove part 2" lre-grape-kill-part-2 t]
                           "---"
                           ["Update event-handler" lre-grape-update-events t]
                           ["Update main part 2" lre-grape-update-main t]))))
  (lre--imenu-add)
  (when (lre-memb 'flock)
    (lre-set-local font-lock-defaults '(sysdul-font-lock-keywords nil t))))

(defun lre-cf-mode ()
  (or (assq 'cf-mode font-lock-defaults)
      (setq font-lock-defaults
            (append font-lock-defaults
                    (list
                     (cons 'cf-mode
                           (cdr (assq 'c-mode font-lock-defaults
                                      )))))))
  (lre-cc-common)
  (lre-brace-mode 0)
  (if (listp 'align-c++-modes) (add-to-list 'align-c++-modes 'cf-mode))
  (if (lre-memb 'imenu)
      (setq imenu-generic-expression
            (cons '(nil
                    "^[ \t]*\\(Control\\(Handling\\|Procedure\\)\\|Cp\\|Proc\\(Db\\)?\\|Tad\\(OL\\|HV\\)?\\(Cp\\|Proc\\)\\(W\\(curr\\|param\\|paramCurr\\)\\)?\\)Start(\\([A-Za-z0-9_|]+\\)"
                    8)
                  sysdul-imenu-expression)))
  (c-toggle-auto-hungry-state -1) ; Vil ikke ha auto-hungry i cf-filer
  (tplsub-set-according-to-mode))


;;; --------------------------------------------------------------------------
;;; Elint

(defvar lre-elint-init nil)

(defun lre-elint-init ()
  (or lre-elint-init
      (elint-initialize))
  (setq lre-elint-init t))

(defadvice elint-defun (before init-elint activate)
  "Make sure internal data is initialized."
  (lre-elint-init))

(defadvice elint-current-buffer (before init-elint activate)
  "Make sure internal data is initialized."
  (lre-elint-init))


;;; --------------------------------------------------------------------------
;;; /// help/man ///

(defun lre-man-mouse (evt)
  "Get new man page based upon word under mouse when \\[lre-man-mouse\] is given."
  (interactive "e")
  (mouse-set-point evt)
  (man (Man-default-man-entry)))

(defun lre-man-quit-frame ()
  "Quit view + exit frame."
  (interactive)
  (Man-quit)
  (delete-frame))

(defun lre-man-mode ()
  "Additions to `Man-mode'."
  (setq Man-switches "-a"
        Man-notify (if (lre-memb 'pc)
                       'friendly
                     'newframe))
  (and (lre-memb-all 'keys 'mouse)
       (define-key Man-mode-map [mouse-2] 'lre-man-mouse))
  (and (lre-memb 'keys)
       (define-key Man-mode-map "Q" 'lre-man-quit-frame))
  (when (lre-memb 'hilit)
    (hilit-set-mode-patterns
     'help-mode
     '(
       ("\\`\\([-+a-zA-Z0-9_*]+\\)\\(:\\)?" 1 keyword)
       ("`\\([-+a-zA-Z0-9_:*][-+a-zA-Z0-9_:*]+\\)'" nil crossref)
       )
     nil 'case-insensitive)
    (lre-add-x-hilit 'help-mode)))

(defun lre-help-mode ()
  "Additions to `help-mode'."
  (define-key help-mode-map [backspace] 'help-go-back))

(defun lre-info-mode ()
  "Additions to info-mode."
  (define-key Info-mode-map [M-backspace] 'Info-scroll-down)
  (define-key Info-mode-map [backspace]   'Info-last))

;;; --------------------------------------------------------------------------
;;; /// Buffer/win/file functions ///
;
;; (defun make-backup-file-name (file)
;;    (concat "x:/backups/" (file-name-nondirectory file) "~"))
;; (setq auto-save-list-file-prefix "x:/backups")

(defvar lre-internal-buf-list
  '("*Messages*" "*scratch*"))

(defun lre-internal-buf-p (name)  "Internal buffers."
  (save-match-data (or (member name lre-internal-buf-list)
                       (string-match "^ " name))))

(defsubst lre-special-buf-p (name)  "Buffers to be treated specially..."
  (save-match-data (string-match "^ *\\*" name)))

(defvar lre-junk-buf-list
  '("*Completions*"     "*Help*"        "*compilation*" "*Elint*"
    "*Compile-Log*"     "*Apropos*"     "*Occurr*"      "*grep*"
    "*Text Properties*" "*Faces*"       "*cbox*"        "*DefMacro*"
    "*Ediff Registry*"  "*Shadows*"     "*Buffer List*" "*vc*"
    "*ediff-merge*"     "*Warnings*"    "*eww*"
    "*Directory Summary Information*")
  "Buffers that are always expendable...")

(defsubst lre-junk-buf-p (name)  "Buffers always containing junk..."
  (member name lre-junk-buf-list))

(defun lre-kill-buff-win ()
  "kill-buffer(\\[kill-buffer])+delete-window (\\[delete-window])"
   (interactive)
   (and (kill-buffer nil)
        (delete-window)))

(defun lre-kill-buf-frame ()
  "kill-buffer after save-buffer followed by delete-frame
(equals \\[save-buffer] \\[kill-buffer] \\[delete-frame])."
   (interactive)
   (save-buffer)
   (kill-buffer nil)
   (delete-frame))

(defun lre-kill-save-buf (&optional kill-win)
  "Get rid of buffer/window...
1. Save buffer (save-buffer==\\[save-buffer])
2. If client buffer - exit client (server-edit==\\[server-edit])
   else kill buffer (kill-buffer==\\[kill-buffer])
3. If emacs 21+ and this is the only window in the current frame and
   there are other frames
4. If multiple prefix given - remove frame (delete-frame==\\[delete-frame])
   or if other prefix given - remove window (delete-window==\\[delete-window])
   Removing the last frame or window is not allowed."
   (interactive "P")
   (let* ((junk    (lre-junk-buf-p (buffer-name)))
          (special (lre-special-buf-p (buffer-name)))
          (double-pfx (or (and kill-win
                               (listp kill-win)
                               (integerp (car kill-win))
                               (> (car kill-win) 4))
                          (and (lre-memb 'e21+)
                               (one-window-p t nil)
                               (delete-frame-enabled-p)
                               (not (window-dedicated-p (selected-window)))
                               (not (and (boundp 'server-buffer-clients)
                                         server-buffer-clients))
                               (not (and (boundp 'gnuserv-buffer-clients)
                                         gnuserv-buffer-clients))
                               )))
          (single-pfx (and kill-win
                           (not (window-dedicated-p (selected-window))))))
     (if junk (set-buffer-modified-p nil)
       (if (or (and special
                    (buffer-modified-p)
                    (y-or-n-p (concat "Save buffer " (buffer-name) " ")))
               (not special))
           (save-buffer)))
     (if (and (boundp 'gnuserv-buffer-clients)
              gnuserv-buffer-clients)
         (gnuserv-edit)
       (if (and (boundp 'server-buffer-clients)
                server-buffer-clients)
           (server-edit)
         (kill-buffer nil)))
     (cond (double-pfx (delete-frame))
           (single-pfx (delete-window)))))

(defun lre-switch-to-new-frame()
  "Display this buffer in a new frame"
  (interactive)
  (unless (one-window-p)
    (let ((currwin (get-buffer-window (current-buffer))))
      (switch-to-buffer-other-frame (current-buffer))
      (and currwin (delete-window currwin)))))

(defun lre-open-in-ie ()
  "Open current buffer in Internet Explorer"
  (interactive)
  (ext-element-menu-command "IE"))

(defun lre-mouse-open (evt pfx)
  "Open-file-other-window for file under mouse."
  (interactive "e\nP")
  (save-mark-and-excursion
    (mouse-set-point evt)
    (let ((name (or (thing-at-point 'filename)
                    (buffer-file-name)
                    "")))
      (setq name
            (if pfx (read-string "man " name)
                (read-file-name "Open: "
                                (file-name-directory name)
                                (file-name-nondirectory name)
                                t
                                name)))
      (if window-system
          (find-file-other-frame name)
        (find-file-other-window name)))))

(defun lre-scratch-offer-save ()
  (if (string= (buffer-name) "*scratch*")
      (setq last-nonmenu-event t
            buffer-offer-save  t)))

(defun lre-file-name-split (fname)
  "Split file name into list: \(directory name suffix\)."
  (interactive "FFile name:")
  (if (or (null fname)
          (not (stringp fname)))
      (list "" "" "")
    (let* ((f-d (file-name-directory fname))
           (f-n+s (file-name-nondirectory fname))
           (f-n (file-name-sans-extension f-n+s))
           (f-s (cond ((string= f-n+s "") "")
                      ((string= f-n+s f-n) "")
                      (t (substring f-n+s
                                    (- (- (length f-n+s) (length f-n))))))))
      (and (string= f-n "")
           (not (string= f-n+s ""))
           (setq f-n f-s
                 f-s ""))
      (and (null f-d) (setq f-d ""))
    (list f-d f-n f-s))))

(defun lre-subst-tmpl (tmpl)
  "Create new file from template"
  (interactive
   (list
    (read-file-name "Mal (*.vpl): " (concat (if (listp tplsub-file-directory)
                                                (car tplsub-file-directory)
                                              tplsub-file-directory) "/")
                    nil t)))
  (when tmpl
    (tplsub-buffer-subst tmpl "*New file from template*")
    (call-interactively 'save-buffer)))

(defun lre-alternate-file ()
  "Find file FILENAME, select its buffer, kill previous buffer.
If the current buffer now contains an empty file that you just visited
\(presumably by mistake), use this command to visit the file you really want.
Preserves the cusrsor location when reopening the same file"
  (interactive)
  (let ((fn buffer-file-name)
        (pos (point)))
    (call-interactively 'find-alternate-file)
    (if (and fn
             buffer-file-name
             (string= buffer-file-name fn))
          (goto-char pos))))

(defun lre-unique-fnam (dir &optional subdir fpfx)
  (interactive "s\ns")
  (let* ((dirpfx (concat dir
                         "/"
                         (if subdir (concat subdir "/") "")
                         (if fpfx fpfx "m")))
         (nbr 1)
         (fnam (format "%s%04d" dirpfx nbr)))
    (while (file-exists-p fnam)
      (setq nbr (1+ nbr))
      (setq fnam (format "%s%04d" dirpfx nbr)))
    fnam))

(if (lre-memb 'win32)
    (defun lre-find-file ()
      "Find file under win32"
      (interactive)
      (let ((win32-downcase-file-names t)
            (w32-downcase-file-names   t))
        (call-interactively 'find-file))))

(defun lre-dired ()
  "Dired setup."
  (define-key dired-mode-map "W" 'woman-dired-find-file))

(defun lre-enable-table()
  "Enable table-mode"
  (interactive)
  (when (lre-memb 'e22+)
    (lre-safe-require 'table)
    (if (fboundp 'table-recognize)
        (table-recognize))))


(defvar lre-save-place-menu-1 nil "1st personal save-place menu")
(defvar lre-save-place-menu-2 nil "2nd personal save-place menu")
(defvar lre-save-place-menu-3 nil "3rd personal save-place menu")


(defun lre-show-save-places ()
  (interactive)
  (find-file-other-window (expand-file-name save-place-file)))


(defun lre-save-place-hook ()
  "Copy save-place-menu to lre-menu"
  (when save-place-recent-no
    (setq lre-save-place-menu-1 (save-place-make-recent-files-menu)
          lre-save-place-menu-2 (save-place-make-recent-files-menu 'window)
          lre-save-place-menu-3 (save-place-make-recent-files-menu 'frame))
    (define-key-after (lookup-key global-map [menu-bar lre])
      [lambda-save-lre] '("--") t)
    (define-key-after (lookup-key global-map [menu-bar lre])
      [recent-1] lre-save-place-menu-1 t)
    (define-key-after (lookup-key global-map [menu-bar lre])
      [recent-2] lre-save-place-menu-2 t)
    (define-key-after (lookup-key global-map [menu-bar lre])
      [recent-3] lre-save-place-menu-3 t)))

;; better movement routines for ThisStyleOfVariablesCommonInCPlusPlus
;; originally contributed by Terry_Glanfield.Southern@rxuk.xerox.com
(defun lre-forward-caps (&optional arg)
  "Move forward to end of a nomenclature section or word.
With arg, do it arg times."
  (interactive "p")
  (let ((case-fold-search nil))
    (if (> arg 0)
        (re-search-forward "\\W*\\([A-Z]*[a-z0-9]*\\)" (point-max) t arg)
      (while (and (< arg 0)
                  (re-search-backward
                   "\\(\\(\\W\\|[a-z0-9]\\)[A-Z]+\\|\\W\\w+\\)"
                   (point-min) 0))
        (forward-char 1)
        (setq arg (1+ arg))))))

(defun lre-backward-caps (&optional arg)
  "Move backward to beginning of a nomenclature section or word.
With optional ARG, move that many times.  If ARG is negative, move
forward."
  (interactive "p")
  (lre-forward-caps (- arg)))

(defun lre-scroll-left (pfx)
  "`scroll-left' if appropriate, otherwise `lre-backward-caps'"
  (interactive "p")
  (if (or truncate-lines
          (and truncate-partial-width-windows
                      (< (window-width) (frame-width))))
      (scroll-left pfx)
    (lre-backward-caps pfx)))

(defun lre-scroll-right (pfx)
  "`scroll-right' if appropriate, otherwise `lre-forward-caps'"
  (interactive "p")
  (if (or truncate-lines
          (and truncate-partial-width-windows
                      (< (window-width) (frame-width))))
      (scroll-left pfx)
    (lre-forward-caps pfx)))

(defun lre-copy-from-above (arg)
  "Copy from previous line.  With ARG, copy rest of line, otherwise 1 char."
  (interactive "P")
  (let ((track-eol nil)
        (goal-column nil)
        p1)
    (insert
     (save-mark-and-excursion
       (forward-line -1)
       (setq p1 (point))
       (if arg (end-of-line)
         (forward-char 1))
       (buffer-substring p1 (point))))))

(defun lre-convert-slashes (p-min p-max &optional pfx)
  "Convert between forward and backward slashes.
Without prefix: / to \\.
With prefix: \\ to /."
  (interactive "*r\nP")
  (let ((from-s "/")
        (to-s "\\"))
    (if pfx (setq from-s to-s
                  to-s   "/"))
    (replace-string from-s to-s nil p-min p-max)))

;;; --------------------------------------------------------------------------
;;; /// Date/time/history ///
;;

(defvar lre-today-day nil "Today: DD")
(defvar lre-today-month nil "Today: MM")
(defvar lre-today-year nil "Today: YY")
(defvar lre-today-century nil "Today: CC")
(defvar lre-today-trans '(("Jan" . "01") ("Feb" . "02") ("Mar" . "03")
                          ("Apr" . "04") ("May" . "05") ("Jun" . "06")
                          ("Jul" . "07") ("Aug" . "08") ("Sep" . "09")
                          ("Oct" . "10") ("Nov" . "11") ("Dec" . "12"))
  "Months.")

(defun lre-todays-date(in-fmt &optional with-sign in-sep)
  "Inserts todays date.
Format (IN-FMT) is
  1: dd.mm.yy   2: dd.mm.yyyy
  3: yymmdd     4: yyyymmdd
  5: yy/mm/dd   6: yyyy/mm/dd
With prefix (WITH-SIGN), signature is added."
  (interactive "*n1=dd.mm.yy 2=1+yy 3=yymmdd 4=3+yy 5=yy/mm/dd 6=5+yy? \nP")
  (setq lre-today-day   (format "%02d"
                                (To-int
                                 (substring (current-time-string) 8 10)))
        lre-today-month (cdr (assoc (substring (current-time-string) 4 7)
                                    lre-today-trans))
        lre-today-century (format "%02d"
                                  (To-int
                                   (substring (current-time-string) 20 22)))
        lre-today-year  (format "%02d"
                                (To-int
                                 (substring (current-time-string) 22 24))))
  (let ((fmt in-fmt)
        (sep (or in-sep "")))
    (if (or (< fmt 1) (> fmt 6))
        (setq fmt 4))
    (if (string= sep "")
        (if (or (= fmt 5) (= fmt 6))
            (setq sep "/")
          (if (or (= fmt 1) (= fmt 2))
              (setq sep "."))))
    (if (or (= fmt 1) (= fmt 2))
        (insert lre-today-day   sep lre-today-month sep))
    (if (not (oddp fmt))
        (insert lre-today-century))
    (insert lre-today-year)
    (if (> fmt 2)
        (insert sep lre-today-month sep lre-today-day))
    (if with-sign (insert (format "  %-5s" lre-fe-user)))))

(defun lre-insert-sign (&optional N pfx sep)
  "Insert signature."
  (interactive "*P")
  (let ((spfx (or pfx "--- "))
        (ssep (or sep ", ")))
    (if N (insert spfx))
    (insert lre-user-sign)
    (if N (insert ssep))
    (if N (lre-todays-date 4))))

(defun lre-insert-hist-rec ()
  "Insert full history record"
  (interactive)
  (lre-todays-date 3)
  (insert "  " (format "%-7s" lre-user-sign)))

(defvar lre-history-list '("PM"
                           "Opprettet"
                           "Created"
                           "Corrected "
                           "Added "
                           "Removed "
                           "No change"
                           "Removed superfluous variables/assignments"
                           "Dummy version")
  "Record of history comments.")

(defvar lre-last-hist-no 1
  "Sequence number of last change.")

(defvar lre-history-before-newline ""
  "String to insert before newline when adding new history record")

(defun lre-history-point ()
  "LRE find history."
  (interactive)
  (setq lre-history-before-newline "")
  (let (pp)
    (goto-char (point-min))
    (search-forward-regexp
       (concat "^\\(\\*V +;\\| *\\*\\|C\\| *;\\| *#\\)"
               "\\( *\\)"
               (cond ((lre-memb 'tvist)
                      "\\(HISTORIKK:\\)")
                     (t
                      "\\(HISTORY:\\)"))
               "\\( *\\)"
               "$"))
    (goto-char (match-beginning 2))
    (setq pp (point))
    (beginning-of-line)
    (setq lre-last-hist (buffer-substring (point) pp))
    (search-forward-regexp (concat "^" (regexp-quote lre-last-hist) "$"))
    (save-mark-and-excursion
      (setq pp (point))
      (beginning-of-line 0)
      (if (search-forward-regexp "[0-9]+" pp t)
          (setq lre-last-hist-no
                (1+ (string-to-number (buffer-substring (match-beginning 0)
                                                        (match-end 0)))))))))

(defun lre-cf-history-point()
  "Find history point in cf files"
  (interactive)
  (let (pp)
    (goto-char (point-min))
    (search-forward-regexp "^#define TadHode_Historikk")
    (setq lre-last-hist    ";"
          lre-last-hist-no 1
          lre-history-before-newline " @@\\")
    (search-forward-regexp (concat "^ *$"))))

(defun lre-vsq-history-point()
  "Find history point in vsq files"
  (interactive)
  (let (pp)
    (goto-char (point-min))
    (search-forward-regexp "^\\s-*\\(Dat.\\|YYMMDD\\)\\s-+\\(Sign\\|Init\\)\\s-+\\(Besk\\|Desc\\)")
    (setq lre-last-hist    " "
          lre-last-hist-no 1
          lre-history-before-newline "")
    (search-forward-regexp "^ *\\([*]+/\\)?$")
    (beginning-of-line)))

(defun lre-sub-hist-search (usr-n org-n)
   "Search for username\", \"orgname."
  (let ((s-str (concat usr-n ", " org-n)))
    (save-mark-and-excursion
      (goto-char (point-min))
      (search-forward s-str nil t))))

(defun lre-history-insert (mess &optional hno)
  "LRE insert history record."
    (interactive (list
                (read-from-minibuffer "Comment: "
                                      (cons (car lre-history-list) 1)
                                      nil nil '(lre-history-list . 1))
                "p"))
    (if (string-match ".*[.]xut$" (or (buffer-file-name) "nix"))
        (lre-xut-histrec mess)
      (cond ((eq major-mode 'cf-mode)
             (lre-cf-history-point))
            ((eq major-mode 'vsq-mode)
             (lre-vsq-history-point))
            (t
             (lre-history-point)))
      (end-of-line 0)
      (insert (format "%s\n%s " lre-history-before-newline lre-last-hist))
      (lre-todays-date 3 t)
      (insert (format "  %s" mess))))

;;; --------------------------------------------------------------------------
;;; /// gnuserv ///
;
;;  (defadvice server-make-window-visible
;;    (after lre-server-make-window-visible-before activate)
;;    "Raise also on NT..."
;;    (cond ((fboundp 'raise-frame)
;;           (raise-frame (selected-frame)))
;;          ((fboundp 'deiconify-screen)
;;           (deiconify-screen (selected-screen))
;;           (raise-screen (selected-screen)))
;;          ((fboundp 'mapraised-screen)
;;           (mapraised-screen))
;;          ((fboundp 'x-remap-window)
;;           (x-remap-window)
;;           ;; give window chance to re-display text
;;           (accept-process-output))))

(defvar lre-server-file nil
  "Semaphore file for server function.")

(defun lre-server-end ()
  (if (and lre-server-file
           (file-exists-p lre-server-file))
      (delete-file lre-server-file)))

(defun lre-server-start ()
  (cond ((not (lre-memb 'server)) t)
        (t
         (if lre-gnuserv-server
             (progn
               (require 'gnuserv)
               (gnuserv-start))
           (require 'server)
           (when (lre-memb 'e23+)
                (defun server-ensure-safe-dir (dir) "Noop" t))
           (server-start)
           (setq lre-server-file
                 (or (getenv "EMACS_SERVER")
                     (expand-file-name server-name server-auth-dir)))
           (add-hook 'kill-emacs-hook 'lre-server-end))
         (setq minor-mode-alist
               (cons '(server-buffer-clients " Srv")
                     (delq (assoc 'server-buffer-clients minor-mode-alist)
                           minor-mode-alist))))))

(defsubst lre-server-raise()
  (make-frame-visible))

(if (lre-memb 'old-gnunix-where-gnuserv-didnt-work)
    (add-hook 'server-switch-hook 'lre-server-raise))

(defvar lre-initial-frame (car (frame-list)))

(defun lre-focus-frame(frame)
  "Raise frame and give it focus"
  (make-frame-visible frame)
  (raise-frame frame)
  (select-frame frame)
  (if (lre-memb 'win32)
      (if (lre-memb 'e23+)
          (x-focus-frame frame)
        (w32-focus-frame frame))))

(defsubst lre-focus-initial-frame ()
  "Focus on `lre-initial-frame'"
  (lre-focus-frame lre-initial-frame))

;;; --------------------------------------------------------------------------
;;; /// VC/SPE/SCCS ///
;

(defvar lre-vc-buf-ex-p nil
  "Buffer-local, tells whether buffer was executable on check-out.")
(make-variable-buffer-local 'lre-vc-buf-ex-p)
(put 'lre-vc-buf-ex-p 'permanent-local t)

(defun lre-vc-checkin-hook-fun ()
"Offer to change file mode to executable."
  (if (and lre-vc-buf-ex-p
           (y-or-n-p (format "Change %s to executable? " (buffer-name))))
      (shell-command (concat "chmod +x " (buffer-file-name)))))

;;(defadvice vc-next-action (around lre-ad-vc-next-action activate)
;;  "Add test for executable-p, setup \`lre-vc-buf-ex-p\' to offer chmoding
;;whenever necessary"
;;  (if (not buffer-read-only) ()
;;      (setq lre-vc-buf-ex-p (file-executable-p (buffer-file-name))))
;;  (ad-do-it)
;;  (if (not buffer-read-only)
;;      (lre-vc-checkin-hook-fun)))

(defun lre-vc-next-action (verbose)
  "Hook for vc-next-action."
  (interactive "P")
  (and buffer-read-only
       (setq lre-vc-buf-ex-p (file-executable-p (buffer-file-name))))
  (vc-next-action verbose)
  (or buffer-read-only
      (lre-vc-checkin-hook-fun)))

(defsubst lre-vc-no-action (verbose)
  "Replacement for vc-next-action."
  (interactive "P")
  (error "lre: This command is not available"))

(defsubst lre-load-sccs ()
  "Load SPE SCCS."
  (and (lre-safe-require 'spe-sccs) (spe-sccs-mode 1)))

(defsubst lre-wversion ()
  "LRE SCCS-string."
  (interactive "*")
  (insert sccs-prefix-string)
  (save-mark-and-excursion
    (insert " " sccs-suffix-string)))

(defun lre-merge (fCreate fOld fNew &optional fAncestor)
  "Merge FOLD and FNEW, with optional ancester FANCESTOR.
The result file is associated with FCREATE."
  (if fAncestor
      (ediff-merge-files-with-ancestor fOld fNew fAncestor nil fCreate )
     (ediff-merge-files fOld fNew nil fCreate)))


;;; --------------------------------------------------------------------------
;;; // Shell ///
;;

(defvar lre-cygwin-init nil "Cygwin init er kjørt")

(defun lre-cygwin ()
  "Setup for cygwin"
  (when (and (lre-memb 'personal)
             (lre-memb 'win32)
             (string-match "cygwin" (getenv "PATH")))
    ;; explicit-bash-args '("-login" "-i")
    ;; w32-quote-process-args ?\"
    (setq explicit-shell-file-name "bash.exe"
          shell-file-name explicit-shell-file-name)
    (setenv "SHELL" shell-file-name)
    (setenv "CYGWIN" "nodosfilewarning")
    (unless lre-cygwin-init
      (and (lre-safe-require 'cygwin-mount)
           (cygwin-mount-activate))
      (add-hook 'comint-output-filter-functions
                'shell-strip-ctrl-m nil t)
      (add-hook 'comint-output-filter-functions
                'comint-watch-for-password-prompt nil t))))


;;; --------------------------------------------------------------------------
;;; // Printing - again ///
;;

(defun lre-print-file (fnam)
  "Load file, print it and close it again."
  (let* ((save-place nil)
         (recentf-mode nil)
         (filename   (abbreviate-file-name (expand-file-name fnam)))
         (buf        (find-file-noselect filename)))
    (set-buffer buf)
    (font-lock-fontify-buffer)
    (ps-print-buffer-with-faces)
    (kill-buffer buf)))

(defun lre-print-greip (p)
  "Utskrift av 1 del av Sysdul kildekode i greipformat.
Default del 1 - numerisk prefiks avgjør hvilken."
  (interactive "p")
  (if (not (lre-is-greip-p))
      (ps-print-buffer-with-faces)
    (let ((curr-buf (current-buffer))
          (dirpart (concat "(" (ps-header-dirpart) ")"))
          (filepart (format "(%s[%d])" (ps-get-buffer-name) p))
          (cmd (format "greip %d %s" p buffer-file-name)))
      (save-some-buffers)
      (set-buffer (get-buffer-create "*greiprint*"))
      (erase-buffer)
      (shell-command cmd t)
      (sysdul-mode)
      (if (lre-memb 'flock)
        (font-lock-fontify-buffer))
      (set-buffer-modified-p nil)
      (let ((ps-left-header (list filepart dirpart)))
        (ps-print-buffer-with-faces))
      (set-buffer curr-buf)
      (kill-buffer "*greiprint*"))))

(defun lre-print-2up (&optional duplex)
  "Print 2 (or 4, with prefix) pages on one sheet."
  (interactive "P")
  (if (not (boundp 'ps-landscape-mode))
      (error "This function requires a newer ps-print.el")
    (let ((ps-landscape-mode    t)
          (ps-number-of-columns 2)
          (ps-spool-duplex      duplex)
          (ps-lpr-command       (concat ps-lpr-command lre-duplex-switch)))
      (ps-print-buffer-with-faces))))

(defun lre-print ()
  "Print according to settings."
  (interactive)
  (let* ((ps-landscape-mode lre-ps-use-landscape)
         (ps-lpr-command lre-ps-print-cmd)
         (ps-lpr-switches lre-ps-print-cmd-opt)
         (ps-spool-duplex lre-ps-use-duplex)
         (ps-zebra-stripes lre-ps-use-zebra)
         (ps-zebra-stripe-height (or lre-ps-use-zebra 1))
         (ps-n-up-printing lre-ps-use-n-up)
         (ps-printer-name lre-ps-printer-name)
         (ps-line-number lre-ps-use-line-no)
         (ps-line-number-step lre-ps-use-line-no)
         (ps-line-number-start (if (numberp lre-ps-use-line-no)
                                   lre-ps-use-line-no
                                 1))
         cmd-name
         use-reg)
    (setq use-reg (and mark-active
                       (not lre-ps-ignore-region)
                       (y-or-n-p "Print only current region? ")))
    (if (and (not use-reg)
             (lre-is-greip-p))
        (lre-print-greip 1)
      (setq cmd-name (concat "(ps-"
                             (if lre-ps-use-spool "spool-" "print-")
                             (if use-reg "region" "buffer")
                             (if lre-ps-use-faces "-with-faces" "")
                             (if use-reg (format " %d %d"
                                                 (min (point) (mark))
                                                 (max (point) (mark))) "")
                             ")"))
      (eval (read cmd-name)))))

(defun lre-print-setup ()
  "Configure printer settings"
  (interactive)
  (if (lre-memb 'e21+)
      (let ((custom-unlispify-menu-entries t)
            (custom-unlispify-remove-prefixes t))
        (customize-group 'lre-ps-group))))

;;; --------------------------------------------------------------------------
;;; /// Misc. functions ///
;

(defun lre-fixup-wordpress()
  "* Fixup Word2WordPress-output"
  (interactive)
  (goto-char (point-min))
  (save-mark-and-excursion (query-replace "[" "&#91;"))
  (save-mark-and-excursion (query-replace "]" "&#93;"))
  (save-mark-and-excursion
    (when (search-forward "code lang=X" nil t)
      (goto-char (point-min))
      (let ((lang (read-from-minibuffer "Kodespråk: " "java")))
        (query-replace "code lang=X"
                       (concat "code lang=\"" lang "\""))))))

(defun lre-disp-macro (sym)
  "Denne funksjonen har betastatus... Den prøver å vise en makrodefinisjon...
Dvs.:
- Finn symbolet cursor står ved.
- Let etter en definisjon (*V MACRO / @LOCAL@ / #define) av denne i
  (a) Sist lagrete utgave av kildefila
  (b) Alle svappfiler under $PROJ_HOME/src/svapp
  (c) Alle cf-filer under $SPE_HOME/maler
  (d) Alle hql-filer under $PROJ_HOME/src/sql
- For hver funnet forekomst, vis *avsnittet* definisjonen finnes i.
  For makrofiler uten blanke linjer, kan dette bli ganske mye..."
  (interactive (list (symbol-near-point)))
  (if (not (and sym (length sym)))
      (error "No symbol found!")
    (let* ((nsym (cond ((string-match "^X_" sym) (substring sym 2))
                       ((string-match "^X"  sym) (substring sym 1))
                       (t sym)))
           (p-home (getenv "PROJ_HOME"))
           (s-home (getenv "SPE_HOME"))
           (svpdir (concat p-home "/src/svapp/"))
           (sqldir (concat p-home "/src/sql/"))
           (s-cmd (concat "egrep -i -p "
                          "\'^(\\*V +MACRO +| *@LOCAL@_*|#define +)"
                          nsym
                          "\' "
                          (or buffer-file-name "")
                          " "
                          s-home "/maler/*cf"
                          " "
                          svpdir "*.svp"
                          " "
                          svpdir "[a-z]*/*.svp"
                          " "
                          sqldir "*.hql"
                          )))
      (let ((currbuf (current-buffer))
            (case-fold-search t))
        (pop-to-buffer (get-buffer-create "*DefMacro*"))
        (erase-buffer)
        (insert "MACRO: " sym "\n\n")
        (shell-command s-cmd t)
        (goto-char (point-min))
        (search-forward (downcase sym) nil t 2)
        (set-buffer-modified-p nil)
        (pop-to-buffer currbuf)))))

(defun lre-which-function ()
  "Echo name of current function."
  (interactive)
  (and (fboundp 'which-function)
       (message "%s" (which-function))))

(defun lre-load-spell-number()
  "Load functions to spell out numbers as text"
  (interactive)
  (load "spell-number"))

(defun lre-script-conv ()
  "Convert from 7-bit to 8-bit XML, replace script name"
  (interactive)
  (save-mark-and-excursion
    (goto-char (point-min))
    (save-mark-and-excursion (brace-swap-norwegian t))
    (save-mark-and-excursion (brace-swap-ml nil))
    (save-mark-and-excursion (query-replace "SPE" "\&System;"))
    (let ((script (read-string "Script name (or blank): "
                               (file-name-sans-extension (buffer-name)))))
      (if script
          (query-replace script "\&I;")))))

(defun lre-sort-lines-nocase (reverse beg end)
  "Case-insensitive `sort-lines'"
  (interactive "*P\nr")
  (let ((sort-fold-case t))
    (sort-lines reverse beg end)))

(defun lre-indent-or-expand (arg)
  "Either indent according to mode, or expand the word preceding point."
  (interactive "*P")
  (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp) (not (= ?w (char-syntax (char-after))))))
      (hippie-expand arg)
    (indent-according-to-mode)))

(when (lre-memb 'e23+)
  (defun lre-show-timers ()
    "Show misc. timings"
    (interactive)
    (with-output-to-temp-buffer "*TIMINGS*"
      (princ "Init: ")
      (princ (emacs-init-time))
      (princ "\t")
      (princ "Uptime: ")
      (princ (emacs-uptime)))))

(defun lre-rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  ;; http://whattheemacsd.com/
  (interactive)
  (if (fboundp 'rename-visited-file)
      (call-interactively 'rename-visited-file)
    (let ((name (buffer-name))
          (filename (buffer-file-name)))
      (if (not (and filename (file-exists-p filename)))
          (error "Buffer '%s' is not visiting a file!" name)
        (let ((new-name (read-file-name "New name: " filename)))
          (if (get-buffer new-name)
              (error "A buffer named '%s' already exists!" new-name)
            (rename-file filename new-name 1)
            (rename-buffer new-name)
            (set-visited-file-name new-name)
            (set-buffer-modified-p nil)
            (message "File '%s' successfully renamed to '%s'"
                     name (file-name-nondirectory new-name))))))))

(defun lre-delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  ;; http://whattheemacsd.com/
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

; ---------------------------------------------------------------------
; Utdaterte tvist-funksjoner

(defun tvist-lazy-lock ()
  (setq lazy-lock-walk-windows nil
        lazy-lock-stealth-time 300
        lazy-lock-stealth-lines nil
        lazy-lock-stealth-nice 1))

; -----------------------------------------------------------------------
;; Misc proposals

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I don't know if anyone else will find this useful, but here's the regexp
;; that will parse the errors generated by the JVC compiler.  Just add this to
;; your compilation-error-regexp-alist:
;;     ;; Microsoft JVC:
;;     ;;sample.java(6,1) : error J0020: Expected 'class' or 'interface'
;;     ("\n\\(\\([a-zA-Z]:\\)?[^:( \t\n-]+\\)[:(][
;; \t]*\\([0-9]+\\)[,]\\([0-9]+\\)[) \t]" 1 3 5)
;; Now you can jump to the errors as normal... (^X-`)
;;     bmerrill@microsoft.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Jonathan Payne    jpayne@marimba.com
;; (setq compilation-error-regexp-alist
;;   '(
;;     ;; UNIX utilities, compiler, Javac, grep, etc
;;     ("\n\
;; \\([a-zA-Z]?:?[^:( \t\n]+\\)[:(][ \t]*\\([0-9]+\\)\\([) \t]\\|\
;; :\\([^0-9\n]\\|\\([0-9]+:\\)\\)\\)" 1 2 5)
;;
;;     ;; Microsoft C/C++, symantec
;;     ("\n\\(\\([a-zA-Z]:\\)?[^:( \t\n-]+\\)[:(][ \t]*\\([0-9]+\\)[:) \t]"
;; 1 3)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      (defun sds-word-count (start end)
;;        ;; replacement for count-lines-region
;;        "Count lines/words/characters from START to END"
;;        (interactive "r")
;;        (save-mark-and-excursion
;;          (save-restriction
;;            (narrow-to-region start end)
;;            (goto-char (min start end))
;;            (message "Region (%d to %d) has: lines: %d; words: %d;
;;      characters: %d."
;;                start end (count-lines start end)
;;                (string-to-number (how-many "\\<")) (- end start)))))
;;   sshteingold@cctrading.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; From David J. Biesack [sasdjb@unx.sas.com]
;; (defun sgml-matching-tag ()
;;   "Find the matching tag for the current tag.
;; Invoke this from the opening < or within the tag name itself."
;;   (interactive)
;;   (let ((case-fold-search t)
;;         tag on-tag-open regexp
;;         (nesting 1))
;;     (or (looking-at "<") (search-backward "<" nil t))
;;     (cond ((looking-at "<\\(/?\\)\\(\\w+\\)")
;;            (setq on-tag-open (= (match-beginning 1) (match-end 1))
;;                  tag  (buffer-substring-no-properties (match-beginning 2) (match-end 2))
;;                  regexp (concat "</?\\(" tag "\\b\\)"))
;;            (cond (on-tag-open
;;                   (goto-char (match-end 2)) ; <strong> bold text  </strong>
;;                   (let (begin)
;;                     (while (and (> nesting 0) (re-search-forward regexp nil t))
;;                       (setq begin (match-beginning 0))
;;                       (goto-char begin)
;;                       (cond ((looking-at "<\\w")
;;                              (setq nesting (1+ nesting)))
;;                             (t               ; must be an end dag
;;                              (setq nesting (1- nesting))))
;;                       (goto-char begin))
;;                     ))
;;                  (t ; currently on an end tag
;;                   (goto-char (match-beginning 2))
;;                   (while (and (> nesting 0) (re-search-backward regexp nil t))
;;                     (cond ((looking-at "<\\w")
;;                            (setq nesting (1- nesting)))
;;                           (t ; must be an end dag
;;                            (setq nesting (1+ nesting))))
;;                     )
;;                   )))
;;           (t (error "Invoke html-matching-tag from the opening < or the tag name")))))
;; (defalias 'html-matching-tag 'sgml-matching-tag)
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; From:  Jason Rumney
;; Sent: Thursday, June 03, 1999 10:16 AM
;; Subject: Re: How to add/remove items from the standard menu
;;
;; ;; Replace "Open New Display..." with "Push to Back"
;; (define-key menu-bar-files-menu [make-frame-on-display]
;;    '("Push to Back" . lower-frame))
;;
;; ;; Rename "Read Net News" to "Read Mail/News"
;; (define-key menu-bar-tools-menu [gnus]
;;   '("Read Mail/News" . gnus))
;;
;; ;; Remove "Read Mail"
;; (define-key menu-bar-tools-menu [rmail] nil)
;;
;; ;; Add "Hexlify" to Tools menu
;; (define-key menu-bar-tools-menu [hexl]
;;   '("Hexlify" . hexlify-buffer))
;;
;; (defun faces-info ()
;;   "Returns information of all faces as a list where each element
;; is comprised of the following face attributes:
;;
;;   face foreground-color background-color bold italic"
;;   (interactive)
;;   (let  ((faces (sort (face-list) (function string-lessp))))
;;     (mapcar (lambda (f) (list f
;;                               (face-foreground f)
;;                               (face-background f)
;;                               (face-bold-p f)
;;                               (face-italic-p f))) faces)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  (defun favorites-write-url (url-name)
;;    (interactive "sName For Favorite File: ") ; By Galen Boyer
;;    (save-excursion
;;      (switch-to-buffer url-name)
;;      (insert "[DEFAULT]\nBASEURL=")
;;      (yank)
;;      (insert "\n[InternetShortcut]\nURL=")
;;      (yank)
;;      (insert "\nModified=C0649490B1E9BE01A1")
;;      (setq favorites-dir
;;            (concat  "c:/Documents And Settings/" (user-login-name)
;;  "/favorites/"))
;;      ;; Need to make this work on all versions of windows.
;;                                          ;     (setq favorites-dir
;;                                          ;     (concat (getenv
;;  "WINDIR") "/profiles/" (user-login-name) "/favorites/"))
;;      (write-file (concat favorites-dir url-name ".url"))
;;      (kill-buffer (concat url-name ".url"))
;;      (dired favorites-dir)
;;      (beginning-of-buffer)
;;      (revert-buffer)
;;      (nonincremental-search-forward url-name)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun my-shell-setup ()
;;    "For Cygwin bash under Emacs 20"
;;    (setq comint-scroll-show-maximum-output 'this
;;          comint-completion-addsuffix       t
;;          comint-eol-on-sendt               t
;;          w32-quote-process-args            ?\"
;;          binary-process-input              t
;;          shell-file-name                   "bash"
;;          explicit-shell-file-name          shell-file-name
;;          explicit-sh-args                  '("-login" "-i"))
;;    (setenv "SHELL" shell-file-name)
;;    (make-variable-buffer-local 'comint-completion-addsuffix))
;; (setq-shell-mode-hook 'my-shell-setup)






;;; --------------------------------------------------------------
;;; /// Hooks etc ///
;;;
; (add-hook 'awk-mode-hook 'lre-c-mode)
; (add-hook 'c++-mode-hook 'lre-c++-mode)

(defvar lre-mousefix-mode-list nil
  "List of modes in which to fix mouse bindings")

(defvar lre-mouse-2-keys
  (list
   [mouse-2]
   [S-mouse-2]
   [C-mouse-2]
   [M-mouse-2]
   [down-mouse-2]
   [S-down-mouse-2]
   [C-down-mouse-2]
   [M-down-mouse-2]
   [double-mouse-2]
   [triple-mouse-2]
   [vertical-line mouse-2]
   [vertical-line S-mouse-2]
   [vertical-line C-mouse-2]
   [vertical-line M-mouse-2]
   [vertical-scroll-bar mouse-2]
   [vertical-scroll-bar S-mouse-2]
   [vertical-scroll-bar C-mouse-2]
   [vertical-scroll-bar M-mouse-2]
   [mode-line mouse-2]
   [mode-line S-mouse-2]
   [mode-line C-mouse-2]
   [mode-line M-mouse-2]
   [header-line mouse-2]
   [header-line S-mouse-2]
   [header-line C-mouse-2]
   [header-line M-mouse-2]
   [vertical-line down-mouse-2]
   [vertical-line S-down-mouse-2]
   [vertical-line C-down-mouse-2]
   [vertical-line M-down-mouse-2]
   [vertical-scroll-bar down-mouse-2]
   [vertical-scroll-bar S-down-mouse-2]
   [vertical-scroll-bar C-down-mouse-2]
   [vertical-scroll-bar M-down-mouse-2]
   [mode-line down-mouse-2]
   [mode-line S-down-mouse-2]
   [mode-line C-down-mouse-2]
   [mode-line M-down-mouse-2]
   [header-line down-mouse-2]
   [header-line S-down-mouse-2]
   [header-line C-down-mouse-2]
   [header-line M-down-mouse-2]
   )
  "Remappable mouse-2 events")

(defvar lre-mouse-3-keys
  (list
   [mouse-3]
   [S-mouse-3]
   [C-mouse-3]
   [M-mouse-3]
   [down-mouse-3]
   [S-down-mouse-3]
   [C-down-mouse-3]
   [M-down-mouse-3]
   [double-mouse-3]
   [triple-mouse-3]
   [vertical-line mouse-3]
   [vertical-line S-mouse-3]
   [vertical-line C-mouse-3]
   [vertical-line M-mouse-3]
   [vertical-scroll-bar mouse-3]
   [vertical-scroll-bar S-mouse-3]
   [vertical-scroll-bar C-mouse-3]
   [vertical-scroll-bar M-mouse-3]
   [mode-line mouse-3]
   [mode-line S-mouse-3]
   [mode-line C-mouse-3]
   [mode-line M-mouse-3]
   [header-line mouse-3]
   [header-line S-mouse-3]
   [header-line C-mouse-3]
   [header-line M-mouse-3]
   [vertical-line down-mouse-3]
   [vertical-line S-down-mouse-3]
   [vertical-line C-down-mouse-3]
   [vertical-line M-down-mouse-3]
   [vertical-scroll-bar down-mouse-3]
   [vertical-scroll-bar S-down-mouse-3]
   [vertical-scroll-bar C-down-mouse-3]
   [vertical-scroll-bar M-down-mouse-3]
   [mode-line down-mouse-3]
   [mode-line S-down-mouse-3]
   [mode-line C-down-mouse-3]
   [mode-line M-down-mouse-3]
   [header-line down-mouse-3]
   [header-line S-down-mouse-3]
   [header-line C-down-mouse-3]
   [header-line M-down-mouse-3]
   )
  "Mappable mouse-3 events")

(defun lre-swap-mouse-2-3 ()
  (let (b2 b3 l2 l3)
    (setq b2 (mapcar 'key-binding lre-mouse-2-keys)
          b3 (mapcar 'key-binding lre-mouse-3-keys)
          l2 lre-mouse-2-keys
          l3 lre-mouse-3-keys)
    (mapc (function
           (lambda (ev)
             (if ev (local-set-key (car l3) ev))
             (setq l3 (cdr l3))))
          'b3)
    (mapc (function
           (lambda (ev)
             (if ev (local-set-key (car l2) ev))
             (setq l2 (cdr l2))))
          'b2)))

(defun lre-mouse-swap-bindings()
  "Swap mouse-2 and mouse-3 bindings"
;; TODO !!!!!!!!!!!!!!!!!
;; w32-num-mouse-buttons
;; w32-swap-mouse-buttons
;; w32-mouse-button-tolerance
)
(when (lre-memb-all 'mouse 'keys 'ikkeKlartEnda)
  (mapc (function
         (lambda (mod-n)
           (add-hook (intern (concat (symbol-name mod-n) "-hook"))
                     'lre-mouse-swap-bindings)
           ))
        lre-mousefix-mode-list))

(if LRE-autocomplete-pkg (lre--setup-ac))
(if LRE-multiple-cursors-pkg (lre--setup-mc))

(add-hook 'Man-mode-hook 'lre-man-mode)
(add-hook 'c++-mode-hook 'lre-c++-mode)
(add-hook 'cal-mode-hook 'lre-cal-mode)
(add-hook 'cf-mode-hooks 'lre-cf-mode)
(add-hook 'cf-mode-hook 'lre-cf-mode)
(add-hook 'change-log-mode-hook 'lre-fontify 'append)
(add-hook 'csh-mode-hook 'lre-csh-mode)
(add-hook 'css-mode-hook 'lre-css-mode)
(add-hook 'dired-mode-hook 'lre-dired)
(add-hook 'emacs-lisp-mode-hook 'lre-lisp-mode)
;; (add-hook 'fe-mode-hook 'lre-fe-mode)
(if (lre-memb 'fume)
    (add-hook 'find-file-hooks 'fume-add-menubar-entry))
(add-hook 'first-change-hook 'lre-scratch-offer-save)
(add-hook 'gnus-load-hook 'lre-gnus-start)
(add-hook 'gnus-startup-hook 'lre-gnus-mode)
(add-hook 'help-mode-hook 'lre-help-mode)
(add-hook 'html-helper-mode-hook 'lre-html-mode)
(add-hook 'html-mode-hook 'lre-html-mode)
(add-hook 'htmlize-after-hook 'lre-htmlize-notab)
(add-hook 'ksh-mode-hook 'lre-ksh-mode)
(if (and (lre-memb 'TAD) (not (lre-memb 'win32)))
    (add-hook 'ksh-mode-hook 'lre-load-sccs))
(add-hook 'Info-mode-hook 'lre-info-mode)
(add-hook 'makefile-mode-hook 'lre-makefile-mode)
;; ??? (add-hook 'minibuffer-setup-hook 'iswitchb-minibuffer-setup)
(add-hook 'markdown-mode-hook 'lre-markdown-mode)
(add-hook 'nroff-mode-hook 'lre-nroff-mode)
(add-hook 'nxml-mode-hook 'lre-nxml-mode)
(add-hook 'psgml-mode-hook 'lre-sgml-keys)
(add-hook 'psgml-mode-hook 'lre-psgml-mode)
(add-hook 'pxml-mode-hook 'lre-pxml-mode)
(add-hook 'save-place-recent-hooks 'lre-save-place-hook)
(if (fboundp 'save-place-mode) (save-place-mode 1))
(add-hook 'shell-mode-hook 'lre-cygwin)
(add-hook 'sh-mode-hook 'lre-shell-mode)
(add-hook 'sgml-mode-hook 'lre-sgml-keys)
(add-hook 'sgml-mode-hook 'lre-sgml-mode)
(add-hook 'sql-mode-hook (if (lre-memb 'e21+) 'lre-sql-mode-21
                           'lre-sql-mode))
(add-hook 'sql-interactive-mode-hook 'lre-sql-mode-21i)
(when (and (lre-memb 'TAD) (not (lre-memb 'win32)))
    (add-hook 'sql-mode-hook 'lre-load-sccs)
    (add-hook 'sysdul-mode-hook 'lre-load-sccs))
(add-hook 'sysdul-mode-hook 'lre-sysdul-mode)
(add-hook 'text-mode-hook 'lre-text-mode)
(or (lre-memb 'win32)
    (add-hook 'vc-checkin-hook 'lre-vc-checkin-hook-fun))
(add-hook 'vm-mode-hook 'lre-vm-mode)
(add-hook 'wml-mode-hook 'lre-wml-mode)
(add-hook 'xml-mode-hook 'lre-xml-mode)
(add-hook 'xsl-mode-hook 'lre-xsl-mode)
(lre-server-start)

(when (file-exists-p lre-load-all-dir)
  (mapc 'load (directory-files lre-load-all-dir nil "[.]el$")))

(message "loading done")

(provide 'lresetup)

;;; lresetup.el ends here
