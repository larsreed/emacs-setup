;;; woman.el --- browse UN*X manual pages `wo (without) man'

;; Copyright (C) 1997, 1998 Francis J. Wright

;; Author: Francis J. Wright <F.J.Wright@Maths.QMW.ac.uk>
;; Maintainer: Francis J. Wright <F.J.Wright@Maths.QMW.ac.uk>
;; Version: 0.32 (beta), 2 April 1998
;; Keywords: man, help, UN*X, manual

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; WARNING: The variable `man-path' has been renamed `woman-manpath'.
;; If you set the former then please change it to the latter.  The
;; former is still accepted, but the compatibility code will disappear
;; in some future version!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Read on for (currently) the only documentation for WoMan!

;; See also the documentation for the WoMan interactive commands and
;; user option variables, all of which begin with the prefix `woman-'.
;; This can be done most easily by loading WoMan and then running the
;; command `woman-mini-help', or selecting the WoMan menu option `Mini
;; Help' when WoMan is running.

;; This is work in progress!  Please let me know what doesn't work --
;; I am adding functionality as testing shows that it is necessary.
;; See below for guidance on reporting bugs.

;; The latest versions of this (and related) files are available from
;; the URL

;;   http://www.maths.qmw.ac.uk/~fjw/public_emacs/

;; This file is not (currently) part of GNU Emacs.

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


;;; Commentary:
;;; ==========

;; WoMan implements a subset of the formatting performed by the Emacs
;; `man' (or `manual-entry') command to format a UN*X manual `page'
;; for display, but without calling any external programs.  It is
;; intended to emulate the whole of the -man macro package, plus those
;; ?roff requests that are most commonly used in man pages.  However,
;; the emulation is modified to include the reformatting done by the
;; Emacs `man' command.  No hyphenation is performed.

;; Advantages

;;   much more direct, does not require any external programs.

;; Disadvantages

;;   not a complete emulation, may be slower.  Currently no support
;;   for eqn or tbl.

;; This browser works quite well on simple well-written man files.  It
;; works less well on idiosyncratic files that `break the rules' or
;; use the more obscure ?roff requests directly.  Current test results
;; are available in the file woman.status.

;; I support WoMan only with GNU Emacs versions 19.34 and later
;; (specifically the current principal distributed version of NTEmacs
;; under Microsoft Windows 95).

;; WoMan supports the use of gzipped man files by explicitly calling
;; gunzip, which is assumed to be in your exec path, unless
;; `auto-compression-mode' is on, in which case the decompression will
;; be handled implicitly (in much the same way, but by different
;; code).


(require 'man)
(eval-when-compile			; to avoid compiler warnings
  (require 'jka-compr)
  (require 'dired))


;; Recommended use
;; ===============

;; Put this library somewhere in your load-path and byte-compile it.
;; Put this in your .emacs:
;;   (autoload 'woman "woman"
;;             "Decode and browse a UN*X man page." t)
;;   (autoload 'woman-find-file "woman"
;;             "Find, decode and browse a specific UN*X man-page file." t)

;; Then either (1 -- *RECOMMENDED*): If the `MANPATH' environment
;; variable is set then WoMan will use it; otherwise you may need to
;; reset the Lisp variable `woman-manpath', and you may also want to
;; set the Lisp variable `woman-path'.  Please see the online
;; documentation for these variables.  Now you can execute the
;; extended command `woman', enter or select a manual entry topic,
;; using completion, and if necessary select a filename, using
;; completion.  WoMan suggests the word nearest to the cursor in the
;; current buffer as the topic.

;; Or (2): Execute the extended command `woman-find-file' and enter a
;; filename, using completion.  This mode of execution may be useful
;; for temporary files outside the standard UN*X manual directory
;; structure.

;; Or (3): Put the next two sexpr's in your .emacs:
;; (autoload 'woman-dired-find-file "woman"
;;   "In dired, run the WoMan man-page browser on this file." t)
;; (add-hook 'dired-mode-hook
;; 	  (function
;; 	   (lambda ()
;; 	     (define-key dired-mode-map "W" 'woman-dired-find-file))))
;; and open the directory containing the man page file using dired,
;; put the cursor on the file, and press `W'.

;; In each case, the result should (!) be a buffer in Man mode showing
;; a formatted manual entry.  When called from WoMan, Man mode should
;; work as advertised, but modified where necessary in the context of
;; WoMan.  (However, `Man' will still invoke the standard Emacs
;; manual-browsing facility rather than `WoMan' -- this is
;; intentional!)

;; (By default, WoMan will automatically define the dired keys "W" and
;; "w" when it loads, but only if they are not already defined.  This
;; behaviour is controlled by the user option `woman-dired-keys'.
;; Note that the `dired-x' (dired extra) package binds
;; `dired-copy-filename-as-kill' to the key "w" (as pointed out by Jim
;; Davidson), although "W" appears to be really unused.  The `dired-x'
;; package will over-write the WoMan binding to "w", whereas (by
;; default) WoMan will not overwrite the `dired-x' binding.)

;; If you put the following in your .emacs then you can call WoMan via
;; the standard Help menu without the need to call it first via the
;; keyboard:

(define-key-after
  ;; Repeated calls do not seem to matter!
  (lookup-key global-map [menu-bar help-menu])
  [woman] '("WoMan..." . woman) 'man)


;; The following is based on suggestions by Guy Gascoigne-Piggford and
;; Juanma Barranquero.  If you really want to square the man-woman
;; circle then you might care to define the following bash function in
;; .bashrc:

;;   man() { gnudoit -q '(raise-frame (selected-frame)) (woman' \"$1\" ')' ; }

;; If you use Microsoft COMMAND.COM then you can create a file called
;; man.bat somewhere in your path containing the two lines:

;;   @echo off
;;   gnudoit -q (raise-frame (selected-frame)) (woman \"%1\")

;; and then (e.g. from a command prompt or the Run... option in the
;; Start menu) just execute

;;   man man_page_name


;; Face colours
;; ============

;; By default, if face support is available then WoMan uses the
;; default bold and italic faces unless they are identical to the
;; default face, in which case it uses blue and red faces
;; respectively.  You can change this behaviour as follows.

;; If the variable `woman-always-colour-faces' (NB: British spelling!)
;; is set to a non-nil value (by default it is `nil') *WHEN WOMAN
;; LOADS* then it will colour the bold and italic faces respectively
;; blue and red even if they already differ from the default face.
;; The bold and italic face colouring can be changed after WoMan is
;; loaded by running the commands `woman-colour-faces' and
;; `woman-black-faces', which are also available via the WoMan menu.

;; Alternatively, you can set the variables `woman-italic-face' and
;; `woman-bold-face' to whatever faces you wish, e.g. as follows:

;;    (copy-face 'italic 'woman-italic-face)
;;    (set-face-foreground 'woman-italic-face "Red")
;;    (copy-face 'bold 'woman-bold-face)
;;    (set-face-foreground 'woman-bold-face "Blue")

;; See also the online documentation for the above variables and
;; functions.


;; Hooks, Customization and Imenu
;; ==============================

;; WoMan currently runs two hooks: `woman-pre-format-hook' immediately
;; before formatting a buffer and `woman-post-format-hook' immediately
;; after formatting a buffer.  These hooks can be used for special
;; customizations that require code to be executed, etc., although
;; most customization should be possible by setting WoMan user option
;; variables, e.g. in `.emacs' and should NOT require the use of the
;; hooks.  `woman-pre-format-hook' might be appropriate for face
;; customization, whereas `woman-post-format-hook' might be
;; appropriate for installing a dynamic menu using `imenu' (although
;; it is better to use the built-in WoMan imenu support).

;; The WoMan menu provides an option to make a contents menu for the
;; current man page (using imenu).  Alternatively, if you set the
;; variable `woman-imenu' to `t' then WoMan will do it automatically
;; for every man page.  The menu title is the value of the variable
;; `woman-imenu-title', which is "CONTENTS" by default.  By default,
;; the menu shows manual sections and subsections, but you can change
;; this by changing the value of `woman-imenu-generic-expression'.
;; This facility is not yet widely tested and may be fooled by obscure
;; man pages that `break the rules'.

;; WoMan is configured not to replace spaces in an imenu *Completion*
;; buffer.  For further documentation of the use of imenu, such as
;; menu sorting, see the source file imenu.el, which is distributed
;; with GNU Emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Howard Melman made (essentially) the following suggestions, which
;; are slightly different from the expression that I currently use.
;; You may prefer one of Howard's suggestions, which I think assume
;; that `case-fold-search' is `t' (which it is by default):

;; (setq woman-imenu-generic-expression 
;;       '((nil "^\\(   \\)?\\([A-Z][A-Z ]+[A-Z]\\)[ \t]*$" 2)))

;; will give support for .SH and .SS, though it won't show the heading
;; name hierarchy.  If you just want .SH in the imenu then use:

;; (setq woman-imenu-generic-expression 
;;       '((nil "^\\([A-Z][A-Z ]+[A-Z]\\)[ \t]*$" 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Vertical spacing and blank lines
;; ================================

;; The number of consecutive blank lines in the formatted buffer
;; should be either 0 or 1.  A blank line should leave a space like
;; .sp 1 (p. 14).  Current policy is to output vertical space only
;; immediately before text is output.


;; Horizontal and vertical spacing and resolution
;; ==============================================

;; WoMan currently assumes 10 characters per inch horizontally, hence
;; a horizontal resolution of 24 basic units, and 5 lines per inch
;; vertically, hence a vertical resolution of 48 basic units.  (nroff
;; uses 240 per inch).


;; The *WoMan-Log* buffer
;; ======================

;; This is modelled on the byte-compiler.  It logs all files formatted
;; by WoMan, and if WoMan finds anything that it cannot handle then it
;; writes a warning to this buffer.  If the variable `woman-show-log'
;; is non-nil (by default it is `nil') then WoMan automatically
;; displays this buffer.  Many WoMan warnings can be completely
;; ignored, because they are reporting the fact that WoMan has ignored
;; requests that it is correct to ignore.  In some future version this
;; level of paranoia will be reduced, but not until WoMan is more
;; reliable.  At present, all warnings should be treated with some
;; suspicion.  Uninterpreted escape sequences are also logged (in some
;; cases).

;; Uninterpreted ?roff requests can optionally be left in the
;; formatted buffer to indicate precisely where they occur by
;; resetting the variable `woman-ignore' to `nil' (by default it is
;; `t').


;; Reporting Bugs
;; ==============

;; If WoMan fails completely, or formats a file incorrectly
;; (i.e. obviously wrongly or significantly differently from man) or
;; inelegantly, then please

;; (a) check that you are running the latest version of woman.el
;;     available from my web site (see above),

;; (b) check that the problem is not already described in the file
;;     woman.status, also available from my web site.

;; If both of the above are true then please email me the entry from
;; the *WoMan-Log* buffer relating to the problem file, together with
;; a brief description of the problem.  Please indicate where you got
;; the source file from, but do not send it to me unless I ask you to!
;; Thanks.  (There is at present no automated bug-reporting facility
;; for WoMan.)


;; TO DO
;; =====

;; Improve major-mode documentation.
;; Correct handling of \ escape sequences, especially \\!
;; Pre-process conditionals in macro bodies if possible for speed?
;; Emulate some preprocessor support for tbl (.TS/.TE) and eqn (.EQ/.EN)
;; Re-write filling and adjusting code!
;; Allow word wrap at comma (for long option lists)?
;; Buffer list handling not quite right.
;; Make 10 or 12 pitch (cpi) optional -- 12 => ll = 78
;; Handle unpaddable space; use it for tabbing?
;; Tidy up handling of fonts when filling and adjusting
;;   -- see text/text properties?
;; Improve speed
;; Add font-lock support (for quoted strings, etc.)?
;; Optionally save large files in enriched format?
;; Add apropos facility by searching NAME (?) entry in man files?
;; Documentation -- optional auto-display of formatted WoMan man page?
;; Implement a bug reporter?
;; Support diversion and traps (to some extent) - for Tcl/tk pages?


;; Strategy [this description is now well out of date!] -- three main
;; passes, each to process respectively:

;;   1) non-breaking `.' requests including font macros
;;   2) \ escape sequences, mainly special characters and font changes
;;   3) breaking `.' requests, mainly filling and justification

;; For each pass, a control function finds and pre-processes the
;; escape or request and then calls the appropriate function to
;; perform the required formatting.  Based originally on enriched.el
;; and format.el.

;; See also c:/usr/local/share/groff/tmac/tmac.an


;; Acknowledgements
;; ================

;; For Heather, Kathryn and Madelyn, the women in my life
;; (although they will probably never use it)!

;; I also thank the following for helpful suggestions, bug reports,
;; code fragments, general interest, etc.:
;;   Dean Andrews <dean@dra.com>
;;   Juanma Barranquero <barranquero@laley-actualidad.es>
;;   Karl Berry <kb@cs.umb.edu>
;;   Jim Chapman <jchapman@netcomuk.co.uk>
;;   Frederic Corne <frederic.corne@erli.fr>
;;   Peter Craft <craft@alacritech.com>
;;   Jim Davidson <jdavidso@teknowledge.com>
;;   Kevin D'Elia <Kevin.DElia@mci.com>
;;   John Fitch <jpff@maths.bath.ac.uk>
;;   Hans Frosch <jwfrosch@rish.b17c.ingr.com>
;;   Guy Gascoigne-Piggford <ggp@informix.com>
;;   Brian Gorka <gorkab@sanchez.com>
;;   Nicolai Henriksen <nhe@lyngso-industri.dk>
;;   Thomas Herchenroeder <the@software-ag.de>
;;   Alexander Hinds <ahinds@thegrid.net>
;;   Theodore Jump <tjump@cais.com>
;;   Paul Kinnucan <paulk@mathworks.com>
;;   Howard Melman <howard@silverstream.com>
;;   Dennis Pixton <dennis@math.binghamton.edu>
;;   Bruce Ravel <bruce.ravel@nist.gov>
;;   Fabio Somenzi <fabio@joplin.colorado.edu>
;;   Karel Sprenger <ks@ic.uva.nl>
;;   Chris Szurgot <szurgot@itribe.net>
;;   Paul A. Thompson <pat@po.cwru.edu>
;;   Geoff Voelker <voelker@cs.washington.edu>


;; Automatic initiation of woman decoding

;; (Probably not a good idea.  If you use it, be careful!)

;; Put something like this in your .emacs.  The call to
;; set-visited-file-name is to avoid font-locking triggered by
;; automatic major mode selection.

;; (autoload 'woman-decode-region "woman")

;; (setq format-alist
;;      (cons
;;       '(man "UN*X man-page source format" "\\.\\(TH\\|ig\\) "
;;	     woman-decode-region nil nil
;;	     (function (lambda (arg)
;;			 (set-visited-file-name
;;			  (file-name-sans-extension buffer-file-name)))))
;;       format-alist))


;;; Code:

;;; User options:

(defvar man-path nil
  "*Obsolete: please set the variable `woman-manpath' instead.")

(defvar woman-manpath
  (or man-path  ; temporary backward compatibility hack
      (let ((manpath (getenv "MANPATH")))
	(if manpath
	    (parse-colon-path manpath)
	  ;; NB: `parse-colon-path' creates null elements for redundant
	  ;; (semi-)colons and trailing `/'s!
	  '("/usr/man" "/usr/local/man")
	  )))
  "*List of general directories to search for UN*X manual DIRECTORIES.
Each element is a string (directory name).  Non-directory and
unreadable files are ignored.  The default is the value of the UN*X
MANPATH environment variable if set, otherwise

  `(\"/usr/man\" \"/usr/local/man\")'.

Trailing `/'s are ignored.
I recommend you include the drive letter for Microsoft Windows, e.g.

  `(\"C:/Cygnus/b19/man\" \"C:/usr/man\" \"C:/usr/local/man\")'

Specific directories in `woman-path' are also searched.

\(This variable was formerly called `man-path'.)")

(defvar woman-path nil
  "*List of specific directories to search for UN*X manual FILES.
They are searched in addition to those specified more generally in
`woman-manpath'.  Each element is a string (directory name) or nil
(current directory when the path is expanded and cached).  However,
the last component of each directory string is treated as a regexp
(Emacs, not shell) and the string is expanded into a list of matching
directories.  Non-directory and unreadable files are ignored.  For
example, \(\"/usr/man/man\") might expand to

  (\"/usr/man/man1\" \"/usr/man/man3\" \"/usr/man/man5\").

But such standard paths are handled better by setting `woman-manpath'.
Trailing `/'s are discarded.  The default value is nil.
I recommend you include the drive letter for Microsoft Windows.")

(defvar woman-directory-cache-filename
  "~/.wmncach.el"
  "*The full pathname of the WoMan directory and topic cache file.
It is used to save and restore the cache between sessions.  This is
especially useful with remote-mounted man page files!  A value of
`nil' suppresses this action.  The default value is \"~/.wmncach.el\".
Remember that a prefix argument forces the `woman' command to update
this cache.")

(defvar woman-fill-column 65
  "*Right margin for formatted text -- default is 65.")

(defvar woman-fill-frame nil
  ;; Based loosely on a suggestion by Theodore Jump:
  "*If non-nil then most of the frame width is used.")

(defvar woman-default-indent 5
  "*Default prevailing indent set by -man macros -- default is 5.
Set this variable to 7 to emulate Linux man formatting.")

(defvar woman-fontify window-system
  "*If non-nil then WoMan assumes that face support is available.
It defaults to the value of the variable `window-system'.")

(defvar woman-italic-face 'woman-italic-face
  "*Name of face to use for italic font in man pages.
If the name is not a face then it will be set to a reasonable default.")
(defvar woman-bold-face 'woman-bold-face
  "*Name of face to use for bold font in man pages.
If the name is not a face then it will be set to a reasonable default.")
(defvar woman-unknown-face 'woman-unknown-face
  "*Name of face to use for all unknown fonts in man pages.
If the name is not a face then it will be set to a reasonable default.")
(defvar woman-addition-face 'woman-addition-face
  "*Name of face to use for all additions to the man pages.
If the name is not a face then it will be set to a reasonable default.")

(defvar woman-always-colour-faces nil
  "*If non-nil then WoMan ALWAYS colours the italic and bold faces.
It sets their foreground colours to red and blue respectively.
If nil then WoMan only colours the faces if they would otherwise be
the same as the default face.")

(defvar woman-dired-keys t
  "*List of `dired' mode keys to define to run WoMan on current file,
e.g. '(\"w\" \"W\"), or any non-null atom to automatically define
\"w\" and \"W\" if they are unbound, or `nil' to do nothing.
Default is t.")

(defvar woman-show-log nil
  "*If non-nil then show the *WoMan-Log* buffer if appropriate.
I.e. if any warning messages are written to it.  Default is nil.")

(defvar woman-ignore t
  "*If non-nil unrecognised requests etc. are ignored.  Default is t.
This gives the standard ?roff behaviour.  If nil then they are left in
the buffer, which may aid debugging.")

(defvar woman-imenu-generic-expression	; EXPERIMENTAL!
  '((nil "\n\\([A-Z].*\\)" 1) ; SECTION, but not TITLE
    ("*Subsections*" "^   \\([A-Z].*\\)" 1))
  "*Imenu support for Sections and Subsections.
An alist with elements of the form (MENU-TITLE REGEXP INDEX) --
see the documentation for `imenu-generic-expression'.")

(defvar woman-imenu nil
  "*If non-nil then WoMan automatically calls `imenu-add-to-menubar'
to add a Contents menu to the menubar.  Default is nil.")

(defvar woman-imenu-title "CONTENTS"
  "*The title to use if WoMan adds a Contents menu to the menubar.
Default is \"CONTENTS\".")

(defvar woman-pre-format-hook nil
  "*Hook run by WoMan immediately before formatting a buffer.
Not really a user option -- change only via the function `add-hook'.")

(defvar woman-post-format-hook nil
  "*Hook run by WoMan immediately after formatting a buffer.
Not really a user option -- change only via the function `add-hook'.")



;;; Define default faces:

(if (not woman-fontify)
    ()
  (if (facep woman-italic-face) ()
    (copy-face 'italic woman-italic-face))
  ;; Don't stomp on user's definition of italic:
  (if (or woman-always-colour-faces
	  (not (face-differs-from-default-p woman-italic-face)))
      (set-face-foreground woman-italic-face "Red"))
  ;; but always underline ?????
  (set-face-underline-p woman-italic-face t) ; nroff uses underline!
  (if (facep woman-bold-face) ()
    (copy-face 'bold woman-bold-face))
  ;; Don't stomp on user's definition of bold:
  (if (or woman-always-colour-faces
	  (not (face-differs-from-default-p woman-bold-face)))
      (set-face-foreground woman-bold-face "Blue")) ; ignored by nroff!
  (copy-face 'default woman-unknown-face)
  ;; (set-face-foreground woman-unknown-face "Orange")
  (set-face-foreground woman-unknown-face "Brown")
  ;; Brown is a good compromise: it is distinguishable from the default
  ;; but not enough to make font errors look terrible.  (Files that use
  ;; non-standard fonts seem to do so badly or in idiosyncratic ways!)
  (copy-face 'default woman-addition-face)
  (set-face-foreground woman-addition-face "Orange")
  )

(defun woman-colour-faces ()
  "Set foreground colours of italic and bold faces to red and blue."
  (interactive)
  (set-face-foreground woman-italic-face "Red")
  (set-face-foreground woman-bold-face "Blue"))

(defun woman-black-faces ()
  "Set foreground colours of italic and bold faces both to black."
  (interactive)
  (set-face-foreground woman-italic-face "Black")
  (set-face-foreground woman-bold-face "Black"))


;;; Internal variables:

(defvar woman-left-margin woman-default-indent
  "Current left margin.")
(defvar woman-prevailing-indent woman-default-indent
  "Current prevailing indent.")
(defvar woman-interparagraph-distance 1
  "Interparagraph distance in lines.
Set by .PD; used by .SH, .SS, .TP, .LP, .PP, .P, .IP, .HP.")
(defvar woman-leave-blank-lines nil
  "Blank lines to leave as vertical space.")
(defconst woman-tab-width 5
  "Default tab width set by -man macros.")
(defvar woman-justify 'full
  "Current justification style.")
(defvar woman-nofill nil
  "Current fill mode: nil for filling.")
(defvar woman-RS-left-margin nil
  "Left margin stack for nested use of `.RS/.RE'.")
(defvar woman-RS-prevailing-indent nil
  "Prevailing indent stack for nested use of `.RS/.RE'.")
(defvar woman-nospace nil
  "Current no-space mode: nil for normal spacing.
Set by `.ns' request; reset by any output or `.rs' request")

(defsubst woman-reset-nospace ()
  (setq woman-nospace nil))

(defconst woman-mode-line-format
  ;; This is essentially the Man-mode format with page numbers removed
  ;; and line numbers added.  (Online documents do not have pages, but
  ;; they do have lines!)
  '("" mode-line-modified
    mode-line-buffer-identification "   "
    global-mode-string
    ;; "   %[(" mode-name mode-line-process minor-mode-alist ")%]----"
    "   %[(" "WoMan" mode-line-process minor-mode-alist ")%]----"
    (line-number-mode "L%l----")
    (-3 . "%p") "-%-")
  "Mode line format for woman buffer.")

(defconst woman-request-regexp "^[.'][ \t]*\\(\\S +\\) *"
  ;; Was "^\\.[ \t]*\\([a-z0-9]+\\) *" but cvs.1 uses a macro named
  ;; "`" and CGI.man uses a macro named "''"!
  ;; CGI.man uses ' as control character in places -- it *should*
  ;; suppress breaks!
  ;; Could end with "\\( +\\|$\\)" instead of " *"
  "Regexp to match a ?roff request plus trailing white space.")

(defvar woman-imenu-done nil
  "Buffer-local: set to true if `woman-imenu' has been called.")
(make-variable-buffer-local 'woman-imenu-done)

;; From imenu.el -- needed when reformatting a file in its old buffer.
;; The latest buffer index used to update the menu bar menu.
(defvar imenu--last-menubar-index-alist nil)
(make-variable-buffer-local 'imenu--last-menubar-index-alist)


;;; File and buffer handling:

;; The SysV standard man pages use two character suffixes, and this is
;; becoming more common in the GNU world.  For example, the man pages
;; in the ncurses package include "toe.1m", "form.3x", etc.
;;   Dennis Pixton -- dennis@math.binghamton.edu

(defconst woman-file-regexp
  ;; "\\(\\`[^.]+\\|\\.\\([0-9l]\\|[nt]\\w*\\)\\)\\(\\.g?z\\)?\\'"
  ;; Disallow no extension:
  ;; "\\.\\([0-9l]\\|[nt]\\w*\\)\\(\\.g?z\\)?\\'"
  "\\.\\([0-9lmnt]\\w*\\)\\(\\.g?z\\)?\\'"
  "Regexp used to select (possibly compressed) man source files.")

(defvar woman-expanded-directory-path nil
  "Expanded directory list cache.  Resetting to nil forces update.")

(defvar woman-topic-all-completions nil
  "Expanded topic alist cache.  Resetting to nil forces update.")

;;;###autoload
(defun woman (&optional topic re-cache)
  "Decode and browse a UN*X man page WithOut using a MAN program.
The major browsing mode used is essentially the standard Man mode.
Choose the filename for the man page using completion, based on the
topic selected from the directories specified in `woman-manpath' and
`woman-path'.  The directory expansions and topics are cached for
speed, but a non-nil interactive argument forces the caches to be
updated (e.g. to re-interpret the current directory).

Used non-interactively, arguments are optional: if they are given then
the FIRST argument should be a topic string and the SECOND may be
non-nil to force re-caching."
  (interactive (list nil current-prefix-arg))
  ;; Handle the caching of the directory and topic lists:
  (if (and (not re-cache)
	   (or
	    (and woman-expanded-directory-path woman-topic-all-completions)
	    (woman-read-directory-cache)))
      ()
    (message "Building list of manual directory expansions ...")
    (setq woman-expanded-directory-path
	  (woman-expand-directory-path woman-manpath woman-path))
    (message "Building completion list of all manual topics ...")
    (setq woman-topic-all-completions
	  (woman-topic-all-completions woman-expanded-directory-path))
    (woman-write-directory-cache))
  ;; The following test is for non-interactive calls via gnudoit etc.
  (if (or (interactive-p) (string-match "\\S " topic))
      (let ((file-name (woman-file-name topic)))
	(if file-name
	    (woman-find-file file-name)
	  (message
	   "WoMan Error: No matching manual files found in search path")
	  (ding))
	)
    (message "WoMan Error: No topic specified in non-interactive call")
    (ding))
  )

(defun woman-read-directory-cache ()
  "Load the directory and topic cache from the file named precisely as
specified by the variable `woman-directory-cache-filename' and return
`t' if the file exists and `nil' otherwise."
  (if woman-directory-cache-filename
      (load woman-directory-cache-filename t nil t)))

(defun woman-write-directory-cache ()
  "Write the directory and topic cache to the file named precisely as
specified by the variable `woman-directory-cache-filename'."
  (if woman-directory-cache-filename
      (save-excursion			; to restore current buffer
	;; Make a temporary buffer; name starting with space "hides" it.
	(let ((standard-output
	       (set-buffer (generate-new-buffer "WoMan tmp buffer")))
	      (backup-inhibited t))
	  ;; (switch-to-buffer standard-output t) ; only for debugging
	  (buffer-disable-undo standard-output)
	  (princ
	   ";;; WoMan directory and topic cache -- generated automatically\n")
	  (print
	   `(setq woman-expanded-directory-path
		  ',woman-expanded-directory-path))
	  (print
	   `(setq woman-topic-all-completions
		  ',woman-topic-all-completions))
	  (write-file woman-directory-cache-filename) ; write CURRENT buffer
	  (kill-buffer standard-output)
	  ))))

(defvar woman-topic-history nil "Topic read history.")
(defvar woman-file-history nil "File-name read history.")

(defun woman-file-name (topic)
  "Get the name of the UN*X man-page file describing a chosen topic.
Initial suggestion is the word nearest to point." ; make this optional?
  (let ((topic (if (stringp topic)
		   topic
		 (completing-read
		  "Manual entry: "
		  woman-topic-all-completions nil 1
		  ;; Initial input suggestion (was nil), with
		  ;; cursor at left ready to kill suggestion!:
		  (cons (current-word) 0)
		  'woman-topic-history)))
	files)
    ;; Note that completing-read always returns a string
    (if (= (length topic) 0)
	()
      (setq files (woman-file-name-all-completions
		   woman-expanded-directory-path topic))
      (if (null (cdr files))
	  ;; Only 1 file for topic.
	  (car (car files))
	;; Multiple files for topic, so must select 1.
	;; Unread the command event (TAB = ?\t = 9) that runs the command
	;; `minibuffer-complete' in order to automatically complete the
	;; minibuffer contents as far as possible.
	(setq unread-command-events '(9))	; and delete any type-ahead!
	(completing-read "Manual file: " files nil 1
			 (try-completion "" files) 'woman-file-history)
	))))

(defun woman-select (predicate list)
  "Select unique elements for which PREDICATE is true in LIST.
\(Note that this function changes the value of LIST.)"
  ;; Intended to be fast by avoiding recursion and list copying.
  (while (and list
	      (or 
	       (member (car list) (cdr list))
	       (not (funcall predicate (car list)))))
    (setq list (cdr list)))
  (if list
      (let ((newlist list) cdr_list)
	(while (setq cdr_list (cdr list))
	  (if (and
	       (not (member (car cdr_list) (cdr cdr_list)))
	       (funcall predicate (car cdr_list)))
	      (setq list cdr_list)
	    (setcdr list (cdr cdr_list)))
	  )
	newlist)))

(defun woman-file-readable-p (dir)
  "Return `t' if dir is readable, otherwise log a warning."
  (or (file-readable-p dir)
      (WoMan-warn "Ignoring unreadable `manpath' directory tree `%s'!" dir)))

(defun woman-directory-files (head dir)
  "Return a sorted list of the absolute pathnames of all the files in
directory head, or the current directory if head is null, that match the
regexp that is the final component of dir.  Log a warning if list is empty."
  (or (directory-files
       (or head (directory-file-name default-directory)) ; was "."
       t
       (file-name-nondirectory dir))
      (WoMan-warn "No directories match `woman-path' entry `%s'!" dir)))

(defun woman-file-accessible-directory-p (dir)
  "Return `t' if dir is accessible, otherwise log a warning."
  (or (file-accessible-directory-p dir)
      (WoMan-warn "Ignoring inaccessible `man-page' directory `%s'!" dir)))

(defun woman-expand-directory-path (woman-manpath woman-path)
  "Expand the general manual directories in WOMAN-MANPATH and the
specific manual directory regexps in WOMAN-PATH.  Ignore any paths
that are unreadable or not directories."
  ;; Allow each path to be a single string or a list of strings:
  (if (not (listp woman-manpath)) (setq woman-manpath (list woman-manpath)))
  (if (not (listp woman-path)) (setq woman-path (list woman-path)))
  (let (dir head dirs)
    (while woman-manpath
      (setq
       dir (car woman-manpath)
       woman-manpath (cdr woman-manpath))
      (if (and dir (woman-file-readable-p dir))
	  ;; NB: `parse-colon-path' creates null elements for
	  ;; redundant (semi-)colons and trailing `/'s!
	  ;; If does not matter here if dir ends with `/'.
	  ;; Need regexp "man" here to avoid "cat?", `.', `..', etc.
	  (setq dirs (nconc dirs (directory-files dir t "man")))))
    (while woman-path
      (setq
       dir (car woman-path)
       woman-path (cdr woman-path))
      ;; A path that ends with / matches all directories in it,
      ;; including `.' and `..', so remove any trailing / !!!
      (if (and dir (string= (substring dir -1) "/"))
	  (setq dir (substring dir 0 -1)))
      (if (or (null dir)
	      (null (setq head (file-name-directory dir)))
	      (woman-file-readable-p head))
	  (setq dirs
		(if dir
		    (nconc dirs (woman-directory-files head dir))
		  (cons (directory-file-name default-directory) dirs))
		;; was "." -- at head of list for later filtering
		)))
    (woman-select 'woman-file-accessible-directory-p dirs)))

(defsubst woman-not-member (dir path)
  "Returns true if DIR is not a member of the list PATH.
If DIR is `.' it is first replaced by the current directory."
  (not (member
	;(if (string= dir ".")
	    ;(directory-file-name default-directory)
	  dir;)
	path)))

(defun woman-topic-all-completions (path)
  "Return an alist of the man files in all man directories in the list PATH."
  (let (dir files)
    (while path
      (setq dir (car path)
	    path (cdr path))
      (if (woman-not-member dir path)	; use each directory only once!
	  (setq files
		(nconc files
			(mapcar
			 ;; Convert list to alist of non-directory files:
			 (function
			  (lambda (f)
			    (if (file-directory-p f) nil
			      (if (string-match "\\.g?z\\'" f)
				  (setq f (file-name-sans-extension f)))
			      (list (file-name-sans-extension f)))))
			 (directory-files dir nil woman-file-regexp)
			 ))))
      )
    ;; If this proves useful, re-implement this function?
    ;; It seems to make no significant difference!
    ;; (sort				; for speed ????
    (woman-select 'identity files)
    ;; (function (lambda (a b) (string-lessp (car a) (car b)))))
    ))

(defun woman-file-name-all-completions (path topic)
  "Return an alist of the files in all man directories in the list PATH
that match TOPIC."
  ;; Topic must match first `word' of filename, so ...
  (setq topic (concat "\\`" (regexp-quote topic) "\\(\\.\\|\\'\\)"))
  (let (dir files)
    (while path
      (setq dir (car path)
	    path (cdr path))
      (if (woman-not-member dir path)	; use each directory only once!
	  (setq files
		(nconc files
			(mapcar
			 (function (lambda (f)
				     (if (string-match woman-file-regexp f)
					 (list f))))
			 (directory-files dir t topic)))))
      )
    (woman-select 'identity files)
    ))

(defvar woman-buffer-alist nil
  "Alist of (file-name . buffer-name) pairs for WoMan buffers already
decoded.")

(defvar woman-buffer-number 0
  "Ordinal number of current buffer entry in `woman-buffer-alist'
counting from 0.")

(defun WoMan-previous-manpage ()
  "Find the previous WoMan buffer."
  ;; Assumes currently in a WoMan buffer!
  (interactive)
  (WoMan-find-buffer)			; find current existing buffer
  (if (null (cdr woman-buffer-alist))
      (error "No previous WoMan buffer."))
  (if (>= (setq woman-buffer-number (1+ woman-buffer-number))
	 (length woman-buffer-alist))
      (setq woman-buffer-number 0))
  (if (WoMan-find-buffer)
      ()
    (if (< (setq woman-buffer-number (1- woman-buffer-number)) 0)
	(setq woman-buffer-number (1- (length woman-buffer-alist))))
    (WoMan-previous-manpage)))

(defun WoMan-next-manpage ()
  "Find the next WoMan buffer."
  ;; Assumes currently in a WoMan buffer!
  (interactive)
  (WoMan-find-buffer)			; find current existing buffer
  (if (null (cdr woman-buffer-alist))
      (error "No next WoMan buffer."))
  (if (< (setq woman-buffer-number (1- woman-buffer-number)) 0)
      (setq woman-buffer-number (1- (length woman-buffer-alist))))
  (if (WoMan-find-buffer)
      ()
    (WoMan-next-manpage)))

(defun WoMan-find-buffer ()
  "Switch to buffer corresponding to `woman-buffer-number' if it exists and
return it, otherwise remove it from `woman-buffer-alist' and return nil."
  (if (zerop woman-buffer-number)
      (let ((buffer (get-buffer (cdr (car woman-buffer-alist)))))
	(if buffer
	    (switch-to-buffer buffer)
	  ;; Delete alist element:
	  (setq woman-buffer-alist (cdr woman-buffer-alist))
	  nil))
    (let* ((prev-ptr (nthcdr (1- woman-buffer-number) woman-buffer-alist))
	   (buffer (get-buffer (cdr (car (cdr prev-ptr))))))
      (if buffer
	  (switch-to-buffer buffer)
	;; Delete alist element:
	(setcdr prev-ptr (cdr (cdr prev-ptr)))
	(if (>= woman-buffer-number (length woman-buffer-alist))
	    (setq woman-buffer-number 0))
	nil)
      )))

(defun woman-dired-define-key (key)
  (define-key dired-mode-map key 'woman-dired-find-file))

(defsubst woman-dired-define-key-maybe (key)
  (if (eq (lookup-key dired-mode-map key) 'undefined)
      (woman-dired-define-key key)))

(defun woman-dired-define-keys ()
  "Define dired keys to run WoMan according to `woman-dired-keys'."
  (if (listp woman-dired-keys)
      (mapcar 'woman-dired-define-key woman-dired-keys)
    (woman-dired-define-key-maybe "w")
    (woman-dired-define-key-maybe "W")))

(if woman-dired-keys
    (if (featurep 'dired)
	(woman-dired-define-keys)
      (add-hook 'dired-mode-hook 'woman-dired-define-keys)))

;;;###autoload
(defun woman-dired-find-file ()
  "In dired, run the WoMan man-page browser on this file."
  (interactive)
  ;; dired-get-filename is defined in dired.el
  (woman-find-file (dired-get-filename)))

(defvar woman-last-file-name nil
  "The full pathname of the last file formatted by WoMan.")

(defun woman-reformat-last-file ()
  "Reformat last file, e.g. after changing fill column."
  (interactive)
  (if woman-last-file-name
      (woman-find-file woman-last-file-name t)
    (call-interactively 'woman-find-file)))

;;;###autoload
(defun woman-find-file (file-name &optional reformat)
  "Find, decode and browse a specific UN*X man-page source file.
Use existing buffer if possible; reformat only if prefix arg given.
No external programs are used, except that `gunzip' will be used to
decompress the file if appropriate.  See the documentation for the
`woman' command for further details."
  (interactive "fBrowse UN*X manual file: \nP")
  (setq woman-last-file-name
	(setq file-name (expand-file-name file-name)))	; to canonicalize
  (let ((alist-tail woman-buffer-alist) exists)
    (setq woman-buffer-number 0)
    (while (and alist-tail (not (string= file-name (car (car alist-tail)))))
      (setq alist-tail (cdr alist-tail)
	    woman-buffer-number (1+ woman-buffer-number)))
    (if (and (setq exists
		   (and alist-tail (WoMan-find-buffer))) ; buffer exists
	     (not reformat))
	()
      ;; Format new buffer or reformat current buffer:
      (let* ((bufname (file-name-nondirectory file-name))
	     (case-fold-search t)
	     (compressed (not (not (string-match "\\.g?z\\'" bufname)))))
	(if compressed
	    (setq bufname (file-name-sans-extension bufname)))
	(let ((dot (string-match "\\." bufname)))
	  (if dot (setq bufname (concat
			      (substring bufname (1+ dot)) " "
			      (substring bufname 0 dot)))))
	(setq bufname (if exists
			  (buffer-name)
			(generate-new-buffer-name ; ensure uniqueness
			 (concat "*WoMan " bufname "*"))))
	(woman-really-find-file file-name compressed bufname)
	(or exists
	    (setq woman-buffer-alist
		  (cons (cons file-name bufname) woman-buffer-alist)
		  woman-buffer-number 0))
	)))
  (Man-build-section-alist)
  (Man-build-references-alist)
  (goto-char (point-min)))

(defun woman-really-find-file (filename compressed bufname)
  "Find, decode, and set buffer name and major mode for a specific
file in UN*X man-page format, using `gunzip' to decompress the file if
appropriate.  Do not call this function directly!"
  (let ((WoMan-current-file filename))	; used for message logging
    (switch-to-buffer (get-buffer-create bufname))
    (buffer-disable-undo)
    (setq buffer-read-only nil)
    (erase-buffer)			; NEEDED for reformat
    (woman-insert-file-contents filename compressed)
    ;; Set buffer's default directory to that of the file.
    (setq default-directory (file-name-directory filename))
    (set (make-local-variable 'backup-inhibited) t)
    (set-visited-file-name "")
    (woman-decode-buffer)
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (woman-mode)))

(defun woman-insert-file-contents (filename compressed)
  "Inset file FILENAME into the current buffer, using `gunzip' to
decompress the file if COMPRESSED evaluates to `t' or to non-nil and
the filename implies compression.  Leaves point at start of new text."
  (save-excursion
    ;; Preserve text mode under MS-DOS/Windows (cf. `find-file-text' in
    ;; `winnt.el' by Geoff Voelker) unless file is compressed:
    (let* ((case-fold-search t)
	   (compressed
	    (if compressed
		(or (eq compressed t)
		    (not (not (string-match "\\.g?z\\'" filename))))))
	   (file-name-buffer-file-type-alist (list (cons "" compressed)))
	   (binary-process-input t)	; for MS-DOS/Windows gunzip
	   (length
	    (nth 1
		 (condition-case ()
		     (insert-file-contents filename nil)
		   (file-error
;		    ;; Run find-file-not-found-hooks until one returns non-nil.
;		    (run-hook-with-args-until-success
;		     'find-file-not-found-hooks)
		    (insert "\n***** File " filename " not found! *****\n\n")
		    ))
		 )))
      ;; Co-operate with auto-compression mode:
      (if (and compressed
	       ;; (not auto-compression-mode)
	       (not (and (fboundp 'jka-compr-installed-p)
			 (jka-compr-installed-p))) )
	  ;; Based on info.el, but I support only gunzip.
	  ;; Should probably check return status here!
	  (call-process-region (point) (+ (point) length) "gunzip" t t))
      )))


;;; Major mode (Man) interface:

(defvar woman-mode-map nil "Keymap for woman mode.")
(if woman-mode-map
    ()
  ;; Set up the keymap, mostly inherited from Man-mode-map:
  (setq woman-mode-map (make-sparse-keymap))
  (set-keymap-parent woman-mode-map Man-mode-map)
  ;; Above two lines were
  ;; (setq woman-mode-map (cons 'keymap Man-mode-map))
  (define-key woman-mode-map "R" 'woman-reformat-last-file)
  (define-key woman-mode-map "w" 'woman)
  ;; (define-key woman-mode-map "m" 'woman)
  (define-key woman-mode-map "\en" 'WoMan-next-manpage)
  (define-key woman-mode-map "\ep" 'WoMan-previous-manpage)
  ;; *** WoMan menu bar and pop-up menu ***
  (let ((menu (make-sparse-keymap "WoMan")))
    (define-key woman-mode-map [menu-bar] (make-sparse-keymap))
    (define-key woman-mode-map [menu-bar woman] (cons "WoMan" menu))
    ;; Define menu entries (from bottom to top):
    (define-key menu [mini-help]
      '("Mini Help" . woman-mini-help))
    (define-key menu [imenu]
      '("Make Contents Menu" . WoMan-menu-imenu))
    (define-key menu [separator-6] '("--"))
    (define-key menu [reformat]
      '("Reformat Last File" . woman-reformat-last-file))
    (define-key menu [toggle-fill-frame]
      '("Toggle Fill Frame Width" . woman-toggle-fill-frame))
    (define-key menu [separator-5] '("--"))
    (define-key menu [black-faces]
      '("Use Black Main Faces" . woman-black-faces))
    (define-key menu [colour-faces]
      '("Use Coloured Main Faces" . woman-colour-faces))
    (define-key menu [separator-4] '("--"))
    (define-key menu [describe-mode]
      '("Describe Man Mode" . describe-mode))
    (define-key menu [Man-kill]
      '("Kill WoMan Buffer" . Man-kill))
    (define-key menu [Man-quit]
      '("Bury WoMan Buffer" . Man-quit))
    (define-key menu [separator-3] '("--"))
    (define-key menu [next-manpage]
      '("Next WoMan Buffer" . WoMan-next-manpage))
    (define-key menu [previous-manpage]
      '("Previous WoMan Buffer" . WoMan-previous-manpage))
    (define-key menu [separator-2] '("--"))
    (define-key menu [follow-reference]
      '("Follow Reference..." . Man-follow-manual-reference))
    (define-key menu [goto-see-also]
      '("Goto See-Also Section" . Man-goto-see-also-section))
    (define-key menu [goto-section]
      '("Goto Section..." . Man-goto-section))
    (define-key menu [previous-section]
      '("Previous Section" . Man-previous-section))
    (define-key menu [next-section]
      '("Next Section" . Man-next-section))
    (define-key menu [separator-1] '("--"))
    (define-key menu [woman]
      '("WoMan..." . woman))
    ))

(defun woman-mode ()
  "Turn on (most of) Man mode to browse a woman buffer."
  (let ((Man-build-page-list (symbol-function 'Man-build-page-list))
	(Man-strip-page-headers (symbol-function 'Man-strip-page-headers))
	(Man-unindent (symbol-function 'Man-unindent))
	(Man-goto-page (symbol-function 'Man-goto-page)))
    ;; Prevent inappropriate operations:
    (fset 'Man-build-page-list 'ignore)
    (fset 'Man-strip-page-headers 'ignore)
    (fset 'Man-unindent 'ignore)
    (fset 'Man-goto-page 'ignore)
    (unwind-protect
	(progn
	  (set (make-local-variable 'Man-mode-map) woman-mode-map)
	  ;; Install Man mode:
	  (Man-mode)
	  ;; Reset inappropriate definitions:
	  (setq mode-line-format woman-mode-line-format)
          (put 'Man-mode 'mode-class 'special))
      ;; Restore the status quo:
      (fset 'Man-build-page-list Man-build-page-list)
      (fset 'Man-strip-page-headers Man-strip-page-headers)
      (fset 'Man-unindent Man-unindent)
      (fset 'Man-goto-page Man-goto-page)
      )
    ;; Imenu support:
    (set (make-local-variable 'imenu-generic-expression)
	 ;; `make-local-variable' in case imenu not yet loaded!
	 woman-imenu-generic-expression)
    (set (make-local-variable 'imenu-space-replacement) " ")
    ;; For reformat ...
    ;; necessary when reformatting a file in its old buffer:
    (setq imenu--last-menubar-index-alist nil)
    ;; necessary to avoid re-installing the same imenu:
    (setq woman-imenu-done nil)
    (if woman-imenu (woman-imenu))
    ))

(defun woman-imenu (&optional redraw)
  "Add a \"Contents\" menu to the menubar."
  (interactive)
  (if woman-imenu-done
      ;; This is PRIMARILY to avoid a bug in imenu-add-to-menubar that
      ;; causes it to corrupt the menu bar if it is run more than once
      ;; in the same buffer.
      ()
    (setq woman-imenu-done t)
    (imenu-add-to-menubar woman-imenu-title)
    (if redraw (force-mode-line-update))))

(put 'WoMan-menu-imenu 'menu-enable '(not woman-imenu-done))
(defun WoMan-menu-imenu ()
  "Add a \"Contents\" menu to the menubar and redisplay the frame.
This is necessary only when called from the WoMan menu."
  (interactive)
  (woman-imenu t))

(defun woman-toggle-fill-frame ()
  "Toggle formatting to fill (most of) the width of the current frame."
  (interactive)
  (setq woman-fill-frame (not woman-fill-frame))
  (message "Woman fill column set to %s."
	   (if woman-fill-frame "frame width" woman-fill-column)
	   ))

(defun woman-mini-help ()
  "Display WoMan commands and user options in an `apropos' buffer."
  (interactive)
  (apropos-command "woman" t))

(defun WoMan-getpage-in-background (topic)
  "Use TOPIC to start WoMan from `Man-follow-manual-reference'."
  ;; topic is a string, generally of the form "section topic"
  (let ((s (string-match " " topic)))
    (if s (setq topic (substring topic (1+ s))))
    (woman topic)))

(defadvice Man-getpage-in-background
  (around Man-getpage-in-background-advice (topic) activate)
  "Use WoMan unless invoked outside a WoMan buffer or invoked explicitly.
Otherwise use Man."
  (if (and (eq mode-line-format woman-mode-line-format)
	   (not (eq last-command 'man)))
      (WoMan-getpage-in-background topic)
    ad-do-it))



;;; Specialized utility functions:

;;; Fast deletion without saving on the kill ring (cf. simple.el):

(defun woman-delete-line (&optional arg)
  "Delete the rest of the current line; if no nonblanks there, delete
thru newline.  With argument, delete that many lines from point.
Negative arguments delete lines backward."
  ;; This is a non-interactive version of kill-line in simple.el that
  ;; deletes instead of killing and assumes kill-whole-line is nil,
  ;; which is essential!
  (delete-region (point)
		 (progn
		   (if arg
		       (forward-line arg)
		     (if (eobp)
			 (signal 'end-of-buffer nil))
		     (if (looking-at "[ \t]*$")
			 (forward-line 1)
		       (end-of-line)))
		 (point))))

(defsubst woman-delete-whole-line ()
  "Delete current line from beginning including eol."
  (beginning-of-line)
  (woman-delete-line 1))

(defsubst woman-delete-following-space ()
  "Delete all spaces and tabs FOLLOWING point (cf. delete-horizontal-space)."
  ;; cf. delete-horizontal-space in simple.el:
  (delete-region (point) (progn (skip-chars-forward " \t") (point))))

(defsubst woman-delete-match (subexp)
  "Delete specified subexpression of buffer text matched by last search."
  (delete-region (match-beginning subexp) (match-end subexp)))

;; delete-char does not kill by default
;; delete-backward-char does not kill by default
;; delete-horizontal-space does not kill
;; delete-blank-lines does not kill


;;; The main decoding driver:

(defvar font-lock-mode)			; for the compiler!

(defun woman-decode-buffer ()
  "Decode a buffer in UN*X man-page source format.
No external programs are used."
  (interactive)				; mainly for testing
  (run-hooks 'woman-pre-format-hook)
  (and (boundp 'font-lock-mode) font-lock-mode (font-lock-mode -1))
  ;; (fundamental-mode)
  (message "Formatting buffer ...")
;  (goto-char (point-min))
;  (cond
;   ((re-search-forward "^\\.[ \t]*TH" nil t) ; wrong format if not found?
;    (beginning-of-line)
;    (delete-region (point-min) (point))) ; potentially dangerous!
;   (t (message "WARNING: .TH request not found -- not man-page format?")))
  (woman-decode-region (point-min) (point-max))
  (run-hooks 'woman-post-format-hook)
  (message "Formatting buffer ... done")
  )

(defvar woman-string-alist		; rebound in woman-decode-region
  '(("S" . "") ("R" . "(Reg.)") ("Tm" . "(TM)")
    ("lq" . "\"") ("rq" . "\"")
    ("''" . "\"")			; needed for gcc.1
    (".T" . "")				; output device from -T option?
    )
  "Alist of strings predefined in the -man macro package `tmac.an'.")

(defvar woman-negative-vertical-space nil ; rebound in woman-decode-region
  "Set to t if .sp N with N < 0 encountered.")

(defun woman-decode-region (from to)
  "Decode a region in UN*X man-page source format."
  ;; Suitable for use in format-alist.
  ;; But this requires care to control major mode implied font locking.
  ;; Must return the new end of file.  See format.el for details.
  (WoMan-log-file)
  ;; First force the correct environment:
  (let ((case-fold-search nil)		; This is necessary!
	(woman-string-alist woman-string-alist)
	(woman-fill-column woman-fill-column)
	woman-negative-vertical-space)
    (setq woman-left-margin woman-default-indent
	  woman-prevailing-indent woman-default-indent
	  woman-interparagraph-distance 1
	  woman-leave-blank-lines nil
	  woman-RS-left-margin nil
	  woman-RS-prevailing-indent nil
	  woman-justify 'full
	  woman-nofill nil)

    ;; Based loosely on a suggestion by Theodore Jump:
    (if (or woman-fill-frame
	    (not (and (integerp woman-fill-column) (> woman-fill-column 0))))
	(setq woman-fill-column (- (frame-width) woman-default-indent)))

    ;; Delete comments .\"<anything>, \"<anything> and pre-processor
    ;; directives '\"<anything> (should give warning?):
    (goto-char from)
    (while (re-search-forward "^[.'][ \t]*\\\\\".*\n\\|\\\\\".*" nil t)
      (woman-delete-match 0))

    ;; Process \k escapes BEFORE changing tab width (?):
    (goto-char from)
    (woman-mark-horizonal-position)

    ;; Set buffer-local variables:
    (setq fill-column woman-fill-column
	  tab-width woman-tab-width)

    ;; Discard optional hyphen \%; concealed newlines \<newline>;
    ;; point-size change function \sN,\s+N, \s-N:
    (goto-char from)
    (while (re-search-forward "\\\\\\([%\n]\\|s[-+]?[0-9]+\\)" nil t)
      (woman-delete-match 0))

    ;; Process ignore requests, macro definitions,
    ;; conditionals and switch source requests:
    ;; (goto-char from)
    (woman0-roff-buffer from)

;    ;; Delete comments .\"<anything>, \"<anything> and pre-processor
;    ;; directives '\"<anything> (should give warning?):
;    (goto-char from)
;    (while (re-search-forward "^[.'][ \t]*\\\\\".*\n\\|\\\\\".*" nil t)
;      (woman-delete-match 0))
;    ;; Process trivial escapes \-, \\, \', \`, \.:
;    (goto-char from)
;    (while (re-search-forward "\\\\\\([-\\\\'`.]\\)" nil t)
;      (replace-match "\\1"))
    ;; BEWARE: THIS SHOULD PROBABLY ALL BE DONE MUCH LATER!!!!!
    ;; Process trivial escapes \-, \`, \.
    ;; (\' must be done after tab processing!):
    (goto-char from)
    (while (re-search-forward "\\\\\\([-`.]\\)" nil t)
      (replace-match "\\1"))
;    ;; Discard optional hyphen \%; concealed newlines \<newline>;
;    ;; point-size change function \sN,\s+N, \s-N:
;    (goto-char from)
;    (while (re-search-forward "\\\\\\([%\n]\\|s[-+]?[0-9]+\\)" nil t)
;      (woman-delete-match 0))
    ;; NB: Must keep ALL zero-width characters \&, \|, and \^ until
    ;; ALL requests processed!
    
    ;; Process no-break requests and macros (including font-change macros):
    (goto-char from)
    (woman1-roff-buffer)

    ;; Process strings and special character escapes \(xx:
    ;; (Must do this BEFORE fontifying!)
    (goto-char from)
    (woman-strings)
    ;; Special chars moved after translation in
    ;; `woman2-process-escapes' (for pic.1):
;    (goto-char from)
;    (woman-special-characters)
    
    ;; Process standard font-change requests and escapes:
    (goto-char from)
    (woman-change-fonts)
    
    ;; 1/2 em vertical motion \d, \u and general local vertical motion
    ;; \v'+/-N' simulated using TeX ^ and _ symbols for now.
    (goto-char from)
    (let ((first t))			; assume no nesting!
      (while (re-search-forward "\\\\\\([du]\\|v'[^']*'\\)" nil t)
	(let* ((esc (match-string 1))
	       (repl (if (or (= (aref esc 0) ?u)
			     (and (>= (length esc) 2) (= (aref esc 2) ?-)))
			 "^" "_")))
	  (cond (first
		 (replace-match repl nil t)
		 (put-text-property (1- (point)) (point)
				    'face woman-addition-face)
		 (WoMan-warn
		  "Initial vertical motion escape \\%s simulated" esc)
		 (WoMan-log
		  "      by TeX `%s' in woman-addition-face!" repl))
		(t
		 (woman-delete-match 0)
		 (WoMan-warn
		  "Terminal vertical motion escape \\%s ignored!" esc)))
	  (setq first (not first))
	  )))

    ;; \h'+/-N' local horizontal motion -- ignore for now.
    ;; Could implement large forward motions.
    (goto-char from)
    (while (re-search-forward "\\\\h'[^']*'" nil t)
      (WoMan-warn
       "Local horizontal motion escape \\%s ignored!" (match-string 0))
      (woman-delete-match 0))

    ;; Process remaining unpaddable and digit-width spaces:
    (goto-char from)
;    (while (re-search-forward "\\(\\\\\\) " nil t)
;      ;; Done like this to preserve any text properties of the space.
;      (woman-delete-match 1))
    (while (re-search-forward "\\\\[ 0]" nil t)
      (replace-match " "))
    
    ;; Process formatting macros
    (goto-char from)
    (woman2-roff-buffer)

    ;; Go back and process negative vertical space if necessary:
    (if woman-negative-vertical-space
	(woman-negative-vertical-space from))

    ;; Must return the new end of file if used in format-alist.
    (point-max)))


;;; Process ignore requests (.ig), conditionals (.if etc.),
;;; source-switch (.so), macro definitions (.de etc.) and macro
;;; expansions.

(defvar woman0-if-to)			; marker bound in woman0-roff-buffer
(defvar woman0-macro-alist)		; bound in woman0-roff-buffer
(defvar woman0-search-string)		; bound in woman0-roff-buffer
(defvar woman0-search-string-start	; bound in woman0-roff-buffer
  "^[.'][ \t]*\\(ig\\|if\\|ie\\|el\\|so\\|de\\|am")
(defconst woman0-search-string-end "\\)\\([ \t]+\\|$\\)")
;; May need other terminal characters, e.g. \, but NOT \n!
;; Alternatively, force maximal match (Posix?)

(defun woman0-roff-buffer (from)
  "Process conditional-type requests and user-defined macros,
re-scanning new text as appropriate."
  (goto-char from)
  (let ((woman0-if-to (make-marker))
	request woman0-macro-alist
	(woman0-search-string-start woman0-search-string-start)
	(woman0-search-string
	 (concat woman0-search-string-start woman0-search-string-end))
	)
    (while (re-search-forward woman0-search-string nil t)
      (setq request (match-string 1))
      (cond ((string= request "ig") (woman0-ig))
	    ((string= request "if") (woman0-if "if"))
	    ((string= request "ie") (woman0-if "ie"))
	    ((string= request "el") (woman0-el))
	    ((string= request "so") (woman0-so))
	    ((string= request "de") (woman0-de))
	    ((string= request "am") (woman0-de 'append))
	    (t                      (woman0-macro request))))
    (set-marker woman0-if-to nil)
    ))

(defun woman0-ig ()
  ".ig yy -- Discard input up to `..'.
\(Should be up to call of yy, which defaults to `.')."
  ;; The terminal request MUST begin with . (not ')!
  (beginning-of-line)
  (let ((from (point)))
    (delete-region from (re-search-forward "^\\.[ \t]*\\..*\n"))))

(defsubst woman0-process-escapes (from to)
  "Process escapes within an if/ie condition."
  (woman-strings to)
  (goto-char from)			; necessary!
  ;; (woman-special-characters to)
  ;; (beginning-of-line)		; necessary!
  ;; The following does both above two lines AND width function!
  ;; Strip font-change escapes:
  (while (re-search-forward "\\\\f\\((..\\|.\\)" to t)
    (woman-delete-match 0))
  (goto-char from)			; necessary!
  (woman2-process-escapes to 'numeric))

(defun woman0-if (request)
  ".if/ie c anything -- Discard unless c `evaluates to true' and
remember condition for use by a subsequent `.el'."
  ;; c evaluates to a one-character built-in condition name or
  ;; 'string1'string2' or a number > 0, prefix ! negates.
  ;; \{ ... \} for multi-line use.
  ;; Leaves point at start of new text.
  (woman-delete-match 0)
  ;; (delete-horizontal-space)
  ;; Process escapes in condition:
  (let ((from (point)) negated n (c 0))
    (set-marker woman0-if-to
		(save-excursion (skip-syntax-forward "^ ") (point)))
    ;; Process condition:
    (if (setq negated (= (following-char) ?!)) (delete-char 1))
    (cond
     ((looking-at "[no]") (setq c t))	; accept n(roff) and o(dd page)
     ((looking-at "[te]") (setq c nil))	; reject t(roff) and e(ven page)
     ;; Unrecognised letter so reject:
     ((looking-at "[A-Za-z]") (setq c nil)
      (WoMan-warn "%s %s -- unrecognised condition name rejected!"
		  request (match-string 0)))
     ;; Accept strings if identical:
     (;;(looking-at "\\(['|@]\\)\\(.*\\)\\1\\(.*\\)\\1")
      ;; (looking-at "\\([^-+.0-9\(\\]\\)\\([^ \t]*\\)\\1\\([^ \t]*\\)\\1")
      ;; How general can the string delimiter be?
      ;; Official is '; rcsintro uses |; gcc uses @
      ;; (setq c (string= (match-string 2) (match-string 3)))
      (and (looking-at "[^-+.0-9\(\\]")
	   (looking-at
	    (concat (match-string 0) "\\([^" (match-string 0) "]*\\)"
		    (match-string 0) "[^" (match-string 0) "]*"
		    (match-string 0))))
      (let ((end1 (make-marker)))
	(set-marker end1 (match-end 1))	; end of first string
	(woman0-process-escapes from woman0-if-to)
	(setq c (string= (buffer-substring (1+ from) end1)
			 (buffer-substring (1+ end1) (1- woman0-if-to))))
	(set-marker end1 nil))
      )
     ;; Accept numeric value if > 0:
     ((numberp (setq n (save-excursion
			 (woman0-process-escapes from woman0-if-to)
			 (woman-parse-numeric-arg))))
      (setq c (> n 0)))
     )
    (if (eq c 0)
	(woman-if-ignore woman0-if-to request) ; ERROR!
      (woman-if-body request woman0-if-to (eq c negated)))
    ))

(defun woman-if-body (request to delete) ; should be reversed as `accept'?
  "Process if-body, including \\{ ... \\}, deleting it if arg is non-nil."
  ;; Assume concealed newlines already processed.
  (let ((from (point)))
    (if to (delete-region (point) to))
    (delete-horizontal-space)
    (cond ((looking-at "[^{\n]*\\\\{\\s *") ; multi-line
	   ;; including preceding .if(s) and following newline
	   (let ((from (point)))
	     (woman-delete-match 0)
	     ;; Allow for nested \{ ... \} -- BUT BEWARE that this
	     ;; algorithm only supports one level of nesting!
	     (while
		 (and (re-search-forward
		       "\\(\\\\{\\)\\|\\(\n[.']\\)?[ \t]*\\\\}[ \t]*")
		      (match-string 1))
	       (re-search-forward "\\\\}"))
	     (delete-region (if delete from (match-beginning 0)) (point))
	     (if (looking-at "^$") (delete-char 1))
	     ))
	  (delete (woman-delete-line 1)) ; single-line
	  )
    ;; Process matching .el anything --
    ;; discard unless previous .ie c `evaluated to false'.
    (cond ((and (string= request "ie")		
		(re-search-forward "^[.'][ \t]*el" nil t))
	   (woman-delete-match 0)
	   (delete-horizontal-space)
	   (woman-if-body "el" nil (not delete))))
    (goto-char from)
    ))

(defun woman0-el ()
  "Isolated .el request -- should not happen!"
  (WoMan-warn "el request without matching `ie' rejected!")
  (cond (woman-ignore
	 (woman-delete-match 0)
	 (delete-horizontal-space)
	 (woman-if-body "el" nil t))
	(t				; Ignore -- leave in buffer
	 ;; This does not work too well, but it's only for debugging!
	 (skip-chars-forward "^ \t")
	 (if (looking-at "[ \t]*\\{") (search-forward "\\}"))
	 (forward-line 1))))

(defun woman-if-ignore (to request)
  ;; (WoMan-warn ".if request ignored -- condition not handled!")
  (WoMan-warn-ignored request "ignored -- condition not handled!")
  (if woman-ignore
      (woman-if-body request to t)
    ;; Ignore -- leave in buffer
    ;; This does not work too well, but it's only for debugging!
    (skip-chars-forward "^ \t")
    (if (looking-at "[ \t]*\\{") (search-forward "\\}"))
    (forward-line 1)))

(defun woman0-so ()
  ".so filename -- Switch source file.  `.so' requests may be nested."
  ;; Leaves point at start of new text.
  ;; (skip-chars-forward " \t")
  (let* ((beg (point))
	 (end (progn (woman-forward-arg 'unquote) (point)))
	 (filename (buffer-substring beg end))
	 ;; The following filename handling is a bit of a hack!
	 (filename (file-name-nondirectory filename))
	 (filename (woman-file-name filename))
	 )
    (beginning-of-line)
    (woman-delete-line 1)
    (woman-insert-file-contents filename 0)
    ))


;;; Process macro definitions:

(defsubst woman-unescape (macro)
  "Replace \\\\ by \\."
  (let (start)
    (while (setq start (string-match "\\\\\\\\" macro start)) ; regexp
      (setq macro (replace-match "\\" t t macro)
	    start (1+ start)))
    macro))

(defun woman0-de (&optional append)
  "Process .de/am xx yy -- (re)define/append macro xx; end at `..'.
\(Should be up to call of yy, which defaults to `.')"
  ;; Modelled on woman-strings.  BEWARE: Processing of .am is a hack!
  ;; Add support for .rm?
  ;; (skip-chars-forward " \t")
  (if (eolp)				; ignore if no argument
      ()
    (looking-at "[^ \t\n]+")		; macro name
    (let* ((macro (match-string 0)) from
	   (previous (assoc macro woman0-macro-alist)))
      (if (not previous)
	  (setq woman0-search-string-start
		(concat woman0-search-string-start "\\|" macro)
		woman0-search-string
		(concat woman0-search-string-start woman0-search-string-end)
		))
      ;; Macro body runs from start of next line to line
      ;; beginning with `..'."
      ;; The terminal request MUST begin with `.' (not ')!
      (forward-line)
      (setq from (point))
      (re-search-forward "^\\.[ \t]*\\.")
      (beginning-of-line)
      (let ((body (woman-unescape (buffer-substring from (point)))))
	(if (and append previous)
	    (setq previous (cdr previous)
		  body (concat body (cdr previous))
		  append (car previous)
		  ))
	(setq macro (cons macro (cons append body))))
      ;; This should be an update, but consing a new string
      ;; onto the front of the alist has the same effect:
      (setq woman0-macro-alist (cons macro woman0-macro-alist))
      (forward-line)
      (delete-region from (point))
      (backward-char)			; return to end of .de/am line
      ))
  (beginning-of-line)			; delete .de/am line
  (woman-delete-line 1))

(defun woman0-macro (request)
  "Process macro call like a request."
  ;; Leaves point at start of new text.
  (let ((macro (assoc request woman0-macro-alist)))
    (if macro
	(woman-interpolate-macro (cdr macro))
      ;; SHOULD DELETE THE UNINTERPRETED REQUEST!!!!!
      ;; Output this message once only per call (cf. strings)?
      (WoMan-warn "Undefined macro %s not interpolated!" request))))

(defun woman-interpolate-macro (macro)
  "Interpolate (.de) or append (.am) a macro expansion into the buffer."
  ;; Could make this more efficient by checking which arguments are
  ;; actually used in the expansion!
  (skip-chars-forward " \t")
  ;; Process arguments:
  (let ((argno 0) (append (car macro))
	argno-string formal-arg from actual-arg start)
    (setq macro (cdr macro))
    (while (not (eolp))
      ;; Get next actual arg:
      (setq argno (1+ argno))
      (setq argno-string (format "%d" argno))
      (setq formal-arg (concat "\\\\\\$" argno-string)) ; regexp
      (setq from (point))
      (woman-forward-arg 'unquote 'noskip)
      (setq actual-arg (buffer-substring from (point)))
      (skip-chars-forward " \t")  ; now skip following whitespace!
      ;; Replace formal arg with actual arg:
      (setq start nil)
      (while (setq start (string-match formal-arg macro start))
	(setq macro (replace-match actual-arg t t macro)))
      )
    ;; Delete any remaining formal arguments:
    (setq start nil)
    (while (setq start (string-match "\\\\\\$." macro start))
      (setq macro (replace-match "" t t macro)))
    ;; Replace .$ number register with actual arg:
    ;; (Do this properly via register mechanism later!)
    (setq start nil)
    (while (setq start (string-match "\\\\n(\\.\\$" macro start)) ; regexp
      (setq macro (replace-match argno-string t t macro)))
    (if append
	(forward-char)
      (beginning-of-line)
      (woman-delete-line 1))
    (save-excursion			; leave point at start of new text
      (insert macro))))


;;; Process strings:

(defun woman-strings (&optional to)
  "Process ?roff strings: defined/updated by `.ds xx string' requests,
interpolated by `\*x' and `\*(xx' escapes."
  ;; Add support for .as and .rm?
  (while
      ;; Find .ds requests and \* escapes:
      (re-search-forward "\\(^[.'][ \t]*ds\\)\\|\\\\\\*" to t)
    (cond ((match-string 1)		; .ds
	   (skip-chars-forward " \t")
	   (if (eolp)			; ignore if no argument
	       ()
	     (re-search-forward "[^ \t\n]+")
	     (let ((string (match-string 0)))
	       (skip-chars-forward " \t")
;		 (setq string
;		       (cons string
;			     ;; hack (?) for CGI.man!
;			     (cond ((looking-at "\"\"") "\"")
;				   ((looking-at ".*") (match-string 0)))
;			     ))
	       ;; Above hack causes trouble in arguments!
	       (looking-at ".*")
	       (setq string (cons string (match-string 0)))
	       ;; This should be an update, but consing a new string
	       ;; onto the front of the alist has the same effect:
	       (setq woman-string-alist (cons string woman-string-alist))
	       ))
	   (beginning-of-line)
	   (woman-delete-line 1))
	  (t				; \*
	   (let ((beg (match-beginning 0)))
	     (cond ((= (following-char) ?\( )
		    (forward-char)
		    (re-search-forward ".."))
		   (t (re-search-forward ".")))
	     (let* ((stringname (match-string 0))
		   (string (assoc stringname woman-string-alist)))
	       (cond (string
		      (delete-region beg (point))
		      ;; Temporary hack in case string starts with a
		      ;; control character:
		      (if (bolp) (insert "\\&"))
		      (insert (cdr string)))
		     (t
		      (WoMan-warn "Undefined string %s not interpolated!"
			       stringname)
		      (cond (woman-ignore
			     ;; Output above message once only per call
			     (delete-region beg (point))
			     (setq woman-string-alist
				   (cons (cons stringname "")
					 woman-string-alist))))
		      ))
	       ))
	   ))
    ))


;;; Process special character escapes \(xx:

(defconst woman-special-characters
  ;; To be built heuristically as required!
  '(("em" . "--") ("bu" . "*") ("fm" . "'") ("co" . "(C)")
    ("pl" . "+") ("mi" . "-") ("**" . "*")
    ("aa" . "'") ("ul" . "_")
    ("*S" . "Sigma")
    (">=" . ">=") ("<=" . "<=") ("mu" . " x ")
    ("+-" . "+/-") ("bv" . "|")
    ("lq" . "\"") ("rq" . "\"")
    ;; groff etc. extensions:
    ("aq" . "'") ("ha" . "^") ("ti" . "~")
    )
  "Alist of special character codes and nroff equivalents.")

(defun match-string-no-properties (subexp)
  "Like `match-string' but with no text properties."
  (if (match-beginning subexp)
      (buffer-substring-no-properties
       (match-beginning subexp) (match-end subexp))))

(defun woman-special-characters (to)
  "Process special character escapes \(xx."
  ;; Must be done AFTER translation, which may use special chars.
  (while (re-search-forward "\\\\(\\(..\\)" to t)
    (let ((replacement
	   (assoc (match-string-no-properties 1) woman-special-characters)))
      (if replacement
	  (replace-match (cdr replacement) t t)
	(WoMan-warn "Special character \\(%s not interpolated!"
		    (match-string 1))
	(if woman-ignore (woman-delete-match 0)))
      )))


;;; Formatting macros that do not cause a break:

(defvar request)  ; Bound locally by woman1-roff-buffer
(defvar unquote)  ; Bound locally by woman1-roff-buffer

(defun woman-unquote (to)
  "Delete any double-quote characters between point and TO
and leave point at TO (which should be a marker)."
  (let (in-quote)
    (while (search-forward "\"" to 1)
      (if (and in-quote (looking-at "\""))
	  ;; Repeated double-quote represents single double-quote
	  (delete-char 1)
	(if (or in-quote (looking-at ".*\"")) ; paired
	    (delete-char -1))
	(setq in-quote (not in-quote))
	))
    (if in-quote
	(WoMan-warn "Unpaired \" in .%s arguments." request))
    ))

(defsubst woman-unquote-args ()
  "Delete any double-quote characters up to the end of the line."
  (woman-unquote (save-excursion (end-of-line) (point-marker))))

(defun woman1-roff-buffer ()
  "Process non-breaking requests."
  (let ((case-fold-search t)
	request fn unquote)
    (while
	;; Find next control line:
	(re-search-forward woman-request-regexp nil t)
      (cond
       ;; Construct woman function to call:
       ((setq fn (intern-soft
		  (concat "woman1-"
			  (setq request (match-string 1)))))
	(if (get fn 'notfont)		; not a font-change request
	    (funcall fn)
	  ;; Delete request or macro name:
	  (woman-delete-match 0)
	  ;; If no args then apply to next line else unquote args
	  ;; (unquote is used by called function):
	  (setq unquote (not (eolp)))
	  (if (eolp) (delete-char 1))
;	    ;; Hide leading control character in unquoted argument:
;	    (cond ((memq (following-char) '(?. ?'))
;		   (insert "\\&")
;		   (beginning-of-line)))
	  ;; Call the appropriate function:
	  (funcall fn)
	  ;; Hide leading control character in quoted argument (only):
	  (if (and unquote (memq (following-char) '(?. ?')))
	      (insert "\\&"))
	  )
	)))))

;;; Font-changing macros:

(defun woman1-B ()
  ".B -- Set words of current line in bold font."
  (woman1-B-or-I ".ft B\n"))

(defun woman1-I ()
  ".I -- Set words of current line in italic font."
  (woman1-B-or-I ".ft I\n"))

(defun woman1-B-or-I (B-or-I)
  ".B/I -- Set words of current line in bold/italic font."
  ;; Should NOT concatenate the arguments!
  (insert B-or-I) ; because it might be a control line
  ;; Return to bol to process .SM/.B, .B/.if etc.
  ;; or start of first arg to hide leading control char.
  (save-excursion
    (if unquote
	(woman-unquote-args)
      (while (looking-at "^[.']") (forward-line))
      (end-of-line)
      (delete-horizontal-space))
    (insert "\\fR")))

(defun woman1-SM ()
  ".SM -- Set the current line in small font, i.e. IGNORE!"
  nil)

(defalias 'woman1-SB 'woman1-B)
;; .SB -- Set the current line in small bold font, i.e. just embolden!
;; (This is what c:/usr/local/share/groff/tmac/tmac.an does.  The
;; Linux man.7 is wrong about this!)

(defun woman1-BI ()
  ".BI -- Join words of current line alternating bold and italic fonts."
  (woman1-alt-fonts (list "\\fB" "\\fI")))

(defun woman1-BR ()
  ".BR -- Join words of current line alternating bold and Roman fonts."
  (woman1-alt-fonts (list "\\fB" "\\fR")))

(defun woman1-IB ()
  ".IB -- Join words of current line alternating italic and bold fonts."
  (woman1-alt-fonts (list "\\fI" "\\fB")))

(defun woman1-IR ()
   ".IR -- Join words of current line alternating italic and Roman fonts."
 (woman1-alt-fonts (list "\\fI" "\\fR")))

(defun woman1-RB ()
   ".RB -- Join words of current line alternating Roman and bold fonts."
  (woman1-alt-fonts (list "\\fR" "\\fB")))

(defun woman1-RI ()
   ".RI -- Join words of current line alternating Roman and italic fonts."
  (woman1-alt-fonts (list "\\fR" "\\fI")))

(defun woman1-alt-fonts (fonts)
  "Join words using alternating fonts in FONTS, which MUST be a dynamic list."
  (nconc fonts fonts)			; circular list!
  (insert (car fonts))
  ;; Return to start of first arg to hide leading control char:
  (save-excursion
    (setq fonts (cdr fonts))
    (woman-forward-arg unquote 'concat)	; unquote is bound above
;    (cond ((= (preceding-char) ?\\)	; was \(space) -- unpaddable space
;	   (delete-char -1) (insert ? )))
    (while (not (eolp))
      (insert (car fonts))
      (setq fonts (cdr fonts))
      (woman-forward-arg unquote 'concat) ; unquote is bound above
;      (cond ((= (preceding-char) ?\\)	; was \(space) -- unpaddable space
;	     (delete-char -1) (insert ? )))
      )
    (insert "\\fR")
    ))

(defun woman-forward-arg (&optional unquote concat)
  "Move forward over one ?roff argument, deleting any argument quotes
if optional arg UNQUOTE is non-nil and joining arguments if optional
arg CONCAT is non-nil."
  (if (eq (following-char) ?\")
      (progn
	(if unquote (delete-char 1) (forward-char))
	(re-search-forward "\"\\|$")
	;; Repeated double-quote represents single double-quote
	(while (eq (following-char) ?\") ; paired
	  (if unquote (delete-char 1) (forward-char))
	  (re-search-forward "\"\\|$"))
	(if (eq (preceding-char) ?\")
	    (if unquote (delete-backward-char 1))
	  (WoMan-warn "Unpaired \" in .%s arguments." request)
	  ))
    ;; (re-search-forward "[^\\\n] \\|$")	; inconsistent
    (skip-syntax-forward "^ ")
    (while (= (preceding-char) ?\\)	; skip escaped space
      (forward-char)
      (skip-syntax-forward "^ "))
    )
;  (if concat
;      (woman-delete-following-space)
;    (skip-chars-forward " \t"))		; don't skip eol!
  (cond ((null concat) (skip-chars-forward " \t")) ; don't skip eol!
	((eq concat 'noskip))  ; do not skip following whitespace
	(t (woman-delete-following-space)))
  )

(put 'woman1-ul 'notfont t)
(defun woman1-ul ()
  ".ul N -- Underline (italicize) the next N input lines, default N = 1."
  (let ((N (if (eolp) 1 (woman-parse-numeric-arg)))) ; woman-get-numeric-arg ?
    (woman-delete-whole-line)
    (insert ".ft I\n")
    (forward-line N)
    (insert ".ft R\n")
    ))

;; Other non-breaking requests:

;; Hyphenation
;; Warnings commented out.

(put 'woman1-nh 'notfont t)
(defun woman1-nh ()
  ".nh -- No hyphenation, i.e. IGNORE!"
  ;; Must be handled here to avoid breaking!
  ;; (WoMan-log-1 ".nh request ignored -- hyphenation not supported!")
  (woman-delete-whole-line))

(put 'woman1-hy 'notfont t)
(defun woman1-hy ()
  ".hy N -- Set hyphenation mode to N, i.e. IGNORE!"
  ;; (WoMan-log-1 ".hy request ignored -- hyphenation not supported!")
  (woman-delete-whole-line))

(put 'woman1-hc 'notfont t)
(defun woman1-hc ()
  ".hc c -- Set hyphenation character to c, i.e. delete it!"
  (let ((c (char-to-string (following-char))))
    ;; (WoMan-log
     ;; "Hyphenation character %s deleted -- hyphenation not supported!" c)
    (woman-delete-whole-line)
    (setq c (concat "\\(" c "\\)\\|^[.'][ \t]*hc"))
    (save-excursion
      (while (and (re-search-forward c nil t)
		  (match-string 1))
	(delete-char -1)))
    ))

(put 'woman1-hw 'notfont t)
(defun woman1-hw ()
  ".hw words -- Set hyphenation exception words, i.e. IGNORE!"
  ;; (WoMan-log-1 ".hw request ignored -- hyphenation not supported!")
  (woman-delete-whole-line))

;; Other non-breaking requests correctly ignored by nroff:

(put 'woman1-ps 'notfont t)
(defalias 'woman1-ps 'woman-delete-whole-line)
  ;; .ps -- Point size -- IGNORE!

(put 'woman1-ss 'notfont t)
(defalias 'woman1-ss 'woman-delete-whole-line)
  ;; .ss -- Space-character size -- IGNORE!

(put 'woman1-cs 'notfont t)
(defalias 'woman1-cs 'woman-delete-whole-line)
  ;; .cs -- Constant character space (width) mode -- IGNORE!

(put 'woman1-ne 'notfont t)
(defalias 'woman1-ne 'woman-delete-whole-line)
  ;; .ne -- Need vertical space -- IGNORE!

(put 'woman1-bd 'notfont t)
(defalias 'woman1-bd 'woman-delete-whole-line)
  ;; .bd -- Embolden font -- IGNORE!

;; Non-breaking SunOS-specific macros:

(defun woman1-TX ()
  ".TX t p -- Resolve SunOS abbrev t and join to p (usually punctuation)."
  (insert "SunOS ")
  (woman-forward-arg 'unquote 'concat))

(put 'woman1-IX 'notfont t)
(defalias 'woman1-IX 'woman-delete-whole-line)
  ;; .IX -- Index macro, for Sun internal use -- IGNORE!


;;; Direct font selection:

(defconst woman-font-alist
  '(("R" . 'default)
    ("I" . woman-italic-face)
    ("B" . woman-bold-face)
    ("P" . 'previous)
    ("1" . 'default)
    ("2" . woman-italic-face)
    ("3" . woman-bold-face)		; used in bash.1
    )
  "Alist of ?roff font indicators and woman font variables and names.")

(defun woman-change-fonts ()
  "Process font changes."
  ;; ***** NEEDS REVISING IF IT WORKS OK *****
  ;; Paragraph .LP/PP/HP/IP/TP and font .B/.BI etc. macros reset font.
  ;; Should .SH/.SS reset font?
  ;; Font size setting macros (?) should reset font.
  (let ((woman-font-alist woman-font-alist) ; for local updating
	(previous-pos (point))
	(previous-font 'default)
	(current-font 'default))
    (while
	;; Find font requests, paragraph macros and font escapes:
	(re-search-forward
	 "^[.'][ \t]*\\(\\(\\ft\\)\\|\\(.P\\)\\)\\|\\(\\\\f\\)" nil 1)
      (let (font beg notfont)
	;; Match font indicator and leave point at end of sequence:
	(cond (;;(string= (match-string 1) "ft")
	       (match-string 2)
	       ;; .ft request found
	       (setq beg (match-beginning 0))
	       (skip-chars-forward " \t")
	       (if (eolp)		; default is previous font
		   (setq font previous-font)
		 (looking-at "[^ \t\n]+"))
	       (forward-line))		; end of control line and \n
	      (;;(member (match-string 1) '("LP" "PP")) ; TEMPORARY!
	       (match-string 3)
	       ;; Macro that resets font found
	       (setq font 'default))
	      ((match-string 4)
	       ;; \f escape found
	       (setq beg (match-beginning 0))
	       (cond ((= (following-char) ?\( )
		      (forward-char)
		      (re-search-forward ".."))
		     (t (re-search-forward ".")))
	       )
	      (t (setq notfont t)))
	(if notfont
	    ()
	  ;; Get font name:
	  (or font
	      (let ((fontstring (match-string 0)))
		(setq font (assoc fontstring woman-font-alist)
		      ;; NB: woman-font-alist contains VARIABLE NAMES.
		      font (if font
			       (eval (cdr font))
			     (WoMan-warn "Unknown font %s." fontstring)
			     ;; Output this message once only per call ...
			     (setq woman-font-alist
				   (cons (cons fontstring 'woman-unknown-face)
					 woman-font-alist))
			     woman-unknown-face)
		      )))
	  ;; Delete font control line or escape sequence:
	  (cond (beg (delete-region beg (point))
		     (if (eq font 'previous) (setq font previous-font))))
	  (woman-set-face previous-pos (point) current-font)
	  (if beg
	      ;; Explicit font control
	      (setq previous-pos (point)
		    previous-font current-font)
	    ;; Macro that resets font
	    ;; (forward-line)		; DOES NOT WORK!  but unnecessary?
	    ;; Must process font changes in any paragraph tag!
	    (setq previous-pos (point)
		  previous-font 'default))
	  (setq current-font font)
	  )))
    ;; Set font after last request up to eob:
    (woman-set-face previous-pos (point) current-font)
    ))

(defun woman-set-face (from to face)
  "Set the face of the text from FROM to TO to face FACE,
ignoring the default face and underlining only word characters."
  (or (eq face 'default)		; ignore
      (not woman-fontify)
      (if (face-underline-p face)
	  (save-excursion
	    (goto-char from)
	    (skip-syntax-forward "^w" to)
	    (while (< (point) to)
	      (let ((here (point)))
		(skip-syntax-forward "w" to)
		(put-text-property here (point) 'face face)
		(skip-syntax-forward "^w" to)
		)))
	(put-text-property from to 'face face))
      ))


;;; Output translation

(defvar translations nil)  ; Also bound locally by woman2-roff-buffer
;; A list of the form (\"[ace]\" (a . b) (c . d) (e . ?\ )) or nil.

(defun woman-get-next-char ()
  "Return and delete next char in buffer, including special chars."
  (if ;;(looking-at "\\\\(\\(..\\)")
      ;; Match special \(xx and strings \*x, \*(xx: 
      (looking-at "\\\\\\((..\\|\\*\\((..\\|.\\)\\)")
      (prog1 (match-string 0)
	(woman-delete-match 0))
    (prog1 (char-to-string (following-char))
      (delete-char 1))))

(defun woman2-tr (to)
  ".tr abcde -- Translate a -> b, c -> d, ..., e -> space.
\(Breaks, but should not.)  Supports special chars."
  ;; This should be an update, but consing onto the front of the alist
  ;; has the same effect and match duplicates should not matter.
  ;; Initialize translation data structures:
  (let ((matches (car translations))
	(alist (cdr translations))
	a b)
    ;; `matches' must be a string:
    (setq matches
	  (concat (if matches (substring matches 1 -1)) "]"))
    ;; Process .tr arguments:
    (while (not (eolp))			; (looking-at "[ \t]*$") ???
      (setq a (woman-get-next-char))
      (if (eolp)
	  (setq b " ")
	(setq b (woman-get-next-char)))
      (setq matches
	    (if (= (length a) 1)
		(concat a matches)
	      (concat matches "\\|\\" a))
	    alist (cons (cons a b) alist)))
    (delete-char 1)			; no blank line
    ;; Rebuild translations list:
    (setq matches
	  (if (= (string-to-char matches) ?\])
	      (substring matches 3)
	    (concat "[" matches))
	  translations (cons matches alist))
    ;; Format any following text:
    (woman2-format-paragraphs to)
    ))

(defsubst woman-translate (to)
  "Translate up to marker TO.  Do this last of all transformations."
  (if translations
      (let ((matches (car translations))
	    (alist (cdr translations)))
	(while (re-search-forward matches to t)
	  ;; Done like this to retain text properties and
	  ;; support translation of special characters:
	  (insert-before-markers-and-inherit
	   (cdr (assoc
		 (buffer-substring-no-properties
		  (match-beginning 0) (match-end 0))
		 alist)))
	  (woman-delete-match 0))
	)))


;;; Registers:

(defvar woman-registers			; these are all read-only
  '((".H" . 24) (".V" . 48)		; resolution in basic units
    (".g" . 0)				; not groff
    ;; (Iff emulating groff need to implement groff italic correction
    ;; \/, e.g. for pic.1)
    (".i" . left-margin)		; current indent
    (".j" . woman-justify)		; current adjustment
    (".l" . fill-column)		; current line length
    (".v" . 48)				; current vertical line spacing
    )
  "Register alist: the key is the register name as a string.
Also bound locally in `woman2-roff-buffer'.")

(defun woman-mark-horizonal-position ()
  "\\kx -- Store current horizontal position in INPUT LINE in register x."
  (while (re-search-forward "\\\\k\\(.\\)" nil t)
    (goto-char (match-beginning 0))
    (setq woman-registers
	  (cons (cons (match-string 1) (current-column))
		woman-registers))
    (woman-delete-match 0)))

(defsubst woman2-process-escapes-to-eol (&optional numeric)
  "Process remaining escape sequences up to eol.  Handle numeric
arguments specially if optional argument NUMERIC is non-nil."
  (woman2-process-escapes
   (save-excursion (end-of-line) (point-marker))
   numeric))

(defun woman2-nr (to)
  ".nr R +/-N M -- Register R is assigned value +/-N wrt to previous
value, if any.  The increment for auto-incrementing is set to M.
\[Increment etc. not implemented.  Breaks, but should not!]"
  (let* ((name (buffer-substring
		(point)
		(progn (forward-word 1) (point))))
	 (value (progn
		  (skip-chars-forward " \t")
		  (woman2-process-escapes-to-eol 'numeric)
		  (woman-parse-numeric-arg))))
    (cond ((null value)
	   (WoMan-warn ".nr %s: null value assigned!" name))
	  ((symbolp value)
	   (setq value (list 'quote value))))
    (setq woman-registers
	  (cons (cons name value) woman-registers))
    (woman-delete-whole-line)
    (woman2-format-paragraphs to)))


;;; Numeric (and "non-text") request arguments:

(defsubst woman-get-numeric-arg ()
  "Get the value of a numeric argument at or after point, without
moving point.  The argument can include the width function and scale
indicators.  Assumes 10 characters per inch."
  (woman2-process-escapes-to-eol 'numeric)
  (save-excursion (woman-parse-numeric-arg)))

(defun woman-parse-numeric-arg ()
  "Get the value of a numeric expression at or after point, leaving
point after the argument.  The expression may be an argument in quotes."
  (if (= (following-char) ?\") (forward-char))
  (let ((value (woman-parse-numeric-value)) op)
    (while (cond
	    ((looking-at "[+-/*%]")	; arithmetic operators
	     (forward-char)
	     (setq op (intern-soft (match-string 0)))
	     (setq value (funcall op value (woman-parse-numeric-value))))
	    ((looking-at "[<=>]=?")	; relational operators
	     (goto-char (match-end 0))
	     (setq op (or (intern-soft (match-string 0))
			 (intern-soft "=")))
	     (setq value (if (funcall op value (woman-parse-numeric-value))
			     1 0)))
	    ((memq (setq op (following-char)) '(?& ?:)) ; Boolean and / or
	     (forward-char)
	     (setq value
		   ;; and / or are special forms, not functions, in ELisp
		   (if (eq op ?&)
		       ;; and
		       (if (> value 0)
			   (if (> (woman-parse-numeric-value) 0) 1 0)
			 ;; skip second operand
			 (prog1 0 (woman-parse-numeric-value)))
		     ;; or
		     (if (> value 0)
			 ;; skip second operand
			 (prog1 1 (woman-parse-numeric-value))
		       (if (> (woman-parse-numeric-value) 0) 1 0))
		     )))
	    ))
;    (if (looking-at "[ \t\nRC\)\"]")	; R, C are tab types
;	()
;      (WoMan-warn "Unimplemented numerical operator `%c' in %s"
;		  (following-char)
;		  (buffer-substring
;		   (save-excursion (beginning-of-line) (point))
;		   (save-excursion (end-of-line) (point))))
;      (skip-syntax-forward "^ "))
    value
    ))

(defun woman-parse-numeric-value ()
  "Get a single numeric value at or after point, leaving point after
the value.  It can be a number register or width function (which
assumes 10 characters per inch) and can include scale indicators.
The value may be an expression in parentheses."
  ;; Must replace every \' by some different single character first
  ;; before calling this function by calling
  ;; (woman2-process-escapes-to-eol 'numeric)
  (if (eq (following-char) ?\()
      ;; Treat parenthesized expression as a single value.
      (let (n)
	(forward-char)
	(setq n (woman-parse-numeric-arg))
	(skip-syntax-forward " ")
	(if (eq (following-char) ?\))
	    (forward-char)
	  (WoMan-warn "Parenthesis confusion in numeric expression!"))
	n)
    (let ((n (cond ((looking-at "[-+.0-9]+")
		    ;; currently needed to set match-end, even though
		    ;; string-to-number returns 0 if number not parsed.
		    (string-to-number (match-string 0)))
		   ((looking-at "\\\\n\\(\(\\(..\\)\\|\\(.\\)\\)")
		    ;; interpolate number register
		    (let ((n (cdr (assoc (or (match-string-no-properties 2)
					     (match-string-no-properties 3))
				      woman-registers))))
		      (if n (eval n)
			(WoMan-warn "Undefined register %s defaulted to 0."
				    (match-string 0))
			0)		; default to zero
		      ))
		   ((looking-at "\\\\w'\\([^']*\\)'")
		    ;; interpolate width of string
		    (- (match-end 1) (match-beginning 1)))
		   )))
      (if (null n)
	  ;; ERROR -- should handle this better!
	  (progn
	    (WoMan-warn "Numeric/register argument error: %s"
			(buffer-substring
			 (point)
			 (save-excursion (end-of-line) (point))))
	    (skip-syntax-forward "^ ")
	    0)
	(goto-char (match-end 0))
	;; Check for scale factor:
	(if
	    (cond
	     ((looking-at "\\s ") nil)	; stay put!
	     ((looking-at "[mnu]"))	; ignore
	     ((looking-at "i") (setq n (* n 10))) ; inch
	     ((looking-at "c") (setq n (* n 3.9))) ; cm
	     ((looking-at "P") (setq n (* n 1.7))) ; Pica
	     ((looking-at "p") (setq n (* n 0.14))) ; point
	     ;; NB: May be immediately followed by + or -, etc.,
	     ;; in which case do nothing and return nil.
	     )
	    (goto-char (match-end 0)))
	(if (numberp n) (round n) n))
      )))


;;; VERTICAL FORMATTING -- Formatting macros that cause a break:

; Vertical spacing philosophy:
; Delete all vertical space as it is encountered.  Then insert
; vertical space only before text, as required.

(defun woman2-roff-buffer ()
  "Process breaks.  Format paragraphs and headings."
  (let ((case-fold-search t)
	(to (make-marker))
	(canonically-space-region
	 (symbol-function 'canonically-space-region))
	(insert-and-inherit (symbol-function 'insert-and-inherit))
	(set-text-properties (symbol-function 'set-text-properties))
	(woman-registers woman-registers)
	fn request translations
	tab-stop-list)
    ;; ?roff does not squeeze multiple spaces, but does fill, so...
    (fset 'canonically-space-region 'ignore)
    ;; Try to avoid spaces inheriting underlines from preceding text!
    (fset 'insert-and-inherit (symbol-function 'insert))
    (fset 'set-text-properties 'ignore)
    (unwind-protect
	(while
	    ;; Find next control line:
	    (re-search-forward woman-request-regexp nil t)
	  (cond
	   ;; Construct woman function to call:
	   ((setq fn (intern-soft
		      (concat "woman2-"
			      (setq request (match-string 1)))))
	    ;; Delete request or macro name:
	    (woman-delete-match 0))
	   ;; Unrecognised request:
	   ((prog1 nil
	      ;; (WoMan-warn ".%s request ignored!" request)
	      (WoMan-warn-ignored request "ignored!")
	      ;; (setq fn 'woman2-LP)
	      ;; AVOID LEAVING A BLANK LINE!
	      ;; (setq fn 'woman2-format-paragraphs)
	      ))
	   ;; .LP assumes it is at eol and leaves a (blank) line,
	   ;; so leave point at end of line before paragraph:
	   ((or (looking-at "[ \t]*$")	; no argument
		woman-ignore)		; ignore all
	    ;; (beginning-of-line) (kill-line)
	    ;; AVOID LEAVING A BLANK LINE!
	    (beginning-of-line) (woman-delete-line 1))
	   (t (end-of-line) (insert ?\n))
	   )
	  (if (not (or fn
		       (and (not (memq (following-char) '(?. ?')))
			    (setq fn 'woman2-format-paragraphs))))
	      ()
	    ;; Find next control line:
	    (set-marker to (woman-find-next-control-line))
	    ;; Call the appropriate function:
	    (funcall fn to)))
      (if (not (eobp))			; This should not happen, but ...
	  (woman2-format-paragraphs (point-max-marker) woman-left-margin))
      (fset 'canonically-space-region canonically-space-region)
      (fset 'set-text-properties set-text-properties)
      (fset 'insert-and-inherit insert-and-inherit)
      (set-marker to nil))))

(defun woman-find-next-control-line ()
  "Find and return start of next control line."
;  (let ((to (save-excursion
;	      (re-search-forward "^\\." nil t))))
;    (if to (1- to) (point-max)))
  (let (to)
    (save-excursion
      ;; Must handle
      ;; ...\c
      ;; .br (and other requests?)
      ;; by deleting both the \c and the following request.
      ;; BEWARE THAT THIS CODE MAY BE UNRELIABLE!!!!!
      (while
	  (and
	   (setq to (re-search-forward "\\(\\\\c\\)?\n[.']" nil t))
	   (match-string 1)
	   (looking-at "br"))
	(goto-char (match-beginning 0))
	(woman-delete-line 2)))
    (if to (1- to) (point-max))))

(defun woman2-PD (to)
  ".PD d -- Set the interparagraph distance to d.
Round to whole lines, default 1 line.  (Breaks, but should not.)"
  ;; .ie \\n[.$] .nr PD (v;\\$1)
  ;; .el .nr PD .4v>?\n[.V]
  (woman-set-interparagraph-distance)
  (woman2-format-paragraphs to))

(defun woman-set-interparagraph-distance ()
  (setq woman-interparagraph-distance
	(if (eolp) 1 (woman-get-numeric-arg)))
  ;; Should allow .PD 0 to set zero line spacing
  (woman-delete-line 1))		; ignore remaining args

(defsubst woman-interparagraph-space ()
;  (if (> woman-interparagraph-distance 0)
;      (forward-line 1)			; leave 1 blank line
;    (woman-delete-line 1))		; do not leave blank line
  (setq woman-leave-blank-lines woman-interparagraph-distance)
  )

(defun woman2-TH (to)
  ".TH n c x v m -- Begin page named n of chapter c; x is extra
commentary; v alters page foot left; m alters page head center.
\(Should set prevailing indent (and tabs) to 5.)"
  (woman-forward-arg 'unquote 'concat)
  (insert ?\()
  (woman-forward-arg 'unquote 'concat)
  (insert ?\))
  (let ((start (point)) here)
    (while (not (eolp))
      (cond ((looking-at "\"\"[ \t]")
	     (delete-char 2)
	     ;; (delete-horizontal-space)
	     ))
      (delete-horizontal-space)
      (setq here (point))
      (insert " -- ")
      (woman-forward-arg 'unquote 'concat)
      ;; Delete repeated arguments:
      (if (string-match (buffer-substring here (point))
			(buffer-substring start here))
	  (delete-region here (point)))
      ))
  ;; Set heading in bold (point is at end of heading):
  ;; (Maybe this should be optional?)
  (woman-set-face
   (save-excursion (beginning-of-line) (point)) (point) woman-bold-face)
  (forward-line)
  (delete-blank-lines)
  (setq woman-left-margin woman-default-indent)
  (setq woman-prevailing-indent woman-default-indent)
  (woman2-format-paragraphs to woman-left-margin))

(defun woman2-SH (to)
  ".SH -- Sub-head.  Leave blank line and subhead.
Format following paragraph.  Set prevailing indent to 5."
  (if (eolp)				; If no args then
      (delete-char 1)			; apply to next line
    (woman-unquote-args)		; else unquote to end of heading
    (beginning-of-line))
  (woman2-process-escapes-to-eol)
  (woman-leave-blank-lines woman-interparagraph-distance)
  (setq woman-leave-blank-lines nil)
  ;; Set heading in bold (point is at beginning of heading):
  (woman-set-face
   (point) (save-excursion (end-of-line) (point)) woman-bold-face)
  (forward-line)
  (setq woman-left-margin woman-default-indent
	woman-nofill nil)		; fill output lines
  (setq woman-prevailing-indent woman-default-indent)
  (woman2-format-paragraphs to woman-left-margin))

(defun woman2-SS (to)
  ".SS -- Sub-sub-head.  Like .SH but indent heading 3 spaces."
  (if (eolp)				; If no args then
      (delete-char 1))			; apply to next line.
  (insert "   ")
  (beginning-of-line)
  (woman2-SH to))

(defun woman2-LP (to)
  ".LP,.PP -- Begin paragraph.  Set prevailing indent to 5.
Leave 1 blank line and format following paragraph."
  (woman-delete-line 1)			; ignore any arguments
  (woman-interparagraph-space)
  (setq woman-prevailing-indent woman-default-indent)
  (woman2-format-paragraphs to woman-left-margin))

(defalias 'woman2-PP 'woman2-LP)
(defalias 'woman2-P 'woman2-LP)

(defun woman2-ns (to)
  ".ns -- Turn on no-space mode and format following paragraph."
  ;; Should not cause a break!
  (woman-delete-line 1)			; ignore argument(s)
  (setq woman-nospace t)
  (woman2-format-paragraphs to))

(defun woman2-rs (to)
  ".rs -- Turn off no-space mode and format following paragraph."
  ;; Should not cause a break!
  (woman-delete-line 1)			; ignore argument(s)
  (setq woman-nospace nil)
  (woman2-format-paragraphs to))

(defun woman2-sp (to)
  ".sp N -- If N > 0 then leave 1 blank line and format following paragraph."
  (let ((N (if (eolp) 1 (woman-get-numeric-arg))))
    (if (>= N 0)
	(woman-delete-line 1)		; ignore argument(s)
      (setq woman-negative-vertical-space t)
      (insert ".sp ")
      (forward-line))
    (setq woman-leave-blank-lines N)
    (woman2-format-paragraphs to)))

(defun woman-negative-vertical-space (from)
  ".sp N with N < 0 => overlap following with preceding lines."
  ;; Run by woman-decode-region if necessary -- not usually required.
  (WoMan-warn "Negative vertical spacing support is experimental!")
  (goto-char from)
  (while
      ;; Find next control line:
      (re-search-forward "^\\.sp " nil t)
    (let ((N (woman-get-numeric-arg))
	  overlap overwritten)
      (woman-delete-whole-line)
      (setq from (point)
	    overlap (buffer-substring from
				      (progn (forward-line (- N)) (point))))
      (delete-region from (point))
      (forward-line N)
      (let ((imax (length overlap))
	    (i 0) c)
	(while (< i imax)
	  (setq c (aref overlap i))
	  (cond ((eq c ?\n)		; skip
		 (forward-line))
		((eolp)			; extend line
		 ;; Insert character INCLUDING TEXT PROPERTIES:
		 ;; (insert (substring overlap i (1+ i)))
		 (let ((eol (string-match "\n" overlap i)))
		   (insert (substring overlap i eol))
		   (setq i (or eol imax)))
		 )
		((eq c ?\ )		; skip
		 (forward-char))
		((eq c ?\t)		; skip
		 (if (eq (following-char) ?\t)
		     (forward-char)	; both tabs, just skip
		   (let ((i woman-tab-width))
		     (while (> i 0)
		       (if (eolp)
			   (insert ?\ )	; extend line
			 (forward-char)) ; skip
		       (setq i (1- i)))
		     )))
		(t
		 (if (or (eq (following-char) ?\ ) ; overwrite OK
			 overwritten) ; warning only once per ".sp -"
		     ()
		   (setq overwritten t)
		   (WoMan-warn
		    "Character(s) overwritten by negative vertical spacing in line %d"
		    (count-lines 1 (point))))
		 (delete-char 1) (insert (substring overlap i (1+ i)))))
	  (setq i (1+ i))
	  ))
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following function should probably do ALL width and number
;; register interpolation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun woman2-process-escapes (to &optional numeric)
  "Process remaining escape sequences up to marker TO, preserving point."
  ;; The first two cases below could be merged (maybe)!
  (let ((from (point)))
    ;; Discard zero width filler character used to hide leading dots
    ;; and zero width characters \|, \^:
    (while (re-search-forward "\\\\[&|^]" to t)
      (woman-delete-match 0))
    (goto-char from)
    ;; Interrupt text processing -- CONTINUE current text with the
    ;; next text line (after any control lines, unless processing to
    ;; eol):
    (while (re-search-forward "\\\\c.*\n?" to t)
      (woman-delete-match 0))
    ;; but do not delete the final newline ...
    (if (and (or (eobp) (= (point) to)) (not (bolp)))
	(insert-before-markers ?\n))
    (goto-char from)
    ;; The following two transformations moved from end of function.
    ;; CHECK THIS!!!!!
    (woman-translate to)
    (goto-char from)
    (woman-special-characters to)
    (goto-char from)
    ;; Printable version of the current escape character, ASSUMED to be `\'
    ;; This must be done LAST of all escape processing!
    ;; Done like this to preserve any text properties of the `\'
;    (while (re-search-forward "\\\\\\(e\\)" to t)
;      (woman-delete-match 1))
;    (goto-char from)
    (while (search-forward "\\" to t)
      (let ((c (following-char)))
	(cond ((memq c '(?e ?\\))	; \e -> \, \\ -> \
	       (delete-char 1))
	      ((eq c ?')		; \' -> '
	       (delete-char -1)
	       (cond (numeric		; except in numeric args, \' -> `
		      (delete-char 1)
		      (insert ?`))))
	      ((eq c ?\( ))		; uninterpreted special character
					; \(.. -- do nothing
	      ((eq c ?t)		; non-interpreted tab \t
	       (delete-char 1)
	       (delete-char -1)
	       (insert "\t"))
	      ((and numeric
		    (memq c '(?w ?n))))	; leave \w, \n in numeric args
	      ((eq c ?l) (woman-horizontal-line))
	      (t
	       ;; \? -> ? where ? is any remaining character
	       (WoMan-warn "Escape ignored: \\%c -> %c" c c)
	       (delete-char -1))
	      )))
    (goto-char from)
    ;; Process non-default tab settings:
    (cond (tab-stop-list
	   (while (search-forward "\t" to t)
	     (woman-tab-to-tab-stop))
	   (goto-char from)))

    ;; Must replace \' by something before processing \w, done above.
    (while (re-search-forward "\\\\w'\\([^']*\\)'" to t)
      ;; interpolate width of string
      (replace-match
       (number-to-string
	(- (match-end 1) (match-beginning 1))) t t))
    (goto-char from)

;    (woman-translate to)
;    (goto-char from)
;    (woman-special-characters to)
;    (goto-char from)
    ))

(defun woman-horizontal-line ()
  "\\l'Nc' -- Draw a horizontal line of length N using character c, default _."
  (delete-char -1)
  (delete-char 1)
  (looking-at "\\(.\\)\\(.*\\)\\1")
  (let ((to (make-marker)) from N c)
    (set-marker to (match-end 2))
    (delete-char 1)
    (setq from (point)
	  N (woman-parse-numeric-arg))
    (setq c (if (< (point) to) (following-char) ?_))
    (delete-region from to)
    (delete-char 1)
    (set-marker to nil)
    (insert (make-string N c))
    ))

;;; 4. Text Filling, Adjusting, and Centering

(defun woman2-br (to)
  ".br -- Break.  Leave no blank line and format following paragraph."
  (woman-delete-line 1)			; ignore any arguments
  (woman2-format-paragraphs to))

(defun woman2-fi (to)
  ".fi -- Fill subsequent output lines.
Leave no blank line and format following paragraph"
  (setq woman-nofill nil)
  (woman-delete-line 1)			; ignore any arguments
  (woman2-format-paragraphs to))

(defun woman2-nf (to)
  ".nf -- Nofill.  Subsequent output lines are neither filled nor
adjusted.  Input text lines are copied directly to output lines
without regard for the current line length."
  (setq woman-nofill t)
  (woman-delete-line 1)			; ignore any arguments
  (woman2-format-paragraphs to))

(defvar woman-justify-previous 'full	; ???
  "Previous justification style.")

(defun woman2-ad (to)
  ".ad c -- Line adjustment is begun (once fill mode is on).
Set justification mode to c if specified.  (Breaks, but should not.)"
  ;; c = l -- left, r -- right, c -- center, b or n -- full,
  ;; absent -- unchanged.  Initial mode adj,both, i.e. full.
  ;;(delete-blank-lines)
  (setq woman-justify
	(if (eolp)
	    woman-justify-previous
	  (cond ((eq (following-char) ?l) 'left)
		((eq (following-char) ?r) 'right)
		((eq (following-char) ?c) 'center)
		((memq (following-char) '(?b ?n)) 'full)
		(t (woman-get-numeric-arg))
		)))
  (woman-delete-line 1)			; ignore any remaining arguments
  (woman2-format-paragraphs to))

(defun woman2-na (to)
  ".na -- No adjusting.  (Breaks, but should not.)"
  (setq woman-justify-previous woman-justify
	woman-justify 'left)		; fill but do not adjust
  (woman-delete-line 1)			; ignore any arguments
  (woman2-format-paragraphs to))

;;; The main formatting functions:

(defun woman-leave-blank-lines (&optional leave)
  "Delete all blank lines around point, but leave one if LEAVE is
non-null and non-zero, or if LEAVE is null and WOMAN-LEAVE-BLANK-LINES
is non-null and non-zero."
  ;; ***** It may suffice to delete only lines ABOVE point! *****
  ;; NOTE: Function arguments are evaluated left to right
  ;; (*note (elisp)Function Forms::.).
  (delete-region
   (save-excursion
     (if (not (eq (skip-syntax-backward " ") 0)) 
	 (forward-line))			; forward-char ?
     (point))
   (progn (skip-syntax-forward " ")
	  (beginning-of-line)
	  (point)))
  (if woman-nospace
      ()
    (if (or (null leave) (eq leave 0))
	;; output any `pending' vertical space ...
	(setq leave woman-leave-blank-lines))
    (if (and leave (> leave 0)) (insert-before-markers ?\n)) ; 0.31
    )
  (setq woman-leave-blank-lines nil)
  )

;; `fill-region-as-paragraph' in `fill.el' appears to be the principal
;; text filling function, so that is what I use here.

(defvar woman-temp-indent nil)

(defun woman2-format-paragraphs (to &optional new-left)
  "Indent paragraphs to current left margin, optionally resetting it,
and optionally filling and adjusting."
  ;; Blank space should only ever be output before text.
  (if new-left (setq left-margin new-left))
  (if (looking-at "^\\s *$")
      ;; A blank line should leave a space like .sp 1 (p. 14).
      (setq woman-leave-blank-lines 1))
  (skip-syntax-forward " ")
  ;; Successive control lines are sufficiently common to be worth a
  ;; special case (maybe):
  (if (= (point) to)
      ()
    ;; (woman-leave-blank-lines)
    (woman-reset-nospace)
    (woman2-process-escapes to)
    (if woman-nofill			; (not woman-justify)
	;; Indent without filling or adjusting...
	(progn
	  (woman-leave-blank-lines)
	  (cond (woman-temp-indent
		 (indent-to woman-temp-indent)
		 (forward-line)))
	  (indent-rigidly (point) to left-margin))
      ;; Fill and justify...
      ;; Blank lines and initial spaces cause a break.
;      (cond ((and (= (point) to) (not (looking-at ".nf"))) ; Yuk!!!
;	     ;; No text after a request that caused a break, so delete
;	     ;; any spurious blank line left:
;	     (forward-line -1)
;	     (if (looking-at "^\\s *$") (kill-line) (forward-line))))
      (while (< (point) to)
	(woman-leave-blank-lines)
	(let ((from (point)))
	  ;; Indent first line of paragraph:
	  (indent-to (or woman-temp-indent left-margin))
	  ;; Find the beginning of the next paragraph:
	  (forward-line)
;	  (if (re-search-forward "\\(^\\s *$\\)\\|\\(^\\s +\\)" to 1)
;	      ;; A blank line should leave a space like .sp 1 (p. 14).
;	      (if (eolp)
;		  (progn
;		    (skip-syntax-forward " ")
;		    (setq woman-leave-blank-lines 1))
;		(setq woman-leave-blank-lines nil)))
	  (and (re-search-forward "\\(^\\s *$\\)\\|\\(^\\s +\\)" to 1)
	       ;; A blank line should leave a space like .sp 1 (p. 14).
	       (eolp)
	       (skip-syntax-forward " ")
	       (setq woman-leave-blank-lines 1))
	  (beginning-of-line)
	  ;; If a single short line then just leave it.
	  ;; This is necessary to preserve some table layouts.
	  ;; PROBABLY NOT NECESSARY WITH SQUEEZE MODIFICATION !!!!!
	  (if (or (> (count-lines from (point)) 1)
		  (save-excursion
		    (backward-char)
		    (> (current-column) fill-column)))
	      ;; ?roff does not squeeze multiple spaces
	      ;; (fill-region-as-paragraph from (point) woman-justify t)
	      ;; NOSQUEEZE has no effect if JUSTIFY is full, so
	      ;; redefine canonically-space-region, see above.
	      (progn
		;; Needs a re-write of the paragraph formatter to
		;; avoid this nonsense to handle temporary indents!
		(if (and woman-temp-indent (< woman-temp-indent left-margin))
		    (let ((left-margin woman-temp-indent))
		      (fill-region-as-paragraph from (point) woman-justify)
		      (save-excursion
			(goto-char from)
			(forward-line)
			(setq from (point)))))
		(fill-region-as-paragraph from (point) woman-justify))
	    )
	  ;; A blank line should leave a space like .sp 1 (p. 14).
	  ;; Delete all but 1 trailing blank lines:
	  ;;(woman-leave-blank-lines 1)
	  ))
      )
    (setq woman-temp-indent nil)
    ;; Non-white-space text has been processed, so ...
    ;;(setq woman-leave-blank-lines nil)
    ))


;;; Tagged, indented and hanging paragraphs

(defun woman2-TP (to)
  ".TP i -- Set prevailing indent to i.  Begin indented paragraph with
hanging tag given by next text line.  If tag doesn't fit, place it on
separate line."
  (let ((i (woman2-get-prevailing-indent)))
    (woman-leave-blank-lines woman-interparagraph-distance)
    (woman2-tagged-paragraph to i)))

(defun woman2-IP (to)
  ".IP x i -- Same as .TP with tag x."
  (woman-interparagraph-space)
  (if (eolp)				; no args
      ;; Like LP without resetting prevailing indent
      (woman2-format-paragraphs to (+ woman-left-margin
				      woman-prevailing-indent))
    (woman-forward-arg 'unquote)
    (let ((i (woman2-get-prevailing-indent 'leave-eol)))
      (beginning-of-line)
      (woman-leave-blank-lines)		; must be here,
      (woman2-tagged-paragraph to i))))

(defun woman-find-next-control-line-carefully ()
  "Find and return start of next control line, even if already there!"
  (if (looking-at "^[.']")
      (point)
    (woman-find-next-control-line)))

(defun woman2-tagged-paragraph (to i)
  "Set prevailing indent to i.  Begin indented paragraph with hanging
tag given by current text line.  If tag doesn't fit, leave it on
separate line."
  (if (not (looking-at "\\s *$"))	; non-empty tag
      (setq woman-leave-blank-lines nil))

  ;; Temporary hack for bash.1 and groff_mmse.7 until code is revised
  ;; to process all requests uniformly:
  (cond ((and (= (point) to) (looking-at "^[.'][ \t]*\\(PD\\|br\\) *"))
	 (if (string= (match-string 1) "br")
	     (woman-delete-line 1)
	   (woman-delete-match 0)
	   (woman-set-interparagraph-distance))
	 (set-marker to (woman-find-next-control-line-carefully))
	 ))
  
  (let ((tag (point)))
    (woman-reset-nospace)
    ;; Format the tag:
    (woman2-process-escapes-to-eol)
    ;; TIDY UP THE FOLLOWING CODE
    ;; (indent-to woman-left-margin)
    (setq left-margin woman-left-margin)
    (forward-line)
    (fill-region-as-paragraph (save-excursion (forward-line -1) (point))
			      (point) woman-justify)
    
    ;; Temporary hack for bash.1 until all requests processed uniformly:
    (cond ((and (= (point) to) (looking-at "^[.'][ \t]*PD *"))
	   (woman-delete-match 0)
	   (woman-set-interparagraph-distance)
	   (set-marker to (woman-find-next-control-line-carefully))
	   ))

    ;; Format the paragraph body, if there is one!  Set indented left
    ;; margin anyway, because the paragraph body may begin with a
    ;; control line:
    (setq left-margin (+ woman-left-margin i))
    (cond ((< (point) to)
	   (woman2-format-paragraphs to)
	   (goto-char tag)  (end-of-line)
	   (cond ((> (setq i (- left-margin (current-column))) 0)
		  (delete-char 1)
		  (delete-horizontal-space)
		  ;; Necessary to avoid spaces inheriting underlines.
		  ;; Cannot simply delete (current-column) whitespace
		  ;; characters because some may be tabs!
		  (while (> i 0) (insert ? ) (setq i (1- i)))))
	   (goto-char to)		; necessary ???
	   ))
    ))

(defun woman2-HP (to)
  ".HP i -- Set prevailing indent to i.  Begin paragraph with hanging
indent."
  (let ((i (woman2-get-prevailing-indent)))
    (woman-interparagraph-space)
;    (indent-to woman-left-margin)
;    (woman2-format-paragraphs to (+ woman-left-margin i))
    (setq woman-temp-indent woman-left-margin)
    (woman2-format-paragraphs to (+ woman-left-margin i))
    ))

(defun woman2-get-prevailing-indent (&optional leave-eol)
  "Return an integer argument, if it exists, and set the prevailing
indent to its value -- return prevailing indent if no argument.
Delete line from point and eol unless LEAVE-EOL is non-nil."
  (if (eolp)
      (or leave-eol (delete-char 1))
    (let ((i (woman-get-numeric-arg)))
      (woman-delete-line) (or leave-eol (delete-char 1))
      ;; i = 0 if the argument was not a number
      (if (> i 0) (setq woman-prevailing-indent i))))
  woman-prevailing-indent)

(defmacro woman-push (value stack)
  `(setq ,stack (cons ,value ,stack)))

(defmacro woman-pop (stack)
  `(prog1 (car ,stack)
    (setq ,stack (cdr ,stack))))

(defun woman2-RS (to)
  ".RS i -- Start relative indent, move left margin in distance i.
Set prevailing indent to 5 for nested indents."
  (woman-push woman-left-margin woman-RS-left-margin)
  (woman-push woman-prevailing-indent woman-RS-prevailing-indent)
  (setq woman-left-margin (+ woman-left-margin
			     (woman2-get-prevailing-indent))
	woman-prevailing-indent woman-default-indent)
  (woman2-format-paragraphs to woman-left-margin))

(defun woman2-RE (to)
  ".RE -- End of relative indent.
Set prevailing indent to amount of starting .RS."
  (setq woman-left-margin (woman-pop woman-RS-left-margin)
	woman-prevailing-indent (woman-pop woman-RS-prevailing-indent))
  (woman-delete-line 1)			; ignore any arguments
  (woman2-format-paragraphs to woman-left-margin))


;;; Line Length and Indenting:

(defun woman-set-arg (arg &optional previous)
  "Reset, increment or decrement argument, which must be quoted,
and delete the whole remaining control line."
  (if (eolp)				; space already skipped
      (set arg (if previous (eval previous) 0))
    (if previous (set previous (eval arg)))
    (woman2-process-escapes-to-eol 'numeric)
    (let ((pm (if (looking-at "[+-]")
		(prog1 (following-char)
		  (forward-char 1))))
	(i (woman-parse-numeric-arg)))
    (cond ((null pm) (set arg i))
	  ((= pm ?+) (set arg (+ (eval arg) i)))
	  ((= pm ?-) (set arg (- (eval arg) i)))
	  ))
    (beginning-of-line))
  (woman-delete-line 1))		; ignore any remaining arguments

;; NEED TO RATIONALIZE NAMES FOR PREVIOUS VALUES!
(defvar woman-ll-fill-column woman-fill-column)
(defvar woman-in-left-margin woman-left-margin)

(defun woman2-ll (to)
  ".ll +/-N -- Set, increment or decrement line length.
\(Breaks, but should not.)"
  (woman-set-arg 'fill-column 'woman-ll-fill-column)
  (woman2-format-paragraphs to))

(defun woman2-in (to)
  ".in +/-N -- Set, increment or decrement the indent."
  (woman-set-arg 'left-margin 'woman-in-left-margin)
  (woman2-format-paragraphs to))

(defun woman2-ti (to)
  ".ti +/-N -- Temporary indent."
  ;; Ignore if no argument.
  ;; Indent next output line only wrt current indent.
  ;; Current indent is not changed.
  (setq woman-temp-indent left-margin)
  (woman-set-arg 'woman-temp-indent)
  (woman2-format-paragraphs to nil))


;;; Tabs, Leaders, and Fields:

(defun woman2-ta (to)
  ".ta Nt ... -- Set tabs, left type, unless t=R(right), C(centered).
\(Breaks, but should not.)  The tab stops are separated by spaces;
a value preceded by + represents an increment to the previous stop value."
  (setq tab-stop-list nil)
  (woman2-process-escapes-to-eol 'numeric)
  (save-excursion
    (let ((tab-stop 0))
      (while (not (eolp))
	(let ((plus (cond ((eq (following-char) ?+) (forward-char 1) t)))
	      (i (woman-parse-numeric-arg)))
	  (setq tab-stop (if plus (+ tab-stop i) i)))
	(if (memq (following-char) '(?R ?C))
	    (setq tab-stop (cons tab-stop (following-char))))
	(setq tab-stop-list (cons tab-stop tab-stop-list))
	(skip-syntax-forward "^ ")	; skip following R, C, `;', etc.
	(skip-chars-forward " \t")
	)))
  (woman-delete-line 1)			; ignore any remaining arguments
  (setq tab-stop-list (reverse tab-stop-list))
  (woman2-format-paragraphs to))

(defsubst woman-get-tab-stop (tab-stop-list)
  (if (consp tab-stop-list) (car tab-stop-list) tab-stop-list))

(defun woman-tab-to-tab-stop ()
  "Insert spaces to next defined tab-stop column.
The variable `tab-stop-list' is a list of columns where there are tab stops,
or pairs `column . type' where type is R or C."
  ;; Based on tab-to-tab-stop in indent.el.
  ;; R & C tabs probably not quite right!
  (delete-backward-char 1)
  (let ((tabs tab-stop-list))
    (while (and tabs (>= (current-column)
			 (woman-get-tab-stop (car tabs))))
      (setq tabs (cdr tabs)))
    (if tabs
	(let* ((tab (car tabs))
	       (type (and (consp tab) (cdr tab)))
	       eol n)
	  (if type
	      (setq tab (woman-get-tab-stop tab)
		    eol (save-excursion (end-of-line) (point))
		    n (save-excursion
			(search-forward "\t" eol t))
		    n (- (if n (1- n) eol) (point))
		    tab (- tab (if (eq type ?C) (/ n 2) n))) )
	  (setq n (- tab (current-column)))
	  (while (> n 0)
	    (insert ?\ )
	    (setq n (1- n))))
      (insert ?\ ))))

(defun woman2-DT (to)
  ".DT -- Restore default tabs.  (Breaks, but should not.)"
  ;; Currently just terminates special tab processing.
  (setq tab-stop-list nil)
  (woman-delete-line 1)			; ignore any arguments
  (woman2-format-paragraphs to))


;;; WoMan message logging:

;;; The basis for this logging code was shamelessly pirated from bytecomp.el
;;; by Jamie Zawinski <jwz@lucid.com> & Hallvard Furuseth <hbf@ulrik.uio.no>

(defvar WoMan-current-file nil)		; bound in woman-really-find-file
(defvar WoMan-Log-header-point-max nil)

(defun WoMan-log-file ()
  "Log the start of a file in *WoMan-Log*."
  (let ((WoMan-current-buffer (buffer-name)))
    (save-excursion
      (set-buffer (get-buffer-create "*WoMan-Log*"))
      (goto-char (point-max))
      (insert "\n\^L\nFormatting "
	      (if (stringp WoMan-current-file)
		  (concat "file " WoMan-current-file)
		(concat "buffer " WoMan-current-buffer))
	      " at " (current-time-string) "\n")
      (setq WoMan-Log-header-point-max (point-max))
      )))

(defun WoMan-log (format &rest args)
  (WoMan-log-1 (apply 'format format args)))

(defun WoMan-warn (format &rest args)
  (setq format (apply 'format format args))
  (WoMan-log-1 (concat "**  " format)))

(defun WoMan-warn-ignored (request ignored)
  (let ((tail
	 (buffer-substring (point)
			   (save-excursion (end-of-line) (point)))))
    (if (and (> (length tail) 0)
	     (/= (string-to-char tail) ?\ ))
	(setq tail (concat " " tail)))
    (WoMan-log-1
     (concat "**  " request tail "  request " ignored))))

(defun WoMan-log-1 (string)
  "Log a message STRING in *WoMan-Log*."
  (save-excursion
    (set-buffer (get-buffer-create "*WoMan-Log*"))
    (goto-char (point-max))
    (insert "  " string "\n")
    (if woman-show-log
	(select-window			; to return to
	 (prog1 (selected-window)	; WoMan window
	   (select-window (display-buffer (current-buffer)))
	   (cond (WoMan-Log-header-point-max
		  (goto-char WoMan-Log-header-point-max)
		  (recenter 1)))
	   ))))
  nil)					; for woman-file-readable-p etc.

(provide 'woman)

;;; woman.el ends here
