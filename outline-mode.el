;; LCD Archive Entry:
;; allout|Ken Manheimer|klm@cme.nist.gov
;; |A more thorough outline-mode
;; |22-Dec-1992|$Id: allout.el,v 2.12 1993/04/18 19:49:26 klm Exp $||

;; A full-fledged outline mode, based on the original rudimentary
;; GNU emacs outline functionality.
;;
;; Ken Manheimer		 	Nat'l Inst of Standards and Technology
;; klm@cme.nist.gov (301)975-3539	(Formerly Nat'l Bureau of Standards)
;;    Factory Automation Systems Division Unix Systems Support Manager

;; Copyright (C) 1992 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

(provide 'outline)

;=======================================================================
;                     Outline Variables

;-----------------------------------------------------------------------
; Basic configuration vars:

(defvar outline-header-prefix "."
  "*   Leading string for greater than level 0 topic headers.")
(make-variable-buffer-local 'outline-header-prefix)

(defvar outline-bullets-exclude-header-lead nil
  "*   THIS OPTION/VARIABLE IS PROBABLY BEING ELIMINATED.
  If set, exclude lead char from bullet string (when constructing
  it, via set-outline-regexp), in order to, eg, use code comment
  symbol as lead, and avoid repeated comment chars from seeming like a
  header.")
(make-variable-buffer-local 'outline-bullets-exclude-header-lead)

(defvar outline-primary-bullet "*") ;; Changing this var disables any
                                    ;; backwards compatability with
                                    ;; the original outline mode.
(make-variable-buffer-local 'outline-primary-bullet)

(defvar outline-plain-bullets-string ""
  "*   The bullets normally used in outline topic prefixes.  See
   'outline-distinctive-bullets-string' for the other kind of
   bullets.

   DO NOT include the close-square-bracket, ']', among any bullets.

   You must run 'set-outline-regexp' in order for changes to the
   value of this var to effect outline-mode operation.")
(setq outline-plain-bullets-string (concat outline-primary-bullet
                                           "+-:.;,"))
(make-variable-buffer-local 'outline-plain-bullets-string)

(defvar outline-distinctive-bullets-string ""
  "*   The bullets used for distinguishing outline topics.  These
   bullets are not offered among the regular rotation, and are not
   changed when automatically rebulleting, as when shifting the
   level of a topic.  See 'outline-plain-bullets-string' for the
   other kind of bullets.

   DO NOT include the close-square-bracket, ']', among any bullets.

   You must run 'set-outline-regexp' in order for changes
   to the value of this var to effect outline-mode operation.")
(setq outline-distinctive-bullets-string "=>([{}&!?#%\"X@$~")
(make-variable-buffer-local 'outline-distinctive-bullets-string)

(defvar outline-numbered-bullet ()
  "*   Bullet signifying outline prefixes which are to be numbered.
   Leave it nil if you don't want any numbering, or set it to a
   string with the bullet you want to be used.")
(setq outline-numbered-bullet "#")
(make-variable-buffer-local 'outline-numbered-bullet)

;-----------------------------------------------------------------------
;                  Topic Header Style Configuration
;
; The following vars affect the basic behavior of outline topic
; creation and manipulation.

(defvar outline-stylish-prefixes t
  "*A true value for this var makes the topic-prefix creation and modification
   functions vary the prefix bullet char according to level.  Otherwise, only
   asterisks ('*') and distinctive bullets are used.

   This is how an outline can look with stylish prefixes:

   * Top level
   .* A topic
   . + One level 3 subtopic
   .  . One level 4 subtopic
   . + Another level 3 subtopic
   .  . A level 4 subtopic
   .  #2 A distinguished, numbered level 4 subtopic
   .  ! A distinguished ('!') level 4 subtopic
   .  #4 Another numbered level 4 subtopic
   
   This would be an outline with stylish prefixes inhibited:

   * Top level
   .* A topic
   .! A distinctive (but measly) subtopic
   . * A sub-subtopic - no bullets from outline-plain-bullets-string but '*'

   Stylish and constant prefixes (as well as old-style prefixes) are
   always respected by the topic maneuvering functions, regardless of
   this variable setting.

   The setting of this var is not relevant when outline-old-style-prefixes
   is t.")
(make-variable-buffer-local 'outline-stylish-prefixes)

(defvar outline-old-style-prefixes nil
  "*Setting this var causes the topic-prefix creation and modification
   functions to make only asterix-padded prefixes, so they look exactly
   like the old style prefixes.

   Both old and new style prefixes are always respected by the topic
   maneuvering functions.")
(make-variable-buffer-local 'outline-old-style-prefixes)

;-----------------------------------------------------------------------
; Incidental configuration vars (incidental subsystems interaction with
; outline):

(defvar outline-file-xref-bullet "@"
  "*  Set this var to the bullet you want to use for file cross-references.
   Set it 'nil' if you want to inhibit this capability.")

                   ;;; Currently only works with Dan LaLiberte's isearch-mode:
(defvar outline-enwrap-isearch-mode t
  "*  Set this var non-nil if you have Dan LaLiberte's 'isearch-mode'
   stuff, and want isearches to reveal hidden stuff encountered in the
   course of a search (and reconceal it if you go past).")

(defvar outline-use-hanging-indents t
  "*  Set this var non-nil if you have Kyle E Jones' filladapt stuff,
  and you want outline to fill topics as hanging indents to the
  bullets.")
(make-variable-buffer-local 'outline-use-hanging-indents)

(defvar outline-reindent-bodies t
  "*  Set this var non-nil if you want topic depth adjustments to
  reindent hanging bodies (ie, bodies lines indented to beginning of
  heading text).  The performance hit is small.")
(make-variable-buffer-local 'outline-reindent-bodies)

;-----------------------------------------------------------------------
; Internal configuration variables

(defvar outline-mode-map nil "Keybindings for outline mode.")

(defvar outline-regexp ""
  "*   Regular expression to match the beginning of a heading line.
   Any line whose beginning matches this regexp is considered a
   heading.  This var is set according to the user configuration vars
   by set-outline-regexp.")

(defvar outline-bullets-string ""
  "   A string dictating the valid set of outline topic bullets.  This
   var should *not* be set by the user - it is set by 'set-outline-regexp',
   and is composed from the elements of 'outline-plain-bullets-string'
   and 'outline-distinctive-bullets-string'.")

(defvar outline-line-boundary-regexp ()
  "   outline-regexp with outline-style beginning of line anchor (ie,
   C-j, *or* C-m, for prefixes of hidden topics, *or* beginning of the
   buffer).  This is properly set when outline-regexp is produced by
   'set-outline-regexp', so that (match-beginning 2) and (match-end 2)
   delimit the prefix.")

;;; Recent-topic-search-state data:

;;; All basic outline functions which directly do string matches to
;;; evaluate heading prefix location set the variables
;;; outline-recent-prefix-beginning and outline-recent-prefix-end when
;;; successful.  Functions starting with 'outline-recent-' all use
;;; this state, providing the means to avoid redundant searches for
;;; just established data.  This optimization can be significant but
;;; must be employed carefully.

(defvar outline-recent-prefix-beginning 0
  "   Buffer point of the start of the last topic prefix encountered.")
(make-variable-buffer-local 'outline-recent-prefix-beginning)
(defvar outline-recent-prefix-end 0
  "   Buffer point of the end of the last topic prefix encountered.")
(make-variable-buffer-local 'outline-recent-prefix-end)

;=======================================================================
;                         Outline Initializations

(if outline-mode-map
    nil
  (setq outline-mode-map (copy-keymap text-mode-map))
                                        ; Motion commands:

  (define-key outline-mode-map "\C-c\C-n" 'outline-next-visible-heading)
  (define-key outline-mode-map "\C-c\C-p" 'outline-previous-visible-heading)
  (define-key outline-mode-map "\C-c\C-u" 'outline-up-current-level)
  (define-key outline-mode-map "\C-c\C-f" 'outline-forward-current-level)
  (define-key outline-mode-map "\C-c\C-b" 'outline-backward-current-level)
  (define-key outline-mode-map "\C-c\C-a" 'outline-beginning-of-current-entry)
  (define-key outline-mode-map "\C-c\C-e" 'outline-end-of-current-entry)
                                        ; Exposure commands:
  (define-key outline-mode-map "\C-c\C-i" 'outline-show-current-children)
  (define-key outline-mode-map "\C-c\C-s" 'outline-show-current-subtree)
  (define-key outline-mode-map "\C-c\C-h" 'outline-hide-current-subtree)
  (define-key outline-mode-map "\C-c\C-o" 'outline-show-current-entry)
  (define-key outline-mode-map "\C-c!" 'outline-show-all)
                                        ; Alteration commands:
  (define-key outline-mode-map "\C-c " 'open-sibtopic)
  (define-key outline-mode-map "\C-c." 'open-subtopic)
  (define-key outline-mode-map "\C-c," 'open-supertopic)
  (define-key outline-mode-map "\C-c'" 'outline-shift-in)
  (define-key outline-mode-map "\C-c>" 'outline-shift-in)
  (define-key outline-mode-map "\C-c<" 'outline-shift-out)
  (define-key outline-mode-map "\C-c\C-m" 'outline-rebullet-topic)
  (define-key outline-mode-map "\C-cb" 'outline-rebullet-current-heading)
  (define-key outline-mode-map "\C-c#" 'outline-number-siblings)
  (define-key outline-mode-map "\C-k" 'outline-kill-line)
  (define-key outline-mode-map "\C-y" 'outline-yank)
  (define-key outline-mode-map "\M-y" 'outline-yank-pop)
  (define-key outline-mode-map "\C-c\C-k" 'outline-kill-topic)
                                        ; Miscellaneous commands:
  (define-key outline-mode-map "\C-c@" 'outline-resolve-xref)
  (define-key outline-mode-map "\C-cc" 'outline-copy-exposed)
  )

(defun outline-mode ()
  "  Set major mode for editing outlines with selective display.

   Below the description of the bindings is explanation of the outline
   mode terminology.

Exposure Commands		      Movement Commands
C-c C-h	outline-hide-current-subtree  C-c C-n outline-next-visible-heading
C-c C-i	outline-show-current-children C-c C-p outline-previous-visible-heading
C-c C-s	outline-show-current-subtree  C-c C-u outline-up-current-level
C-c C-o	outline-show-current-entry    C-c C-f outline-forward-current-level
C-c !   outline-show-all              C-c C-b outline-backward-current-level
	outline-hide-current-leaves   C-c C-e outline-end-of-current-entry
                                     C-c C-a outline-beginning-of-current-entry


Topic Header Generation Commands
C-c<SP>	open-sibtopic		Create a new sibling after current topic
C-c .	open-subtopic		... an offspring of current topic
C-c ,	open-supertopic		... a sibling of the current topic's parent

Level and Prefix Adjustment Commands
C-c >	outline-shift-in	Shift current topic and all offspring deeper
C-c <	outline-shift-out	... less deep
C-c<CR>	outline-rebullet-topic	Reconcile bullets of topic and its offspring
                                - distinctive bullets are not changed, all
                                  others set suitable according to depth
C-c b	outline-rebullet-current-heading Prompt for alternate bullet for
					 current topic
C-c #	outline-number-siblings	Number bullets of topic and siblings - the
				offspring are not affected.  With repeat
				count, revoke numbering.

Killing and Yanking - all keep siblings numbering reconciled as appropriate
C-k	outline-kill-line	Just like regular kill line, w/reconciliation
C-c C-k	outline-kill-topic	Kill current topic, including offspring
C-y	outline-yank		Yank, adjusting depth of yanked topic to
				depth of heading if yanking into bare topic
                                heading (ie, prefix sans text)
M-y	outline-yank-pop	Is to outline-yank as yank-pop is to yank

Misc commands
C-c @   outline-resolve-xref    pop-to-buffer named by xref (cf
                                outline-file-xref-bullet)
C-c c	outline-copy-exposed	Copy outline sans all hidden stuff to
				another buffer whose name is derived
				from the current one - \"XXX exposed\"
M-x outlineify-sticky           Activate outline mode for current buffer
                                and establish -*- outline -*- mode specifier
                                as well as file local vars to automatically
                                set exposure.  Try it.

                             Terminology

Topic: A basic cohesive component of an emacs outline, which can
       be closed (made hidden), opened (revealed), generated,
       traversed, and shifted as units, using outline-mode functions.
       A topic is composed of a HEADER, a BODY, and SUBTOPICs (see below).

Exposure: Hidden (~closed~) topics are represented by ellipses ('...')
          at the end of the visible SUPERTOPIC which contains them,
          rather than by their actual text.  Hidden topics are still
          susceptable to editing and regular movement functions, they
          just are not displayed normally, effectively collapsed into
          the ellipses which represent them.  Outline mode provides
          the means to selectively expose topics based on their
          NESTING.

          SUBTOPICS of a topic can be hidden and subsequently revealed
          based on their DEPTH relative to the supertopic from which
          the exposure is being done.

          The BODIES of a topic do not generally become visible except
          during exposure of entire subtrees (see documentation for
          '-current-subtree'), or when the entry is explicitly exposed
          with the 'outline-show-entry' function, or (if you have a
          special version of isearch installed) when encountered by
          incremental searches.

          The CURRENT topic is the more recent visible one before or
          including the text cursor.

Header: The initial portion of an outline topic.  It is composed of a
        topic header PREFIX at the beginning of the line, followed by
        text to the end of the EFFECTIVE LINE.

Body: Any subsequent lines of text following a topic header and preceeding
      the next one.  This is also referred to as the entry for a topic.

Prefix: The text which distinguishes topic headers from normal text
        lines.  There are two forms, both of which start at the beginning
        of the topic header (EFFECTIVE) line.  The length of the prefix
        represents the DEPTH of the topic.  The fundamental sort begins
        either with solely an asterisk ('*') or else dot ('.')  followed
        by zero or more spaces and then an outline BULLET.  The second
        form is for backwards compatability with the original emacs
        outline mode, and consists solely of asterisks.  Both sorts are
        recognized by all outline commands.  The first sort is generated
        by outline topic production commands if the emacs variable
        outline-old-style-prefixes is nil, otherwise the second style is
        used.

Bullet: An outline prefix bullet is one of the characters on either
        of the outline bullet string vars, 'outline-plain-bullets-string'
        and 'outline-distinctive-bullets-string'.  (See their
        documentation for more details.)  The default choice of bullet
        for any prefix depends on the DEPTH of the topic.

Depth and Nesting:
       The length of a topic header prefix, from the initial
       character to the bullet (inclusive), represents the depth of
       the topic.  A topic is considered to contain the subsequent
       topics of greater depth up to the next topic of the same
       depth, and the contained topics are recursively considered to
       be nested within all containing topics.  Contained topics are
       called subtopics.  Immediate subtopics are called 'children'.
       Containing topics are supertopicsimmediate supertopics are
       'parents'.  Contained topics of the same depth are called
       siblings.

Effective line: The regular ascii text in which form outlines are
                saved are manipulated in outline-mode to engage emacs'
                selective-display faculty.  The upshot is that the
                effective end of an outline line can be terminated by
                either a normal Unix newline char, \n, or the special
                outline-mode eol, ^M.  This only matters at the user
                level when you're doing searches which key on the end of
                line character."

  (interactive)
  (kill-all-local-variables)
  (setq selective-display t)
  (setq indent-tabs-mode nil)
  (use-local-map outline-mode-map)
  (setq mode-name "Outline")
  (setq major-mode 'outline-mode)
  (define-abbrev-table 'text-mode-abbrev-table ())
  (setq local-abbrev-table text-mode-abbrev-table)
  (set-syntax-table text-mode-syntax-table)
  ;; klm extension:
  (set-outline-regexp)

  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat paragraph-start "\\|^\\("
				outline-regexp "\\)"))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate (concat paragraph-separate "\\|^\\("
				   outline-regexp "\\)"))

  ;; klm extension:
  (if outline-enwrap-isearch-mode
      (outline-enwrap-isearch))
  ;; klm extension:
  (if (and outline-use-hanging-indents
           (boundp 'filladapt-prefix-table))
      ;; Add outline-prefix recognition to filladapt - not standard:
      (progn (setq filladapt-prefix-table
                   (cons (cons (concat "\\(" outline-regexp "\\) ")
                               'filladapt-hanging-list)
                         filladapt-prefix-table))
             (setq filladapt-hanging-list-prefixes
                   (cons outline-regexp filladapt-hanging-list-prefixes))))


  (run-hooks 'text-mode-hook 'outline-mode-hook))

(defun outline-reset-header-lead (header-lead)
  "*  Reset the leading string used to identify topic headers."
  (interactive "sNew lead string: ")
  (setq outline-header-prefix header-lead)
  (set-outline-regexp)
  )
(defun outline-lead-with-comment-string (header-lead)
  "*   Set the topic-header leading string to specified comment string, and
  exclude the lead string from the set of bullets, so, eg, bunches of
  comment strings on a line don't inherently seem like a level two topic."
  (interactive "sComment string for leading string: ")
  ;(if (> 2 (length header-lead))
  ;    (setq outline-bullets-exclude-header-lead t))
  (setq outline-primary-bullet header-lead)
  (setq outline-reindent-bodies nil)
  (outline-reset-header-lead header-lead)
  )

(defun set-outline-regexp ()
  "   Generate proper topic-header regexp form for outline functions, from
   outline-plain-bullets-string and outline-distinctive-bullets-string."

  (interactive)
  ;; Derive outline-bullets-string from user configured components:
  (setq outline-bullets-string "")
  (let ((strings (list 'outline-plain-bullets-string
                       'outline-distinctive-bullets-string))
        cur-string
        cur-len
        cur-char-string
        index
        new-string)
    (while strings
      (setq new-string "") (setq index 0)
      (setq cur-len (length (setq cur-string (symbol-value (car strings)))))
      (while (< index cur-len)
        (setq cur-char (aref cur-string index))
        (setq outline-bullets-string
              (concat outline-bullets-string
                      (cond
                       ; Excluding header-lead as dictated
                       ((and outline-bullets-exclude-header-lead
                             (eq cur-char
                                 (string-to-char outline-header-prefix)))
                        "")
                                        ; Single dash would denote a
                                        ; sequenc, repeated denotes
                                        ; a dash:
                       ((eq cur-char ?-) "--")
                                        ; literal close-square-bracket
                                        ; doesn't work right in the
                                        ; expr, exclude it:
                       ((eq cur-char ?\]) "")
                       (t (regexp-quote  (char-to-string cur-char))))))
        (setq index (1+ index)))
      (setq strings (cdr strings)))
    )
  ;; Derive next for repeated use in outline-pending-bullet:
  (setq outline-plain-bullets-string-len (length outline-plain-bullets-string))
  ;; Produce the new outline-regexp:
  (setq outline-regexp (concat "\\(\\"
                               outline-header-prefix
                               "[ \t]*["
                               outline-bullets-string
                               "]\\)\\|\\*+\\|\^l"))
  (setq outline-line-boundary-regexp
        (concat "\\([\C-j\C-m]\\)\\(" outline-regexp "\\)"))
  ;(setq outline-line-boundary-regexp
  ;      (concat "\\(\\`\\|[\C-j\C-m]\\)\\(" outline-regexp "\\)"))
  (make-variable-buffer-local 'outline-regexp)
  (make-variable-buffer-local 'outline-bullets-string)
  (make-variable-buffer-local 'outline-line-boundary-regexp)
  )

;-----------------------------------------------------------------------
;                      Outline State Functions

(defun outline-recent-depth ()
  "   Return depth of last heading encountered by an outline maneuvering
   function.

   All outline functions which directly do string matches to assess
   headings set the variables outline-recent-prefix-beginning and
   outline-recent-prefix-end if successful.  This function uses those settings
   to return the current depth."

  (- outline-recent-prefix-end outline-recent-prefix-beginning))

(defun outline-recent-prefix ()
  "   Like outline-recent-depth, but returns text of last encountered prefix.

   All outline functions which directly do string matches to assess
   headings set the variables outline-recent-prefix-beginning and
   outline-recent-prefix-end if successful.  This function uses those settings
   to return the current depth."
  (buffer-substring outline-recent-prefix-beginning outline-recent-prefix-end))

(defun outline-recent-bullet ()
  "   Like outline-recent-prefix, but returns bullet of last encountered
   prefix.

   All outline functions which directly do string matches to assess
   headings set the variables outline-recent-prefix-beginning and
   outline-recent-prefix-end if successful.  This function uses those settings
   to return the current depth of the most recently matched topic."
  (buffer-substring (1- outline-recent-prefix-end) outline-recent-prefix-end))

;--------------------------------------------------------------------
;                   Fundamental Location Assement

(defun outline-on-current-heading-p ()
  "   Return prefix beginning point if point is on same line as current
   visible topic's header line."
  (save-excursion
    (beginning-of-line)
    (and (looking-at outline-regexp)
         (setq outline-recent-prefix-end (match-end 0)
               outline-recent-prefix-beginning (match-beginning 0)))))

(defun outline-hidden-p ()
  "True if point is in hidden text."
  (interactive)
  (save-excursion
    (and (re-search-backward "[\C-j\C-m]" (point-min) t)
         (looking-at "\C-m"))))

(defun outline-current-depth ()
  "   Return the depth to which the current containing visible topic is
   nested in the outline."
  (save-excursion
    (if (outline-back-to-current-heading)
        (- outline-recent-prefix-end
           outline-recent-prefix-beginning)
      0)))

(defun outline-depth ()
  "   Like outline-current-depth, but respects hidden as well as visible
   topics."
  (save-excursion
    (if (outline-goto-prefix)
        (outline-recent-depth)
      (progn
        (setq outline-recent-prefix-end (point)
              outline-recent-prefix-beginning (point))
        0))))

;--------------------------------------------------------------------
;                 Fundamental Topic Prefix Assement

(defun outline-get-current-prefix ()
  "   Topic prefix of the current topic."
  (save-excursion
    (if (outline-goto-prefix)
        (outline-recent-prefix))))
(defun outline-get-bullet ()
  "   Return bullet of current topic."
  (save-excursion
    (and (outline-goto-prefix)
         (outline-recent-bullet))))

(defun outline-get-prefix-bullet (prefix)
  "   Return the bullet of the header prefix string PREFIX."
  ;; Doesn't make sense if we're old-style prefixes, but this just
  ;; oughtn't be called then, so forget about it...
  (if (string-match outline-regexp prefix)
      (substring prefix (1- (match-end 0)) (match-end 0))))

;-----------------------------------------------------------------------
;                         Fundamental Motion

(defun outline-goto-prefix ()
  "  Put point at beginning of outline prefix for current topic, visible
   or not.

   Returns a list of char address of the beginning of the prefix and the
   end of it, or nil if none."

  (cond ((and (or (bobp)
                  (memq (preceding-char) '(?\n ?\^M)))
              (looking-at outline-regexp))
         (setq outline-recent-prefix-end (match-end 0)
               outline-recent-prefix-beginning
               (goto-char (match-beginning 0))))
        ((re-search-backward outline-line-boundary-regexp
                             ;; unbounded search,
                             ;; stay at limit and return nil if failed:
                             nil 1)
         (setq outline-recent-prefix-end (match-end 2)
               outline-recent-prefix-beginning
               (goto-char (match-beginning 2)))))
 )

(defun outline-end-of-prefix ()
  "   Position cursor at beginning of header text."
  (if (not (outline-goto-prefix))
      nil
    (let ((match-data (match-data)))
      (goto-char (match-end 0))
      (while (looking-at "[0-9]") (forward-char 1))
      (if (looking-at "\\s-") (forward-char 1))
      (store-match-data match-data))
    ;; Reestablish where we are:
    (outline-current-depth))
  )

(defun outline-back-to-current-heading ()
  "   Move to heading line of current visible topic, or beginning of heading
   if already on visible heading line."
  (beginning-of-line)
  (prog1 (or (outline-on-current-heading-p)
             (and (re-search-backward (concat "^\\(" outline-regexp "\\)")
                                      nil
                                      'move)
                  (setq outline-recent-prefix-end (match-end 1)
                        outline-recent-prefix-beginning (match-beginning 1))))
    (if (interactive-p) (outline-end-of-prefix))
    )
  )

(defun outline-pre-next-preface ()
  "Skip forward to just before the next heading line.

   Returns that character position."

  (if (re-search-forward outline-line-boundary-regexp nil 'move)
      (progn (goto-char (match-beginning 0))
             (setq outline-recent-prefix-end (match-end 2)
                   outline-recent-prefix-beginning (match-beginning 2))))
  )


(defun outline-ascend-to-depth (depth)
  "   Ascend to depth DEPTH, returning depth if successful, nil if not."
  (if (and (> depth 0)(<= depth (outline-depth)))
      (let ((last-good (point)))
        (while (and (< depth (outline-depth))
                    (setq last-good (point))
                    (outline-beginning-of-level)
                    (outline-previous-heading)))
        (if (= (outline-recent-depth) depth)
            (progn (goto-char outline-recent-prefix-beginning)
                   depth)
          (goto-char last-good)
          nil))
    (if (interactive-p) (outline-end-of-prefix))
    )
  )

(defun outline-descend-to-depth (depth)
  "   Descend to depth DEPTH within current topic, returning depth if
   successful, nil if not."
  (let ((start-point (point))
        (start-depth (outline-depth)))
    (while
        (and (> (outline-depth) 0)
             (not (= depth (outline-recent-depth))) ; ... not there yet
             (outline-next-heading)     ; ... go further
             (< start-depth (outline-recent-depth)))) ; ... still in topic
    (if (and (> (outline-depth) 0)
             (= (outline-recent-depth) depth))
        depth
      (goto-char start-point)
      nil))
  )

(defun outline-end-of-current-subtree ()
  "  Put point at the end of the last leaf in the currently visible topic."
  (interactive)
  (outline-back-to-current-heading)
  (let ((opoint (point))
	(level (outline-recent-depth)))
    (outline-next-heading)
    (while (and (not (eobp))
                (> (outline-recent-depth) level))
      (outline-next-heading))
    (if (not (eobp)) (forward-char -1))
    (if (memq (preceding-char) '(?\n ?\^M)) (forward-char -1))))

(defun outline-next-visible-heading (arg)
  "   Move to the next visible heading line.

   With argument, repeats, backward if negative."
  (interactive "p")
  (if (< arg 0) (beginning-of-line) (end-of-line))
  (if (re-search-forward (concat "^\\(" outline-regexp "\\)")
                         nil
                         'go
                         arg)
      (progn (outline-end-of-prefix)
             (setq outline-recent-prefix-end (match-end 1)
                   outline-recent-prefix-beginning (match-beginning 1))))
  )

(defun outline-previous-visible-heading (arg)
  "   Move to the previous heading line.

   With argument, repeats or can move forward if negative.
   A heading line is one that starts with a `*' (or that outline-regexp
   matches)."
  (interactive "p")
  (outline-next-visible-heading (- arg))
  )

(defun outline-next-heading (&optional backward)
  "   Move to the heading for the topic (possibly invisible) before this one.

   Optional arg BACKWARD means search for most recent prior heading.

   Returns the location of the heading, or nil if none found."

  (if backward (outline-goto-prefix)
    (if (bobp) (forward-char 1)))

  (if (if backward
          ;; searches are unbounded and return nil if failed:
          (re-search-backward outline-line-boundary-regexp
                              nil
                              0)
        (re-search-forward outline-line-boundary-regexp
                           nil
                           0))
      (progn;; Got some valid location state - set vars:
        (setq outline-recent-prefix-end (match-end 2))
        (goto-char (setq outline-recent-prefix-beginning
                         (match-beginning 2))))
    )
  )

(defun outline-previous-heading ()
  "   Move to the next (possibly invisible) heading line.

   Optional repeat-count arg means go that number of headings.

   Return the location of the beginning of the heading, or nil if not found."

  (outline-next-heading t)
  )

(defun outline-next-sibling (&optional backward)
  "   Like outline-forward-current-level, but respects invisible topics.

   Go backward if optional arg BACKWARD is non-nil.

   Return depth if successful, nil otherwise."

  (if (and backward (bobp))
      nil
    (let ((start-depth (outline-depth))
          (start-point (point))
          last-good)
      (while (and (not (if backward (bobp) (eobp)))
                  (if backward (outline-previous-heading)
                    (outline-next-heading))
                  (> (outline-recent-depth) start-depth)))
      (if (and (not (eobp))
               (and (> (outline-depth) 0)
                    (= (outline-recent-depth) start-depth)))
          outline-recent-prefix-beginning
        (goto-char start-point)
        nil)
      )
    )
  )

(defun outline-previous-sibling (&optional arg)
  "   Like outline-forward-current-level, but goes backwards and respects
   invisible topics.

   Optional repeat count means go number backward.

   Note that the beginning of a level is (currently) defined by this
   implementation to be the first of previous successor topics of
   equal or greater depth.

   Return depth if successful, nil otherwise."
  (outline-next-sibling t)
  )

(defun outline-beginning-of-level ()
  "   Go back to the first sibling at this level, visible or not."
  (outline-end-of-level 'backward))

(defun outline-end-of-level (&optional backward)
  "   Go to the last sibling at this level, visible or not."

  (while (outline-previous-sibling))
  (prog1 (outline-recent-depth)
    (if (interactive-p) (outline-end-of-prefix)))
)

(defun outline-up-current-level (arg)
  "   Move to the heading line of which the present line is a subheading.
   With argument, move up ARG levels."
  (interactive "p")
  (outline-back-to-current-heading)
  (let ((present-level (outline-recent-depth)))
    ;; Loop for iterating arg:
    (while (and (> (outline-recent-depth) 1)
                (> arg 0)
                (not (bobp)))
      ;; Loop for going back over current or greater depth:
      (while (and (not (< (outline-recent-depth) present-level))
                  (outline-previous-visible-heading 1)))
      (setq present-level (outline-current-depth))
      (setq arg (- arg 1)))
    )
  (prog1 (if (<= arg 0)
             outline-recent-prefix-beginning
           (if (interactive-p) (outline-end-of-prefix))
           (error "Can't ascend past level 1."))
    (if (interactive-p) (outline-end-of-prefix)))
  )

(defun outline-forward-current-level (arg &optional backward)
  "   Position the point at the next heading of the same level, taking
   optional repeat-count.

   Returns that position, else nil if is not found."
  (interactive "p")
  (outline-back-to-current-heading)
      (let ((amt (if arg (if (< arg 0)
                             ;; Negative arg - invert direction.
                             (progn (setq backward (not backward))
                                    (abs arg))
                           arg);; Positive arg - just use it.
                   1)));; No arg - use 1:
        (while (and (> amt 0)
                    (outline-next-sibling backward))
          (setq amt (1- amt)))
        (if (interactive-p) (outline-end-of-prefix))
        (if (> amt 0)
            (error "This is the %s topic on level %d."
                   (if backward "first" "last")
                   (outline-current-depth))
          t)
        )
  )
(defun outline-backward-current-level (arg)
  "   Position the point at the previous heading of the same level, taking
   optional repeat-count.

   Returns that position, else nil if is not found."
  (interactive "p")
  (unwind-protect
      (outline-forward-current-level arg t)
    (outline-end-of-prefix))
)

(defun outline-beginning-of-current-entry ()
  "   Position the point at the beginning of the body of the current topic."
  (interactive)
  (outline-show-entry)
  (outline-end-of-prefix)
)
(defun outline-end-of-current-entry ()
  "   Position the point at the end of the current topic's entry."
  (interactive)
  (outline-show-entry)
  (prog1 (outline-pre-next-preface)
    (if (and (not (bobp))(looking-at "^$"))
        (forward-char -1)))
)

;-----------------------------------------------------------------------
;            Fundamental Outline Exposure Control

(defun outline-flag-region (from to flag)
  "   Hides or shows lines from FROM to TO, according to FLAG.
   Uses emacs selective-display, where text is show if FLAG put at
   beginning of line is `\\n' (newline character), while text is
   hidden if FLAG is `\\^M' (control-M).

   returns nil iff no changes were effected."
  (let ((buffer-read-only nil))
    (subst-char-in-region from to
                          (if (= flag ?\n) ?\^M ?\n)
                          flag t)))

;-----------------------------------------------------------------------
;                  Topic Component Exposure Control

(defun outline-hide-current-entry ()
  "Hide the body directly following this heading."
  (interactive)
  (outline-back-to-current-heading)
  (save-excursion
   (outline-flag-region (point)
                        (progn (outline-end-of-current-entry) (point))
                        ?\^M)))

(defun outline-show-current-entry (&optional arg)
  "Show body directly following this heading, or hide it if repeat count."
  (interactive "P")
  (if arg
      (outline-hide-current-entry)
    (save-excursion
      (outline-flag-region (point)
                           (progn (outline-end-of-current-entry) (point))
                           ?\n))))

; outline-show-entry basically for isearch dynamic exposure, as is...
(defun outline-show-entry ()
  "   Like outline-show-current-entry, but reveals an entry that is nested
   within hidden topics."
  (interactive)
  (save-excursion
    (outline-goto-prefix)
    (outline-flag-region (if (not (bobp)) (1- (point)) (point))
                         (progn (outline-pre-next-preface) (point)) ?\n)))

; ... outline-hide-current-entry-completely also for isearch dynamic exposure:
(defun outline-hide-current-entry-completely ()
  "Like outline-hide-current-entry, but conceal topic completely."
  (interactive)
  (save-excursion
    (outline-goto-prefix)
    (outline-flag-region (if (not (bobp)) (1- (point)) (point))
                         (progn (outline-pre-next-preface) (point)) ?\C-m)))

(defun outline-hide-bodies ()
  "Hide all of buffer except headings."
  (interactive)
  (outline-hide-region-body (point-min) (point-max)))

;-----------------------------------------------------------------------
;                 Composite Topics Exposure Control

(defun outline-hide-region-body (start end)
  "Hide all body lines in the region, but not headings."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (not (eobp))
	(outline-flag-region (point)
                             (progn (outline-pre-next-preface) (point)) ?\^M)
	(if (not (eobp))
	    (forward-char
	     (if (looking-at "[\n\^M][\n\^M]")
		 2 1)))))))

(defun outline-show-all ()
  "Show all of the text in the buffer."
  (interactive)
  (outline-flag-region (point-min) (point-max) ?\n))

(defun outline-show-current-branches ()
  "Show all subheadings of this heading, but not their bodies."
  (interactive)
  (outline-show-current-children 1000))

(defun outline-flag-current-subtree (flag)
  (save-excursion
    (outline-back-to-current-heading)
    (outline-flag-region (point)
			  (progn (outline-end-of-current-subtree) (point))
			  flag)))

(defun outline-hide-current-subtree ()
  "   Hide everything after this heading at deeper levels, or if
  it's already closed, hide the current level."
  (interactive)
  (let ((orig-eol (save-excursion (outline-goto-prefix)(end-of-line)(point))))
    (outline-flag-current-subtree ?\^M)
    (if (and (= orig-eol (save-excursion (end-of-line)(point)))
             ;; Structure didn't change - try hiding current level:
             (outline-up-current-level 1))
        (outline-hide-current-subtree))))

(defun outline-hide-current-leaves ()
  "Hide all body after this heading at deeper levels."
  (interactive)
  (outline-back-to-current-heading)
  (outline-hide-region-body (point) (progn (outline-end-of-current-subtree)
                                           (point))))

(defun outline-show-current-subtree ()
  "Show everything after this heading at deeper levels."
  (interactive)
  (outline-flag-current-subtree ?\n))

(defun outline-current-bullet ()
  "  Return bullet of current (visible) topic heading, or none if none found."
  (condition-case err
      (save-excursion
        (outline-back-to-current-heading)
        (buffer-substring (- outline-recent-prefix-end 1)
                          outline-recent-prefix-end))
    ;; Quick and dirty provision, ostensibly for missing bullet:
    (args-out-of-range nil))
  )
(defun outline-resolve-xref ()
  "  Pop to file associated with current heading, if it has an xref bullet
  (according to setting of 'outline-file-xref-bullet')."
  (interactive)
  (if (not outline-file-xref-bullet)
      (error
       "outline cross references disabled - no 'outline-file-xref-bullet'")
    (if (not (string= (outline-current-bullet) outline-file-xref-bullet))
        (error "current heading lacks cross-reference bullet '%s'"
               outline-file-xref-bullet)
      (let (file-name)
        (save-excursion
          (let* ((text-start outline-recent-prefix-end)
                 (heading-end (progn (outline-pre-next-preface)
                                     (point))))
            (goto-char text-start)
            (setq file-name
                  (if (re-search-forward "\\s-\\(\\S-*\\)" heading-end t)
                      (buffer-substring (match-beginning 1) (match-end 1))))))
        (setq file-name
              (if (not (= (aref file-name 0) ?:))
                  (expand-file-name file-name)
                                        ; A registry-files ref, strip the ':'
                                        ; and try to follow it:
                (let ((reg-ref (reference-registered-file
                                (substring file-name 1) nil t)))
                  (if reg-ref (car (cdr reg-ref))))))
        (if (or (file-exists-p file-name)
                (if (file-writable-p file-name)
                    (y-or-n-p (format "%s not there, create one? "
                                      file-name))
                  (error "%s not found and can't be created" file-name)))
            (condition-case failure
                (find-file-other-window file-name)
              (error failure))
          (error "%s not found" file-name))
        )
      )
    )
  )

(defun outline-show-current-children (&optional level)
  "  Show all direct subheadings of this heading.  Optional LEVEL specifies
   how many levels below the current level should be shown."
  (interactive "p")
  (or level (setq level 1))
  (save-excursion
   (save-restriction
    (beginning-of-line)
    (setq level (+ level (progn (outline-back-to-current-heading)
                                (outline-recent-depth))))
    (narrow-to-region (point)
		      (progn (outline-end-of-current-subtree) (1+ (point))))
    (goto-char (point-min))
    (while (and (not (eobp))
                (outline-next-heading))
      (if (<= (outline-recent-depth) level)
	  (save-excursion
	   (let ((end (1+ (point))))
	     (forward-char -1)
	     (if (memq (preceding-char) '(?\n ?\^M))
		 (forward-char -1))
	     (outline-flag-region (point) end ?\n))))))))

;-------------------------------------------------------------------
; Something added solely for use by a "smart menu" package someone got
; off the net.  I have no idea whether this is appropriate code.

(defun outline-to-entry-end (&optional include-sub-entries curr-entry-level)
  "   Go to end of whole entry if optional INCLUDE-SUB-ENTRIES is non-nil.
   CURR-ENTRY-LEVEL is an integer representing the length of the current level
   string which matched to 'outline-regexp'.  If INCLUDE-SUB-ENTRIES is nil,
   CURR-ENTRY-LEVEL is not needed."
  (while (and (setq next-entry-exists
		    (re-search-forward outline-regexp nil t))
	      include-sub-entries
	      (save-excursion
		(beginning-of-line)
		(> (outline-depth) curr-entry-level))))
  (if next-entry-exists
      (progn (beginning-of-line) (point))
    (goto-char (point-max))))

;;;-------------------------------------------------------------------------
;;; Outline topic prefix and level adjustment funcs:

(defun outline-bullet-for-depth (&optional depth)
  "   Return outline topic bullet suited to DEPTH, or for current depth if none
   specified."
  ;; Find bullet in plain-bullets-string modulo DEPTH.
  (if (null depth) (setq depth (outline-visible-depth)))
  (if outline-stylish-prefixes
      (char-to-string (aref outline-plain-bullets-string
                            (% (max 0 (- depth 2))
                               outline-plain-bullets-string-len)))
    outline-primary-bullet)
  )


(defun outline-sibling-index (&optional depth)
  "   Item number of this prospective topic among it's siblings.

   If optional arg depth is greater than current depth, then we're
   opening a new level, and return 0.

   If less than this depth, ascend to that depth and count..."

  (save-excursion
    (cond ((and depth (<= depth 0) 0))
          ((or (not depth) (= depth (outline-depth)))
           (let ((index 1))
             (while (outline-previous-sibling) (setq index (1+ index)))
             index))
          ((< depth (outline-recent-depth))
           (outline-ascend-to-depth depth)
           (outline-sibling-index))
          (0))))

(defun solicit-char-in-string (prompt string &optional do-defaulting)
  "   Solicit (with first arg PROMPT) choice of a character from string STRING.

   Optional arg DO-DEFAULTING indicates to accept empty input (CR)."

  (let ((new-prompt prompt)
        got)

    (while (not got)
      (message "%s" new-prompt)

      ;; We do our own reading here, so we can circumvent, eg, special
      ;; treatment for '?' character.  (Might oughta change minibuffer
      ;; keymap instead, oh well.)
      (setq got
            (char-to-string (let ((cursor-in-echo-area t)) (read-char))))

      (if (null (string-match got string))
          (if (and do-defaulting (string= got "\^M"))
              ;; We're defaulting, return null string to indicate that:
              (setq got "")
            ;; Failed match and not defaulting,
            ;; set the prompt to give feedback,
            (setq new-prompt (concat prompt
                                     got
                                     " ...pick from: "
                                     string
                                     ""))
            ;; and set loop to try again:
            (setq got nil))
        ;; Got a match - give feedback:
        (message "")))
    ;; got something out of loop - return it:
    got)
  )

(defun outline-solicit-alternate-bullet (depth &optional current-bullet)

  "   Prompt for and return a bullet char as an alternative to the
   current one, but offer one suitable for current depth DEPTH
   as default."

  (let* ((default-bullet (or current-bullet
                             (outline-bullet-for-depth depth)))
	 (choice (solicit-char-in-string
                  (format "Choose a bullet from '%s' [%s]: "
                          outline-bullets-string
                          default-bullet)
                  outline-bullets-string
                  t)))
    (if (string= choice "") default-bullet choice))
  )

(defun outline-distinctive-bullet (bullet)
  "   True if bullet is one of those on outline-distinctive-bullets-string."
  (string-match (regexp-quote bullet) outline-distinctive-bullets-string))

(defun outline-numbered-type-prefix (&optional prefix)
  "   True if current header prefix bullet is numbered bullet."
  (and outline-numbered-bullet
        (string= outline-numbered-bullet
                 (if prefix
                     (outline-get-prefix-bullet prefix)
                   (outline-get-bullet)))))

(defun outline-make-topic-prefix (&optional prior-bullet
                                            new
                                            depth
                                            solicit
                                            number-control
                                            index)
  ;; Depth null means use current depth, non-null means we're either
  ;; opening a new topic after current topic, lower or higher, or we're
  ;; changing level of current topic.
  ;; Solicit dominates specified bullet-char.
  "   Generate a topic prefix suitable for optional arg DEPTH, or current
   depth if not specified.

   All the arguments are optional.

   PRIOR-BULLET indicates the bullet of the prefix being changed, or
   nil if none.  This bullet may be preserved (other options
   notwithstanding) if it is on the outline-distinctive-bullets-string,
   for instance.

   Second arg NEW indicates that a new topic is being opened after the
   topic at point, if non-nil.  Default bullet for new topics, eg, may
   be set (contingent to other args) to numbered bullets if previous
   sibling is one.  The implication otherwise is that the current topic
   is being adjusted - shifted or rebulleted - and we don't consider
   bullet or previous sibling.

   Third arg DEPTH forces the topic prefix to that depth, regardless of
   the current topics' depth.

   Fourth arg SOLICIT non-nil provokes solicitation from the user of a
   choice among the valid bullets.  (This overrides other all the
   options, including, eg, a distinctive PRIOR-BULLET.)

   Fifth arg, NUMBER-CONTROL, matters only if 'outline-numbered-bullet'
   is non-nil *and* soliciting was not explicitly invoked.  Then
   NUMBER-CONTROL non-nil forces prefix to either numbered or
   denumbered format, depending on the value of the sixth arg, INDEX.

   (Note that NUMBER-CONTROL does *not* apply to level 1 topics.  Sorry...)

   If NUMBER-CONTROL is non-nil and sixth arg INDEX is non-nil then
   the prefix of the topic is forced to be numbered.  Non-nil
   NUMBER-CONTROL and nil INDEX forces non-numbered format on the
   bullet.  Non-nil NUMBER-CONTROL and non-nil, non-number INDEX means
   that the index for the numbered prefix will be derived, by counting
   siblings back to start of level.  If INDEX is a number, then that
   number is used as the index for the numbered prefix (allowing, eg,
   sequential renumbering to not requre this function counting back the
   index for each successive sibling)."

  ;; The options are ordered in likely frequence of use, most common
  ;; highest, least lowest.  Ie, more likely to be doing prefix
  ;; adjustments than soliciting, and yet more than numbering.
  ;; Current prefix is least dominant, but most likely to be commonly
  ;; specified...

  (let* (body
         numbering
         denumbering
         (depth (or depth (outline-depth)))
         (header-lead outline-header-prefix)
         (bullet-char

          ;; Getting value for bullet char is practically the whole job:

          (cond
                                        ; Simplest situation - level 1:
           ((<= depth 1) (setq header-lead "") outline-primary-bullet)
                                        ; Simple, too: all asterisks:
           (outline-old-style-prefixes
            ;; Cheat - make body the whole thing, null out header-lead and
            ;; bullet-char:
            (setq body (make-string depth
                                    (string-to-char outline-primary-bullet)))
            (setq header-lead "")
            "")

           ;; (Neither level 1 nor old-style, so we're space padding.
           ;; Sneak it in the condition of the next case, whatever it is.)

           ;; Solicitation overrides numbering and other cases:
           ((progn (setq body (make-string (- depth 2) ?\ ))
                   ;; The actual condition:
                   solicit)
            (let* ((got (outline-solicit-alternate-bullet depth)))
              ;; Gotta check whether we're numbering and got a numbered bullet:
              (setq numbering (and outline-numbered-bullet
                                   (not (and number-control (not index)))
                                   (string= got outline-numbered-bullet)))
              ;; Now return what we got, regardless:
              got))

           ;; Numbering invoked through args:
           ((and outline-numbered-bullet number-control)
            (if (setq numbering (not (setq denumbering (not index))))
                outline-numbered-bullet
              (if (and current-bullet
                       (not (string= outline-numbered-bullet
                                     current-bullet)))
                  current-bullet
                (outline-bullet-for-depth depth))))

          ;;; Neither soliciting nor controlled numbering ;;;
             ;;; (may be controlled denumbering, tho) ;;;

           ;; Check wrt previous sibling:
           ((and new				  ; only check for new prefixes
                 (<= depth (outline-depth))
                 outline-numbered-bullet	      ; ... & numbering enabled
                 (not denumbering)
                 (let ((sibling-bullet
                        (save-excursion
                          ;; Locate correct sibling:
                          (or (>= depth (outline-depth))
                              (outline-ascend-to-depth depth))
                          (outline-get-bullet))))
                   (if (and sibling-bullet
                            (string= outline-numbered-bullet sibling-bullet))
                       (setq numbering sibling-bullet)))))

           ;; Distinctive prior bullet?
           ((and prior-bullet
                 (outline-distinctive-bullet prior-bullet)
                 ;; Either non-numbered:
                 (or (not (and outline-numbered-bullet
                               (string= prior-bullet outline-numbered-bullet)))
                     ;; or numbered, and not denumbering:
                     (setq numbering (not denumbering)))
                 ;; Here 'tis:
                 prior-bullet))

           ;; Else, standard bullet per depth:
           ((outline-bullet-for-depth depth)))))

    (concat header-lead
            body
            bullet-char
            (if numbering
                (format "%d" (cond ((and index (numberp index)) index)
                                   (new (1+ (outline-sibling-index depth)))
                                   ((outline-sibling-index))))))
    )
  )

(defun outline-reindent-body (old-depth new-depth)
  "  Reindent body lines which were indented at old-depth to new-depth.

  Note that refill of indented paragraphs is not done, and tabs are
  not accomodated.  ('untabify' your outline if you want to preserve
  hanging body indents.)"

  (save-excursion
    (save-restriction
      (outline-goto-prefix)
      (forward-char 1)
      (let* ((old-spaces-expr (make-string (1+ old-depth) ?\ ))
             (new-spaces-expr (concat (make-string (1+ new-depth) ?\ )
                                      ;; spaces followed by non-space:
                                      "\\1")))
        (while (and (re-search-forward "[\C-j\C-m]" nil t)
                    (not (looking-at outline-regexp)))
          (if (looking-at old-spaces-expr)
              (replace-match new-spaces-expr)))))))

(defun outline-rebullet-current-heading (arg)
  "   Like non-interactive version 'outline-rebullet-heading', but work on
   (only) visible heading containing point.

   With repeat count, solicit for bullet."
  (interactive "P")
  (save-excursion (outline-back-to-current-heading)
                  (outline-rebullet-heading (not arg)	;;; solicit
                                            nil		;;; depth
                                            nil		;;; number-control
                                            nil		;;; index
                                            t)		;;; do-successors
                  )
  )
(defun outline-rebullet-heading (&optional solicit
                                           new-depth
                                           number-control
                                           index
                                           do-successors)

  "   Adjust bullet of current topic prefix.

   All args are optional.

   If SOLICIT is non-nil then the choice of bullet is solicited from
   user.  Otherwise the distinctiveness of the bullet or the topic
   depth determines it.

   Second arg DEPTH forces the topic prefix to that depth, regardless
   of the topic's current depth.

   Third arg NUMBER-CONTROL can force the prefix to or away from
   numbered form.  It has effect only if 'outline-numbered-bullet' is
   non-nil and soliciting was not explicitly invoked (via first arg).
   Its effect, numbering or denumbering, then depends on the setting
   of the forth arg, INDEX.

   If NUMBER-CONTROL is non-nil and forth arg INDEX is nil, then the
   prefix of the topic is forced to be non-numbered.  Null index and
   non-nil NUMBER-CONTROL forces denumbering.  Non-nil INDEX (and
   non-nil NUMBER-CONTROL) forces a numbered-prefix form.  If non-nil
   INDEX is a number, then that number is used for the numbered
   prefix.  Non-nil and non-number means that the index for the
   numbered prefix will be derived by outline-make-topic-prefix.

   Fifth arg DO-SUCCESSORS t means re-resolve count on succeeding
   siblings.

   Cf vars 'outline-stylish-prefixes', 'outline-old-style-prefixes',
   and 'outline-numbered-bullet', which all affect the behavior of
   this function."

  (let* ((current-depth (outline-depth))
         (new-depth (or new-depth current-depth))
         (mb outline-recent-prefix-beginning)
         (me outline-recent-prefix-end)
         (current-bullet (buffer-substring (- me 1) me))
         (new-prefix (outline-make-topic-prefix current-bullet
                                                nil
                                                new-depth
                                                solicit
                                                number-control
                                                index)))

    ;; Don't need to reinsert identical one:
    (if (and (= current-depth new-depth)
             (string= current-bullet
                      (substring new-prefix (1- (length new-prefix)))))
        t

      ;; New prefix probably different from old:
      ;; get rid of old one:
      (delete-region mb me)
      (goto-char mb)
      ;; Dispense with number if numbered-bullet prefix:
      (if (and outline-numbered-bullet
               (string= outline-numbered-bullet current-bullet)
               (looking-at "[0-9]+"))
          (delete-region (match-beginning 0)(match-end 0)))

      ;; Put in new prefix:
      (insert-string new-prefix)
      )

    ;; Reindent the body if elected and depth changed:
    (if (and outline-reindent-bodies
             (not (= new-depth current-depth)))
        (outline-reindent-body current-depth new-depth))

    ;; Recursively rectify successive siblings if selected:
    (if do-successors
        (save-excursion
          (while (outline-next-sibling)
            (setq index
                  (cond ((numberp index) (1+ index))
                        ((not number-control)  (outline-sibling-index))))
            (if (outline-numbered-type-prefix)
                (outline-rebullet-heading nil		;;; solicit
                                          new-depth	;;; new-depth
                                          number-control;;; number-control
                                          index		;;; index
                                          nil)))))	;;;(dont!)do-successors
      )
  )

(defun outline-rebullet-topic (arg)
  "   Like outline-rebullet-topic-grunt, but start from topic visible at point.
   Descends into invisible as well as visible topics, however.

   With repeat count, shift topic depth by that amount."
  (interactive "P")
  (let ((start-col (current-column))
        (was-eol (eolp)))
    (save-excursion
      ;; Normalize arg:
      (cond ((null arg) (setq arg 0))
            ((listp arg) (setq arg (car arg))))
      ;; Fill the user in, in case we're shifting a big topic:
      (if (not (zerop arg)) (message "Shifting..."))
      (outline-back-to-current-heading)
      (if (<= (+ (outline-recent-depth) arg) 0)
          (error "Attempt to shift topic below level 1"))
      (outline-rebullet-topic-grunt arg)
      (if (not (zerop arg)) (message "Shifting... done.")))
    (move-to-column (+ start-col arg)))
  )
(defun outline-rebullet-topic-grunt (&optional relative-depth
                                               starting-depth
                                               starting-point
                                               index
                                               do-successors)

  "   Rebullet the topic at point, visible or invisible, and all
   contained subtopics.  See outline-rebullet-heading for rebulleting
   behavior.

   All arguments are optional.

   First arg RELATIVE-DEPTH means to shift the depth of the entire
   topic that amount.

   The rest of the args are for internal recursive use by the function
   itself.  The are STARTING-DEPTH, STARTING-POINT, and INDEX."

  (let* ((relative-depth (or relative-depth 0))
         (new-depth (outline-depth))
         (starting-depth (or starting-depth new-depth))
         (on-starting-call  (null starting-point))
         (index (or index
                    ;; Leave index null on starting call, so rebullet-heading
                    ;; calculates it at what might be new depth:
                    (and (or (zerop relative-depth)
                             (not on-starting-call))
                         (outline-sibling-index))))
         (moving-outwards (< 0 relative-depth))
         (starting-point (or starting-point (point))))

    ;; Sanity check for excessive promotion done only on starting call:
    (and on-starting-call
         moving-outwards
         (> 0 (+ starting-depth relative-depth))
         (error "Attempt to shift topic out beyond level 1."))	;;; ====>

    (cond ((= starting-depth new-depth)
           ;; We're at depth to work on this one:
           (outline-rebullet-heading nil		;;; solicit
                                     (+ starting-depth	;;; starting-depth
                                        relative-depth)
                                     nil		;;; number
                                     index		;;; index
                                     ;; Every contained topic will get hit,
                                     ;; and we have to get to outside ones
                                     ;; deliberately:
                                     nil)		;;; do-successors
           ;; ... and work on subsequent ones which are at greater depth:
           (setq index 0)
           (outline-next-heading)
           (while (and (not (eobp))
                       (< starting-depth (outline-recent-depth)))
             (setq index (1+ index))
             (outline-rebullet-topic-grunt relative-depth   ;;; relative-depth
                                           (1+ starting-depth);;;starting-depth
                                           starting-point   ;;; starting-point
                                           index)))	    ;;; index

          ((< starting-depth new-depth)
           ;; Rare case - subtopic more than one level deeper than parent.
           ;; Treat this one at an even deeper level:
           (outline-rebullet-topic-grunt relative-depth   ;;; relative-depth
                                         new-depth	  ;;; starting-depth
                                         starting-point	  ;;; starting-point
                                         index)))	  ;;; index

    (if on-starting-call
        (progn
          ;; Rectify numbering of former siblings of the adjusted topic,
          ;; if topic has changed depth
          (if (or do-successors
                  (and (not (zerop relative-depth))
                       (or (= (outline-recent-depth) starting-depth)
                           (= (outline-recent-depth) (+ starting-depth
                                                        relative-depth)))))
              (outline-rebullet-heading nil nil nil nil t))
          ;; Now rectify numbering of new siblings of the adjusted topic,
          ;; if depth has been changed:
          (progn (goto-char starting-point)
                 (if (not (zerop relative-depth))
                     (outline-rebullet-heading nil nil nil nil t)))))
    )
  )
(defun outline-number-siblings (&optional denumber)
  "   Assign numbered topic prefix to this topic and its siblings.

   With universal argument, denumber - assign default bullet to this
   topic and its siblings.

   With repeated universal argument (`^U^U'), solicit bullet for each
   rebulleting each topic at this level."

  (interactive "P")

  (save-excursion
    (outline-back-to-current-heading)
    (outline-beginning-of-level)
    (let ((index (if (not denumber) 1))
          (use-bullet (equal '(16) denumber))
          (more t))
      (while more
        (outline-rebullet-heading use-bullet		;;; solicit
                                  nil			;;; depth
                                  t			;;; number-control
                                  index			;;; index
                                  nil)			;;; do-successors
        (if index (setq index (1+ index)))
        (setq more (outline-next-sibling)))
      )
    )
  )


(defun outline-shift-in (arg)
  "   Decrease prefix depth of current heading and any topics collapsed
   within it."
  (interactive "p")
  (outline-rebullet-topic arg))
(defun outline-shift-out (arg)
  "   Decrease prefix depth of current heading and any topics collapsed
   within it."
  (interactive "p")
  (outline-rebullet-topic (* arg -1)))

;;;-------------------------------------------------------------------------
;;; Outline topic creation forms:

(defun open-topic (relative-depth &optional before)
  " Open a new topic at depth DEPTH.  New topic is situated after current
  one, unless optional flag BEFORE is non-nil, or unless current line
  is complete empty (not even whitespace), in which case open is done
  on current line.

  Nuances:

   - Creation of new topics is with respect to the visible topic
     containing the cursor, regardless of intervening concealed ones.
   - New headers are generally created after (or before) the body of a
     topic.  However, they are created right at cursor location if the
     cursor is on a blank line, even if that breaks the current topic
     body.  This is intentional, to provide a simple means for
     deliberately dividing topic bodies.
   - Double spacing of topic lists is preserved.  Also, the first
     level two topic is created double-spaced (and so would be
     subsequent siblings, if that's left intact).  Otherwise,
     single-spacing is used.
   - Creation of sibling or deeper topics is with respect to the topic
     at the starting point, even when creating backwards.  This way
     you can easily create a sibling in front of the current topic
     without having to first find the sibling currently in front of
     it."

  (let* ((depth (+ (outline-current-depth) relative-depth))
         (opening-on-blank (if (looking-at "^\$")
                               (not (setq before nil))))
         opening-numbered	; Will get while computing ref-topic, below
         ref-depth		; Will get while computing ref-topic, next
         (ref-topic (save-excursion
                      (cond ((< relative-depth 0)
                             (outline-ascend-to-depth depth))
                            ((>= relative-depth 1) nil)
                            (t (outline-back-to-current-heading)))
                      (setq ref-depth (outline-recent-depth))
                      (setq opening-numbered
                            (save-excursion
                              (and outline-numbered-bullet
                                   (or (<= relative-depth 0)
                                       (outline-descend-to-depth depth)))
                              (if (outline-numbered-type-prefix)
                                  outline-numbered-bullet)))
                      (point)))
         dbl-space
         )
                                        ; Motion stuff - all
                                        ; overridden if opening-on-blank:
    (if (not opening-on-blank)
        (progn 
          (goto-char ref-topic)
                                        ; Determine double space action:
                                        ; When ref is not bobp,
          (setq dbl-space (or (bobp)
                                        ; ... and not descending
                              (and (not (> relative-depth 0))
                                   (save-excursion
                                     ;; preceeded by a blank line?
                                     (forward-line -1)
                                     (looking-at "^\\s-*$")))))

                                        ; Reuse start-depth to
                                        ; register current depth:

                                        ; Position to prior heading,
                                        ; if inserting backwards:
          (if before (progn (outline-back-to-current-heading)
                            (if (and (not (outline-previous-sibling))
                                     (not (bobp)))
                                (outline-previous-heading))))

          (if (and (<= depth ref-depth)
                   (= ref-depth (outline-recent-depth)))
              ;; Not going inwards, don't snug up:
              (outline-end-of-current-subtree)
            ;; Going inwards - double-space if first offspring is,
            ;; otherwise snug up.
            (end-of-line)		; So we skip any concealed progeny.
            (outline-pre-next-preface)
            (if (bolp)
                ;; Blank lines between current header body and next
                ;; header - get to last substantive (non-white-space)
                ;; line in body:
                (re-search-backward "[^ \t\n]"))
            (if (save-excursion
                  (outline-next-heading)
                  (if (> (outline-recent-depth) ref-depth)
                      ;; This is an offspring.
                      (progn (forward-line -1)
                             (looking-at "^\\s-*$"))))
                (progn (forward-line 1)
                       (open-line 1)))
            (end-of-line))
          (if (not (bobp)) (newline (if dbl-space 2 1)))
          ))
    (insert-string (concat (outline-make-topic-prefix opening-numbered
                                                      t
                                                      depth)
                           " "))
    (outline-rebullet-heading nil		;;; solicit
                              depth 		;;; depth
                              nil 		;;; number-control
                              nil		;;; index
                              t)     (end-of-line)
    )
  )

(defun open-subtopic (arg)
  "   Open new topic header at deeper level than the current one.

  Negative universal arg means to open deeper, but place the new topic
  prior to the current one."
  (interactive "p")
  (open-topic 1 (> 0 arg)))
(defun open-sibtopic (arg)
  "   Open new topic header at same level as the current one.  Negative
  universal arg means to place the new topic prior to the current
  one."
  (interactive "p")
  (open-topic 0 (> 0 arg)))
(defun open-supertopic (arg)
  "   Open new topic header at shallower level than the current one.
  Negative universal arg means to open shallower, but place the new
  topic prior to the current one."

  (interactive "p")
  (open-topic -1 (> 0 arg)))

;;;-------------------------------------------------------------------------
;;; Surgery (kill-ring) functions with special provisions for outlines:

(defun outline-kill-line (&optional arg)
  "   Kill line, adjusting subsequent lines suitably for outline mode."

  (interactive "*P")
  (if (not (and outline-numbered-bullet (bolp) (looking-at outline-regexp)))
      (kill-line arg)
    (let* ((depth (outline-depth))
           (ascender depth))
      (kill-line arg)
      (sit-for 0)
      (save-excursion
        (if (not (looking-at outline-regexp))
            (outline-next-heading))
        (if (> (outline-depth) depth)
            ;; An intervening parent was removed from after a subtree:
            (setq depth (outline-recent-depth)))
        (while (and (> (outline-depth) 0)
                    (> (outline-recent-depth) ascender)
                    (outline-ascend-to-depth (setq ascender
                                                   (1- ascender)))))
        ;; Have to try going forward until we find another at
        ;; desired depth:
        (if (and outline-numbered-bullet
                 (outline-descend-to-depth depth))
            (outline-rebullet-heading nil		;;; solicit
                                      depth		;;; depth
                                      nil 		;;; number-control
                                      nil		;;; index
                                      t)		;;; do-successors
          )
        )
      )
    )
  )
(defun outline-kill-topic ()
  "   Kill topic together with subtopics."

  ;; Some finagling is done to make complex topic kills appear faster
  ;; than they actually are.  A redisplay is performed immediately
  ;; after the region is disposed of, though the renumbering process
  ;; has yet to be performed.  This means that there may appear to be
  ;; a lag *after* the kill has been performed.

  (interactive)
  (let* ((beg (outline-back-to-current-heading))
         (depth (outline-recent-depth)))
    (outline-end-of-current-subtree)
    (if (not (eobp))
        (forward-char 1))
    (kill-region beg (point))
    (sit-for 0)
    (save-excursion
      (if (and outline-numbered-bullet
               (outline-descend-to-depth depth))
          (outline-rebullet-heading nil		;;; solicit
                                    depth	;;; depth
                                    nil		;;; number-control
                                    nil		;;; index
                                    t)		;;; do-successors
        )
      )
    )
  )

(defun outline-yank (&optional arg)
  "   Like regular yank, except does depth adjustment of yanked topics, when:

   1 the stuff being yanked starts with a valid outline header prefix, and
   2 it is being yanked at the end of a line which consists of only a valid
     topic prefix.

   If these two conditions hold then the depth of the yanked topics
   are all adjusted the amount it takes to make the first one at the
   depth of the header into which it's being yanked.

   The point is left in from of yanked, adjusted topics, rather than
   at the end (and vice-versa with the mark).  Non-adjusted yanks,
   however, (ones that don't qualify for adjustment) are handled
   exactly like normal yanks.

   Outline-yank-pop is used with outline-yank just as normal yank-pop
   is used with normal yank in non-outline buffers."

  (interactive "*P")
  (setq this-command 'yank)
  (let ((beginning (point))
        topic-yanked
        established-depth)      ; Depth of the prefix into which we're yanking.
    ;; Get current depth and numbering ... Oops, not doing anything
    ;; with the number just yet...
    (if (and (eolp)
             (save-excursion (beginning-of-line)
                             (looking-at outline-regexp)))
        (setq established-depth (- (match-end 0) (match-beginning 0))))
    (yank arg)
    (exchange-dot-and-mark)
    (if (and established-depth          ; the established stuff qualifies.
             ;; The yanked stuff also qualfies - is topic(s):
             (looking-at (concat "\\(" outline-regexp "\\)")))
        ;; Ok, adjust the depth of the yanked stuff.  Note that the
        ;; stuff may have more than a single root, so we have to
        ;; iterate over all the top level ones yanked, and do them in
        ;; such a way that the adjustment of one new one won't affect
        ;; any of the other new ones.  We use the focus of the
        ;; narrowed region to successively exclude processed siblings.
        (let* ((yanked-beg (match-beginning 1))
               (yanked-end (match-end 1))
               (yanked-bullet (buffer-substring (1- yanked-end) yanked-end))
               (yanked-depth (- yanked-end yanked-beg))
               (depth-diff (- established-depth yanked-depth))
               done
               (more t))
          (setq topic-yanked t)
          (save-excursion
            (save-restriction
              (narrow-to-region yanked-beg (mark))
              ;; First trim off excessive blank line at end, if any:
              (goto-char (point-max))
              (if (looking-at "^$") (delete-char -1))
              (goto-char (point-min))
              ;; Work backwards, with each shallowest level,
              ;; successively excluding the last processed topic
              ;; from the narrow region:
              (goto-char (point-max))
              (while more
                (outline-back-to-current-heading)
                ;; go as high as we can in each bunch:
                (while (outline-ascend-to-depth
                        (1- (outline-depth))))
                (save-excursion
                  (outline-rebullet-topic-grunt depth-diff
                                                (outline-depth)
                                                (point)))
                (if (setq more (not (bobp)))
                    (progn (widen)
                           (forward-char -1)
                           (narrow-to-region yanked-beg (point)))))))
          ;; Preserve new bullet if it's a distinctive one, otherwise
          ;; use old one:
          (if (string-match yanked-bullet outline-distinctive-bullets-string)
              (delete-region (save-excursion
                               (beginning-of-line)
                               (point))
                             yanked-beg)
            (delete-region yanked-beg (+ yanked-beg established-depth))
            ;; and extraneous digits and a space:
            (while (looking-at "[0-9]") (delete-char 1))
            (if (looking-at " ") (delete-char 1))
            )
          )
      (exchange-dot-and-mark))
    (if (and topic-yanked outline-numbered-bullet)
        (progn
          ;; Renumber, in case necessary:
          (sit-for 0)
          (save-excursion
            (goto-char beginning)
            (if (outline-goto-prefix)
                (outline-rebullet-heading nil		;;; solicit
                                          (outline-depth) ;;; depth
                                          nil		;;; number-control
                                          nil		;;; index
                                          t)		;;; do-successors
                  )
            )
          )
      )
    )
  )
(defun outline-yank-pop (&optional arg)
  "   Just like yank-pop, but works like outline-yank when popping
  topics just after fresh outline prefixes.  Adapts level of popped
  stuff to level of fresh prefix."

  (interactive "*p")
  (if (not (eq last-command 'yank))
      (error "Previous command was not a yank"))
  (setq this-command 'yank)
  (delete-region (point) (mark))
  (rotate-yank-pointer arg)
  (outline-yank)
  )

;;;-------------------------------------------------------------------------
;;; isearch wrappers for special outline provisions:

(defvar outline-search-reconceal nil
  "Used for outline isearch provisions, to track whether current search
match was concealed outside of search.  The value is the location of the
match, if it was concealed, regular if the entire topic was concealed, in
a list if the entry was concealed.")
(defun outline-enwrap-isearch ()
  "   Impose isearch-mode wrappers so isearch progressively exposes and
   reconceals hidden topics when working in outline mode, but works
   elsewhere.

   The function checks to ensure that the rebindings are done only once"
  ;; Make sure isearch-mode is loaded,
  (if (or (not outline-enwrap-isearch-mode)
          (fboundp 'real-isearch-terminate))
      nil
    (if (not (and (fboundp 'isearch-mode)
                  (fboundp 'isearch-quote-char)))
        (load-library "isearch-mode.el"))
    ;; stash the crux-point functions so they're in known places, then
    ;; register the wrapper functions under their old names, instead:
                                        ; 'isearch-quit' is pre v 1.2:
    (fset 'real-isearch-terminate (or (if (fboundp 'isearch-quit)
                                          (symbol-function 'isearch-quit))
                                      (if (fboundp 'isearch-abort)
                                        ; 'isearch-abort' is v 1.2 and on:
                                          (symbol-function 'isearch-abort))))
    (fset 'isearch-quit 'isearch-terminate/outline-provisions)
    (fset 'isearch-abort 'isearch-terminate/outline-provisions)
    (fset 'real-isearch-done (symbol-function 'isearch-done))
    (fset 'isearch-done 'isearch-done/outline-provisions)
    (fset 'real-isearch-update (symbol-function 'isearch-update))
    (fset 'isearch-update 'isearch-update/outline-provisions)
    (make-variable-buffer-local 'outline-search-reconceal))
  )
(defun outline-isearch-arrival-business ()
  "   Do outline business like exposing current point, if necessary,
   registering reconceal state accordingly."
  (setq outline-search-reconceal
        (if (outline-hidden-p)
            ;; set to just point if the entire topic is hidden, or is
            ;; supposed to be hidden (according to already pending
            ;; setting of outline-search-reconceal), or point in a list
            ;; if just part of the entry is hidden:
            (save-excursion (outline-goto-prefix)
                            (prog1 (if (outline-hidden-p)
                                        (point)
                                      (list (point)))
                              (outline-show-entry)))
            )
    )
  )
(defun outline-isearch-advancing-business ()
  "   Do outline business like deexposing current point, if necessary,
   according to reconceal state registration."
  (if outline-search-reconceal
      (save-excursion
        (if (listp outline-search-reconceal)
            ;; Leave the topic visible:
            (progn (goto-char (car outline-search-reconceal))
                   (outline-hide-current-entry))
          ;; Rehide the entire topic:
          (goto-char outline-search-reconceal)
          (outline-hide-current-entry-completely))))
  )

(defun isearch-terminate/outline-provisions ()
  (interactive)
  (if (and outline-enwrap-isearch-mode
           (string= mode-name "Outline"))
      (outline-isearch-advancing-business))
  (real-isearch-terminate))
(defun isearch-done/outline-provisions ()
  (interactive)
  (if (and outline-enwrap-isearch-mode
           (string= mode-name "Outline"))
      (save-excursion
        (if (and outline-search-reconceal
                 (not (listp outline-search-reconceal)))
            ;; The topic was concealed - reveal it, its siblings,
            ;; and any ancestors that are still concealed:
            (progn (message "(exposing destination)")(sit-for 0)
                   (outline-ascend-to-depth (1- (outline-depth)))
                   ;; Ensure that the target topic's ancestors are exposed
                   (while (outline-hidden-p) (outline-show-current-children))
                   ;; Ensure target topic's siblings are exposed:
                   (outline-show-current-children)
                   (outline-show-current-entry)))
        (outline-isearch-arrival-business)))
  (real-isearch-done)
  )
(defun isearch-update/outline-provisions ()
  "    Wrapper around isearch which exposes and conceals hidden outline
   portions encountered in the course of searching."
  (if (not (and outline-enwrap-isearch-mode
                (string= mode-name "Outline")))
      ;; Just do the plain business:
      (real-isearch-update)

    ;; Ah - provide for outline conditions:
    (outline-isearch-advancing-business)
    (real-isearch-update)
    (cond (isearch-success (outline-isearch-arrival-business))
          ((not isearch-success) (outline-isearch-advancing-business)))
    )
  )



;;;-------------------------------------------------------------------------
;;; Scattered and Sundries

(defun outline-copy-exposed (&optional workbuf)
  "   Duplicate buffer to other buffer, sans hidden stuff.

   Without repeat count, this simple-minded function just generates
   the new buffer by concatenating the current buffer name with \"
   exposed\", and doing a 'get-buffer' on it."

  (interactive)
  (if (not workbuf) (setq workbuf (concat (buffer-name) " exposed")))
  (let ((buf (current-buffer)))
    (if (not (get-buffer workbuf))
	(generate-new-buffer workbuf))
    (pop-to-buffer workbuf)
    (erase-buffer)
    (insert-buffer buf)
    (replace-regexp "\^M[^\^M\^J]*" "")
    (goto-char (point-min))
    )
  )

(defun outlineify-sticky ()
  "   Activate outline mode and establish an -*-outline-*- explicit mode
   trigger in buffer."
  (interactive)
  (outline-mode)
  (save-excursion
    (goto-char 0)
    (if (not (looking-at ".*-\*-outline-*-"))
	(insert "* -*-outline-*-\n" ))
    (goto-char (point-max))
    (insert-string "
* Local emacs vars.
.* local")
    ;; Break this string in the code so it isn't interpreted as a
    ;; local vars declaration in the source code file.
    (insert-string " variables:
.* eval: (save-excursion
           (message \"Adjusting '%s' visibility\" (buffer-name))
	    (goto-char 0)
           (outline-hide-current-subtree)
           (outline-show-current-children)
           (goto-char (point-max))
           (re-search-backward \"^* Local emacs vars.\")
           (outline-hide-current-subtree))
.* End:
")
    )
  )
                                        
;; Change history (since last RCS rev)
