;;; lre-net.el --- Lars Reed - Emacs init file
;; Setup for Gnus/VM/RMAIL/...

;; Copyright (C) 2002 Lars Reed @@(#) LRE %M% %I% %E%>
;;   See lresetup.el
;; Author:		Lars Reed <Lars@kalars.net>
;; Last-Modified:	$Date: 2004/09/10 19:24:52 $
;; Version:		$Id: lre-net.el,v 1.2 2004/09/10 19:24:52 larsr Exp $
; Hi-lock: (("^;;; lre.*\\.el[^\n]*" (0 (quote hi-black-hb) t)))
; Hi-lock: (("Lars.*R[e]ed" (0 (quote hi-blue) t)))
; Hi-lock: (("^;;; [^\n]+" (0 (quote hi-green) t)))

;;; Code

;; --------------------------------------------------------------


(defun lre-win32-mail ()
  "Win32 mail/news setup."
  (setq user-full-name                  lre-full-name
	user-mail-address		lre-mail-address
	mail-host-address		lre-std-domain
	smtpmail-default-smtp-server	lre-smtp-domain
	smtpmail-local-domain		nil
	feedmail-buffer-eating-function 'feedmail-buffer-to-smtpmail
	rmail-primary-inbox-list	(list (concat "po:" lre-mail-user)
					  'rmail-pop-password-required t)
	nnmail-spool-file		(concat "po:" lre-mail-user)
	nnmail-pop-password-required	t
	gnus-secondary-select-methods   '((nnml ""))
	gnus-button-url			'shell-execute-url
	vm-url-browser			'shell-execute-url
	message-send-mail-function	'smtpmail-send-it
	send-mail-function	        'smtpmail-send-it)
  ;;                                    'feedmail-send-it
  ;;    feedmail-enable-queue		t
  ;;	feedmail-queue-chatty		nil
  (autoload 'feedmail-send-it "feedmail")
  (autoload 'feedmail-run-the-queue "feedmail")
  (setenv "MAILHOST" lre-pop-domain)
  (load-library "smtpmail"))

(defun lre-vm-auto-folder ()
"Search for \"User Name  <mail@server>\" or \"mail@server (User Name)\"
in From-header, strip off useless prefix"
  (goto-char (point-min))
  (if (search-forward-regexp (concat "<"
				     "\\(x06\\|tt\\(x\\)?\\|xx\\)?"
				     "\\([^@\n]+\\)"
				     "\\(@[^>]+\\)?"
				     ">")
			     nil t)
      (buffer-substring (match-beginning 3) (match-end 3))
      (if (search-forward-regexp (concat "^ *"
					 "\\(x06\\|tt\\(x\\)?\\|xx\\)?"
					 "\\([a-zA-z0-9]+\\)"
					 "@")
				 nil t)
	  (buffer-substring (match-beginning 3) (match-end 3))
	  "")))

(defun lre-vm-expunge-and-quit ()
  "Expunge deleted messages, quit VM & Emacs."
  (interactive)
  (vm-expunge-folder)
  (vm-quit)
  (save-buffers-kill-emacs))

(defun lre-vm-setup ()
  (setq
   vm-auto-next-message                t
   vm-berkeley-mail-compatibility      t
   vm-confirm-new-folders              t
   vm-confirm-quit                     1
   vm-delete-after-saving              t
   vm-delete-empty-folders             t
   vm-edit-message-mode                'indented-text-mode
   vm-folder-directory                 "~/Mail"
   vm-group-by                         "subject"
   vm-highlighted-header-regexp        "From:\\|Subject:"
   vm-honor-page-delimiters            nil
   vm-included-text-attribution-format "At %d/%M/%y (%H), %f (%F) wrote:\n"
   vm-included-text-prefix             "> "
   vm-inhibit-startup-message          t
   vm-move-after-deleting	       t
   vm-mutable-frames                   nil
   vm-mutable-windows                  t
   vm-preview-lines                    4
   vm-primary-inbox                    "~/VMBOX"
   vm-print-command		       "skriv"
   vm-print-command-switches	       "-M"
   vm-reply-subject-prefix             "Re: "
   vm-spool-files                      (if (lre-memb 'win32)
					   (list
					    (list
					     "~/INBOX"
					     (concat lre-pop-domain
						     ":110:pass:"
						     lre-mail-user)
					     "~/INBOX.CRASH"))
					 '("~/mbox" "~/Mail/toVM"))
   vm-summary-show-threads	       t
   vm-url-browser		       (if (lre-memb 'win32)
					   'nt-url-browse-url
					 'vm-mouse-send-url-to-netscape)
   vm-warp-mouse-to-new-frame	       t
   vm-summary-format                   (concat "%n "         "%*"
					       "%a "         "%-9.9f "
					       "%2d.%-2M %H " "%4lL "
					       "%I\"%s\""    "\n")
   vm-auto-folder-alist                '(("Subject" ("^Ferdigmelding" .
						     "ferdig"))
					 ("From" ("." lre-vm-auto-folder)))
   ))

(defun lre-vm-font-lock ()
  "From zeek@primenet.com (John Reynolds) in gnu.emacs.vm.info..."
  (cond
   (window-system
    (setq mail-font-lock-keywords
	  (append (list (cons "^[	]*\\sw*[>|}].*"
			      (lre-font-lock-ref-face)))
		  '(
  ("^\\\*?To\\|^Apparently-To\\|^From\\|^\\\*?[FB]?[Cc][Cc]\\|^Reply-[Tt]o\\|^Sender\\|^Priority" . font-lock-keyword-face)
  ("^Subject\\|^In-Reply-To\\|^References\\|^Date\\|^Posted-Date\\|^M[Ii][Mm][Ee]-Version\\|^Content-Type\\|^Received\\|^Message-I[Dd]\\|^Status" . font-lock-keyword-face)
  ("^\\(X-[A-Za-z0-9-]+\\):.*" . font-lock-string-face)
  ("^\\\*?To:\\s *\\(.+\\)" 1 font-lock-comment-face)
  ("^Apparently-To:\\s *\\(.+\\)" 1 font-lock-comment-face)
  ("^Reply-[Tt]o:\\s *\\(.+\\)" 1 font-lock-comment-face)
  ("^Sender:\\s *\\(.+\\)" 1 font-lock-comment-face)
  ("^From:\\s *\\(.+\\)" 1 font-lock-comment-face)
  ("^\\\*?[FB]?[cC][cC]:\\s *\\(.+\\)" 1 font-lock-comment-face)
  ("^Posted-Date:\\s *\\(.+\\)" 1 font-lock-comment-face)
  ("^Subject:\\s *\\(.+\\)" 1 font-lock-comment-face)
  ("^Date:\\s *\\(.+\\)" 1 font-lock-comment-face)
  ("^References:\\s *\\(.+\\)" 1 font-lock-comment-face)
  ("^M[Ii][Mm][Ee]-Version:\\s *\\(.+\\)" 1 font-lock-comment-face)
  ("^Content-Type:\\s *\\(.+\\)" 1 font-lock-comment-face)
  ("^Received:\\s *\\(.+\\)" 1 font-lock-comment-face)
  ("^Message-I[Dd]:\\s *\\(.+\\)" 1 font-lock-comment-face)
  ("^In-Reply-To:\\s *\\(.+\\)" 1 font-lock-comment-face)
  ("^Status:\\s *\\(.+\\)" 1 font-lock-comment-face)
  ("^Priority:\\s *\\(.+\\)" 1 font-lock-comment-face)
  ("^\\(--text follows this line--\\)$" 1 font-lock-comment-face)
  vm-mode-hook '(lambda ()
		  (font-lock-mode)
		  (setq font-lock-keywords mail-font-lock-keywords)
		  )))))))


(defun lre-vm-mode ()
  (define-key vm-mode-map "Q" 'lre-vm-expunge-and-quit)
  (lre-set-local file-precious-flag t))

(defun lre-gnus-select ()
  (if (eq 'nnvirtual
	  (car (gnus-find-method-for-group gnus-newsgroup-name)))
      (progn
	(lre-set-local gnus-thread-sort-functions
		       '(gnus-thread-sort-by-subject
			 gnus-thread-sort-by-date
			 gnus-thread-sort-by-score)))))

(defvar lre-gnus-pri
  '(("alt.humor.best-of-usenet" . 2)    ("alt.religion.emacs" . 10)
    ("alt.unix.wizards" . 1)		("bit.listserv.info-gnu" . 10)
    ("bit.listserv.word-pc" . 2)	("bit.mailserv.word-pc" . 2)
    ("biz.oreilly.announce" . 20)	("biz.softquad.announce" . 20)
    ("comp.data.administration" . 1)    ("comp.databases.theory" . 1)
    ("comp.doc" . 1)			("comp.doc.management" . 1)
    ("comp.emacs" . 100)		("comp.emacs.xemacs" . 90)
    ("comp.lang.awk" . 250)		("comp.lang.lisp" . 1)
    ("comp.programming.literate" . 30)  ("comp.software.config-mgmt" . 200)
    ("comp.std.unix" . 20)		("comp.std.announce" . 1)
    ("comp.text" . 250)			("comp.text.sgml" . 220)
    ("comp.unix.programmer" . 1)	("comp.unix.questions" . 1)
    ("comp.unix.shell" . 200)		("fou.drift" . 2)
    ("fou.humor" . 1)			("fou.sladder" . 2)
    ("fou.unix" . 10)			("gnu.announce" . 1000)
    ("gnu.bash.bug" . 1)		("gnu.config" . 10)
    ("gnu.emacs.announce" . 1000)	("gnu.emacs.bug" . 70)
    ("gnu.emacs.gnews" . 90)		("gnu.emacs.gnus" . 92)
    ("gnu.emacs.help" . 110)
    ("gnu.emacs.sources" . 120)		("gnu.emacs.vm.bug" . 60)
    ("gnu.emacs.vm.info" . 90)		("gnu.g++" . 1)
    ("gnu.g++.announce" . 50)		("gnu.g++.bug" . 1)
    ("gnu.g++.lib.bug" . 1)		("gnu.gcc.announce" . 50)
    ("gnu.gcc.bug" . 1)			("gnu.gdb.bug" . 1)
    ("gnu.ghostscript.bug" . 1)		("gnu.groff.bug" . 10)
    ("gnu.utils.bug" . 2)		("ifi.drift" . 10)
    ("ifi.drift.info" . 10)		("ifi.emacs" . 70)
    ("ifi.tavle" . 10)			("ifi.tips" . 10)
    ("ifi.unix" . 10)			("ifi.www" . 1)
    ("microsoft.public.technet" . 1)
    ("microsoft.public.win32.programmer.tools" . 1)
    ("microsoft.public.vc.utilities" .1 )
    ("misc.books.technical" . 1)	("news.announce.important" . 10)
    ("no.alt.gullkorn" . 10)		("no.c" . 1)
    ("no.lisp" . 4)			("no.standard" . 1)
    ("no.svar" . 1)			("no.unix" . 5)
    ("no.www" . 1)			("uio.tekst" . 4)
    ("uio.unix" . 1)			("uio.unix-drift" . 1))
  "Newsgroup priorities.")

(defun lre-gnus-group-sort (info1 info2)
  (let* ((s1 (car info1))
	 (s2 (car info2))
	 (p1 (or (cdr-safe (assoc s1 lre-gnus-pri))
		 0))
	 (p2 (or (cdr-safe (assoc s2 lre-gnus-pri))
		 0)))
    (if (= p1 p2)
	(string< s1 s2)
	(> p1 p2))))

(defun lre-gnus-start ()
  (setq gnus-nntp-server              lre-nntp-domain
	gnus-inhibit-startup-message  t)
  (add-hook 'gnus-select-group-hook 'lre-gnus-select))

(defun lre-gnus-mode ()
  "Personal gnus setup."
  (setq
   ;; gnus-ignored-newsgroups    "^xyz"
   gnus-subscribe-hierarchical-interactive t
   gnus-author-copy              "~/Mail/usenet"
   gnus-button-url		 (if (lre-memb 'win32) 'nt-url-browse-url)
   gnus-default-article-saver    (function gnus-summary-save-in-file)
   gnus-inhibit-startup-message  t
   gnus-interactive-post         t
   gnus-large-newsgroup          150
   gnus-local-domain             lre-std-domain
   gnus-local-organization       lre-full-org
   gnus-novice-user              t
   gnus-show-threads             t
   gnus-suppress-duplicates	 t
   gnus-use-cross-references     t
   gnus-use-long-file-name       t
   gnus-use-generic-from         "mesan.no"
   gnus-user-from-line           (concat lre-mail-address " \("
					 lre-full-name	  "\)")
   gnus-group-faq-directory      "/ftp@ftp.sunet.se:/pub/usenet"
   gnus-summary-line-format      "\%U\%R\%z\%(\%[\%4L: \%-16,16n\%]\%) \%s\n"
   gnus-group-line-format	 "\%M\%S\%p\%5y: \%(\%G\%)\n"
   gnus-group-sort-function	 'lre-gnus-group-sort
   nntp-maximum-request          75
   gnus-required-headers
     '(From Date Newsgroups Subject Message-ID Path Organization Lines)
   gnus-ignored-headers
     (eval-and-compile
     (concat "^\\("
	     (mapconcat 'identity
			'("Path"      "Posting-Version" "Article-I.D."
			  "Expires"   "Date-Received"	"References"
			  "Control"   "Xref"		"Lines"
			  "Posted"    "Relay-Version"	"Message-ID"
			  "Nf-ID"     "Nf-From"		"Sender"
			  "Received"  "Mail-from")
			"\\|")
	     "\\):"))
   gnus-visible-headers
     (eval-and-compile
       (concat "^\\("
	       (mapconcat 'identity
			  '("From"	   "Newsgroups"   "Subject"  "To"
			    "Date"	   "Followup-To"  "Reply-To" "Cc"
			    "Organization" "Summary"	  "Keywords")
			  "\\|")
	       "\\):"))
   gnus-buffer-configuration
   '((group ([group 1.0 point] (if gnus-carpal [group-carpal 4])))
     (summary ([summary 1.0 point] (if gnus-carpal [summary-carpal 4])))
     (article ([summary 0.5 point] (if gnus-carpal [summary-carpal 4])
	       [article 1.0]))
     (server ([server 1.0 point] (if gnus-carpal [server-carpal 2])))
     (browse ([browse 1.0 point] (if gnus-carpal [browse-carpal 2])))
     (group-mail ([mail 1.0 point]))
     (summary-mail ([mail 1.0 point]))
     (summary-reply ([article 0.4] [mail 1.0 point]))
     (info ([nil 1.0 point]))
     (summary-faq ([summary 0.25] [faq 1.0 point]))
     (edit-group ([group 0.5] [edit-group 1.0 point]))
     (edit-server ([server 0.5] [edit-server 1.0 point]))
     (edit-score ([summary 0.25] [edit-score 1.0 point]))
     (post ([post 1.0 point]))
     (reply ([article 0.4] [mail 1.0 point]))
     (mail-forward ([mail 1.0 point]))
     (post-forward ([post 1.0 point]))
     (reply-yank ([mail 1.0 point]))
     (followup ([article 0.4] [post 1.0 point]))
     (followup-yank ([post 1.0 point])))
   )
  (if (lre-memb 'keys) (define-key gnus-summary-mode-map "\eo" 'lre-gnus-save))
  )

(defun lre-gnus-save (nr)
  "Save and next."
  (interactive "p")
  (let (news-file)
    (while (> nr 0)
      (message "%d" nr)
      (setq nr (- nr 1)
	    news-file (lre-unique-fnam "~/News" gnus-newsgroup-name))
      (gnus-summary-save-in-file news-file)
      (message "%d..." nr)
      (gnus-summary-next-article nil))))

(defun lre-vm ()
  "Start vm."
  (if (lre-memb 'inet)
      (call-interactively 'vm)))

(defun lre-gnus ()
  "Start gnus."
  (if (lre-memb 'inet)
      (call-interactively 'gnus)))


(provide 'lre-net)

;;; lre-net.el ends here
