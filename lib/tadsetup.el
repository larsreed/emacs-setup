(defconst tvist-setup-version "@(#) tadsetup.el 1.88 97/12/12"
  "Versjon av tadsetup.el")
(defconst tvist-xemacs-p
  (not (not (string-match "XEmacs\\|Lucid" (emacs-version))))
  "`nil' hvis vi ikke er i XEmacs, `t' hvis vi er")

;; Konfigurer hva som gjøres ------------------------------------------
(defvar tvist-inhibit-advanced t
  "* Sett til `nil' for å definere en del avanserte variable.")
(defvar tvist-inhibit-printer nil
  "* Sett til `t' for å ikke gjøre printersetup")
(defvar tvist-inhibit-banner nil
  "* Sett til `t' for å fjerne banner fra utskriftene.")
(defvar tvist-inhibit-vars nil
  "* Sett til `t' for å ikke definere en del standard variable.")
(defvar tvist-inhibit-keys nil
  "* Sett til `t' for å ikke omdefinere F1, backspace etc.")
(defvar tvist-inhibit-sysdul nil
  "* Sett til `t' for å ikke definere standard Sysduloppsett.")
(defvar tvist-inhibit-colors nil
  "* Sett til `t' for å ikke definere farger.")
(defvar tvist-inhibit-color-change nil
  "* Sett til `t' for å ikke omdefinere farger.")
(defvar tvist-inhibit-func-menu (not tvist-xemacs-p)
  "* Sett til `t' for å ikke inkludere func-menu.")
(defvar tvist-inhibit-load-path nil
  "* Sett til `t' for å ikke endre load-path.")
(defvar tvist-inhibit-modeline nil
  "* Sett til `t' for å ikke endre statuslinja+minibuffer.")
(defvar tvist-inhibit-lretpl nil
  "* Sett til `t' for å ikke klargjøre template-bibliotek.")
(defvar tvist-inhibit-sql nil
  "* Sett til `t' for å ikke klargjøre SQL-mode.")
(defvar tvist-inhibit-C nil
  "* Sett til `t' for å ikke klargjøre C-mode.")
(defvar tvist-inhibit-makefile nil
  "* Sett til `t' for å ikke klargjøre makefile-mode.")
(defvar tvist-inhibit-ksh nil
  "* Sett til `t' for å ikke klargjøre ksh-mode.")
(defvar tvist-inhibit-lisp nil
  "* Sett til `t' for å ikke klargjøre lisp-mode.")
(defvar tvist-inhibit-text nil
  "* Sett til `t' for å ikke klargjøre text-mode.")
(defvar tvist-inhibit-scroll (not tvist-xemacs-p)
  "* Sett til `t' for å ikke endre scrollemekanismene.")

;; Oppsett ------------------------------------------------------------
(defvar tvist-ps-printer "psutv3"
  "* PostScript-kø for bruk på TVIST-2000")
(defvar tvist-text-printer "laser3"
  "* Printerkø for bruk på TVIST-2000, tekstfiler")
(defvar tvist-key [f12]
  "* Tast som inneholder spesialbindinger")
(defvar tvist-std-indent 3
  "* Standard innrykk i Sysdul.")

(if tvist-inhibit-load-path ()
  (if (getenv "SPE_HOME")
      (setq load-path (append
		       (cons (substitute-in-file-name "$SPE_HOME/site-lisp")
			     load-path)
		       '(".")))))

(autoload 'sysdul-mode        "sysdul" "Edit file in sysdul mode" t)
(autoload 'tplsub             "tplsub" "Fill in templates"  t)
(autoload 'sysdul-comment-box "sysdul" "Create boxed comment" t)
(autoload 'sysdul-fix-white   "sysdul" "Remove unnecessary blanks" t)

(if tvist-inhibit-lretpl
    (setq sysdul-inhibit-templates t)
  (setq sysdul-inhibit-templates nil)
  (require 'lretpl))
(if tvist-inhibit-scroll ()
  (require 'scroll-in-place))

;; Tastaturoppsett ----------------------------------------------------
(defvar tvist-key-description
  '("? - Beskriv bindinger")
  "Beskrivelse av lokale tastebindinger.")

(defun tvist-key-description ()
  "Beskriv spesielle TVIST tastaturbindinger."
  (interactive)
  (setq tvist-key-description (sort tvist-key-description 'string<))
  (with-output-to-temp-buffer "*TVISTATUR*"
    (princ "Spesielle TVIST-2000 tastebindinger.")
    (terpri)
    (princ "\(Disse ligger under en egen tast, default F12\).")
    (terpri)
    (let ((str-list tvist-key-description))
      (while str-list
	(princ (car str-list))
	(terpri)
	(setq str-list (cdr str-list))))))

(defvar tvist-keymap (make-sparse-keymap)
  "Spesielle TVIST tastebindinger.
Gi kommando \"\\[tvist-key-description]\" for beskrivelse")

(if tvist-key
    (define-key global-map tvist-key tvist-keymap))
(define-key tvist-keymap "?" 'tvist-key-description)

(if tvist-inhibit-keys ()
  (define-key global-map [f1] 'help-command)
  (if tvist-xemacs-p
      (progn
	(load "delbackspace")
	(define-key global-map [(control next)] 'fkey-other-window)
	(define-key global-map [(control tab)] 'indent-relative)
	(define-key global-map [(control down)] 'forward-paragraph)
	(define-key global-map [(control up)] 'backward-paragraph)
	(define-key global-map [(control button3)] 'popup-buffer-menu))
    (progn
	(define-key global-map [C-next] 'other-window)
	(define-key global-map [C-tab]  'indent-relative)
	(define-key global-map [C-down] 'forward-paragraph)
	(define-key global-map [C-up]   'backward-paragraph)
;;	(define-key global-map [C-mouse-3] 'popup-buffer-menu)
	))
  (define-key ctl-x-map  "\C-k" 'tvist-kill-save-buf)
  (define-key ctl-x-map  "rd" 'delete-rectangle)
  (define-key tvist-keymap "r" 'query-replace-regexp)
  (define-key tvist-keymap "m" 'tplsub)
  (define-key tvist-keymap "\C-m" 'tvist-disp-macro)
  (define-key tvist-keymap "d" 'tvist-todays-date)
  (if tvist-inhibit-colors ()
    (define-key tvist-keymap "\C-l" 'font-lock-fontify-buffer)
    (setq tvist-key-description
	  (append tvist-key-description
		  (list "C-l - gjenoppfrisk fonter/farger"))))
  (define-key tvist-keymap "\C-c" 'sysdul-comment-box)
  (define-key tvist-keymap " " 'sysdul-fix-white)
  (setq tvist-key-description
	(append tvist-key-description
		(list "r - søk erstatt regexp"
		      "m - kjør malsubstituering"
		      "d - sett inn dato, samt signatur hvis prefiks"
		      "C-c - lag \"kommentarboks\""
		      "C-m - vis makrodefinisjon"
		      "SPACE - fjern overflødige blanke"))))

;; Farger ------------------------------------------------------------
(if tvist-inhibit-colors ()
  (setq-default font-lock-auto-fontify           t
		font-lock-use-colors             t
		font-lock-use-fonts              nil
		font-lock-use-maximal-decoration t)
  (require 'font-lock)
  (require 'paren)
  (if tvist-xemacs-p
      (progn
	(autoload 'turn-on-lazy-lock "lazy-lock"
	  "Unconditionally turn on Lazy Lock mode.")
	(add-hook 'font-lock-mode-hook 'turn-on-lazy-lock)
	(remove-hook 'font-lock-mode-hook 'turn-on-fast-lock))
    (progn
      (global-font-lock-mode t)
      (setq font-lock-maximum-decoration t
	    font-lock-support-mode 'lazy-lock-mode)))
  (if tvist-xemacs-p
      (paren-set-mode 'sexp))
  (cond (tvist-inhibit-color-change t)
	(tvist-xemacs-p
	 (set-face-foreground 'font-lock-comment-face "dodgerBlue")
	 (set-face-foreground 'font-lock-type-face "purple")
	 (set-face-foreground 'font-lock-doc-string-face "coral2")
	 (set-face-foreground 'font-lock-preprocessor-face "black")
	 (set-face-background 'font-lock-preprocessor-face "lightyellow"))))

;; Printeroppsett ----------------------------------------------------
(if tvist-inhibit-printer ()
  (require 'ps-print)
  (setq lpr-command       "lpr"
	lpr-switches      (list "-P" tvist-text-printer)
	ps-lpr-command    "lpr"
	ps-lpr-switches   (list "-P" tvist-ps-printer)
	ps-paper-type     'ps-a4
	ps-font-size      8
	ps-line-height    (* (/ ps-font-size 10.0) 11.29)
	ps-avg-char-width (* (/ ps-font-size 10.0) 5.6)
	ps-space-width    ps-avg-char-width
	ps-print-color-p  nil
	ps-bold-faces     '(font-lock-function-name-face
			    font-lock-keyword-face
			    font-lock-type-face
			    bold
			    bold-italic)
	ps-italic-faces   '(font-lock-doc-string-face
			    font-lock-string-face
			    font-lock-comment-face
			    italic
			    bold-italic))
  (if tvist-inhibit-banner
      (setq lpr-switches    (cons "-h" lpr-switches)
	    ps-lpr-switches (cons "-h" ps-lpr-switches)))
  (define-key tvist-keymap "\C-p" 'lpr-buffer)
  (define-key tvist-keymap "p" 'ps-print-buffer-with-faces)
  (define-key tvist-keymap "P" 'ps-print-region-with-faces)
  (define-key tvist-keymap "\M-p" 'ps-spool-buffer-with-faces)
  (setq tvist-key-description
	(append tvist-key-description
		(list
		 "C-p - print buffer 'plain'"
		 "p - pretty-print buffer (hvis greip-fil, kun del 1)"
		 "P - pretty-print merket område"
		 "M-p - pretty-print til temp.buffer (skrives ut med M-x ps-despool etterpå)"))))

;; Sysdul -------------------------------------------------------------
(defun tvist-is-greip-p ()
  ;; Bestem om buffer er greip-buffer
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp "^\\*V INCLUDE IF SRCTYPE" nil t)))

(defun tvist-print-greip (p)
  "Utskrift av 1 del av Sysdul kildekode i greipformat.
Default del 1 - numerisk prefiks avgjør hvilken."
  (interactive "p")
  (if (not (tvist-is-greip-p))
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
      (if tvist-inhibit-colors ()
	(font-lock-fontify-buffer))
      (set-buffer-modified-p nil)
      (let ((ps-left-header (list filepart dirpart)))
	(ps-print-buffer-with-faces))
      (set-buffer curr-buf)
      (kill-buffer "*greiprint*"))))

(defun tvist-kill-part-2 ()
  "Fjerner del 2 (SRCTYPE=GRAPE) fra cf-fil"
  (interactive)
  (goto-char (point-min))
  (if (not (search-forward "INCLUDE IF SRCTYPE = GRAPE")) ()
    (beginning-of-line 2)
    (let ((p (point)))
      (if (not (search-forward "INCLUDE IF SRCTYPE = PROC")) ()
	(beginning-of-line 0)
	(delete-region p (point))))))

(defun tvist-ready-file ()
  "Klargjør fil for innsjekking.
Kjløres fra vsd-fila - fjerner del 2, og henter inn cf-filene."
  (interactive)
  (goto-char (point-min))
  (let ((b-base (substring (buffer-file-name) 0 (- (length (buffer-file-name))
						   4))))
  (message "Fjerner seksjon 2 (GRAPE)")
  (tvist-kill-part-2)
  (message "Legger inn seksjon 3 (PROC)")
  (if (not (search-forward "INCLUDE IF SRCTYPE = PROC")) ()
    (beginning-of-line 2)
    (insert-file (concat b-base "_proc.cf")))
  (message "Legger inn seksjon 4 (DEF)")
  (if (not (search-forward "INCLUDE IF SRCTYPE = DEF")) ()
    (beginning-of-line 2)
    (insert-file (concat b-base "_def.cf")))
  (sysdul-fix-white)))

(if tvist-inhibit-sysdul ()
  ;; Use sysdul-mode for .sd, .lsd  and .vsd files
  ;; cc-mode brukes for .(v)cf (grape template)
  (setq auto-mode-alist
	(append '(("\\.[vl]?sd$" . sysdul-mode)
		  ("\\.svp$" . sysdul-mode)
		  ("\\.vpl$" . sysdul-mode))
		auto-mode-alist))

  (if tvist-inhibit-colors ()
    (require 'font-lock))

  (defvar tvist-sysdul-submap (make-sparse-keymap)
    "Tilleggsdefinisjoner i sysdul-mode")
  (define-key tvist-sysdul-submap "p" 'tvist-print-greip)
  (define-key tvist-sysdul-submap "C" 'cf-mode)

  (defconst tvist-first-sysdul t)

  (defun tvist-sysdul-hook-function ()
    "Funksjon som kalles ved oppstart av sysdul-mode"
    ;; Automatically breaks lines when they become too long
    (sysdul-auto-fill-mode 1)
    (if tvist-inhibit-keys ()
      (define-key sysdul-mode-map tvist-key tvist-sysdul-submap)
      (if tvist-first-sysdul
	  (setq tvist-key-description
		(append tvist-key-description
			(list "C - kun i sysdul-mode: bytt til cf-mode"))))
      (define-key sysdul-mode-map "\C-cg" 'cf-grapeify-region))
    (if (not tvist-xemacs-p) ()
      (add-menu-button '("Sysdul")
		       ["Grapeify region" cf-grapeify-region
			(mark)])
      (add-menu-button '("Sysdul")
		       ["Ungrapeify region" cf-ungrapeify-region
			(mark)])
      (add-menu-button '("Sysdul" "Grape Admin")
		       ["Prepare checkin" tvist-ready-file t])
      (add-menu-button '("Sysdul" "Grape Admin")
		       ["Remove part 2" tvist-kill-part-2 t])
      )
    (setq tvist-first-sysdul nil)
    (if tvist-inhibit-colors ()
      (turn-on-font-lock)))

  (add-hook 'sysdul-mode-hook 'tvist-sysdul-hook-function)

  ;; The following variables can be given other values if wanted. They
  ;; are shown with their default values.
  (setq sysdul-std-indent tvist-std-indent
	sysdul-fill-column 79
	sysdul-continuation-indent (* 2 tvist-std-indent))
)

;; Show a function menu with all sysdul procedures
(cond (tvist-inhibit-func-menu
       nil)
      ((string-match "XEmacs" emacs-version)
       (require 'func-menu)
       (define-key tvist-keymap "f" 'function-menu)
       (define-key tvist-keymap "g" 'fume-prompt-function-goto)
       (setq tvist-key-description
	     (append tvist-key-description
		     (list "f - funksjonsmeny for gjeldende buffer"
			   "g - gå til funksjon")))
       (add-hook 'find-file-hooks 'fume-add-menubar-entry)
       (define-key global-map '(shift button3) 'mouse-function-menu)
       ))

;; SQL ----------------------------------------------------------------
(if tvist-inhibit-sql ()

  (autoload 'sql "sql-mode"
    "Start the interactive SQL interpretor in a new buffer." t)
  (autoload 'sql-mode "sql-mode"
    "Mode for editing SQL files and running a SQL interpretor." t)
  (autoload 'sql-get-going "sql-mode"
    "Start SQL mode++" t)
  (define-key tvist-keymap "q" 'sql-get-going)
  (setq tvist-key-description
	(append tvist-key-description
		(list
		 "q - start SQL-vinduer (prefiks for kun resultatvindu)")))

  (defvar sql-mode-syntax-table (copy-syntax-table text-mode-syntax-table)
    "Syntax table for SQL-mode")
  (modify-syntax-entry ?_ "w" sql-mode-syntax-table)
  (modify-syntax-entry ?/ ". 14" sql-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" sql-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" sql-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" sql-mode-syntax-table)
  (modify-syntax-entry ?@ "\'" sql-mode-syntax-table)

  (defconst sql-mode-font-lock-keywords
    '(
      ("\\b\\(begin\\|commit\\|rollback\\|save\\|prepare\\)[ \t]+transaction"
       . font-lock-preprocessor-face)
      ("^[ \t]*go[ \t]*$" . bold-italic)
      ("\\b\\(select\\([ \t]+\\(all\\|distinct\\)\\)?\\|update\\([ \t]+statistics\\)?\\|delete\\([ \t]+from\\)?\\|grant\\|insert\\([ \t]+into\\)?\\)\\b"
       . font-lock-function-name-face)
      ("\\balter[ \t]+\\(database\\|table\\)" . font-lock-function-name-face)
      ("\\bcreate[ \t]+\\(unique[ \t]+\\)?\\(\\(non\\)?clustered[ \t]+\\)?index"
       . font-lock-function-name-face)
      ("\\bcreate[ \t]+\\(database\\|table\\|default\\|proc\\(edure\\)?\\|rule\\|trigger\\|view\\)"
       . font-lock-function-name-face)
      ("\\btruncate[ \t]+table\\b" . font-lock-function-name-face)
      ("\\bdrop[ \t]+\\(database\\|table\\|default\\|index\\|proc\\(edure\\)?\\|rule\\|trigger\\|view\\)"
       . font-lock-function-name-face)
      ("^[ \t]*\\(\\(disk\\|dump\\|execute\\|load\\|set\\|use\\)[ \t]+\\w+\\)"
       1 font-lock-function-name-face)
      ("^[ \t]*\\(if\\|while\\|else\\|begin\\|end\\|break\\|continue\\|return\\|goto\\|raiserror\\|disk\\|dump\\|execute\\|print\\|shutdown\\|waitfor\\|writetext\\|readtext\\|checkpoint\\)\\b"
       1 font-lock-function-name-face)
      ("\\b\\(between\\|and\\|group[ \t]+by\\([ \t]+all\\)\\|having\\|convert\\|max\\|min\\|count\\|avg\\|sum\\|where\\)\\b"
       . 1)
      "\\b\\(is[ \t]+null\\|order[ \t]+by\\|holdlock\\|for[ \t]+browse\\|union\\|\\(primary\\|foreign\\)[ \t]+key\\|add[ \t]+constraint\\|\\(not[ \t]+\\)?\\(like\\|exists\\)\\)\\b"
      ("\\b\\(not[ \t]+\\)?null\\b" . font-lock-doc-string-face)
      ("\\bsp_\\w+" . font-lock-function-name-face)
      "\\b\\(varchar\\|char\\|datetime\\|smallint\\|tinyint\\|numeric\\|timestamp\\|bit\\|int\\|money\\)\\b"
      "@@?\\w+"))

  (defun tvist-sql-fun ()
    (set-syntax-table sql-mode-syntax-table)
    (if tvist-inhibit-colors ()
      (setq font-lock-keywords sql-mode-font-lock-keywords)
      (put 'sql-mode 'font-lock-keywords-case-fold-search t)))

  (add-hook 'sql-mode-hook 'tvist-sql-fun)

  (setq auto-mode-alist
	(append '(("\\.sql$" . sql-mode)
		  ("\\.vsq$" . sql-mode)
		  ("\\.hql$" . sql-mode)
		  ("\\.dba$" . sql-mode)
		  ("\\.utl$" . sql-mode)
		  ("\\.inx$" . sql-mode))
		auto-mode-alist))
)

;; Statuslinje -------------------------------------------------------

(if tvist-inhibit-modeline ()
  (if tvist-xemacs-p
      (autoload 'resize-minibuffer-mode "rsz-minibuf" nil t)
    (autoload 'resize-minibuffer-mode "rsz-mini" nil t))
  (resize-minibuffer-mode)
  (if tvist-inhibit-func-menu ()
    (setq fume-display-in-modeline-p  nil))
  (if tvist-xemacs-p (display-column-mode 1))
  (setq-default modeline-buffer-identification '("%b"))
  (if tvist-xemacs-p
      (setq-default default-modeline-format
		    '(""
		      modeline-modified
		      modeline-buffer-identification
		      "  "
		      global-mode-string
		      "  %[("
		      mode-name
		      minor-mode-alist
		      "%n"
		      modeline-process
		      ")%]---"
		      (display-column-mode (""  current-line
					    "/" current-column "--"))
		      (line-number-mode "L%l--")
		      (-3 . "%p")
		      "-%-"))
    (setq-default mode-line-format
		  '(""
		    mode-line-modified
		    mode-line-buffer-identification
		    " "
		    global-mode-string
		    " %[("
		    "%t:"
		    mode-name
		    mode-line-process
		    minor-mode-alist
		    "%n"
		    ")%]-"
		    (line-number-mode "L%l/")
		    (column-number-mode "C%c=")
		    (-3 . "%p")
		    "-%-"))))

;; Diverse -----------------------------------------------------------
(if tvist-inhibit-lretpl ()
  (define-key tvist-keymap "a" 'lretpl-describe-templates)
  (setq tvist-key-description
	(append tvist-key-description
		(list "a - list \"abbrevs\" for gjeldende modus"))))




(defun tvist-disp-macro (sym)
  "Denne funksjonen har alfastatus... Den prøver å vise en makrodefinisjon...
Dvs.:
- Finn symbolet cursor står ved.
- Let etter en definisjon (*V MACRO / @LOCAL@ / #define) av denne i
  (a) Sist lagrete utgave av kildefila
  (b) Alle svappfiler under $PROJ_HOME/src/svapp
  (c) Alle cf-filer under $SPE_HOME/maler
- For hver funnet forekomst, vis *avsnittet* definisjonen finnes i.
  For makrofiler uten blanke linjer, kan dette bli ganske mye..."
  (interactive (list (symbol-near-point)))
  (if (not (and sym (length sym)))
      (error "Finner intet symbol!")
    (let* ((nsym (cond ((string-match "^X_" sym) (substring sym 2))
		       ((string-match "^X"  sym) (substring sym 1))
		       (t sym)))
	   (p-home (getenv "PROJ_HOME"))
	   (s-home (getenv "SPE_HOME"))
	   (svpdir (concat p-home "/src/svapp/"))
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
			  svpdir "[a-z]*/*.svp")))
      (with-output-to-temp-buffer "*Makrodefinisjon*"
	(progn
	  (set-buffer (get-buffer "*Makrodefinisjon*"))
	  (insert "MAKRO: " sym "\n\n")
	  (shell-command s-cmd t))))))

(defconst tvist-first-cf t)

(defun tvist-c-mode ()
  "Lokale tillegg til c-mode"
  (if tvist-inhibit-keys ()
    (if tvist-first-cf
	(setq tvist-key-description
	      (append tvist-key-description
		      (list
		       "C - kun i C-mode: bytt til sysdul-mode"))))
    (setq tvist-first-cf nil)
    (define-key c-mode-map  (if tvist-xemacs-p
				[(control return)]
			      [C-return]) 'newline-and-indent))
  (setq c-basic-offset    tvist-std-indent
	c-recognize-knr-p t)
  (setq c-offsets-alist
	'((string                . -1000)
	  (c                     . c-lineup-C-comments)
	  (defun-open            . 0)
	  (defun-close           . 0)
	  (defun-block-intro     . +)
	  (class-open            . 0)
	  (class-close           . 0)
	  (inline-open           . +)
	  (inline-close          . 0)
	  (ansi-funcdecl-cont    . -)
	  (knr-argdecl-intro     . +)
	  (knr-argdecl           . 0)
	  (topmost-intro         . 0)
	  (topmost-intro-cont    . 0)
	  (member-init-intro     . +)
	  (member-init-cont      . 0)
	  (inher-intro           . +)
	  (inher-cont            . c-lineup-multi-inher)
	  (block-open            . 0)
	  (block-close           . 0)
	  (brace-list-open       . 0)
	  (brace-list-close      . 0)
	  (brace-list-intro      . +)
	  (brace-list-entry      . 0)
	  (statement             . 0)
	  (statement-cont        . +)
	  ;; some people might prefer
	  ;;(statement-cont        . c-lineup-math)
	  (statement-block-intro . +)
	  (statement-case-intro  . +)
	  (statement-case-open   . 0)
	  (substatement          . +)
	  (substatement-open     . +)
	  (case-label            . +)
	  (access-label          . -)
	  (label                 . +)
	  (do-while-closure      . 0)
	  (else-clause           . 0)
	  (comment-intro         . c-lineup-comment)
	  (arglist-intro         . +)
	  (arglist-cont          . 0)
	  (arglist-cont-nonempty . c-lineup-arglist)
	  (arglist-close         . +)
	  (stream-op             . c-lineup-streamop)
	  (inclass               . +)
	  (cpp-macro             . -1000)
	  (friend                . 0)
	  (objc-method-intro     . -1000)
	  (objc-method-args-cont . c-lineup-ObjC-method-args)
	  (objc-method-call-cont . c-lineup-ObjC-method-call)
	  ))
  (if tvist-inhibit-colors ()
    (font-lock-mode 1)))

(if tvist-inhibit-C ()
  (setq auto-mode-alist
	(append '(("\\.cpp$" .  c++-mode)
		  ("\\.v?cf$" . cf-mode))
		auto-mode-alist))
  (add-hook 'c-mode-hook 'tvist-c-mode)
  (add-hook 'c++-mode-hook 'tvist-c-mode))

(require 'cf-mode)

(defun tvist-ksh-mode ()
  (if tvist-inhibit-lretpl ()
    (lretpl-mode 1))
  (if tvist-inhibit-colors ()
    (font-lock-mode 1)))


(if tvist-inhibit-ksh ()
  (if tvist-inhibit-lretpl ()
    (lretpl-register-mode 'ksh-mode
			  lretpl-ksh-list
			  nil       ; help
			  'default  ; regexp
			  'default  ; case
			  'default  ; cont-string
			  'default  ; help-key
			  ))
  (add-hook 'ksh-mode-hook 'tvist-ksh-mode))

(defun tvist-newline-and-indent ()
  "Create new line, insert TAB"
  (interactive)
  (newline)
  (insert "\t"))

(defun tvist-makefile-mode ()
  (if tvist-inhibit-keys ()
    (define-key makefile-mode-map [kp_enter] 'tvist-newline-and-indent))
  (if tvist-inhibit-lretpl ()
    (lretpl-mode 1))
  (if tvist-inhibit-colors ()
    (font-lock-mode 1)))


(if tvist-inhibit-makefile ()
  (if tvist-inhibit-lretpl ()
    (lretpl-register-mode 'makefile-mode
			  lretpl-makefile-list
			  nil       ; help
			  'default  ; regexp
			  'default  ; case
			  'default  ; cont-string
			  'default  ; help-key
			  ))
  (add-hook 'makefile-mode-hook 'tvist-makefile-mode))


(defun tvist-cf-mode ()
  (if tvist-inhibit-lretpl ()
    (lretpl-mode 1))
  (if tvist-inhibit-colors ()
    (font-lock-mode 1)))

(add-hook 'cf-mode-hook 'tvist-cf-mode)
(add-hook 'cf-mode-hooks 'tvist-cf-mode)

(defun tvist-lisp-mode ()
  "Lokale tillegg til emacs-lisp-mode"
  (if tvist-inhibit-keys ()
    (define-key emacs-lisp-mode-map (if tvist-xemacs-p
					[(control return)]
				      [C-return]) 'newline-and-indent)
    (define-key emacs-lisp-mode-map "\C-cc" 'byte-compile-file)
    (define-key emacs-lisp-mode-map "\C-ce" 'eval-buffer)
    (define-key emacs-lisp-mode-map "\C-cr" 'byte-recompile-directory)))

(if tvist-inhibit-lisp ()
  (add-hook 'emacs-lisp-mode-hook 'tvist-lisp-mode))

(defun tvist-special-buf-p (name)
  "Buffers to be treated specially..."
  (save-match-data
    (string-match "^ *\\*" name)))

(defun tvist-kill-save-buf (&optional kill-win)
  "Get rid of buffer/window...
1. Save buffer (save-buffer==\\[save-buffer])
2. If client buffer - exit client (server-edit==\\[server-edit])
   else kill buffer (kill-buffer==\\[kill-buffer])
3. If multiple prefix given - remove frame (delete-frame==\\[delete-frame])
   or if other prefix given - remove window (delete-window==\\[delete-window])
   Removing the window will also result in the frame being removed if the
   window is the last window in the frame, and there are other frames.
   Removing the last frame or window is not allowed."
   (interactive "P")
   (let* ((special (tvist-special-buf-p (buffer-name)))
	  (double-pfx (and kill-win
			   (listp kill-win)
			   (integerp (car kill-win))
			   (> (car kill-win) 4)))
	  (single-pfx (and kill-win
			   (not double-pfx))))
     (if (or (and special
		  (buffer-modified-p)
		  (y-or-n-p (concat "Save buffer " (buffer-name) " ")))
	     (not special))
	 (save-buffer))
     (if (and (boundp 'server-buffer-clients)
	      server-buffer-clients)
	 (server-edit)
       (kill-buffer nil))
     (cond (double-pfx (delete-frame))
	   (single-pfx (delete-window)))))

(defun tvist-load-stereo (file1 file2 &optional dir)
  "Åpner filene file1 og file2, cder evt. til DIR først."
  (if dir (cd dir))
  (server-find-file file1)
  (sysdul-next-error nil)
  (find-file-other-window file2))

(defvar tvist-today-day nil "Today: DD")
(defvar tvist-today-month nil "Today: MM")
(defvar tvist-today-year nil "Today: YY")
(defvar tvist-today-trans '(("Jan" . "01") ("Feb" . "02") ("Mar" . "03")
			  ("Apr" . "04") ("May" . "05") ("Jun" . "06")
			  ("Jul" . "07") ("Aug" . "08") ("Sep" . "09")
			  ("Oct" . "10") ("Nov" . "11") ("Dec" . "12"))
  "Months")

(defvar tvist-user-sign nil
  "*Fast signatur som skal brukes (utledes ikke)")

(defun tvist-user-sign ()
  "Returner brukernavn uten 9505"
  (or tvist-user-sign
      (save-match-data
	(upcase
	 (let ((user (getenv "USER")))
	   (cond ((null user)
		  "unknown")
		 ((string-match "9505$" user)
		  (substring user 0 (- (length user) 4)))
		 (t
		  user)))))))

(defun tvist-todays-date(fmt &optional sign)
  "Sett inn dagens dato, samt evt. signatur \(hvis prefiks\)"
  (interactive "*cDd.mm.yy / Yymmdd? (Y)\nP")
  (setq tvist-today-day   (format "%02d"
				  (string-to-int
				   (substring (current-time-string) 8 10)))
	tvist-today-month (cdr (assoc (substring (current-time-string) 4 7)
				      tvist-today-trans))
	tvist-today-year  (format "%02d"
				  (string-to-int
				   (substring (current-time-string) 22 24))))
  (if (string= (upcase (char-to-string fmt)) "D")
      (insert tvist-today-day "." tvist-today-month "." tvist-today-year)
    (insert tvist-today-year tvist-today-month tvist-today-day))
  (if sign
      (insert "  " (tvist-user-sign))))

;; Standard variable -------------------------------------------------
(if tvist-inhibit-vars ()
  (setq     next-line-add-newlines          nil
	    backup-by-copying-when-mismatch t
	    backup-by-copying-when-linked   t
	    ksh-indent			    tvist-std-indent
	    default-major-mode		    'indented-text-mode
	    require-final-newline	    'ask)
  (if tvist-xemacs-p
      (setq find-file-use-truenames	    nil
	    find-file-compare-truenames	    t
	    teach-extended-commands-p	    t)
    (setq   find-file-existing-other-name   t
	    frame-title-format	            '("%b")
	    highlight-nonselected-windows   nil)))

(if tvist-inhibit-text ()
  (setq auto-mode-alist
	(append '(("\\.txt$" .  indented-text-mode))
		auto-mode-alist))
  (add-hook 'text-mode-hook 'turn-on-auto-fill))

(if tvist-inhibit-advanced ()
  (setq complex-buffers-menu-p t
	kill-whole-line        t))
