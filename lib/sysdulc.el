;;; sysdul.el --- edit Sysdul source code

;;; Copyright (C) 1995 Lars Reed

;; Author:		Lars Reed <lre@sysdeco.no>
;; Last-Modified:	96/05/14
;; Version:		1.32
;; Keywords:		languages programming Sysdul
;; Adapted-By:

;;; Commentary:

;; ....

;;; Code:

; ======================================================================

;;; Variables

;; Personal preferences
(defvar sysdul-indent-level 3
  "*Spaces to indent for 1 syntactic Sysdul level")
(defvar sysdul-abbrev-key "$"
  "*Key to expand sysdul abbrevs.")

;; Per project:
(defvar sysdul-msg-function 'sysdul-write-mess
  "*Function to report DB errors.")
(defvar sysdul-ioerr-function 'sysdul-write-mess
  "*Function to report IO errors.")
(defvar sysdul-msg-lines 4
  "*No. of lines written by sysdul-msg-function.")
(defvar sysdul-no-colors nil
  "*Set to t to avoid highlighting")
(defvar sysdul-local-keys nil
  "*Local key setup.
Called with 1 arg - keymap.")
(defvar sysdul-local-abbs nil
  "*Local abbreviations.")
(defvar sysdul-local-lib nil
  "*Local library to load")
(defvar sysdul-abbrev-lib nil
  "*Abbrev library to load")


;; Internal...
(defvar sysdul-mode-syntax-table nil
  "Syntax table used while in sysdul mode.")
(defvar sysdul-mode-abbrev-table nil
  "Abbrev table used while in sysdul mode.")
(define-abbrev-table 'sysdul-mode-abbrev-table ())
(defvar sysdul-mode-map nil
  "Keymap for Sysdul mode.")
(defvar sysdul-abbrev-alist ()
  "Abbreviations for sysdul-electric-dollar.")

; ======================================================================

(if sysdul-local-lib
    (load-library sysdul-local-lib))
(if (not (null sysdul-abbrev-lib))
    (load-library sysdul-abbrev-lib))

; ======================================================================

;;; Prompt history

(defvar sub-list-hist nil
  "History of procedure/file names in Sysdul/SCCS mode.")
(defvar sysdul-last-sub "MTEST"
  "Last procedure name accessed in sysdul-mode.")

; ======================================================================

(if sysdul-mode-syntax-table
    ()
  (setq sysdul-mode-syntax-table (make-syntax-table)))

(if sysdul-mode-map
    ()
  (setq sysdul-mode-map (make-keymap))
  (if sysdul-local-keys
      (funcall sysdul-local-keys sysdul-mode-map))
  (define-key sysdul-mode-map [C-tab] 'sysdul-indent-func)
  (define-key sysdul-mode-map "\C-c\C-i" 'sysdul-indent-region)
  (define-key sysdul-mode-map [C-return] 'newline-and-indent)
  (define-key sysdul-mode-map [C-M-tab] 'indent-code-rigidly)
  (define-key esc-map [C-tab] 'indent-code-rigidly)
  (define-key sysdul-mode-map [C-backspace] 'sysdul-unindent)
  (define-key sysdul-mode-map "\t" 'indent-relative)
  (define-key sysdul-mode-map sysdul-abbrev-key  'sysdul-electric-dollar)
  (define-key sysdul-mode-map "\C-c?" 'sysdul-help)
  (define-key sysdul-mode-map "\C-c=" 'sysdul-equal-to)
  (define-key sysdul-mode-map "\C-c\C-n" 'sysdul-next-err)
  (define-key sysdul-mode-map "\C-c\C-p" 'sysdul-prev-err)
  (define-key sysdul-mode-map "\C-c\C-c" 'sysdul-comment-box)
  (define-key sysdul-mode-map "\C-cfx" 'sysdul-fix)
  (define-key sysdul-mode-map "\C-c\C-ls" 'sysdul-long-sentence)
  (define-key sysdul-mode-map "\C-cc" 'sysdul-comment-region)
  (define-key sysdul-mode-map "\C-cu" 'sysdul-uncomment-region)
  (define-key sysdul-mode-map [menu-bar] (make-sparse-keymap))
  (define-key sysdul-mode-map [menu-bar sysdul]
    (cons "Sysdul" (make-sparse-keymap "Sysdul")))
  (define-key sysdul-mode-map [menu-bar sysdul help]
    '("Help" . sysdul-help))
  (define-key sysdul-mode-map [menu-bar sysdul fix]
    '("Fixup" . sysdul-fix))
  (define-key sysdul-mode-map [menu-bar sysdul lambda-2]
    '("--"))
  (define-key sysdul-mode-map [menu-bar sysdul next-err]
    '("Next error" . sysdul-next-err))
  (define-key sysdul-mode-map [menu-bar sysdul prev-err]
    '("Prev error" . sysdul-prev-err))
  (define-key sysdul-mode-map [menu-bar sysdul lambda-1]
    '("--"))
  (define-key sysdul-mode-map [menu-bar sysdul comment-box]
    '("Comment box" . sysdul-comment-box))
  (define-key sysdul-mode-map [menu-bar sysdul comm-reg]
    '("Comment region" . sysdul-comment-region))
  (define-key sysdul-mode-map [menu-bar sysdul uncomm-reg]
    '("Uncomment region" . sysdul-uncomment-region))
  )

; ======================================================================

(defun sysdul-hei()
  "Nice hello..."
  (interactive)
  (message "Hello, %s!   :-) lre" (user-full-name)))

(defun sysdul-describe-abbrevs ()
  "Overview of sysdul mode abbrev table."
  (interactive)
  (save-window-excursion
    (switch-to-buffer (get-buffer-create "*Help*"))
    (let ((was-r-o buffer-read-only))
      (goto-char (point-max))
      (toggle-read-only 0)
      (insert "\n\n\tAbbrev\tFunction\n\t=======\t=========================\n")
      (mapcar '(lambda (abb-alist)
		 (insert "\t" (car abb-alist) "\t"
			 (symbol-name (cdr abb-alist)) "\n"))
	      sysdul-abbrev-alist)
      (if was-r-o (toggle-read-only 1)))))

(defun sysdul-help ()
"Type M-C-v to scroll HELP window, C-x 1 to close.
========================
* SYSDUL-MODE OVERVIEW *
========================
@(#) sysdul.el 1.32 96/05/14>

\\{sysdul-mode-map}

*** sysdul-comment-box
- this may be used outside Sysdul (e.g. shell programs).
Use prefix argument to select other `box' character than `-',
and optionally delete preceding and following line.
The first non-blank character sequence on the line is always
as comment leader.

\\{spe-sccs-mode-map}

key             binding
---             -------
\\[ccas-history-insert]	ccas-history-insert
\\[ccas-history-point]	ccas-history-point
\\[ccas-wversion]	ccas-wversion

Abbreviations are expanded by pressing  \\[sysdul-electric-dollar]:"
  (interactive)
  (save-excursion
    (describe-function 'sysdul-help)
    (sysdul-describe-abbrevs)))

; ======================================================================

(eval-when-compile
  (load "make-regexp"))

(defvar sysdul-hilit-is-set nil)
(defconst sysdul-hilit-keywords
   (let ((regexp
	  (eval-when-compile
	    (make-regexp '(
			   "ACTIVATE"	   "CALL"
			   "CLOSE"	   "DELETE"
			   "DISABLE"	   "DISPLAY"
			   "ELSE"	   "ENABLE"
			   "GET"	   "IDENTIFY"
			   "IF"		   "NOTE +THAT"
			   "OBTAIN"	   "OR +IF"
			   "PASSIVATE"	   "READ"
			   "REMEMBER"	   "REMOVE"
			   "RETURN"	   "SET"
			   "START"	   "STORE"
			   "WHILE"	   "WRITE"
			   ) t))))
     (concat "^ *\\<\\("
	     "\\(FOR +\\(ALL\\|THOSE\\)\\)\\|"
	     "\\(FIND\\( +\\(FIRST\\|LAST\\)\\)?\\)\\|"
	     "\\(END\\( +\\(IF\\|WHILE\\|READ\\|FOR\\|LOOP\\)\\)?\\)\\|\\("
	     regexp
	     "\\)\\>\\)")))

(cond ((and (not sysdul-no-colors)
	   window-system)
      (hilit-set-mode-patterns
       'sysdul-mode
       '(
	 ("\\<\\(\\(Sysdeco\\( *A/?S\\)?\\)\\|[L][R][E]\\|\\(\\(Lars +\\)?Reed\\)\\)\\>" nil rule)
	 ("\\(^[C;]\\( .+\\)?$\\)\\|\\(;[^'\n]+$\\)" nil comment)
	 ("^ *\\*\\*\\*[^*].+\\*\\*\\*" nil error)
	 ("\\(^\*[Vv] .*$\\)\\|\\(@FI@\\)" nil include)
	 ("^\\( *\\(\\(DE\\(CLAR\\|FIN\\)E\\)\\|\\(UNIVERSAL\\)\\)\\>\\)\\|\\(@LOCAL@\\)" nil decl)
	 ("@[^@\n]+@" nil define)

	 ("^ *\\(\\(\\(MAIN\\)?\\(CONTROL-\\)?PROCEDURE\\)\\|PROGRAM\\|CONTROL-HANDLING\\) +[^ ]+" nil defun)
	 ("^ *\\END +\\(\\(\\(CONTROL-\\)?PROCEDURE\\)\\|PROGRAM\\|CONTROL-HANDLING\\)\\>" nil defun)

	 ("^ *\\(TERMINATE\\|RESTART\\)\\(-LABEL\\| +FROM\\)" nil crossref)
	 ("^ *\\(CONTINUE\\|TERMINATE\\) +LOOP\\>" nil crossref)
	 ("^ *EXIT +[0-9]+ +LEVEL\\(S\\)?\\>" nil crossref)
	 ("^ *DEFAULT +ERROR-HANDLING\\>" nil crossref)

	 ("['][^\'\n]*[']" nil string)
	 )
       nil 'case-insensitive)
      (if sysdul-hilit-is-set ()
	(hilit-add-pattern sysdul-hilit-keywords "" 'keyword 'sysdul-mode)
	(setq sysdul-hilit-is-set t))))

; ======================================================================


; Function to (un)indent N syntactic levels
(defun sysdul-indent-func (&optional count)
  "Indent one (count) level for Sysdul, using sysdul-indent-level."
  (interactive "p")
  (let ((ctrl sysdul-indent-level))
    (if (> count 0)
        (progn
          (if (> count 1) (setq ctrl (* ctrl count)))
          (insert-char ?\ ctrl t))
      (if (> (abs count) 1) (setq ctrl (* ctrl (abs count))))
      (backward-delete-char-untabify ctrl))))

; Function to create unindent on current line
(defun sysdul-unindent (&optional count)
  "Unindent current line."
  (interactive "p")
  (let ((ctrl 1)
	cols)
    (if count (setq ctrl count))
    (while (> ctrl 0)
      (setq cols (% (current-column) sysdul-indent-level))
      (backward-delete-char-untabify (if (= cols 0) sysdul-indent-level
				       cols))
      (setq ctrl (- ctrl 1)))))

; Function to create new (un)indented line
(defun sysdul-indent-newline (&optional count indcount)
  "Newline ; relative indent ; (un)indent."
  (interactive "p\n")
  (let ((ctrl 1))
    (insert "\n")
    (if indcount (setq ctrl indcount))
    (while (> ctrl 0)
      (indent-relative)
      (setq ctrl (- ctrl 1)))
    (if (/= count 0) (sysdul-indent-func count))))

; Function to indent/unindent
(defun sub-sysdul-in-out()
  (sysdul-indent-newline 1)
  (sysdul-indent-newline -1))

(defun sysdul-what-line ()
  (save-excursion
    (beginning-of-line)
    (1+ (count-lines 1 (point)))))

(defun sysdul-indent-region (pt mrk count)
  "Indent region, don't touch lines not starting with a blank."
  (interactive "d\nm\np")
  (let (min-line
	max-line
	tmp-line
	idx)
    ;; First line
    (goto-char pt)
    (setq min-line (sysdul-what-line))
    ;; Last line
    (goto-char mrk)
    (setq max-line (sysdul-what-line))
    ;; Swap if necessary
    (if (> max-line min-line) ()
	(setq tmp-line min-line
	      min-line max-line
	      max-line tmp-line))
    ;; Prepare
    (untabify pt mrk)
    (goto-line min-line)
;    (beginning-of-line)
    ;; Loop over all lines
    (while (< (sysdul-what-line) max-line)
      (cond
       ((> count 0)
	(if (eolp) ()
	  (if (looking-at " *$") ()
	    (if (looking-at " ") (sysdul-indent-func count)
	      (back-to-indentation)
	      (if (bolp)
		  (progn
		    (while (not (looking-at " ")) (forward-char 1))
		    (while (looking-at " ") (forward-char 1))))
	      (if (eolp) ()
		(sysdul-indent-func count))))))
       ((< count 0)
	(let ((ok nil)
	      (idx (* (abs count) sysdul-indent-level)))
	  (if (looking-at " ")
	      (setq ok t)
	    (back-to-indentation)
	    (if (not (eolp)) (forward-char 1))
	    (if (not (eolp)) (setq ok t)))
	  (if ok
	      (while (and (> idx 0)
			  (not (eolp))
			  (looking-at " "))
		(delete-char 1)
		(setq idx (1- idx)))))))
      (beginning-of-line 2))))

(defun sub-sysdul-get (prompt lvar hlist)
  "General sysdul-mode prompt function."
  (let (ans)
    (setq ans
	  (read-from-minibuffer (concat prompt " (M-p/n for hist.): ")
				lvar nil nil hlist))
    (if (= (length ans) 0) (if lvar (setq ans lvar)))
    (setq ans ans)))

; Function to set sysdul-last-sub
(defun sysdul-get-sub ()
  "Function to prompt for and set sysdul-last-sub."
  (setq sysdul-last-sub
	(sub-sysdul-get "Procedure" sysdul-last-sub 'sub-list-hist)))

; ----------------------------------------------------------------------


(defun sysdul-equal-to ()
  "Sysdul EQUAL TO."
  (interactive)
  (insert "EQUAL TO "))

(defun sysdul-comment-box (&optional prompt)
  "Frame for Sysdul comment."
  (interactive "P")
  (let ((ts 0)      ; text-start
	(te 0)	    ; text-end
	(ps 0)      ; prefix-start
	(pe 0)      ; prefix-end
	iter        ; counter
	pfx         ; prefix
	(dele nil)  ; delete preceding and succeeding line
	(cchr "-")) ; comment-character
    (if prompt (progn
		 (setq cchr (read-string "Box-character: " "-"))
		 (setq dele (y-or-n-p "Delete lines above&below "))))
    (if dele
	(progn
	  (save-excursion
	    (beginning-of-line 0)
	    (kill-line 1))
	  (save-excursion
	    (beginning-of-line 2)
	    (kill-line 1))))
    (beginning-of-line)
    (save-excursion
      (setq ps (point))
      (while (not (or (looking-at "[ \t]") (eobp))) (forward-char 1))
      (setq pe (point))
      (while (and (looking-at "[ \t]") (not (eobp))) (forward-char 1))
      (setq ts (point))
      (end-of-line)
      (setq te (point)))
    (setq pfx (buffer-substring ps pe))
    (setq ps (point))
    (insert pfx)
    (setq iter (- ts pe))
    (insert-char ?\  iter t)
    (if (= 1 (length cchr))
	(insert-char (string-to-char cchr) (- te ts) t)
      (setq iter (/ (- te ts) (length cchr)))
      (while (> iter 0)
	(insert cchr)
	(setq iter (1- iter))))
    (setq pfx (buffer-substring ps (point)))
    (insert "\n")
    (end-of-line 1)
    (insert "\n" pfx)))


(defun sysdul-comment-region (from-pt to-pt)
  "Comment out region in Sysdul source."
  (interactive "d\nm")
  (save-excursion
    (let (max-pt)
      (if (> from-pt to-pt)
	  (progn (goto-char to-pt)
		 (setq max-pt from-pt))
	(goto-char from-pt)
	(setq max-pt to-pt))
      (beginning-of-line)
      (save-excursion
	(goto-char max-pt)
	(if (bolp)
	    (forward-char -1))
	(end-of-line)
	(setq max-pt (point)))
      (while (< (point) max-pt)
	(if (bolp)
	    (progn (insert comment-start)
		   (setq max-pt (+ max-pt (length comment-start)))))
	(if (looking-at "@")
	    (progn (insert "@")
		   (forward-char 1)))
	(forward-char 1)))))

(defun sysdul-uncomment-region (from-pt to-pt)
  "Uncomment a region of Sysdul source."
  (interactive "d\nm")
  (save-excursion
    (let (max-pt)
      (if (> from-pt to-pt)
	  (progn (goto-char to-pt)
		 (setq max-pt from-pt))
	(goto-char from-pt)
	(setq max-pt to-pt))
      (beginning-of-line)
      (save-excursion
	(goto-char max-pt)
	(if (bolp)
	    (forward-char -1))
	(end-of-line)
	(setq max-pt (point)))
      (save-restriction
	(narrow-to-region (point) max-pt)
	(save-excursion
	  (while (re-search-forward "^C ?" (point-max) t)
	    (replace-match "")))
	(save-excursion
	  (while (re-search-forward "^ *; ?" (point-max) t)
	    (replace-match "")))
	(save-excursion
	  (while (re-search-forward "@@" (point-max) t)
	    (replace-match "@")))))))

(defun sysdul-fix ()
  "Reformat-buffer"
  (interactive "*")
  (let (pp)
    (save-excursion
      (untabify (point-min) (point-max))
      (goto-char (point-min))
      (replace-regexp "\r$" "")
      (goto-char (point-min))
      (replace-regexp "[ \t]+$" "")
      (goto-char (1- (point-max)))
      (setq pp (point))
      (while (looking-at "\n")
	(forward-char -1))
      (forward-char 1)
      (if (> pp (point)) (progn
			   (delete-char (- pp (point))))))))

(defun sysdul-next-err ()
  "Find next Sysdul error (in listing)."
  (interactive)
  (end-of-line)
  (if (not (search-forward-regexp "^ *\\*\\*\\*[^*].+\\*\\*\\*" nil t))
      (error "no more errors!")))

(defun sysdul-prev-err ()
  "Find previous Sysdul error (in listing)."
  (interactive)
  (beginning-of-line)
  (if (not (search-backward-regexp "^ *\\*\\*\\*[^*].+\\*\\*\\*" nil t))
      (error "no more errors!")))

(defun sysdul-long-sentence (minpt maxpt)
  "Sqeezes a long Sysdul sentence..."
  (interactive "r")
  (save-restriction
    (narrow-to-region minpt maxpt)
    (let ((pos-var 0)
	  (collim 77))
      (goto-char (point-min))
      (save-excursion
	(replace-regexp " +" " " nil))
      (save-excursion
	(replace-regexp " *, *" "," nil))
      (save-excursion
	(replace-regexp ",:\n" "," nil))
      (setq pos-var (- (point-max) (point)))
      (while (> pos-var 77)
	(message "[%d]" pos-var)
	(end-of-line)
	(while (> (current-column) 77)
	  (forward-word -1))
	(while (not (looking-at " "))
	  (forward-char -1))
	(insert ":\n")
	(setq pos-var (- (point-max) (point)))))
    (message "sysdul-long-sentence done!")))

(defun sysdul-none ()
  "Sysdul ."
  (interactive)
  (sysdul-get-sub)
  (let ()
    (save-excursion
      (insert "text")
      (sysdul-indent-newline 1)
      (progn))))

; ======================================================================

(defun sysdul-electric-dollar (&optional pfx)
  "Expand sysdul abbreviations"
  (interactive "P")
  (setq prefix-arg current-prefix-arg)
  (let ((pt (point))
	sub-s
	fun)
    (save-excursion
      (if (re-search-backward "[^-+a-zA-Z0-9?]+" nil t nil)
	  (progn
	    (setq sub-s (buffer-substring (match-end 0) pt)
		  fun   (cdr (assoc sub-s sysdul-abbrev-alist))))))
    (if fun (progn
	      (kill-region (match-end 0) pt)
	      (call-interactively fun)
              (message "")
	      (setq prefix-arg ()))
      (insert last-command-char))))

; ======================================================================


;;;###autoload
(defun sysdul-mode ()
  "Major mode for editing SYSDUL program code.
Special commands:
\\{sysdul-mode-map}
Turning on sysdul mode calls the value of the variable `sysdul-mode-hook',
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map sysdul-mode-map)
  (setq mode-name "Sysdul")
  (setq major-mode 'sysdul-mode)
  (setq indent-tabs-mode nil)
  (setq local-abbrev-table sysdul-mode-abbrev-table)
  (set-syntax-table sysdul-mode-syntax-table)
;  (abbrev-mode 1)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'indent-relative-maybe)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'comment-start)
  (setq comment-start "C ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "^\\(C \\|\\*V *;\\) *")
  (make-local-variable 'comment-column)
  (setq comment-column 0)
  (run-hooks 'sysdul-mode-hook))


(provide 'sysdul)

;;; sysdul.el ends here
