;;; occurmod.el
;; Simple modification to occur.el to highlight matching text in
;; *Occur* buffer.

;;; Few simple mods for occur -- just to provide a couple of shortcuts
;; rather than anything spectacular.

;;; tested on Emacs 19.34 and XEmacs 19.14

;; 1. highlighting of text matching regexp in *occur* buffer.
;; 2. `again' facility.  Press `a' in occur buffer to repeat last M-x
;; occur on same buffer.  (Useful if you have been editing buffer and
;; want to rerun the same occur.)

;; 3. another occur.  Press `o' in occur buffer to do another occur
;; with a new regexp on the same buffer as last time,

;;; User variables

;; These 2 variables could be combined into one.
(defvar occur-highlight-match t
  "*Non-nil means highlight region matching occur regexp.")

(defvar occur-highlight-face 'bold
  "*Face to show occur matches in.
M-x list-faces-display gives a list of possible faces." )

;;; Modified occur from 19.34 version.
(defun occur (regexp &optional nlines)
  "Show all lines in the current buffer containing a match for REGEXP.

If a match spreads across multiple lines, all those lines are shown.

Each line is displayed with NLINES lines before and after, or -NLINES
before if NLINES is negative.
NLINES defaults to `list-matching-lines-default-context-lines'.
Interactively it is the prefix arg.

The lines are shown in a buffer named `*Occur*'.
It serves as a menu to find any of the occurrences in this buffer.
\\[describe-mode] in that buffer will explain how."
  (interactive
   (list (let* ((default (car regexp-history))
		(input
		 (read-from-minibuffer
		  (if default
		      (format "List lines matching regexp (default `%s'): "
			      default)
		    "List lines matching regexp: ")
		  nil nil nil 'regexp-history)))
	   (if (string-equal input "")
	       default
	     (set-text-properties 0 (length input) nil input)
	     input))
	 current-prefix-arg))
  (let ((nlines (if nlines
		    (prefix-numeric-value nlines)
		  list-matching-lines-default-context-lines))
	(first t)
	(buffer (current-buffer))
	(dir default-directory)
	(linenum 1)
	(prevpos (point-min))
	sje-start
	sje-len
	(final-context-start (make-marker)))
;;;	(save-excursion
;;;	  (beginning-of-line)
;;;	  (setq linenum (1+ (count-lines (point-min) (point))))
;;;	  (setq prevpos (point)))
    (save-excursion
      (goto-char (point-min))
      ;; Check first whether there are any matches at all.
      (if (not (re-search-forward regexp nil t))
	  (message "No matches for `%s'" regexp)
	;; Back up, so the search loop below will find the first match.
	(goto-char (match-beginning 0))
	(with-output-to-temp-buffer "*Occur*"
	  (save-excursion
	    (set-buffer standard-output)
	    (setq default-directory dir)
	    ;; We will insert the number of lines, and "lines", later.
	    (insert " matching ")
	    (let ((print-escape-newlines t))
	      (prin1 regexp))
	    (insert " in buffer " (buffer-name buffer) ?. ?\n)
	    (occur-mode)
	    (setq occur-buffer buffer)
	    (setq occur-nlines nlines)
	    (setq occur-pos-list ()))
	  (if (eq buffer standard-output)
	      (goto-char (point-max)))
	  (save-excursion
	    ;; Find next match, but give up if prev match was at end of buffer.
	    (while (and (not (= prevpos (point-max)))
			(re-search-forward regexp nil t))
	      (goto-char (match-beginning 0))
	      (beginning-of-line)
	      (save-match-data
		(setq linenum (+ linenum (count-lines prevpos (point)))))
	      (setq prevpos (point))
	      (goto-char (match-end 0))
	      (let* ((start (save-excursion
			      (goto-char (match-beginning 0))
			      (forward-line
			       (if (< nlines 0) nlines (- nlines)))
			      (point)))
		     (end (save-excursion
			    (goto-char (match-end 0))
			    (if (> nlines 0)
				(forward-line (1+ nlines))
				(forward-line 1))
			    (point)))
		     (sje-start (+ 6	; +6 to skip over line number
				   (save-excursion
				     (goto-char (match-beginning 0))
				     (beginning-of-line)
				     (- (match-beginning 0) (point)))))
		     (sje-len (- (match-end 0) (match-beginning 0))) ;sje
		     (tag (format "%5d" linenum))
		     (empty (make-string (length tag) ?\ ))
		     tem)
		(save-excursion
		  (setq tem (make-marker))
		  (set-marker tem (point))
		  (set-buffer standard-output)
		  (setq occur-pos-list (cons tem occur-pos-list))
		  (or first (zerop nlines)
		      (insert "--------\n"))
		  (setq first nil)
		  (insert-buffer-substring buffer start end)
		  (set-marker final-context-start
			      (- (point) (- end (match-end 0))))
		  (backward-char (- end start))
		  (setq tem nlines)
		  (while (> tem 0)
		    (insert empty ?:)
		    (forward-line 1)
		    (setq tem (1- tem)))
		  (let ((this-linenum linenum))
		    (while (< (point) final-context-start)
		      (if (null tag)
			  (setq tag (format "%5d" this-linenum)))
		      (insert tag ?:)
		      (put-text-property (save-excursion
					   (beginning-of-line)
					   (point))
					 (save-excursion
					   (end-of-line)
					   (point))
					 'mouse-face 'highlight)
		      (if occur-highlight-match	;sje
			  (put-text-property
			   (save-excursion
			     (beginning-of-line)
			     (forward-char sje-start)
			     (point))
			   (save-excursion
			     (beginning-of-line)
			     (forward-char (+ sje-start sje-len))
			     (point))
			   'face occur-highlight-face))
		      (forward-line 1)
		      (setq tag nil)
		      (setq this-linenum (1+ this-linenum)))
		    (while (<= (point) final-context-start)
		      (insert empty ?:)
		      (forward-line 1)
		      (setq this-linenum (1+ this-linenum))))
		  (while (< tem nlines)
		    (insert empty ?:)
		    (forward-line 1)
		    (setq tem (1+ tem)))
		  (goto-char (point-max)))
		(forward-line 1)))
	    (set-buffer standard-output)
	    ;; Put positions in increasing order to go with buffer.
	    (setq occur-pos-list (nreverse occur-pos-list))
	    (goto-char (point-min))
	    (let ((message-string
		   (if (= (length occur-pos-list) 1)
		       "1 line"
		     (format "%d lines" (length occur-pos-list)))))
	      (insert message-string)
	      (if (interactive-p)
		  (message "%s matched" message-string)))))))))

(defun occur-get-buf-regexp ()
  "Return the buffer name and regexp as list (BUFNAME REGEXP)."
  (let ( beg end s re buf)
    (save-excursion
      (if (not (set-buffer "*Occur*"))
	  (error "*Occur* does not exist"))
      (save-excursion
	(goto-char (point-min))
	(setq beg (point))
	(end-of-line)
	(setq end (point))
	)
      (setq s (buffer-substring beg end))
      (if (not (string-match
		"^[0-9]+ lines? matching \"\\(.*\\)\" in buffer \\(.*\\).$"
		s))
	  (error "string-match failed"))
      (setq re (substring s (match-beginning 1) (match-end 1)))
      (setq re (regexp-sans-escapes re))
      (message "re is %s" re)
      (setq buf (substring s (match-beginning 2) (match-end 2)))
      (list re buf))))

;; These functions dont bother checking to see if the buffer still exists.

(defun occur-again ()
  "Repeat last occur using same buffer and regexp."
  (interactive)
  (let* ((info (occur-get-buf-regexp))
	 (re (car info))
	 (buf (car (cdr info)))
	 )
    (set-buffer buf)
    (occur re)
    ))

(defun occur-another (regexp &optional nlines)
  "Rerun occur using same buffer, but prompt for the regexp."
  (interactive
   (list (let* ((default (car regexp-history))
		(input
		 (read-from-minibuffer
		  (if default
		      (format "List lines matching regexp (default `%s'): "
			      default)
		    "List lines matching regexp: ")
		  nil nil nil 'regexp-history)))
	   (if (string-equal input "")
	       default
	     (set-text-properties 0 (length input) nil input)
	     input))
	 current-prefix-arg))
  (let ((buf (car (cdr (occur-get-buf-regexp)))))
    (save-excursion
      (set-buffer buf)
      (occur regexp nlines))))

;; stolen from allout.el
(defun regexp-sans-escapes (regexp &optional successive-backslashes)
  "Return a copy of REGEXP with all character escapes stripped out.

Representations of actual backslashes - '\\\\\\\\' - are left as a
single backslash.

Optional arg SUCCESSIVE-BACKSLASHES is used internally for recursion."
  (if (string= regexp "")
      ""
    ;; Set successive-backslashes to number if current char is
    ;; backslash, or else to nil:
    (setq successive-backslashes
	  (if (= (aref regexp 0) ?\\)
	      (if successive-backslashes (1+ successive-backslashes) 1)
	    nil))
    (if (or (not successive-backslashes) (= 2 successive-backslashes))
	;; Include first char:
	(concat (substring regexp 0 1)
		(regexp-sans-escapes (substring regexp 1)))
      ;; Exclude first char, but maintain count:
      (regexp-sans-escapes (substring regexp 1) successive-backslashes))))

;;; keybindings
(define-key occur-mode-map "a" 'occur-again)
(define-key occur-mode-map "o" 'occur-another)

(provide 'occurmod)
