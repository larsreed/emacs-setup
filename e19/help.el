;;; help.el --- help commands for Emacs

;; Copyright (C) 1985, 1986, 1993, 1994 Free Software Foundation, Inc.

;; Contents: Merge of 19.{28..33} + personal additions (lre)

;; Maintainer: FSF
;; Keywords: help, internal

;; This file is part of GNU Emacs.

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

;; This code implements GNU Emac's on-line help system, the one invoked by
;;`M-x help-for-help'.

;;; Code:

;; Get the macro make-help-screen when this is compiled,
;; or run interpreted, but not when the compiled code is loaded.
(eval-when-compile (require 'help-macro))
(load "apropos" t t nil)
(autoload 'finder-by-keyword "finder"
  "Find packages matching a given keyword." t)
(autoload 'duplicates-list "dups" "Find duplicate packages" t)
(autoload 'duplicates-matches "dups" "Find matching packages" t)
(autoload 'bill-help-on-topic "bill-g" "Get Windows help" t)

(defvar help-map (make-sparse-keymap)
  "Keymap for characters following the Help key.")

(defvar help-mode-map (make-sparse-keymap)
  "Keymap for help mode.")

(defconst help-new-ed
  (or (> emacs-major-version 19)
      (and (= emacs-major-version 19)
	   (>= emacs-minor-version 30))))

(define-key global-map (char-to-string help-char) 'help-command)
(define-key global-map [help] 'help-command)
; (define-key global-map [f1] 'help-command)
(fset 'help-command help-map)

(define-key help-map (char-to-string help-char) 'help-for-help)
(define-key help-map [help] 'help-for-help)
(define-key help-map [f1] 'help-for-help)
(define-key help-map "8" 'eternal-describe-key)
(define-key help-map "?" 'help-for-help)
(define-key help-map "A" 'apropos)
(define-key help-map "C" 'describe-syntax)
(define-key help-map "D" 'duplicates-match)
(define-key help-map "F" 'view-emacs-FAQ)
(define-key help-map "K" 'describe-key-permanent)
(define-key help-map "Ll" 'locate-library)
(define-key help-map "Lf" 'find-function)
(define-key help-map "Lk" 'find-function-on-key)
(define-key help-map "M" 'man)
(define-key help-map "S" 'show-save-places)
(define-key help-map "V" 'view-sun4at-keys)
(define-key help-map "a" 'command-apropos)
(define-key help-map "b" 'describe-bindings)
(define-key help-map "c" 'describe-key-briefly)
(define-key help-map "d" 'duplicates-list)
(define-key help-map "f" 'describe-function)
(define-key help-map "i" 'info)
(define-key help-map "k" 'describe-key)
(define-key help-map "l" 'view-lossage)
(define-key help-map "m" 'describe-mode)
(define-key help-map "n" 'view-emacs-news)
(define-key help-map "p" 'finder-by-keyword)
(define-key help-map "q" 'help-quit)
(define-key help-map "s" 'describe-syntax)
(define-key help-map "t" 'help-with-tutorial)
(define-key help-map "v" 'describe-variable)
(define-key help-map "w" 'where-is)
(define-key help-map "\C-a" 'variable-apropos)
(define-key help-map "\C-c" 'describe-copying)
(define-key help-map "\C-d" 'describe-distribution)
(define-key help-map "\C-f" 'Info-goto-emacs-command-node)
(define-key help-map "\C-k" 'Info-goto-emacs-key-command-node)
(define-key help-map "\C-l" 'list-bookmarks)
(define-key help-map "\C-n" 'view-emacs-news)
(define-key help-map "\C-p" 'describe-project)
(define-key help-map "\C-w" 'bill-help-on-topic)
(define-key help-map "\M-w" 'describe-no-warranty)

(defvar help-font-lock-keywords
  (eval-when-compile
    (let ((name-char "[-+a-zA-Z0-9_*]") (sym-char "[-+a-zA-Z0-9_:*]"))
      (list
       ;;
       ;; The symbol itself.
       (list (concat "\\`\\(" name-char "+\\)\\(\\(:\\)\\|\\('\\)\\)")
	     '(1 (if (match-beginning 3)
		     font-lock-function-name-face
		   font-lock-variable-name-face)
		 nil t))
       ;;
       ;; Words inside `' which tend to be symbol names.
       (list (concat "`\\(" sym-char sym-char "+\\)'")
	     1 'font-lock-reference-face t)
       ;;
       ;; CLisp `:' keywords as references.
       (list (concat "\\<:" sym-char "+\\>") 0 'font-lock-reference-face t))))
  "Default expressions to highlight in Help mode.")

(defun help-mode ()
  "Major mode for viewing help text.
Entry to this mode runs the normal hook `help-mode-hook'.
Commands:
\\{help-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map help-mode-map)
  (setq mode-name "Help")
  (setq major-mode 'help-mode)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(help-font-lock-keywords))
  (view-mode)
  (run-hooks 'help-mode-hook))

(defun help-quit ()
  (interactive)
  nil)

(defun help-with-tutorial ()
  "Select the Emacs learn-by-doing tutorial."
  (interactive)
  (let ((file (expand-file-name "~/TUTORIAL")))
    (delete-other-windows)
    (if (get-file-buffer file)
	(switch-to-buffer (get-file-buffer file))
      (switch-to-buffer (create-file-buffer file))
      (setq buffer-file-name file)
      (setq default-directory (expand-file-name "~/"))
      (setq buffer-auto-save-file-name nil)
      (insert-file-contents (expand-file-name "TUTORIAL" data-directory))
      (goto-char (point-min))
      (search-forward "\n<<")
      (beginning-of-line)
      (delete-region (point) (progn (end-of-line) (point)))
      (let ((n (- (window-height (selected-window))
		  (count-lines (point-min) (point))
		  6)))
	(if (< n 12)
	    (newline n)
	  ;; Some people get confused by the large gap.
	  (newline (/ n 2))
	  (insert "[Middle of page left blank for didactic purposes.  "
		  "Text continues below]")
	  (newline (- n (/ n 2)))))
      (goto-char (point-min))
      (set-buffer-modified-p nil))))

(defun describe-key-briefly (key)
  "Print the name of the function KEY invokes.  KEY is a string."
  (interactive "kDescribe key briefly: ")
  ;; If this key seq ends with a down event, discard the
  ;; following click or drag event.  Otherwise that would
  ;; erase the message.
  (let ((type (aref key (1- (length key)))))
    (if (listp type) (setq type (car type)))
    (and (symbolp type)
	 (memq 'down (event-modifiers type))
	 (read-event)))
  (save-excursion
    (let ((modifiers (event-modifiers (aref key 0)))
	  (window nil)
	  position)
      (if help-new-ed
	  (progn
	    ;; For a mouse button event, go to the button it applies to
	    ;; to get the right key bindings.  And go to the right place
	    ;; in case the keymap depends on where you clicked.
	    (if (or (memq 'click modifiers) (memq 'down modifiers)
		    (memq 'drag modifiers))
		(setq window (posn-window (event-start (aref key 0)))
		      position (posn-point (event-start (aref key 0)))))
	    (if (windowp window)
		(progn
		  (set-buffer (window-buffer window))
		  (goto-char position)))))
      ;; Ok, now look up the key and name the command.
      (let ((defn (key-binding key)))
	(if (or (null defn) (integerp defn))
	    (message "%s is undefined" (key-description key))
	  (message (if (windowp window)
		       "%s at that spot runs the command %s"
		     "%s runs the command %s")
		   (key-description key)
		   (if (symbolp defn) defn (prin1-to-string defn))))))))

(defun print-help-return-message (&optional function)
  "Display or return message saying how to restore windows after help command.
Computes a message and applies the optional argument FUNCTION to it.
If FUNCTION is nil, applies `message' to it, thus printing it."
  (and (not (get-buffer-window standard-output))
       (let ((first-message
	      (cond ((or (member (buffer-name standard-output)
				 special-display-buffer-names)
			 (assoc (buffer-name standard-output)
				special-display-buffer-names)
			 (let (found
			       (tail special-display-regexps)
			       (name (buffer-name standard-output)))
			   (while (and tail (not found))
			     (if (or (and (consp (car tail))
					  (string-match (car (car tail)) name))
				     (and (stringp (car tail))
					  (string-match (car tail) name)))
				 (setq found t))
			     (setq tail (cdr tail)))
			   found))
		     ;; If the help output buffer is a special display buffer,
		     ;; don't say anything about how to get rid of it.
		     ;; First of all, the user will do that with the window
		     ;; manager, not with Emacs.
		     ;; Secondly, the buffer has not been displayed yet,
		     ;; so we don't know whether its frame will be selected.
		     ;; Even the message about scrolling the help
		     ;; might be wrong, but it seems worth showing it anyway.
		     nil)
		    ((not (one-window-p t))
		     "Type \\[switch-to-buffer-other-window] RET to restore the other window.")
		    (pop-up-windows
		     "Type \\[delete-other-windows] to remove help window.")
		    (t
		     "Type \\[switch-to-buffer] RET to remove help window."))))
	 (funcall (or function 'message)
		  (concat
		   (if first-message
		       (substitute-command-keys first-message)
		     "")
		   (if first-message "  " "")
		   (if help-new-ed
		       (progn
			 ;; If the help buffer will go in a separate frame,
			 ;; it's no use mentioning a command to scroll,
			 ;; so don't.
			 (if (or (member (buffer-name standard-output)
					 special-display-buffer-names)
				 (assoc (buffer-name standard-output)
					special-display-buffer-names)
				 (memq t (mapcar
					  '(lambda (elt)
					     (if (consp elt)
						 (setq elt (car elt)))
					     (string-match elt
							   (buffer-name
							    standard-output)))
					  special-display-regexps)))
			     nil
			   (if (or (member (buffer-name standard-output)
					   same-window-buffer-names)
				   (assoc (buffer-name standard-output)
					  same-window-buffer-names)
				   (memq t (mapcar
					    '(lambda (elt)
					       (if (consp elt)
						   (setq elt (car elt)))
					       (string-match
						elt
						(buffer-name
						 standard-output)))
					    same-window-regexps)))
			       ;; Say how to scroll this window.
			       (substitute-command-keys
				"\\[scroll-up] to scroll the help.")
			     ;; Say how to scroll some other window.
			     (substitute-command-keys
			      "\\[scroll-other-window] to scroll the help.")))))
		   (substitute-command-keys
		    "\\[scroll-other-window] to scroll the help."))))))


(defun describe-key (key)
  "Display documentation of the function invoked by KEY.  KEY is a string."
  (interactive "kDescribe key: ")
  ;; If this key seq ends with a down event, discard the
  ;; following click or drag event.  Otherwise that would
  ;; erase the message.
  (let ((type (aref key (1- (length key)))))
    (if (listp type) (setq type (car type)))
    (and (symbolp type)
	 (memq 'down (event-modifiers type))
	 (read-event)))
  (save-excursion
    (let ((modifiers (event-modifiers (aref key 0)))
	  window position)
      ;; For a mouse button event, go to the button it applies to
      ;; to get the right key bindings.  And go to the right place
      ;; in case the keymap depends on where you clicked.
      (if (or (memq 'click modifiers) (memq 'down modifiers)
	      (memq 'drag modifiers))
	  (setq window (posn-window (event-start (aref key 0)))
		position (posn-point (event-start (aref key 0)))))
      (if (windowp window)
	  (progn
	    (set-buffer (window-buffer window))
	    (goto-char position)))
      (let ((defn (key-binding key)))
	(if (or (null defn) (integerp defn))
	    (message "%s is undefined" (key-description key))
	  (with-output-to-temp-buffer "*Help*"
	    (princ (key-description key))
	    (if (windowp window)
		(princ " at that spot"))
	    (princ " runs the command ")
	    (prin1 defn)
	    (princ ":\n")
	    (let ((doc (documentation defn)))
	      (if doc
		  (progn (terpri)
			 (princ doc))
		(princ "not documented")))
	    (save-excursion
	      (set-buffer standard-output)
	      (help-mode))
	    (print-help-return-message)))))))

(defun describe-mode ()
  "Display documentation of current major mode and minor modes.
For this to work correctly for a minor mode, the mode's indicator variable
\(listed in `minor-mode-alist') must also be a function whose documentation
describes the minor mode."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (let ((minor-modes minor-mode-alist)
	  (first t)
	  (locals (buffer-local-variables)))
      (while minor-modes
	(let* ((minor-mode (car (car minor-modes)))
	       (indicator (car (cdr (car minor-modes))))
	       (local-binding (assq minor-mode locals)))
	  ;; Document a minor mode if it is listed in minor-mode-alist,
	  ;; bound locally in this buffer, non-nil, and has a function
	  ;; definition.
	  (if (and local-binding
		   (cdr local-binding)
		   (fboundp minor-mode))
	      (let ((pretty-minor-mode minor-mode))
		(if (string-match "-mode$" (symbol-name minor-mode))
		    (setq pretty-minor-mode
			  (capitalize
			   (substring (symbol-name minor-mode)
				      0 (match-beginning 0)))))
		(while (and indicator (symbolp indicator))
		  (setq indicator (symbol-value indicator)))
		(if first (princ
		  "The minor modes are described first,
followed by the major mode, which is described on the last page.\n\f\n"))
		(setq first nil)
		(princ (format "%s minor mode (%s):\n"
			       pretty-minor-mode
			       (if indicator
				   (format "indicator%s" indicator)
				 "no indicator")))
		(princ (documentation minor-mode))
		(princ "\n\f\n"))))
	(setq minor-modes (cdr minor-modes))))
    (princ mode-name)
    (princ " mode:\n")
    (princ (documentation major-mode))
    (save-excursion
      (set-buffer standard-output)
      (help-mode))
    (print-help-return-message)))

;; So keyboard macro definitions are documented correctly
(fset 'defining-kbd-macro (symbol-function 'start-kbd-macro))

(defun describe-distribution ()
  "Display info on how to obtain the latest version of GNU Emacs."
  (interactive)
  (find-file-read-only
   (expand-file-name "DISTRIB" data-directory)))

(defun describe-copying ()
  "Display info on how you may redistribute copies of GNU Emacs."
  (interactive)
  (find-file-read-only
   (expand-file-name "COPYING" data-directory))
  (goto-char (point-min)))

(defun describe-project ()
  "Display info on the GNU project."
  (interactive)
  (find-file-read-only
   (expand-file-name "GNU" data-directory))
  (goto-char (point-min)))

(defun describe-no-warranty ()
  "Display info on all the kinds of warranty Emacs does NOT have."
  (interactive)
  (describe-copying)
  (let (case-fold-search)
    (search-forward "NO WARRANTY")
    (recenter 0)))

(defun describe-prefix-bindings ()
  "Describe the bindings of the prefix used to reach this command.
The prefix described consists of all but the last event
of the key sequence that ran this command."
  (interactive)
  (let* ((key (this-command-keys)))
    (describe-bindings
     (if (stringp key)
	 (substring key 0 (1- (length key)))
       (let ((prefix (make-vector (1- (length key)) nil))
	     (i 0))
	 (while (< i (length prefix))
	   (aset prefix i (aref key i))
	   (setq i (1+ i)))
	 prefix)))))
;; Make C-h after a prefix, when not specifically bound, 
;; run describe-prefix-bindings.
(setq prefix-help-command 'describe-prefix-bindings)

(defun view-emacs-news ()
  "Display info on recent changes to Emacs."
  (interactive)
  (find-file-read-only (expand-file-name "NEWS" data-directory)))

(defun view-emacs-FAQ ()
  "Display the Emacs Frequently Asked Questions (FAQ) file."
  (interactive)
  (find-file-read-only (expand-file-name "FAQ" data-directory)))

(defun view-lossage ()
  "Display last 100 input keystrokes."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ (mapconcat (function (lambda (key)
				  (if (or (integerp key)
					  (symbolp key)
					  (listp key))
				      (single-key-description key)
				    (prin1-to-string key nil))))
		      (recent-keys)
		      " "))
    (save-excursion
      (set-buffer standard-output)
      (goto-char (point-min))
      (while (progn (move-to-column 50) (not (eobp)))
	(search-forward " " nil t)
	(insert "\n"))
      (help-mode))
    (print-help-return-message)))

(defalias 'help 'help-for-help)
(make-help-screen help-for-help
  "[aAbcCdDfFikKlLmnpsStvVw8] C-[afklcdnw], or ? for more help:"
  "You have typed \\[help-command], the help character.  Type a Help option:
\(Use SPC or DEL to scroll through this text.
Type \\<help-map>\\[help-quit] to exit the Help command.)

a  command-apropos.  Give a substring, and see a list of commands
	(functions interactively callable) that contain
	that substring.  See also the apropos command.
A  apropos.  Give a substring, and see a list of commands and variables
	that contain that substring.
C-a variable-apropos.  Give a substring, and see a list of variables
	that contain that substring.
b  describe-bindings.  Display table of all key bindings.
c  describe-key-briefly.  Type a command key sequence;
	it prints the function name that sequence runs.
C  describe-syntax.  Describe the syntax specifications in the syntax table.
C-c print Emacs copying permission (General Public License).
d  duplicates-list.  List all duplicate files in load-path.
D  duplicates-match.  Find files in load-path matching regexp.
C-d print Emacs ordering information.
f  describe-function.  Type a function name and get documentation of it.
F  view-emacs-FAQ.  Shows emacs frequently asked questions file.
C-f Info-goto-emacs-command-node.  Type a function name;
	it takes you to the Info node for that command.
i  info. The  info  documentation reader.
k  describe-key.  Type a command key sequence;
	it displays the full documentation.
K  describe-key-permanent. Type a command key sequence;
	it inserts a brief description into the current buffer. 
C-k Info-goto-emacs-key-command-node.  Type a command key sequence;
	it takes you to the Info node for the command bound to that key.
l  view-lossage.  Shows last 100 characters you typed.
Ll  locate-library.  Give the name of a library, to find where it is
	located.
Lf  find-function.  Open library containing given function.
Lk  find-function-on-key.  Open library containing function for a given
        key-stroke.
C-l list-bookmarks.  Display a list of existing bookmarks.
m  describe-mode.  Print documentation of current major mode,
	which describes the commands peculiar to it.
M  man.  Lookup manual pages.
n  view-emacs-news.  Shows emacs news file.
C-n print news of recent Emacs changes.
p  finder-by-keyword. Find packages matching a given topic keyword.
C-p print information about the GNU project.
s  describe-syntax.  Display contents of syntax table, plus explanations
S  show-save-places.  Open save-places file.
t  help-with-tutorial.  Select the Emacs learn-by-doing tutorial.
v  describe-variable.  Type name of a variable;
	it displays the variable's documentation and value.
V  view-sun4at-keys.
	View the file containing information about special sun4 at101 keys
w  where-is.  Type command name; it prints which keystrokes
	invoke that command.
C-w print information on absence of warranty for GNU Emacs.
8  eternal-describe-key.  Repeatedly call describe-key-permanent.
"
  help-map)

;; Return a function which is called by the list containing point.
;; If that gives no function, return a function whose name is around point.
;; If that doesn't give a function, return nil.
(defun function-called-at-point ()
  (or (condition-case ()
	  (save-excursion
	    (save-restriction
	      (narrow-to-region (max (point-min) (- (point) 1000)) (point-max))
	      (backward-up-list 1)
	      (forward-char 1)
	      (let (obj)
		(setq obj (read (current-buffer)))
		(and (symbolp obj) (fboundp obj) obj))))
	(error nil))
      (condition-case ()
	  (let ((stab (syntax-table)))
	    (unwind-protect
		(save-excursion
		  (set-syntax-table emacs-lisp-mode-syntax-table)
		  (or (not (zerop (skip-syntax-backward "_w")))
		      (eq (char-syntax (following-char)) ?w)
		      (eq (char-syntax (following-char)) ?_)
		      (forward-sexp -1))
		  (skip-chars-forward "'")
		  (let ((obj (read (current-buffer))))
		    (and (symbolp obj) (fboundp obj) obj)))
	      (set-syntax-table stab)))
	(error nil))))

(defun describe-function-find-file (function)
  (let ((files load-history)
	file functions)
    (while files
      (if (memq function (cdr (car files)))
	  (setq file (car (car files)) files nil))
      (setq files (cdr files)))
    file))

(defun describe-function (function)
  "Display the full documentation of FUNCTION (a symbol)."
  (interactive
   (let ((fn (function-called-at-point))
	 (enable-recursive-minibuffers t)	     
	 val)
     (setq val (completing-read (if fn
				    (format "Describe function (default %s): " fn)
				  "Describe function: ")
				obarray 'fboundp t))
     (list (if (equal val "")
	       fn (intern val)))))
  (with-output-to-temp-buffer "*Help*"
    (prin1 function)
    (princ ": ")
    (let* ((def (symbol-function function))
	   file-name
	   (beg (if (commandp def) "an interactive " "a ")))
      (princ (cond ((or (stringp def)
			(vectorp def))
		    "a keyboard macro")
		   ((subrp def)
		    (concat beg "built-in function"))
		   ((byte-code-function-p def)
		    (concat beg "compiled Lisp function"))
		   ((symbolp def)
		    (format "alias for `%s'" def))
		   ((eq (car-safe def) 'lambda)
		    (concat beg "Lisp function"))
		   ((eq (car-safe def) 'macro)
		    "a Lisp macro")
		   ((eq (car-safe def) 'mocklisp)
		    "a mocklisp function")
		   ((eq (car-safe def) 'autoload)
		    (setq file-name (nth 1 def))
		    (format "%s autoloaded Lisp %s"
			    (if (commandp def) "an interactive" "an")
			    (if (nth 4 def) "macro" "function")
			    ))
		   (t "")))
      (or file-name
	  (setq file-name (describe-function-find-file function)))
      (if file-name
	  (progn
	    (princ " in `")
	    ;; We used to add .el to the file name,
	    ;; but that's completely wrong when the user used load-file.
	    (princ file-name)
	    (princ "'")))
      (princ ".")
      (terpri)
      (let ((arglist (cond ((byte-code-function-p def)
			    (car (append def nil)))
			   ((eq (car-safe def) 'lambda)
			    (nth 1 def))
			   (t t))))
	(if (listp arglist)
	    (progn
	      (princ (cons function
			   (mapcar (lambda (arg)
				     (if (memq arg '(&optional &rest))
					 arg
				       (intern (upcase (symbol-name arg)))))
				   arglist)))
	      (terpri))))
      (let ((doc (documentation function)))
	(if doc
	    (progn (terpri)
		   (princ doc))
	  (princ "not documented"))))
    (print-help-return-message)
    (save-excursion
      (set-buffer standard-output)
      (help-mode)
      ;; Return the text we displayed.
      (buffer-string))))

(defun variable-at-point ()
  (condition-case ()
      (let ((stab (syntax-table)))
	(unwind-protect
	    (save-excursion
	      (set-syntax-table emacs-lisp-mode-syntax-table)
	      (or (not (zerop (skip-syntax-backward "_w")))
		  (eq (char-syntax (following-char)) ?w)
		  (eq (char-syntax (following-char)) ?_)
		  (forward-sexp -1))
	      (skip-chars-forward "'")
	      (let ((obj (read (current-buffer))))
		(and (symbolp obj) (boundp obj) obj)))
	  (set-syntax-table stab)))
    (error nil)))

(if (not (fboundp 'local-variable-p))
    (defun local-variable-p (arg) "empty" nil))

(defun describe-variable (variable)
  "Display the full documentation of VARIABLE (a symbol).
Returns the documentation as a string, also."
  (interactive 
   (let ((v (variable-at-point))
	 (enable-recursive-minibuffers t)
	 val)
     (setq val
	   (completing-read
	    (if v
		(format "Describe variable (default %s): " v)
	      "Describe variable: ")
	    obarray 'boundp t))
     (list (if (equal val "")
	       v (intern val)))))
  (with-output-to-temp-buffer "*Help*"
    (prin1 variable)
    (if (not (boundp variable))
        (princ " is void")
      (princ "'s value is ")
      (prin1 (symbol-value variable)))
    (terpri)
    (if (local-variable-p variable)
	(progn
	  (princ (format "Local in buffer %s; " (buffer-name)))
	  (if (not (default-boundp variable))
	      (princ "globally void")
	    (princ "global value is ")
	    (prin1 (default-value variable)))
	  (terpri)))
    (terpri)
    (princ "Documentation:")
    (terpri)
    (let ((doc (documentation-property variable 'variable-documentation)))
      (princ (or doc "not documented as a variable.")))
    (print-help-return-message)
    (save-excursion
      (set-buffer standard-output)
      (help-mode)
      ;; Return the text we displayed.
      (buffer-string))))

(defun where-is (definition)
  "Print message listing key sequences that invoke specified command.
Argument is a command definition, usually a symbol with a function definition."
  (interactive
   (let ((fn (function-called-at-point))
	 (enable-recursive-minibuffers t)	     
	 val)
     (setq val (completing-read (if fn
				    (format "Where is command (default %s): " fn)
				  "Where is command: ")
				obarray 'fboundp t))
     (list (if (equal val "")
	       fn (intern val)))))
  (let* ((keys (where-is-internal definition overriding-local-map nil nil))
	 (keys1 (mapconcat 'key-description keys ", ")))
    (if (> (length keys1) 0)
	(message "%s is on %s" definition keys1)
      (message "%s is not on any key" definition)))
  nil)

(if (fboundp 'command-apropos) ()
    (defun command-apropos (string)
      "Like apropos but lists only symbols that are names of commands
\(interactively callable functions).  Argument REGEXP is a regular expression
that is matched against command symbol names.  Returns list of symbols and
documentation found."
      (interactive "sCommand apropos (regexp): ")
      (let ((message
	     (let ((standard-output (get-buffer-create "*Help*")))
	       (print-help-return-message 'identity))))
	(if (apropos string t 'commandp)
	    (and message (message message))))))

(defun locate-library (library &optional nosuffix)
  "Show the full path name of Emacs library LIBRARY.
This command searches the directories in `load-path' like `M-x load-library'
to find the file that `M-x load-library RET LIBRARY RET' would load.
Optional second arg NOSUFFIX non-nil means don't add suffixes `.elc' or `.el'
to the specified name LIBRARY (a la calling `load' instead of `load-library')."
  (interactive "sLocate library: ")
  (catch 'answer
    (mapcar
     '(lambda (dir)
	(mapcar
	 '(lambda (suf)
	    (let ((try (expand-file-name (concat library suf) dir)))
	      (and (file-readable-p try)
		   (null (file-directory-p try))
		   (progn
		     (message "Library is file %s" try)
		     (throw 'answer try)))))
	 (if nosuffix '("") '(".elc" ".el" ""))))
     load-path)
    (message "No library %s in search path" library)
    nil))

(defun describe-key-permanent (key)
  "Display documentation of the function invoked by KEY.
KEY is a string.  Uses the *Keys* buffer, not *Help*, and does not erase
its contents."
  (interactive "kDescribe key: ")
  ;; If this key seq ends with a down event, discard the
  ;; following click or drag event.  Otherwise that would
  ;; erase the message.
  (let ((type (aref key (1- (length key)))))
    (if (listp type) (setq type (car type)))
    (and (symbolp type)
         (memq 'down (event-modifiers type))
         (read-event)))
  (let ((defn (key-binding key)))
    (insert (format "%s\t"(key-description key)))
    (if (or (null defn) (integerp defn))
	(insert "undefined")
      (insert (symbol-name defn)))
    (insert "\n")
    defn))

(defun eternal-describe-key ()
  "Describe keys \"forever\"..."
   (interactive)
   (while (not (string= (call-interactively 'describe-key-permanent)
			"keyboard-quit"))))

(defun variable-apropos (string)
 "Like apropos but lists only symbols
that are names of variables.  Argument REGEXP is a regular
expression that is matched against command symbol names.  
list of symbols and documentation found."
 (interactive "sVariable apropos (regexp): ")
 (let ((message
	(let ((standard-output (get-buffer-create "*Help*")))
	  (print-help-return-message 'identity))))
   (if (apropos string t (function (lambda (sy) (not (commandp sy)))))
       (and message (message message)))))

;;; help.el ends here
