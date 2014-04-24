
;;;; Trivial CSS editing mode.

;;; Adds font locking, some rather primitive indentation handling and
;;; some typing help.
;;;
;;; Version 0.08
;;; Lars Marius Garshol, larsga@ifi.uio.no, 12.Mar.98

; IMPORTANT:

; All variables have been renamed to use the prefix cssm- instead of
; css-, which conflicted with css.el from Emacs-W3. The exceptions are
; css-mode and css-mode-hook.

; Send me an email if you want new features (or if you add them yourself).
; I will do my best to preserve the API to functions not explicitly marked
; as internal and variables shown as customizable. I make no promises about
; the rest.

; Bug reports are very welcome. New versions of the package will appear at
; http://www.stud.ifi.uio.no/~larsga/download/css-mode.html
; You can register at the same address if you want to be notified when a
; new version appears.

; Thanks to Philippe Le Hegaret, Kjetil Kjernsmo, Alf-Ivar Holm and
; Alfred Correira for much useful feedback.

; To install, put this in your .emacs:
;
; (autoload 'css-mode "css-mode")
; (setq auto-mode-alist       
;      (cons '("\\.css\\'" . css-mode) auto-mode-alist))

;; Will do:
;
; - throw out cssm-list-2-regexp and use Simon Marshalls package instead
; - must not color URL file name extensions as class selectors
; - color [] selectors
; - support c-like indentation style

;; Possible later additions:
;
; - forward/backward style/@media rule commands
; - better indentation support and support for more than one indentation style
; - more complete syntax table

;; Required modules

(require 'font-lock)
(require 'cl)

;;; The code itself

; Customizable variables:

(defvar cssm-indent-level 2 "The indentation level inside @media rules.")
(defvar cssm-mirror-mode t
  "Whether brackets, quotes etc should be mirrored automatically on
  insertion.")
  
; The rest of the code:

(defvar cssm-properties
  '("font-family" "font-style" "font-variant" "font-weight"
    "font-size" "font" "background-color" "background-image"
    "background-repeat" "background-attachment" "background-position"
    "color" "background" "word-spacing" "letter-spacing"
    "border-top-width" "border-right-width" "border-left-width"
    "border-bottom-width" "border-width" "list-style-type"
    "list-style-image" "list-style-position" "text-decoration"
    "vertical-align" "text-transform" "text-align" "text-indent"
    "line-height" "margin-top" "margin-right" "margin-bottom"
    "margin-left" "margin" "padding-top" "padding-right" "padding-bottom"
    "padding-left" "padding" "border-top" "border-right" "border-bottom"
    "border-left" "border" "width" "height" "float" "clear" "display"
    "list-style" "white-space" "border-style" "border-color"

    ; CSS level 2:

    "azimuth" "border-bottom-color" "border-bottom-style"
    "border-collapse" "border-left-color" "border-left-style"
    "border-right-color" "border-right-style" "border-top-color"
    "border-top-style" "caption-side" "cell-spacing" "clip" "column-span"
    "content" "cue" "cue-after" "cue-before" "cursor" "direction"
    "elevation" "font-size-adjust" "left" "marks" "max-height" "max-width"
    "min-height" "min-width" "orphans" "overflow" "page-break-after"
    "page-break-before" "pause" "pause-after" "pause-before" "pitch"
    "pitch-range" "play-during" "position" "richness" "right" "row-span"
    "size" "speak" "speak-date" "speak-header" "speak-punctuation"
    "speak-time" "speech-rate" "stress" "table-layout" "text-shadow" "top"
    "visibility" "voice-family" "volume" "widows" "z-index")
  "A list of all CSS properties.")

(defvar cssm-properties-alist
  (mapcar (lambda(prop)
	    (cons (concat prop ":") nil)) cssm-properties)
  "An association list of the CSS properties for completion use.")

(defvar cssm-keywords 
  (append '("!\\s-*important"
    
	  ; CSS level 2:

	    "@media" "@import" "@page" "@font-face")
	  (mapcar (lambda(property)
		    (concat property "\\s-*:"))
		  cssm-properties))
  "A list of all CSS keywords.")

(defvar cssm-pseudos
  '("link" "visited" "active" "first-line" "first-letter"

    ; CSS level 2
    "first-child" "before" "after" "hover")
  "A list of all CSS pseudo-classes.")

; internal
(defun cssm-list-2-regexp(altlist)
  "Takes a list and returns the regexp \\(elem1\\|elem2\\|...\\)"
  (let ((regexp "\\("))
    (mapcar (lambda(elem)
	      (setq regexp (concat regexp elem "\\|")))
	    altlist)
    (concat (substring regexp 0 -2) ; cutting the last "\\|"
	    "\\)")
    ))

(defvar cssm-font-lock-keywords
  (list
   (cons (cssm-list-2-regexp cssm-keywords) font-lock-keyword-face)
   (cons "\\.[a-zA-Z][-a-zA-Z0-9.]+" font-lock-variable-name-face)
   (cons (concat ":" (cssm-list-2-regexp cssm-pseudos))
	 font-lock-variable-name-face)
   (cons "#[a-fA-F0-9][a-fA-F0-9][a-fA-F0-9]\\([a-fA-F0-9][a-fA-F0-9][a-fA-F0-9]\\)?"
	 font-lock-reference-face)
   (cons "#[a-zA-Z][-a-zA-Z.]*" font-lock-function-name-face)
   (cons "rgb(\\s-*[0-9]+\\(\\.[0-9]+\\s-*%\\s-*\\)?\\s-*,\\s-*[0-9]+\\(\\.[0-9]+\\s-*%\\s-*\\)?\\s-*,\\s-*[0-9]+\\(\\.[0-9]+\\s-*%\\s-*\\)?\\s-*)"
	 font-lock-reference-face)
   )
  "Rules for highlighting CSS style sheets.")

; internal
(defun cssm-inside-atmedia-rule()
  "Decides if point is currently inside an @media rule."
  (let ((orig-pos (point))
	(atmedia (re-search-backward "@media" 0 t))
	(balance 1)   ; used to keep the {} balance, 1 because we start on a {
	)
     ; Going to the accompanying {
    (re-search-forward "{" (point-max) t)
    (if (null atmedia)
	nil  ; no @media before this point => not inside
      (while (and (< (point) orig-pos)
		  (< 0 balance))
	(if (null (re-search-forward "[{}]" (point-max) 0))
	    (goto-char (point-max)) ; break
	  (setq balance
		(if (string= (match-string 0) "{")
		    (+ balance 1)
		  (- balance 1)))))
      (= balance 1))
    ))

; internal
(defun cssm-rule-is-atmedia()
  "Decides if point is currently on the { of an @media or ordinary style rule."
  (let ((result (re-search-backward "[@}{]" 0 t)))
    (if (null result)
	nil
      (string= (match-string 0) "@"))))

; internal
(defun cssm-find-column()
  "Find which column to indent to." 

  ; Find out where to indent to by looking at previous lines
  ; What we need to find is the first previous piece that matches one
  ; of these: /* */ { }
  
  (let* ((start-of-line (point))
	(cssm-salient-points '("/\\*" "\\*/" "{" "}"))
	(last (re-search-backward (cssm-list-2-regexp cssm-salient-points) 0 t))
	(construct (if (null last)
		       nil
		       (match-string 0)))
	(last-col (current-column)) ; where last construct was
	)
    (cond
     ; Inside a comment
     ((string= "/*" construct) (+ last-col 3))

     ; Inside style rule or @media rule
     ((string= "{" construct)
      (save-excursion
	(if (cssm-rule-is-atmedia)
	    cssm-indent-level
	  (+ last-col 2))))

     ; Possibly inside an @media rule
     ((or (string= "}" construct)
	  (string= "*/" construct))
      (save-excursion
	(if (cssm-inside-atmedia-rule)
	    cssm-indent-level
	  0)
	))
     
     (t 0)
     )))

(defun cssm-indent-line()
  "Indents the current line."
  (interactive)
  (beginning-of-line)
  (let ((beg-of-line (point))
	(indent-column (cssm-find-column)))  

    (goto-char beg-of-line)

    ; Remove all leading whitespace on this line
    (let ((pos (re-search-forward "[@#a-zA-Z0-9;,.\"{}/*\n]" (point-max) t)))
      (if (or (null pos)
	      (= beg-of-line (match-beginning 0)))
	  nil
	  (kill-region beg-of-line (match-beginning 0))
	  ))

    (goto-char beg-of-line)
    
    ; Indent
    (while (< 0 indent-column)
      (insert " ")
      (setq indent-column (- indent-column 1))
      )
    ))

;;; Typing shortcuts

(define-skeleton cssm-insert-curlies
  "Inserts a pair of matching curly parenthesises." nil
  "{ " _ " }")

(define-skeleton cssm-insert-quotes
  "Inserts a pair of matching quotes." nil
  "\"" _ "\"")

(define-skeleton cssm-insert-parenthesises
  "Inserts a pair of matching parenthesises." nil
  "(" _ ")")

(define-skeleton cssm-insert-comment
  "Inserts a full comment." nil
  "/* " _ " */")

(define-skeleton cssm-insert-url
  "Inserts a URL." nil
  "url(" _ ")")

(define-skeleton cssm-insert-brackets
  "Inserts a pair of matching brackets." nil
  "[" _ "]")

(defun cssm-enter-mirror-mode()
  "Turns on mirror mode, where quotes, brackets etc are mirrored automatically
  on insertion."
  (interactive)
  (local-set-key (read-kbd-macro "{")  'cssm-insert-curlies)
  (local-set-key (read-kbd-macro "\"") 'cssm-insert-quotes)
  (local-set-key (read-kbd-macro "(")  'cssm-insert-parenthesises)
  (local-set-key (read-kbd-macro "[")  'cssm-insert-brackets))

(defun cssm-leave-mirror-mode()
  "Turns off mirror mode."
  (interactive)
  (local-set-key (read-kbd-macro "{")  'self-insert-command)
  (local-set-key (read-kbd-macro "\"") 'self-insert-command)
  (local-set-key (read-kbd-macro "(")  'self-insert-command)
  (local-set-key (read-kbd-macro "[")  'self-insert-command))

;;; Property completion

(defun cssm-property-at-point()
  "If point is at the end of a property name: returns it."
  (let ((end (point))
	(start (+ (re-search-backward "[^-A-Za-z]") 1)))
    (goto-char end)
    (buffer-substring start end)))

; internal
(defun cssm-maximum-common(alt1 alt2)
  "Returns the maximum common starting substring of alt1 and alt2."
  (let* ((maxlen (min (length alt1) (length alt2)))
	 (alt1 (substring alt1 0 maxlen))
	 (alt2 (substring alt2 0 maxlen)))
    (while (not (string= (substring alt1 0 maxlen)
			 (substring alt2 0 maxlen)))
      (setq maxlen (- maxlen 1)))
    (substring alt1 0 maxlen)))

; internal
(defun cssm-common-beginning(alts)
  "Returns the maximum common starting substring of all alts elements."
  (let ((common (car alts)))
    (dolist (alt (cdr alts) common)
      (setq common (cssm-maximum-common alt common)))))

(defun cssm-complete-property-frame(completions)
  ; This code stolen from message.el. Kudos to larsi.
  (let ((cur (current-buffer)))
    (pop-to-buffer "*Completions*")
    (buffer-disable-undo (current-buffer))
    (let ((buffer-read-only nil))
      (erase-buffer)
      (let ((standard-output (current-buffer)))
	(display-completion-list (sort completions 'string<)))
      (goto-char (point-min))
      (pop-to-buffer cur))))
  
(defun cssm-complete-property()
  "Completes the CSS property being typed at point."
  (interactive)
  (let* ((prop (cssm-property-at-point))
	 (alts (all-completions prop cssm-properties-alist)))
    (if (= (length alts) 1)
	(insert (substring (car alts) (length prop)))
      (let ((beg (cssm-common-beginning alts)))
	(if (not (string= beg prop))
	    (insert (substring beg (length prop)))
	  (insert (substring
		   (completing-read "Property: " cssm-properties-alist nil
				    nil prop)
		   (length prop)))
	)))))

(defun css-mode()
  "Major mode for editing CSS style sheets."
  (interactive)

  ; Initializing
  (kill-all-local-variables)

  ; Setting up indentation handling
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'cssm-indent-line)
  
  ; Setting up font-locking
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(cssm-font-lock-keywords nil t nil nil))

  ; Setting up typing shortcuts
  (make-local-variable 'skeleton-end-hook)
  (setq skeleton-end-hook nil)
  
  (when cssm-mirror-mode
	(cssm-enter-mirror-mode))
  
  (local-set-key (read-kbd-macro "C-c C-c") 'cssm-insert-comment)
  (local-set-key (read-kbd-macro "C-c C-u") 'cssm-insert-url)
  (local-set-key (read-kbd-macro "C-c TAB") 'cssm-complete-property)
  
  ; Setting up syntax recognition
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)

  (setq comment-start "/* "
	comment-end " */"
	comment-start-skip "/\\*[ \n\t]+")

  ; Setting up syntax table
  (modify-syntax-entry ?* ". 23")
  (modify-syntax-entry ?/ ". 14")
  
  ; Final stuff, then we're done
  (setq mode-name "CSS"
	major-mode 'css-mode)
  (run-hooks 'css-mode-hook)
  )

(provide 'css-mode)

;; CSS-mode ends here