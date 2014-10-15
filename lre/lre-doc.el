;;; lre-doc.el --- Lars Reed - Emacs init file
;; Document handling (troff/*ML/...)

;; Copyright (C) 2002-2014 Lars Reed
;;   See lresetup.el
;; Author:		Lars Reed <Lars@kalars.net>
; Hi-lock: (("^;;; lre.*\\.el[^\n]*" (0 (quote hi-black-hb) t)))
; Hi-lock: (("Lars.*R[e]ed" (0 (quote hi-blue) t)))
; Hi-lock: (("^;;; [^\n]+" (0 (quote hi-green) t)))

;; --------------------------------------------------------------

(require 'cl)
(require 'easymenu)

(defvar outline-regexp)
(defvar outline-level)


;;; ---------------------------------------------------------------
;;; nroff
;;;


;;; Code:
(defun lre-nroff-mode ()
"Additions to `nroff-mode' setup."
  (auto-fill-mode 1)
  (lre-set-local fill-column 70)
  (lre-colors)
  (when (lre-memb 'keys)
    (define-key nroff-mode-map "\C-cs"    'lre-nroff-string)
    (define-key nroff-mode-map "\C-cq"    'lre-nroff-quote)
    (define-key nroff-mode-map "\C-c\""   'lre-nroff-quote)
    (define-key nroff-mode-map "\C-cb"    'lre-nroff-bold)
    (define-key nroff-mode-map "\C-ci"    'lre-nroff-ital)
    (define-key nroff-mode-map "\C-cz"    'lre-nroff-size)
    (define-key nroff-mode-map "\C-cf"    'lre-nroff-file)
    (define-key nroff-mode-map "\C-cp"    'lre-nroff-pair)
    (define-key nroff-mode-map "\C-c\C-p" 'lre-nroff-pair)
    (define-key nroff-mode-map "\C-co"    'lre-nroff-bull)
    (define-key nroff-mode-map "\C-c-"    'lre-nroff-dash)
    (lre-set-norw nroff-mode-map          'lre-nroff-norw))
  (lre-brace-mode 2))

(defun lre-nroff-string ()  "Insert \\*(."  (interactive "*")
  (insert "\\*("))

(defun lre-nroff-electric (words estr &optional sfx pfx)
"Inserts pair of strings."
  (let ((s1 (if pfx pfx "\\*"))
	(s2 (if sfx sfx estr))
	(pp (point)))
    (if (/= words 1) (forward-word -1))
    (insert s1 estr)
    (if (/= words 1) (goto-char (+ pp (length s1) (length estr))))
    (setq pp (point))
    (insert s1 s2)
    (if (= words 1) (goto-char pp))))

(defun lre-nroff-file (&optional N)  "Insert file format."  (interactive "*p")
  (lre-nroff-electric N "(Fn" "(Fm"))

(defun lre-nroff-quote (&optional N)  "Insert quote."  (interactive "*p")
  (lre-nroff-electric N "Q" "U"))

(defun lre-nroff-bold (&optional N)  "Insert bold."  (interactive "*p")
  (lre-nroff-electric N "B" "R" "\\f"))

(defun lre-nroff-ital (&optional N)  "Insert italic."  (interactive "*p")
  (lre-nroff-electric N "I" "R" "\\f"))

(defun lre-nroff-size (dim)
  "Insert size change (negative prefix gives positive size...)."
  (interactive "*p")
  (let (pfx sfx)
    (if (> dim 0) (progn
		    (setq pfx (concat "-" (int-to-string dim)))
		    (setq sfx (concat "+" (int-to-string dim))))
      (setq pfx (concat "+" (int-to-string dim)))
      (setq sfx (concat "-" (int-to-string dim))))
    (lre-nroff-electric 1 pfx sfx "\\s")))

(defun lre-nroff-pair (ch &optional N)
  "Insert pair of \\*C/c."
  (interactive "*cChar: \np")
  (let ((pfx (char-to-string (upcase ch)))
	(sfx (char-to-string (downcase ch))))
    (lre-nroff-electric N pfx sfx)))

(defun lre-nroff-bull ()  "Insert \\(bu."  (interactive "*")
  (insert "\\\(bu"))

(defun lre-nroff-dash ()  "Insert \\-."  (interactive "*")
  (insert "\\\-"))

(defun lre-nroff-norw ()
  "Insert \\*?? - for norwegian character."
  (interactive "*")
  (insert "\\*\(")
  (lre-norw 1))


;;; ---------------------------------------------------------------
;;; SGML/HTML/XML/XSL/CSS

(setq nxml-attribute-indent 6
      nxml-auto-insert-xml-declaration-flag t
      nxml-child-indent 4
      nxml-sexp-element-flag t
      nxml-slash-auto-complete-flag t
)

(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)

(defvar lre-Xml-use-nxml (and (lre-memb 'nxml)
			       t))  ;; enabled!

(defvar lre-Xml-use-psgml (and (lre-memb 'psgml)
			       nil))  ;; disabled!

(defconst css-imenu-expression
  (list nil "^\\(.*\\)[{]\\s*$" 1)
  "Menu builder TODO")

(defun lre-css-mode()
  "CSS additions"
  (lre-colors)
  (setq imenu-generic-expression (list css-imenu-expression))
  (setq imenu-sort-function nil)
  (lre--imenu-add)
)

(defun lre-choose-sgml-mode()
  "Select the right SGML-mode"
  (interactive)
  (if lre-Xml-use-psgml
      (psgml-mode)
    (sgml-mode)))

(defun lre-choose-dtd-mode()
  "Select the right XML-mode for DTDs"
  (interactive)
  (if lre-Xml-use-psgml
      (pxml-mode)
    (xml-mode)))

(defun lre-choose-xml-mode()
  "Select the right XML-mode"
  (interactive)
  (if lre-Xml-use-nxml
      (progn
	(unless (or (fboundp 'nxml)
                    (lre-memb 'e23+))
	  (load-file (concat lre-slisp "/" LRE-nxml-pkg "/" "rng-auto.el")))
	(nxml-mode))
    (if lre-Xml-use-psgml
	(pxml-mode)
      (xml-mode))))


;; /// QUOTES.XML ///
(defvar lre-quotes-auth-hist nil "History of added authors.")
(defun lre-quote-change()
  "Last quote change"
  (goto-char (point-min))
  (when (search-forward "lastChange=\"" nil t)
    (lre-todays-date 6)
    (let ((p (point)))
      (if (search-forward "\"")
	  (delete-region p (1- (point)))))
    (goto-char (point-min))))

(defun lre-quote--name-id(name &optional pcode)
  "Derive ID from name"
  (if pcode pcode
    (let (code)
      (save-excursion
	(set-buffer (get-buffer-create "*IDfix*"))
	(erase-buffer)
	(insert (upcase name))
	(goto-char (point-min))
	(when (re-search-forward "\\(.*\\),\\(.*\\)" nil t)
	  (replace-match "\\2_\\1" t))
	(goto-char (point-min))
	(while (re-search-forward "[- ._,]+" nil t)
	  (replace-match "_" t t))
	(goto-char (point-max))
	(backward-word 1)
	(setq code
	      (lre-replace-in-string
	       (if (= (point) (point-min))
		   (upcase name)
		 (concat
		  (buffer-substring-no-properties (point) (point-max))
		  "_"
		  (buffer-substring-no-properties (point-min)
						  (1- (point)))))
	       "__+"
	       "_")))
      (setq code (read-from-minibuffer "ID: " code))
      code)))


(defsubst lre--quote-empty (txt)
  "Check if string is nil or empty"
  (or (not txt)
      (string= txt "")))

(defun lre-add-quote(&optional pquo pauth psource pwork pauthid)
  "Add quote to quotes.xml"
  ;; Mangler: bedre gjetting av ID
  ;;
  (interactive)
  (lre-quote-change)
  (let ((auth nil)
	(n (1+ (lre-how-many "<quote ")))
	name
	code
	s
	has-auth
	p)
    (when (search-forward "<quotes" nil t)
      (forward-line 1)
      (insert (format "<quote id='q%05d'>\n<qtext>\n" n))
      (insert (or pquo (read-from-minibuffer "Quote: ")))
      (insert "\n</qtext>\n<qmeta>\n")
      (setq has-auth (or (and (not pauth)
			      (y-or-n-p "By author? "))
			 (not (string= pauth ""))))
      (if has-auth
	  (progn
	    (setq name (or pauth (read-from-minibuffer "Name: " nil nil nil
						       'lre-quotes-auth-hist)))
	    (setq code (lre-quote--name-id name pauthid)
		  auth t)
	    (insert "<by aref='"
		    code
		    "'>"
		    name
		    "</by>")
	    (if (or pwork (y-or-n-p "From work? "))
		(if (or (not pwork) (not (string= pwork "")))
		    (insert "\n<work>"
			    (or pwork (read-from-minibuffer "Work: "))
			    "</work>"))))
	(insert "<source>"
		(or psource (read-from-minibuffer "Source: "))
		"</source>"))
      (insert "\n</qmeta>\n</quote>\n")
      (when auth
	(when (and (= 0 (lre-how-many (concat "id=." code)))
;;		   (y-or-n-p "Add author info? ")
		   (search-forward "<authors>"))
	  (setq p (1+ (point)))
	  (insert "\n<auth id=\""
		  code
		  "\"")
	  (setq s (read-from-minibuffer "Description (or blank): "))
	  (or (string= s "")
	      (insert " desc=\"" s "\""))
	  (setq s (read-from-minibuffer "From year (or blank): "))
	  (unless (string= s "")
	    (insert " from=\"" s "\"")
	    (setq s (read-from-minibuffer "To year (or blank): "))
	    (or (string= s "")
		(insert " to=\"" s "\""))
	    (and (y-or-n-p "Approximate? ")
		 (insert " approx=\"yes\"")))
	  (setq s (read-from-minibuffer "Alias (or blank): "))
	  (or (string= s "")
	      (insert " alias=\"" s "\""))
	  (insert ">" name "</auth>")
	  (when (search-forward "</authors>")
	    (beginning-of-line)
	    (sort-lines nil p (point))))
	(goto-char (point-max))
	(search-backward code)))))

(defun lre-add-qauthor(&optional pauth pdesc palias papprox pfrom pto nosort)
  "Add author to quotes.xml
PAUTH/PDESC: optional author name and description
PFROM/PTO: optional from/to year
PAPPROX: >0 approximate, <0 not approximate, nil/0: ask
PALIAS: optional alias"
  ;; Mangler: bedre gjetting av ID
  ;;
  (interactive)
  (lre-quote-change)
  (let ((qfrom
	 (if pfrom (if (stringp pfrom) pfrom (number-to-string pfrom)) ""))
	(qto
	 (if pto (if (stringp pto) pto (number-to-string pto)) ""))
	name
	code
	xcode
	approx
	s
	p)
    (setq name (or pauth (read-from-minibuffer "Name: " nil nil nil
					       'lre-quotes-auth-hist)))
    (setq code (lre-quote--name-id name))
    (setq xcode (concat "id=." code))
    (goto-char (point-min))
    (when (search-forward "<authors>" nil t)
      (if (re-search-forward xcode nil t)
	  (let ((dsc (concat (if palias (concat "alias=\"" palias "\" ") "")
			     (if pdesc  (concat "desc=\""  pdesc  "\" ") "")
			     (if qfrom  (concat "from=\""  qfrom  "\" ") "")
			     (if qto    (concat "to=\""    qto    "\" ") ""))))
	    (unless (y-or-n-p (concat name " " dsc))
	      (end-of-line)
	      (insert "<!-- " dsc " -->")))
	(setq p (1+ (point)))
	(insert "\n<auth id=\"" code "\"")
	(setq s (or pdesc (read-from-minibuffer "Description (or blank): ")))
	(or (string= s "")
	    (insert " desc=\"" s "\""))
	(setq s (if pfrom qfrom
		  (read-from-minibuffer "From year (or blank): ")))
	(unless (string= s "")
	  (insert " from=\"" s "\"")
	  (setq s (if pto qto (read-from-minibuffer "To year (or blank): ")))
	  (or (string= s "")
	      (insert " to=\"" s "\""))
	  (setq approx (cond ((eq papprox t)
			      t)
			     ((or (null papprox)
				  (= 0 papprox))
			      (y-or-n-p "Approximate? "))
			     ((> papprox 0) t)
			     (t nil)))
	  (if approx (insert " approx=\"yes\"")))
	(setq s (or palias (read-from-minibuffer "Alias (or blank): ")))
	(or (string= s "")
	    (insert " alias=\"" s "\""))
	(insert ">" name "</auth>")
	(when (and (not nosort)
		   (search-forward "</authors>" nil t))
	  (beginning-of-line)
	  (sort-lines nil p (point))
	  (goto-char (point-max))
	  (re-search-backward xcode))))))

(defun lre-add-qauth-fast (pauth pdesc
				 &optional pfrom pto dosort papprox palias)
  "For standard batch entry of authors"
  (lre-add-qauthor pauth pdesc (or palias "") (if papprox 1 -1) pfrom pto
		   (if dosort nil t)))

(defun lre-add-cookie(coo)
  "Add cookie to quotes.xml"
  (interactive "sCookie: ")
  (lre-quote-change)
  (let ((auth nil)
	(n (1+ (lre-how-many "<cookie ")))
	name
	code
	s
	p)
    (when (search-forward "<cookies" nil t)
      (forward-line 1)
      (insert (format "<cookie id='c%05d' tagline='yes'>" n))
      (insert coo)
      (insert "</cookie>\n"))))

(defun lre-xml-parse-toplevel (file)
  "Return list: (dtd root-element level1-elements level2-elements)
from FILE"
  (let ((xml (xml-parse-file file t)))
    (list (caar xml)
	  (caadr xml)
	  (delete-duplicates (mapcar 'car (cddadr xml)))
	  (delete-duplicates (apply  'append
				     (mapcar '(lambda (e)
						(mapcar 'car e))
					     (mapcar 'cddr (cddadr xml))))))))


(defun lre-xml-comment-def()
  "Set comment characters - why do I need these???"
  (interactive)
  (or (and (boundp 'comment-start)
	   comment-start)
      (setq comment-start "<!-- "))
  (or (and (boundp 'comment-end)
	   comment-end
	   (not (string= comment-end "")))
      (setq comment-end " -->")))

(defun lre-xml-get-doctype()
  "Search for document type"
  (save-excursion
    (save-match-data
      (let (dt)
        (goto-char (point-min))
        (search-forward "!DOCTYPE" nil t)
        (forward-word 1)
        (let ((p2 (point)))
          (backward-word 1)
          (setq dt (buffer-substring-no-properties (point) p2))
          (when (or (null dt) (string= dt "xml"))
            (goto-char (point-min))
            (when (search-forward-regexp "^[<][a-zA-Z]" nil t)
              (forward-char -1)
              (setq p2 (point))
              (forward-word 1)
              (setq dt (buffer-substring-no-properties p2 (point))))))
        dt))))

(defun lre--xml-mode (thismode)
  "XML add-ons."
  (lre-sgml-common thismode)
  (if (lre-memb 'xsltp) (xslt-process-mode))
  (let ((doctype (lre-xml-get-doctype)))
    (cond ((string-match "^rulez" doctype)
	   (setq imenu-generic-expression
		 '(
		   (nil "<title>\\(.*\\)</title>" 1)
                   ("Sect"
		    "[<]\\(\\(rule\\)?sect\\|appendix\\).*id=.\\([A-Za-z0-9.]+\\)"
		    3)
		   ("Rule"
		    "[<]\\(sub\\)?\\(tip\\|rule\\).*id=.\\([A-Za-z0-9.]+\\)"
		    3) )))
	  ((string= doctype "utility")
	   (setq imenu-generic-expression
		 '(
		   (nil "^\\s-*<\\(history\\|customers\\|strings\\|doc\\|notes\\|bugs\\|messages\\|filerefs\\|seealso\\|retstats\\|sysdoc\\|instdoc\\)" 1)
		   ("Files" "<file \\(.*\\)" 1))))
          ((string= doctype "project") ; Ant
	   (setq imenu-generic-expression
		 '(("Target" "^\\s-*<target\\([^>\n]+\\)" 1)
                   ("Property" "^\\s-*<property\\([^>\n]+\\)" 1)
                   )))
	  (t
	   (setq imenu-generic-expression
		 (cons '(nil
			 "^\\(   \\|\t\\)\\{0,3\\}<\\([^!/>]\\{2,16\\}\\)" 2)
		       imenu-generic-expression)))))
  (lre-xml-comment-def))

; ;   (defun xxy()
; ;     (interactive)
; ;     (rng-next-error 1)
; ;     (while (or (looking-at "span") (looking-at "</span>"))
; ;       (if (looking-at "</span>")
; ;   	(delete-char 7)
; ;         (backward-delete-char-untabify 2 nil)
; ;         (delete-char 5 nil))
; ;       (previous-line 1)
; ;       (rng-next-error 1)))


(defun lre-xml-mode ()
  "XML add-ons."
  (interactive)
  (lre--xml-mode 'xml)
  (define-key xml-mode-map "/" 'self-insert-command) )

(defvar lre--nxml-set-faces t "Should we set special faces?")

(defun lre-nxml-mode ()
  "XML add-ons."
  (interactive)
  (require 'nxml-mode)
  (when lre--nxml-set-faces
    (setq lre--nxml-set-faces nil)
    (custom-set-faces
 '(nxml-cdata-section-content-face ((t (:inherit nxml-text-face :background "khaki1"))))
 '(nxml-comment-content-face ((t (:foreground "goldenrod" :slant italic))))
 '(nxml-name-face ((((class color) (background light)) (:foreground "royal blue"))))
 '(nxml-processing-instruction-content-face ((t (:inherit nxml-delimited-data-face :foreground "forest green"))))
 '(nxml-processing-instruction-target-face ((t (:inherit nxml-name-face :foreground "forest green" :weight bold))))
     '(rng-error-face ((t (:underline "red")))))
    )
  (lre--xml-mode 'nxml))

(defun lre-pxml-mode ()
  "XML add-ons."
  (interactive)
  (lre--xml-mode 'pxml))

(defun lre-xml-scode(&optional pbeg pend)
  "Add scode-tags to all lines in region"
  (interactive "r")
  (save-excursion
    (goto-char pend)
    (if (bolp) (setq pend (1- pend)))
    (save-restriction
      (save-match-data
        (narrow-to-region pbeg pend)
        (goto-char (point-min))
        (replace-regexp "^" "<scode>")
        (goto-char (point-min))
        (replace-regexp "$" "</scode>")
        (goto-char (point-min))
        (replace-regexp "<scode></scode>" "<scode/>")
      ))))


(defun lre-html-lt (&optional pfx)
  "Insert a <, or if used twice in a row insert &lt;."
  (interactive "*P")
  (lre--double-keyfun pfx 'lre-html-lt "<" "&lt;"))

(defun lre-html-gt (&optional pfx)
  "Insert a >, or if used twice in a row insert &gt;."
  (interactive "*P")
  (if (or (eq major-mode 'psgml-mode)
	  (eq major-mode 'pxml-mode))
      (lre--double-keyfunfun pfx 'lre-html-gt ">" 'psgml-close-angle)
    (lre--double-keyfun pfx 'lre-html-gt ">" "&gt;")))

(defun lre-html-quo (&optional pfx)
  "Insert a quote, or if used twice in a row insert &quot;."
  (interactive "*P")
  (lre--double-keyfun pfx 'lre-html-quo "\"" "&quot;"))

(defun lre-html-amp (&optional pfx)
  "Insert an ampersand, or if used twice in a row insert &amp;."
  (interactive "*P")
  (lre--double-keyfun pfx 'lre-html-amp "&" "&amp;"))

(defun lre-html-lit-lt () "Insert a <." (interactive "*") (insert "&lt;"))
(defun lre-html-lit-gt () "Insert a >." (interactive "*") (insert "&gt;"))
(defun lre-html-lit-quo () "Insert a \"." (interactive "*") (insert "&quot;"))
(defun lre-html-lit-amp () "Insert a &." (interactive "*") (insert "&amp;"))

(defmacro lre--sgml-key (keys func &optional in-mode)
  (` (define-key (cond ((eq (, in-mode) 'pxml) pxml-mode-map)
		       ((eq (, in-mode) 'psgml) psgml-mode-map)
		       ((eq (, in-mode) 'nxml) nxml-mode-map)
		       ((eq (, in-mode) 'xml) xml-mode-map)
		       ((eq (, in-mode) 'xsl) xsl-mode-map)
		       (t  sgml-mode-map))
       (, keys)
       (, func))))

(defun lre-sgml-common (sub-mode)
  "Additions to (p)sgml/html/xml-mode setup."
  (let ((p-p (or (eq sub-mode 'pxml)
		 (eq sub-mode 'psgml))))
    (or (and (eq sub-mode 'nxml)
             (lre-not-memb 'nxml-hack))
	(lre-safe-require 'sgml-mode))
    (if p-p (lre-safe-require 'psgml))
    (make-local-variable 'indent-tabs-mode)
    (setq indent-tabs-mode nil)
    (auto-fill-mode 1)
    (tplsub-mode 1)
    (lre-set-local fill-column 72)
    (setq sgml-transformation       'downcase
	  sgml-font-lock-keywords-3 lre-font-lock-specials)
    (lre-colors)
    (setq imenu-generic-expression
	  '((nil
	     "<!\\(element\\|ELEMENT\\)[ \t\n]+\\([A-Za-zæÆøØåÅ][-A-Za-z.0-9_æÆøØåÅ]*\\)"
	     2)
	    ("Entities"
	     "<!\\(entity\\|ENTITY\\)[ \t\n]+\\([A-Za-zæÆøØåÅ][-A-Za-z.0-9_æÆøØåÅ]*\\)"
	     2)
	    ("Entities"
	     "<!\\(entity\\|ENTITY\\)[ \t\n]+\\(%?[ \t]*[A-Za-zæÆøØåÅ][-A-Za-z.0-9_æÆøØåÅ]*\\)"
	     2)
	    ("Attributes"
	     "<!\\(attlist\\|ATTLIST\\)[ \t\n]+\\([A-Za-zæÆøØåÅ][-A-Za-z.0-9_æÆøØåÅ]*\\)"
	     2)
	    ))
    (lre--imenu-add)
    (when (and p-p
	       (lre-memb-all 'flock 'psgml))
      (make-local-variable 'font-lock-defaults)
      (setq font-lock-defaults
	    '(("<\\([!?][a-z][-.a-z0-9]*\\)" 1 font-lock-keyword-face)
	      ("<\\(/?[a-z][-.a-z0-9]*\\)" 1 font-lock-function-name-face)
	      ("[&%][a-z][-.a-z0-9]*;?" . font-lock-variable-name-face)
	      ("<! *--.*-- *>" . font-lock-comment-face)
	      nil
	      t)))
    (when (lre-memb 'e22+)
      (set (make-local-variable 'font-lock-defaults)
           '((sgml-font-lock-keywords
              sgml-font-lock-keywords-1
              sgml-font-lock-keywords-2)
             nil t nil nil
             (font-lock-syntactic-keywords
              . sgml-font-lock-syntactic-keywords))))
    (when (lre-memb 'keys)
      (lre--sgml-key "\C-b"           'lre-html-break sub-mode)
      (lre--sgml-key "\C-c "          'lre-sgml-space sub-mode)
      (lre--sgml-key "\C-c."	      'lre-sgml-hellip sub-mode)
      (lre--sgml-key "\C-c-"	      'lre-sgml-dash sub-mode)
      (lre--sgml-key "\C-c<"	      'lre-sgml-tag sub-mode)
;;      (lre--sgml-key "\C-c?"	      'lre-sgml-help sub-mode)
      (lre--sgml-key "\C-c\C-q"	      'lre-add-quote sub-mode)
      (lre--sgml-key "\C-cc"	      'comment-dwim sub-mode)
      (lre--sgml-key "\C-cC"	      'lre-sgml-cdata sub-mode)
      (lre--sgml-key "\C-ce"	      'lre-sgml-ent sub-mode)
      (lre--sgml-key "\C-ch"	      'lre-xut-histrec sub-mode)
      (if p-p (lre--sgml-key "\C-c\C-i" 'psgml-insert-tag  sub-mode))
      (lre--sgml-key "\C-cm"	      'lre-sgml-marked sub-mode)
      (lre--sgml-key "\C-cp"	      'lre-sgml-pair sub-mode)
      (lre--sgml-key "\C-cq&"	      'lre-html-lit-amp sub-mode)
      (lre--sgml-key "\C-cq<"	      'lre-html-lit-lt sub-mode)
      (lre--sgml-key "\C-cq>"	      'lre-html-lit-gt sub-mode)
      (lre--sgml-key "\C-cq\""	      'lre-html-lit-quo sub-mode)
      (lre--sgml-key "\C-cs"	      'lre-xml-scode sub-mode)
      (lre--sgml-key "\C-ct"	      'lre-html-ins-table sub-mode)
      (lre--sgml-key [backspace]      'backward-delete-char-untabify sub-mode)
      )))

(defun lre-html-mode ()
  "HTML add-ons."
  (lre-sgml-common 'html)
  (when (lre-memb 'keys)
    (lre-set-norw html-mode-map 'lre-html-norw))
;;    (lre-brace-mode 0) ;; Used to be 2
  (setq tplsub-tmpl-help t)
  (lre-safe-require 'sb-html))

(defun lre-html-dup-url ()
  "Create A-tag from URL."
  (interactive)
  (let (s i)
    (setq s (thing-at-point 'url))
    (setq i (length s))
    (delete-char i)
    (insert "<a href=\"" s "\">" s "</a>")))

(defun lre-html-break ()
  "Insert break-tag"
  (interactive)
  (insert "<br/>")
  (newline-and-indent))

(defun lre-html-ins-table (r c)
  "Insert table tags."
  (interactive "nRows: \nnColumns: ")
  (save-excursion
    (let (ci
	  repl
	  (th "tbody")
	  (td "td")
	  (blanks (make-string (current-column) ?\ ))
	  (x-blanks (make-string lre-std-indent ?\ ))
	  (ri 0))
      (insert "<table")
      (setq repl (read-from-minibuffer "Width (or blank): " "100%"))
      (or (string= repl "") (insert " width=\"" repl "\""))
      (if (y-or-n-p "Border? ") (insert " border=\"1\""))
      (setq repl (read-from-minibuffer "Summary (or blank): "))
      (or (string= repl "") (insert " summary=\"" repl "\""))
      (setq repl (read-from-minibuffer "ID (or blank): "))
      (or (string= repl "") (insert " id=\"" repl "\""))
      (insert ">\n")
      (setq repl (read-from-minibuffer "Caption (or blank): "))
      (or (string= repl "") (insert blanks x-blanks "<caption>"
				    repl   "</caption>"))
      (while (< ri r)
	(cond ((and (= ri 0) (y-or-n-p "Special header row? "))
	       (setq th "thead"
		     td "th"))
	      (t (setq td "td")))
	(or (string= th "") (insert blanks x-blanks "<" th ">\n"))
	(insert blanks x-blanks x-blanks "<tr>\n")
	(setq ci 0)
	(while (< ci c)
	  (insert blanks x-blanks x-blanks x-blanks "<" td "></" td ">\n")
	  (incf ci))
	(insert blanks x-blanks x-blanks "</tr>\n")
	(unless (string= th "")
	  (insert blanks x-blanks "</" th ">\n")
	  (if (string= th "thead") (setq th "tbody") (setq th "")))
	(incf ri))
      (insert blanks x-blanks "</tbody>\n")
      (insert blanks "</table>"))))


(defun lre-html-norw ()  "Insert &??; - for norwegian character."
  (interactive "*")
  (insert "&") (lre-norw 3) (insert ";"))

(defun lre-sgml-norw ()  "Insert &??; - for norwegian character."
  (interactive "*")
  (insert "&") (lre-norw 1) (insert ";"))

(defvar lre-sgml-font-lock-keywords
  (append (list (list "<\\([!?] *[a-zA-Z0-9%#]+\\)" 1 (lre-font-lock-ref-face)))
          lre-font-lock-specials
	  '(("<\\([a-zA-Z0-9]+/?\\)" 1 font-lock-function-name-face)
	    ("<\\(/[a-zA-Z0-9]+\\)" 1 font-lock-keyword-face)
	    ("[&%][-.A-Za-z0-9#]+;?" . font-lock-type-face))))

(defsubst lre-sgml-keys ()  "Noop"  nil)
;;  (lre-brace-mode 0)  ;; Used to be 2

(defun lre--sgml-mode (thismode)
"Additions to `sgml-mode' setup."
  (unless (eq major-mode 'html-mode)
    (lre-sgml-common thismode)
    (setq sgml-validate-command  "sgmlfmt -v")
    (when (lre-memb 'flock)
      (lre-set-local font-lock-defaults '(lre-sgml-font-lock-keywords t t)))
    (when (lre-memb 'keys)
      (lre--sgml-key [?\C-c ?\C-<] 'lre-sgml-short-tag)
      (lre-set-norw (if lre-Xml-use-psgml psgml-mode-map
		      sgml-mode-map)
		    'lre-sgml-norw))
    (when (lre-memb 'hilit)
      (hilit-set-mode-patterns
       'sgml-mode
       '(
	 ("</[^>]*>" nil include)
	 ("<[^/>]+/[^/]/*>" nil defun)
	 ("[%&][A-Za-z0-9#]+;" nil define)
	 ("<!-- [^>]* -->\\|<!>" nil comment)
	 ("<[A-Za-z]\\([-.A-Za-z0-9= \n\t]\\|\"[^\"]*\"\\|'[^']*'\\)*[>/]?"
	  nil keyword)
	 ("<! *[^ >]*" nil crossref)
	 )
       nil 'case-insensitive)
      (lre-add-x-hilit 'sgml-mode))))

(defun lre-sgml-mode ()
  "Additions to `sgml-mode' setup."
  (lre--sgml-mode 'sgml))

(defun lre-psgml-mode ()
  "Additions to `psgml-mode' setup."
  (lre--sgml-mode 'psgml))

(defun lre-sgml-help ()
"Type M-C-v to scroll HELP window, C-x 1 to close.
 ====================
* SGML-MODE OVERVIEW *
 ====================

\\{sgml-mode-map}
"
  (interactive)
  (save-excursion
    (describe-function 'lre-sgml-help)))

(defvar lre-sgml-last-tag "p")

(defun lre-sgml-tag (tagn &optional pfx short-tag)
  "Insert SGML/XML tag/etag.
General insertion rules:
o  If no tagname is given, use the previous value.
o  Both short and full tags may be inserted, often depending on prefix.
o  If prefix is given, tag the previous word.
o  Or if the region is active, tag the region.
o  Otherwise insert tag at point."
  (interactive "*sTag: \nP")
  (let ((pp (point))
	(tag-name (if (string= tagn "")
		      lre-sgml-last-tag
		    (setq lre-sgml-last-tag tagn))))
    (cond
     (pfx         (forward-word -1))
     (mark-active (goto-char (region-beginning))))
    (insert "<" tag-name
	    (if short-tag (if (eq major-mode 'xml-mode) "/>" "/") ">"))
    (if (or pfx mark-active)
	(goto-char (+ 2 pp (length tag-name))))
    (setq pp (point))
    (insert (if short-tag (if (not (eq major-mode 'xml-mode)) "/" "")
	      (concat "</" tag-name ">")))
    (if (not (or pfx mark-active))
	(goto-char pp))))

(defun lre-sgml-short-tag (tagn &optional pfx)
  "Insert minimized SGML tag."
  (interactive "*sTag: \nP")
  (lre-sgml-tag tagn pfx t))

(defun lre-sgml-ent ()
  "Insert &\;."
  (interactive "*")
  (lre-text-ins "&;" -1))

(defun lre-sgml-marked ()  "Insert marked section."  (interactive "*")
  (lre-text-ins "<![  []]>" -5))

(defun lre-sgml-cdata ()  "Insert CDATA section."  (interactive "*")
  (lre-text-ins "<![CDATA[]]>" -3))

(defun lre-sgml-hellip ()
  "Insert hellipsis (...)."
  (interactive "*")
  (insert (if (eq major-mode 'sgml-mode)
	      "&hellip;"
	    "&#x2026;"))
  )

(defun lre-sgml-dash ()
  "Insert ndash (--)."
  (interactive "*")
  (insert (if (eq major-mode 'sgml-mode)
	      "&ndash;"
	    "&#x2013;")))

(defun lre-sgml-space (arg)  "Inserts nosp/nbsp."  (interactive "*P")
  (if (eq 'major-mode 'sgml-mode)
	  (insert "&n" (if arg "o" "b") "sp;")
    (insert (if arg "&nosp;" "&#xA0;"))))

(defun lre-sgml-quote (arg)  "Inserts quote."  (interactive "*P")
  (insert "&quot;")
  (if arg (save-excursion (insert "&quot;"))))

(defun lre-sgml-electric (words estr &optional stago etago stagc etagc)
"Inserts pair of strings."
  (let ((s1 (if stago stago "<"))
	(s2 (if etago etago "</"))
	(s3 (if stagc stagc ">"))
	(s4 (if etagc etagc (if stagc stagc ">")))
	(pp (point)))
    (if (/= words 1) (forward-word -1))
    (insert s1 estr s3)
    (if (/= words 1) (goto-char (+ pp (length s1) (length estr) (length s3))))
    (setq pp (point))
    (insert s2 estr s4)
    (if (= words 1) (goto-char pp))))

(defun lre-sgml-comment (&optional N)  "Insert comment."  (interactive "*p")
  (lre-sgml-electric N "--" "<!" " " " " ">"))

(defun lre-sgml-pair (ch &optional N)  "Insert pair of tags."
  (interactive "*sTag: \np")
  (lre-sgml-electric N ch))

(defun lre-sgml-size (dim)
  "Insert size change (negative prefix gives positive size...)."
  (interactive "*p")
  (let (pfx)
    (if (< dim 0) (progn
		    (setq pfx (concat "-" (int-to-string dim))))
      (setq pfx (concat "+" (int-to-string dim))))
    (insert "<size spec=\"" pfx "\">")))

(defun lre-sgml-glossent (pfx)
  "Insert <glossent>."
  (interactive "*P")
  (save-excursion
    (cond
     (pfx         (forward-word -1))
     (mark-active (goto-char (region-beginning))))
    (insert "<glossent word=\""))
  (insert "\">")
  (save-excursion
    (insert "</glossent>")))

(defvar lre-xsl-menu-base
  '("XSL"
      [ "Open line"  xsl-open-line   (not buffer-read-only) ]
      [ "Complete"   xsl-complete    (not buffer-read-only) ]
      [ "Insert tag" xsl-insert-tag  (not buffer-read-only) ]
      [ "Process"    xsl-process     (not buffer-read-only) ]
      [ "Comment"    xsl-comment     (not buffer-read-only) ])
  "XSL menu")


(defun lre-xsl-mode()
  "XSL additions"
  (tplsub-mode 1)
  (lre-colors)
  (lre-xml-comment-def)
  (if (lre-memb 'xsltp) (xslt-process-mode))
  (lre-set-local indent-line-function 'xsl-electric-tab)
  (setq imenu-generic-expression
	(cons '(nil "^\\(   \\|\t\\)\\{0,3\\}<\\([^!/>]\\{2,16\\}\\)" 2)
	      imenu-generic-expression))
  (when (lre-memb 'keys)
    (easy-menu-define lre-xsl-menu xsl-mode-map "XSL menu"
      lre-xsl-menu-base)
    (lre--sgml-key "\C-c "    'lre-sgml-space 'xsl)
    (lre--sgml-key "\C-cq\""  'lre-html-quo 'xsl)
    (lre--sgml-key "\C-cq&"   'lre-html-amp 'xsl)
    (lre--sgml-key [backspace] 'backward-delete-char-untabify 'xsl)
    (lre--sgml-key "\C-c<"    'lre-sgml-tag 'xsl)
    (lre--sgml-key (kbd "C-c C-<") 'xsl-insert-tag 'xsl)
    (lre--sgml-key "\C-c?"    'lre-sgml-help 'xsl)
    (lre--sgml-key "\C-ct"    'lre-html-ins-table 'xsl)
    (lre--sgml-key "\C-cc"    'comment-dwim 'xsl)
    (lre--sgml-key "\C-cm"    'lre-sgml-marked 'xsl)
    (lre--sgml-key "\C-cp"    'lre-sgml-pair 'xsl)))

(defun lre-wml-mode ()
  (lre-html-mode)
  (make-variable-buffer-local 'tplsub-tmpl-help)
  (setq tplsub-tmpl-help t))

(defun lre-htmlize-notab ()
  (save-excursion
    (goto-char (point-min))
    (let ((re-kill
	   " +span\\.lre-\\(tab\\|trailing-space\\)\\(-face\\)? {\n +background-color: [^\n]*\n +} /[*] lre-[^\n]*-face [*]/\n"))
      (while (re-search-forward re-kill nil t)
	(replace-match "" nil nil)))))

(defun lre-xut-awkfunc()
  "konv awk-funksjon til XML-function"
  (interactive)
  (beginning-of-line)
  (insert "<")
  (forward-word 1)
  (forward-char)
  (insert "name=\"")
  (forward-word 1)
  (insert "\" type=\"VOID\">\n    <shortdesc></shortdesc>\n    <fparam name=\"")
  (delete-char 1)
  (end-of-line)
  (backward-delete-char-untabify 3)
  (insert "\" type=\"STD\"></fparam>\n    <fbody>")
  (search-forward-regexp "^}")
  (beginning-of-line)
  (delete-char 1)
  (insert "    </fbody>\n</function>\n"))

(defun lre-xut-globuse (p-start p-end)
  "Convert to globuse-ref"
  (interactive "*r")
  (let ((name (buffer-substring-no-properties p-start p-end)))
    (delete-region p-start p-end)
    (insert "<globuse var=\"" name "\"")
    (if (y-or-n-p "Doc?")
	(save-excursion (insert "></globuse>"))
      (insert "/>"))))

(defun lre-xut-histrec(&optional comment)
  "Insert history record"
  (interactive "*")
  (goto-char (point-min))
  (search-forward "</history>")
  (beginning-of-line)
  (insert "        <histrec state=\"rel\">\n"
	  "            <version>"
	  (read-string "Version: " '("10." . 4))
	  "</version>\n"
	  "            <date>")
  (lre-todays-date 6)
  (insert "</date>\n"
	  "            <author comp=\"Mesan AS\">"
	  lre-fe-user
	  "</author>\n"
	  "            <description>")
  (save-excursion
    (insert "</description>\n        </histrec>\n"))
  (if comment (insert comment)))

;;; Markdown


(defvar tplsub-markup-tmpl-list
  (append '(
            ("a"    "[" | "](" ("URL")")")
            ("b"    "**" | "**")
            ("bq"   o "> ")
            ("c"    "`" | "`")
            ("co"   o "    ")
            ("ext"  "[" | "|" ("link" . "http://www.") "]")
            ("fn"   "{footnote}" | "{footnote}")
            ("h1 "   o "# " |" #")
            ("h2 "   o "## " |" ##")
            ("h3 "   o "### " |" ###")
            ("h4 "   o "#### " |" ####")
            ("h5 "   o "##### " |" #####")
            ("h6 "   o "###### " |" ######")
            ("hr"   "----------" n)
            ("i"    "_" | "_")
            ("img"  "![" | "](" ("file") ")")
            ("ol"   o (for "?antall items" ("1. " | n)))
            ("sub"  "<sub>" | "</sub>")
            ("sup"  "<sup>" | "</sup>")
            )
          tplsub-Xml-tmpl-list)
  "Templates for markup-mode.")

(defvar tplsub-markup-help-list '()
  "Help for markup-mode.")


(defconst markdown-imenu-expression
  (list nil "^[#]+\\(.*?\\)\\( +[#]+\\)?" 1)
  "Menu builder")


(defun lre--markdown-br()
  (interactive)
  (insert "<br />")
  (markdown-enter-key))

(defun lre-markdown-mode()
"Additions to `markdown-mode' setup."
  (lre-colors)
  (auto-fill-mode 0)
  (easy-menu-add-item
   nil
   '("Markdown")
   ["Insert <br/>" lre--markdown-br (not buffer-read-only)]
   "Insert footnote")
  (when (fboundp 'tplsub-register-mode)
      (tplsub-register-mode 'markup-mode
			    tplsub-markup-tmpl-list
			    tplsub-markup-help-list
			    'default  ; template regexp
			    'default  ; case sens.
			    'default  ; cont string
			    'default  ; expansion key
			    )
      (tplsub-register-mode 'gfm-mode
			    tplsub-markup-tmpl-list
			    tplsub-markup-help-list
			    'default  ; template regexp
			    'default  ; case sens.
			    'default  ; cont string
			    'default  ; expansion key
			    )
      (tplsub-mode t))
  (when buffer-file-name
    (add-hook 'after-save-hook
              'check-parens
              nil t))
  (make-local-variable 'imenu-generic-expression)
  (make-local-variable 'imenu-sort-function)
  (setq imenu-generic-expression (list markdown-imenu-expression))
  (setq imenu-sort-function nil)
  (lre--imenu-add)
  (when (lre-memb 'keys)
    (local-set-key (kbd "C-c C-<return>") 'lre--markdown-br)
    )
  )


(provide 'lre-doc)

;;; lre-doc.el ends here
