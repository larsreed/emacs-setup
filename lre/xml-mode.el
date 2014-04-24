;; Simple XML-mode
;; Author:		Lars Reed <Lars@kalars.net>
;; Last-Modified:	$Date: 2004/09/10 19:24:52 $
;; Version:		$Id: xml-mode.el,v 1.2 2004/09/10 19:24:52 larsr Exp $

(require 'sgml-mode)
(require 'derived)
(require 'xml-lite)

(defcustom sgml-xml-validate-command nil
  "*The command to validate an XML document.
The file name of current buffer file name will be appended to this,
separated by a space."
  :type 'string
  :group 'sgml)

(defvar outline-regexp nil)
(defvar outline-heading-end-regexp nil)
(defvar outline-level nil)

(define-derived-mode xml-mode sgml-mode "XML" nil
 (define-key xml-mode-map [menu-bar sgml xml-html]
   (cons "HTML"  (lookup-key html-mode-map [menu-bar html])))
 (make-local-variable 'sgml-tag-alist)
 (make-local-variable 'sgml-face-tag-alist)
 (make-local-variable 'sgml-tag-help)
 (make-local-variable 'outline-regexp)
 (make-local-variable 'outline-heading-end-regexp)
 (make-local-variable 'outline-level)
 (make-local-variable 'sentence-end)
 (make-local-variable 'sgml-validate-command)
 (if sgml-xml-validate-command
     (setq sgml-validate-command sgml-xml-validate-command))
 (setq sgml-tag-alist (append sgml-tag-alist html-tag-alist))
 (or sgml-face-tag-alist (setq sgml-face-tag-alist html-face-tag-alist))
 (setq sgml-tag-help (append sgml-tag-help html-tag-help))
 (if (boundp 'outline-regexp)
     (or outline-regexp (setq outline-regexp "^.*<[Hh][1-6]\\>")))
 (if (boundp 'outline-heading-end-regexp)
     (or outline-heading-end-regexp (setq outline-heading-end-regexp
					  "</[Hh][1-6]>")))
 (if (boundp 'outline-level)
     (or outline-level (setq outline-level (lambda ()
					     (char-after (1- (match-end 0)))))))
 (xml-lite-mode 1)
 )

(provide 'xml-mode)
