;;; wiki-mode.el  --- Simple Confluence wiki editing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author:		Lars Reed <Lars@kalars.net>
;; Version:		X
;; Keywords:		Wiki Confluence
;;
;;; History:
;;
;;; Commentary:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file is NOT part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Code:

(require 'derived)
(require 'cl)
(require 'hi-lock)

(defvar tplsub-wiki-tmpl-list
  '(
    ("TITLE" (yn "Title" (":title=" ("text"))) "}")
    ("TITLE2" (yn "Title" ("|title=" ("text"))))
    ("ALIGN" (yn "Alignment" ("|align="
                              (opt "Align" (("l" . "left") ("c" . "center") ("r" . "right")))
                              )))
    ("a"    "{anchor:" ("bookmark") "}")
    ("att"  "[" | "|^" ("file") "]")
    ("b"    "*" | "*")
    ("bb"   "{*}" | "{*}")
    ("bq"   o "bq. " |)
    ("blue" "{color:blue}" | "{color}")
    ("c"    "{{" | "}}")
    ("cjava" o "{code:java" (= . "TITLE2") "}" n | n "{code}")
    ("cm"   "{html}<!-- " | " -->{html}" |)
    ("co"   "{code}" n | n "{code}")
    ("code" "{code:"
            (opt "Lang" (("j" . "java") ("s" . "scala") ("b" . "bash")
                         ("q" . "sql") ("x" . "xml") ("#" . "cpp")
                         ("c" . "css") ("a" . "javascript") ("n" . "none")))
            (= . "TITLE2")
            (yn "lineno" ("|linenumbers=true"))
            (yn "emacstheme" ("|theme=emacs"))
            "}" n | n "{code}")
    ("cols" o "{section" (yn "border" ":border") "}" n
            (for "?antall kolonner"
                 ("{column:width=" ("width (%)") "%}" n | n "{column}" n))
            "{section}")
    ("col2" o "{section}" n
              "{column}" n | n
              "{column}" n
              "{column}" n | n
              "{column}" n
              "{section}")
    ("csh"  o "{code:bash|linenumbers=true|theme=Emacs" (= . "TITLE2")  "}"
            n | n "{code}")
    ("csql"  o "{code:sql" (= . "TITLE2") "}" n | n "{code}")
    ("cxml"  o "{code:xml" (= . "TITLE2") "}" n | n "{code}")
    ("dispfn" "{display-footnotes}")
    ("esito" "Esito: " (= . "q"))
    ("exc"  "{excerpt}" | "{excerpt}")
    ("exp"  "{expand:" ("title") "}" n | n "{expand}")
    ("ext"  "[" | "|" ("link" . "http://www.") "]")
    ("fn"   "{footnote}" | "{footnote}")
    ("green"  "{color:green}" | "{color}")
    ("h1"   o "h1. " |)
    ("h2"   o "h2. " |)
    ("h3"   o "h3. " |)
    ("h4"   o "h4. " |)
    ("h5"   o "h5. " |)
    ("h6"   o "h6. " |)
    ("hm"   "{hmmm:" (var . lre-user-sign) "}" | "{hmmm}")
    ("hr"   "----" n)
    ("i"    "_" | "_")
    ("ii"   "{_}" | "{_}")
    ("img"  "!" ("file") (yn "Thumbnail" ("|thumbnail")) (= . "ALIGN") "!")
    ("int"  "[#" ("bookmark") "]")
    ("info" o "{info" (= . "TITLE") n | n "{info}")
    ("j"    "[" ("project") "-" ("issue") "@JIRA]")
    ("lnk"  "[" | "|xxxx]")
    ("mail"  "[mailto:" ("link") "]")
    ("mac"  "{"  ("name") | "}" | "{" ("name") "}")
    ("me"   "[jeg|~" (= . "user") "]")
    ("nb"   o "{note" (= . "TITLE") n | n "{note}")
    ("note" (= . "nb"))
    ("ol"   o (for "?antall items" ("# " | n)))
    ("p"    "{" | "}")
    ("pane" o "{panel" (= . "TITLE") n | n "{panel}")
    ("pg"   "h1. " | n n (= . "exc") n (= . "toc") n n "h2. " |)
    ("pg3"  "{pagetree:root=@self|startDepth=1|expandCollapseAll=true|searchBox=true|excerpt=true}")
    ("pre"  o "{noformat}" n | n "{noformat}")
    ("q"    "{quote}" | "{quote}")
    ("qc"   "{qc}" | "{qc}")
    ("red"  "{color:red}" | "{color}")
    ("sub"  "~" | "~")
    ("sup"  "^" | "^")
    ("tab"  "||" (for "?antall kolonner" (| "||")) n
             (for "?antall rader"
                  ("|" (for "?antall kolonner" (| "|")) n)))
    ("tip"  o "{tip" (= . "TITLE") n | n "{tip}")
    ("toc"  o "{panel}{toc:minLevel=2}{panel}")
    ("todo" "{todo}" | "{todo}")
    ("tn" "{todo-note:" ("Hvem") (yn "Jira" ("|jira=" ("issue")))
          "}" | "{todo-note}")
    ("u" "[~" ("Hvem") "]" )
    ("ul" o (for "?antall items" ("* " | n)))
    ("warn" o "{warning" (= . "TITLE") n | n "{warning}")
    ("xi" "{xinclude:" ("Side") "}")
    )
  "Templates for wiki-mode.")


(defvar tplsub-wiki-help-list '()
  "Help for wiki-mode.")

(defconst wiki-pdf-reader
  "C:\\Program Files\\Adobe\\Reader 8.0\\Reader\\acrord32.exe"
  "Program for reading PDF files")

(defconst wiki-pdf-help
  "c:\\usr\\lib\\wiki\\confluence.pdf"
  "Doc file")

(defun wiki-help-file()
  "Show help file"
  (interactive)
  (start-process "wikihelp" nil wiki-pdf-reader wiki-pdf-help))

(defvar bold 'bold)
(defvar italic 'italic)
(defvar hi-blue 'hi-blue)
(defvar hi-black-hb 'hi-black-hb)

(defconst wiki-font-lock-keywords
  (list (cons "[{]+[^}]*[}]*" 'font-lock-constant-face)
        (cons "^[-*#]+ " 'font-lock-keyword-face)
        (cons "^h[0-9][.].*$" 'hi-black-hb)
        (cons "[[][^]]+[]]" 'hi-blue)
        (list "[^\\]\\([*][^*\n\\]+[*]\\)" 1 'bold)
        (list "[{][*][}][^*\n\\]+[{][*][}]" 'bold)
        (list "[^\\]\\([_][^_\n\\]+[_]\\)" 1 'italic)
        (list "[{][_][}][^*\n\\]+[{][_][}]" 'italic)
        (list "[^\\]\\([-][^-\n\\]+[-]\\)" 1 'font-lock-type-face)
        )
  "Fontification in wiki-mode")

(defconst wiki-imenu-expression
  (list nil "^h\\([0-9][.]\\s-+.*\\)$" 1)
  "Menu builder")

(defun wiki-demote (&optional pfx)
  "* Increase heading levels, from this point onwards"
  (interactive "*p")
  (save-mark-and-excursion
    (let ((amount (if pfx pfx 1))
          level)
      (while (re-search-forward "^h\\([1-9]\\)[.] " nil t)
        (setq level (+ amount
                       (string-to-number
                        (buffer-substring (match-beginning 1) (match-end 1)))))
        (replace-match (concat "h" (number-to-string level) ". ") nil t)))))

(defun wiki-promote (&optional pfx)
  "* Decrease heading levels, from this point onwards"
  (interactive "*p")
  (wiki-demote (if pfx (- pfx) -1)))


(define-derived-mode wiki-mode text-mode "Wiki"
  "wiki-mode is an extension of text-mode,
containing customizations for wiki editing."
  (define-key wiki-mode-map [menu-bar wiki]
    (cons "WIKI" (make-sparse-keymap "WIKI")))
  (define-key wiki-mode-map [menu-bar wiki scratch]
    '("New scratchpad" . wiki-scratch))
  (define-key wiki-mode-map [menu-bar wiki help]
    '("Syntax help" . wiki-help-file))
  (define-key wiki-mode-map [menu-bar wiki demote]
    '("Increase heading level" . wiki-demote))
  (define-key wiki-mode-map [menu-bar wiki promote]
    '("Decrease heading level" . wiki-promote))
  (define-key wiki-mode-map [menu-bar wiki code]
    '("Markup as code" . wiki-code))
  (define-key wiki-mode-map [menu-bar wiki mac]
    '("Surround by macro" . wiki-mac))
  (define-key wiki-mode-map [menu-bar wiki sign]
    '("Signature" . wiki-sign))
  (define-key wiki-mode-map [?\C-c ?-] 'wiki-promote)
  (define-key wiki-mode-map [?\C-c ?+] 'wiki-demote)
  (define-key wiki-mode-map [?\C-c ?c] 'wiki-code)
  (define-key wiki-mode-map [?\C-c ?m] 'wiki-mac)
  (define-key wiki-mode-map [?\C-c ?s] 'wiki-sign)
  (define-key wiki-mode-map [C-return] 'wiki-list-enter)
  (when (fboundp 'tplsub-register-mode)
      (tplsub-register-mode 'wiki-mode
			    tplsub-wiki-tmpl-list
			    tplsub-wiki-help-list
			    'default  ; template regexp
			    'default  ; case sens.
			    'default  ; cont string
			    'default  ; expansion key
			    )
      (tplsub-mode t))
  ;; Does not work, see below (auto-fill-mode -1)
  (require 'font-lock)
  (setq font-lock-defaults '(wiki-font-lock-keywords))
  (turn-on-font-lock)
  (make-local-variable 'imenu-generic-expression)
  (make-local-variable 'imenu-sort-function)
  (setq imenu-generic-expression (list wiki-imenu-expression))
  (setq imenu-sort-function nil)
  (lre--imenu-add)
  )

(defun wiki-list-enter (arg)
  "Start a new line with the same prefix as the current line (with positive or
negative argument, increase or reduce indent by 1)."
  (interactive "*P")
  (let (p pfx)
    (beginning-of-line)
      (setq p (point))
      ;; (message "%s" p)
      (forward-word)
      (backward-word)
      (setq pfx (buffer-substring p (point)))
      (end-of-line)
      (newline)
      (insert pfx)
      (unless (null arg)
        (backward-char)
        (while (looking-at "[:white:]")
          (backward-char))
        (if (> (prefix-numeric-value arg) 0)
            (insert (buffer-substring (1- (point)) (point)))
          (delete-char -1))
        (end-of-line))))

(defun wiki-code (beg-region end-region &optional pfx)
  "Marks region as code"
  (interactive "*r\nP")
  (let* ((org-txt (buffer-substring beg-region end-region))
         (new-txt (if pfx (lre-replace-in-string org-txt "[\\]") org-txt)))
    (delete-region beg-region end-region)
    (insert (if pfx "{qc}" "{{")
            new-txt
            (if pfx "{qc}" "}}"))))

(defun wiki-mac (beg-region end-region mac-name &optional escape)
  "Marks region as code"
  (interactive "*r\nsMakronavn: \nP")
  (let* ((org-txt (buffer-substring beg-region end-region))
         (new-txt (if escape (lre-replace-in-string org-txt "[\\]") org-txt)))
    (delete-region beg-region end-region)
    (insert "{" mac-name "}"
            new-txt
            "{" mac-name "}")))

(defun wiki-sign (reg-start reg-end)
  "Change name to user ID (Mesan)."
  (interactive "*r")
  (if (use-region-p)
      (progn
        (goto-char reg-start)
        (insert "[~")
        (downcase-region reg-start reg-end)
        (goto-char (+ 2 reg-end))
        (insert "]"))
    (insert-before-markers "[~")
    (downcase-word 2)
    (insert "]"))
  (backward-word)
  (right-char))

(defun wiki-scratch ()
  "Create a temp.buffer in wiki mode"
  (interactive)
  (switch-to-buffer
   (get-buffer-create "*wiki-scratch*"))
  (wiki-mode))

;; Cannot turn off fill mode in main function
(add-hook 'wiki-mode-hook
          (function (lambda ()
                      (auto-fill-mode 0))))


(provide 'wiki-mode)

;; wiki-mode.el ends here
