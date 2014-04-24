;;; cf-mode.el  --- Addtitions to C-mode for cf-files

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author:		Lars Reed <Lars@kalars.net>
;; Last-Modified:	$Date: 2007/06/19 20:06:54 $
;; Version:		$Id: cf-mode.el,v 1.3 2007/06/19 20:06:54 larsr Exp $
;; Keywords:		programming C Sysdul grape
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

(eval-and-compile
  (require 'sysdul)
  (require 'cc-mode))

(require 'derived)

(defun cf-define-hook (name &optional P)
  "Definer hook, med ifdef-par og kommentarer"
  (interactive (list (read-from-minibuffer "Hooknavn: ")
		     current-prefix-arg))
  (beginning-of-line)
  (insert "#ifndef " name "\n"
	  "#define " name " @@\\\n")
  (insert (if P "\t"
	    (concat "\t; <\""  name "\" @@\\\n\t")))
  (save-excursion
    (if P (insert "\n#endif\n")
      (insert " @@\\\n"
	      "\t; \"" name "\">\n"
	      "#endif\n"))))

(defun cf-linefeed (P)
  "Legger inn @@\\, linjeskift og indent"
  (interactive "P")
  (if P (insert "@@\\")
    (insert " @@\\")
    (newline-and-indent)))

(defun cf-grape-proc-empty ()
  "Erstatter Tad______Empty i proc-filer"
  (interactive)
  (save-excursion (sysdul-fix-white))
  (save-excursion (query-replace-regexp "^$" (concat (make-string 60 ?\ )
						     "Tad______Empty"))))

(defun cf-grape-insert-empty ()
  "Setter inn Tad______Empty i proc-filer"
  (interactive)
  (insert (concat (make-string 60 ?\ ) "Tad______Empty\n")))

(defun cf-grapeify-region (r-begin r-end &optional invert do-tab do-quo)
  "Konverterer mellom grapeformat og vanlig Sysdul.
Markert område (eller arg. 1 & 2 - R-BEGIN & R-END fra Lisp) konverteres,
med prefiks (interaktivt, eller 3. arg INVERT fra Lisp) konverteres fra
grape til Sysdul, ellers fra Sysdul til grape.
Konvertering til grapeformat består i:
     1: legg inn TAB i starten av linja (hvis DO-TAB er gitt, eller
        aksjonen bekreftes interaktivt)
     2: avslutt alle linjer med @@\\, høyrejustert
     3: legg inn  høyrejustert på alle tomme linjer.
     4: erstatt \' med % (hvis DO-QUO er gitt, eller bekreftes interaktivt)
Konvertering til Sysdulformat inverterer disse."
  (interactive "r\nP")
  (let* (empty-line
	 (cont-col   72)
	 (cont-symb  " @@\\")
	 (cont-rx    "[ \t]*@@\\\\[ \t]*$")
	 (cont-rx2   (regexp-quote cont-symb))
	 (empty-symb "Tad______Empty")
	 (empty-rx   "^[ \t]*Tad_[_]+Empty")
	 (toggle-tab (or do-tab
			 (and (interactive-p)
			      (y-or-n-p
			       "Legge til  tabulator i starten av linja? "))))
	 (toggle-quo (or do-quo
			 (and (interactive-p)
			      (y-or-n-p
			       (concat "Konvertere enkle anførselstegn "
				       "til prosenttegn? "))))))
    (setq empty-line (concat
		      (make-string (- cont-col (+ (length cont-symb)
						  (length empty-symb)
						  (if toggle-tab 8 0)
						  1))
				   ?\ )
		      empty-symb))
    (save-excursion
      (goto-char r-begin)
      (beginning-of-line)
      (setq r-begin (point))
      (goto-char r-end)
      (if (bolp)
	  (end-of-line 0)
	(end-of-line))
      (setq r-end (point))
      (save-restriction
	(narrow-to-region r-begin r-end)
	(goto-char (point-min))
	(save-excursion
	  (if invert
	      (while (re-search-forward empty-rx nil t)
		(replace-match ""))
	    (while (re-search-forward "^[ \t]*$" nil t)
	      (replace-match empty-line))))
	(if toggle-quo
	    (save-excursion
	      (if invert
		  (while (re-search-forward "%" nil t)
		    (replace-match "\'"))
		(while (re-search-forward "\'" nil t)
		  (replace-match "%")))))
	(if toggle-tab
	    (save-excursion
	      (if invert
		  (while (re-search-forward "^\t" nil t)
		    (replace-match ""))
		(while (re-search-forward "^" nil t)
		  (replace-match "\t")))))
	(save-excursion
	  (if invert
	      (while (re-search-forward cont-rx nil t)
		(replace-match ""))
	    (end-of-line)
	    (while (< (point) (point-max))
	      (indent-to-column (- cont-col (length cont-symb)) 1)
	      (insert cont-symb)
	      (end-of-line 2))))
	(if invert ()
	  (goto-char (point-max))
	  (beginning-of-line)
	  (if (looking-at (concat "\t" empty-symb))
	      (let ((kill-whole-line nil))
		(kill-line)))
	  (goto-char (- (point-max) 4))
	  (if (looking-at cont-rx2) (delete-char (length cont-symb))))))))

(defun cf-ungrapeify-region (r-begin r-end)
  "Interaktivt interface til `cf-grapeify-region', s.d."
  (interactive "r")
  (cf-grapeify-region r-begin
		      r-end
		      t
		      (y-or-n-p "Fjerne tabulator i starten av linja? ")
		      (y-or-n-p
		       "Konvertere prosenttegn til enkle anførselstegn? ")))

(defun cf-regrapeify-region ()
  "Som `cf-ungrapeify-region' etterfulgt av `cf-grapeify-region'."
  (interactive)
  (let ((b (mark-marker))
	(e (point-marker)))
    (cf-grapeify-region b e t   nil nil)
    (cf-grapeify-region b e nil nil nil)))

(defsubst cf-make-extent (beg end)
  "Make overlay"
  (cond ((fboundp 'make-extent) (make-extent beg end))
	((fboundp 'make-overlay) (make-overlay beg end))
	(t nil)))

(defsubst cf-delete-extent (ovl)
  "Delete overlay"
  (cond ((fboundp 'delete-extent) (delete-extent ovl))
	((fboundp 'delete-overlay) (delete-overlay ovl))
	(t nil)))

(defsubst cf-make-face (sym name)
  (cond ((fboundp 'make-extent) (make-face sym name t))
	((fboundp 'make-overlay) (make-face sym))
	(t nil)))


(defun cf-check-grape (&optional no-ret)
  "Sjekk - delvis - grape-syntaks..."
  (interactive "P")
  (let* (s-pos
	 reg-extent
	 (no-faces (not (or (fboundp 'make-overlay) (fboundp 'make-extent))))
	 (use-dialog-box nil)
	 (cont t)
	 (cont-mark "@@\\")
	 (alt-mark  (concat "\\W" (regexp-quote cont-mark))))
    (if no-faces ()
      (cf-make-face 'grape-check "temporary face")
      (if (and (facep 'font-lock-warning-face)
	       (> emacs-major-version 20))
	  (copy-face 'font-lock-warning-face 'grape-check)
	(copy-face 'default 'grape-check))
      (invert-face 'grape-check))
    (push-mark)
    (or no-ret (goto-char (point-min)))
    (setq cont (search-forward cont-mark nil t))
    (while (and cont
		(not (eobp)))
      (end-of-line)
      (forward-char (- (length cont-mark)))
      (save-excursion
	(beginning-of-line)
	(setq s-pos (point)))
      (if (not (or (looking-at (regexp-quote cont-mark))
		   (looking-at alt-mark)))
	  (progn
	    (beginning-of-line 2)
	    (if (not (looking-at "#endif"))
		(unwind-protect
		    (progn
		      (if no-faces ()
			(save-excursion
			  (end-of-line)
			  (setq reg-extent (cf-make-extent s-pos (point))))
			(if (fboundp 'set-extent-priority)
			    (set-extent-priority reg-extent 50))
			(if (fboundp 'set-extent-face)
			    (set-extent-face reg-extent 'grape-check)
			  (overlay-put reg-extent 'face 'grape-check)))
		      (redraw-display)
		      (setq cont (and (y-or-n-p "Fortsette leting ")
				      (search-forward cont-mark nil t))))
		  (if (not no-faces) (cf-delete-extent reg-extent))
		  t)
	      (setq cont (search-forward cont-mark nil t)))))
      (beginning-of-line 2)))
  (message ""))


(define-derived-mode cf-mode c-mode "Cf"
  "cf-mode is an extension of c-mode,
containing additional commands only applicable for grape cf-files.
It also includes some sysdul-mode features."
  (require 'sysdul)
  (if (fboundp 'add-menu-button)
      (progn   ;  XEmacs
	(add-menu-button '("Grape") ["Insert hook definition"
				     cf-define-hook t])
	(add-menu-button '("Grape") ["Substitute Tad______Empty"
				     cf-grape-proc-empty t])
	(add-menu-button '("Grape") ["Insert Tad______Empty"
				     cf-grape-insert-empty t])
	(add-menu-button '("Grape") ["Grapeify region"
				     cf-grapeify-region (mark)])
	(add-menu-button '("Grape") ["Ungrapeify region"
				     cf-ungrapeify-region (mark)])
	(add-menu-button '("Grape") ["Regrapeify region"
				     cf-regrapeify-region (mark)])
	(add-menu-button '("Grape") ["Check grape syntax"
				     cf-check-grape t])
	)
    (progn
	(define-key cf-mode-map [menu-bar cf]
	  (cons "Grape" (make-sparse-keymap "Grape")))
	(define-key cf-mode-map [menu-bar cf check-syntax]
	  '("Check grape syntax" . cf-check-grape))
	(define-key cf-mode-map [menu-bar cf ungrapeify]
	  '("Ungrapeify region" . cf-ungrapeify-region))
	(define-key cf-mode-map [menu-bar cf grapeify]
	  '("Grapeify region" . cf-grapeify-region))
	(define-key cf-mode-map [menu-bar cf regrapeify]
	  '("Re-grapeify region" . cf-regrapeify-region))
	(define-key cf-mode-map [menu-bar cf empty]
	  '("Substitute Tad______Empty" . cf-grape-proc-empty))
	(define-key cf-mode-map [menu-bar cf empty2]
	  '("Insert Tad______Empty" . cf-grape-insert-empty))
	(define-key cf-mode-map [menu-bar cf insert]
	  '("Insert hook definition" . cf-define-hook))))
  (define-key cf-mode-map "\C-cX"    'cf-check-grape)
  (define-key cf-mode-map [kp-enter] 'cf-linefeed)
  ;; Skru av spesielle C-taster i cf-filer
  (define-key cf-mode-map "," 'self-insert-command)
  (define-key cf-mode-map ";" 'self-insert-command)
  (define-key cf-mode-map ":" 'self-insert-command)
  (define-key cf-mode-map "{" 'self-insert-command)
  (define-key cf-mode-map ")" 'self-insert-command)
  (define-key cf-mode-map "(" 'self-insert-command)
  (define-key c-mode-map "\C-cg" 'cf-grapeify-region)
  (define-key c-mode-map "\C-cd" 'cf-define-hook)
  (define-key c-mode-map [f12 C] 'sysdul-mode)
  (setq imenu-generic-expression sysdul-imenu-expression)
  (if (fboundp 'c-toggle-auto-hungry-state)
      (c-toggle-auto-hungry-state -1))
  (if (fboundp 'tplsub-register-mode)
      (tplsub-register-mode 'cf-mode
			    tplsub-sysdul-tmpl-list
			    tplsub-sysdul-help-list
			    'default  ; template regexp
			    'default  ; case sens.
			    " : @@\\" ; cont string
			    'default  ; expansion key
			    )))

(provide 'cf-mode)

;; cf-mode.el ends here
