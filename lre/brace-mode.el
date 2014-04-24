;;; brace-mode.el  --- using braces/norwegian characters

;;; Copyright (as of yet) (C) 1995-2004 Lars Reed

;; Author:		Lars Reed <Lars@kalars.net>
;; Last-Modified:	$Date: 2007/06/19 20:06:54 $
;; Version:		$Id: brace-mode.el,v 1.3 2007/06/19 20:06:54 larsr Exp $
;; Keywords:		keys setup european

;;; Commentary:

;; ....

;;; Code:

;;; ========================================================================
;; Mode trivia...

(defconst brace-mode-version "1.14"
  "Revision of brace-mode.el")

(defvar brace-mode-name ""
  "Name used in mode-line.")
(make-variable-buffer-local 'brace-mode-name)

(defconst brace-mode-names '((0 . " ")  ; Original bindings
			     (1 . "p")  ; Programming, swap ISO/ascii
			     (2 . "d")) ; Docs, rotate all
  "Symbolic marker for mode-line")

(defvar brace-mode nil
  "Non-nil when brace-mode is activated (and not using defaults).")
(make-variable-buffer-local 'brace-mode)

(defvar brace-mode-keys-hook nil
  "Functions to call right after loading key bindings.")

(defconst brace-mode-keyset
  (cond ((eq system-type 'ms-dos) 1)
	((and (eq system-type 'hpux) (eq window-system 'x)
	      (= emacs-major-version 19)) 2)
	(t 0)))

;; Display minor mode name
(or (assq 'brace-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons '(brace-mode brace-mode-name)
                minor-mode-alist)))

;; ======================================================================
;; Key definitions

(defconst brace-key-list-1
  (cond ((= brace-mode-keyset 1)
	 '([145]   [155]   [134]   [146]   [157]   [143]))
	((= brace-mode-keyset 2)
	 '([215]   [214]   [212]   [211]   [210]   [208]))
	(t
	 '([230]   [248]   [229]   [198]   [216]   [197])))
  "ISO-latin keys")

(defconst brace-key-list-2
      '("{"    "|"    "}"     "["     "\\"    "]")
  "ASCII keys")

(defconst brace-key-list-3
  (cond ((= brace-mode-keyset 1)
	 '([3 145] [3 155] [3 134] [3 146] [3 157] [3 143]))
	((= brace-mode-keyset 2)
	 '([3 215] [3 214] [3 212] [3 211] [3 210] [3 208]))
	(t
	 '([3 230] [3 248] [3 229] [3 198] [3 216] [3 197])))
  "Control keys")

(defvar brace-key-list
  (append brace-key-list-1
	  brace-key-list-2
	  brace-key-list-3)
  "List of keys affected by brace-mode")

(defconst brace-list-len (length brace-key-list))
					; No. of entries in brace-key-list
(defconst brace-no-modes (length brace-mode-names))
					; No. of different modes

(defvar brace-binding-list nil
  "List of actual key bindings for brace-mode.")
(make-variable-buffer-local 'brace-binding-list)

(defvar brace-mode-no 0
  "brace-binding-list relocation")
(make-variable-buffer-local 'brace-mode-no)


; ======================================================================

(defun brace-get-bindings ()
  ;; Fill brace-binding-list with current bindings
  (setq brace-binding-list
	(mapcar
	 (function (lambda (key)
		     (let ((k-b (key-binding key t)))
		       (cond ((eq k-b 'self-insert-command) key)
			     ((eq k-b 'brace-key) key) ; avoids recursion
			     ((null k-b) nil) ; just to be explicit...
			     (t k-b)))))
	 brace-key-list)))

(defun brace-set-bindings ()
  ;; Assign brace-key to all supported keys
  (mapcar (function (lambda (key)
		      (local-set-key key 'brace-key)))
	  brace-key-list))

(defun brace-set-mode (N)
  ;; Reset brace-mode-no
  (setq brace-mode-no   (mod (abs (if (null N)
				      (1+ brace-mode-no)
				    N))
			     brace-no-modes))
  (setq brace-mode-name (format "{%s}"
				(cdr-safe (assq brace-mode-no
						brace-mode-names)))
	brace-mode      (> brace-mode-no 0)))


;;; ======================================================================

;;;###autoload
(defun brace-mode (arg)
  "Toggle brace minor mode."
  (interactive "P")
  (if brace-binding-list ()
    (brace-get-bindings)
    (brace-set-bindings)
    (run-hooks 'brace-mode-keys-hook))
  (brace-set-mode (if (not arg)
		      nil
		    (prefix-numeric-value arg)))
  (force-mode-line-update))

;;;###autoload
(defun brace-key ()
  "Execute proper keybinding for brace-key
- see variable \'brace-binding-list\'"
  (interactive)
  (let* ((this-keys (this-command-keys))
	 (elno (- brace-list-len
		 (length (member this-keys brace-key-list))))
		 ;; I don't know any other way to get the index of the current
		 ;; key in the list...
	 (offset (+ elno
		    (cond ((= brace-mode-no 0)
			   0)
			  ((member this-keys brace-key-list-1)
			   (* 2 brace-no-modes brace-mode-no))
			  ((member this-keys brace-key-list-2)
			   (* -2 brace-no-modes))
			  ((= brace-mode-no 2)
			   (* -2 brace-no-modes))
			  (t 0))))
	elt)
    (setq elt (if (= elno brace-list-len)
		  nil
		  (nth offset brace-binding-list)))
    (cond ((stringp elt)  (insert elt))	            ; ASCII
	  ((vectorp elt)  (insert (aref elt 0)))    ; ISOlatin
	  ((commandp elt)
	   (let ((key (nth offset brace-key-list))
		 last-command-event)
	     (setq last-command-event
		   (cond ((vectorp key) (aref key (1- (length key))))
			 ((stringp key) (string-to-char key))
			 (t key)))
	     (call-interactively elt)))             ; Local binding
	  (t		  (error "unknown binding in brace-mode...")))))


; ======================================================================
; Misc. functions (mostly unrelated...)

(defun brace-subswap-norw (s-from s-to &optional reverse)
  (goto-char (point-min))
  (if reverse
      (query-replace s-to s-from nil)
    (query-replace s-from s-to nil)))

;;;###autoload
(defun brace-swap-norwegian (pfx)
  "With prefix - convert from braces to 8-bit.
Otherwise - convert from 8-bit to braces."
  (interactive "P")
  (save-excursion
    (brace-subswap-norw "æ" "{"  pfx)
    (brace-subswap-norw "ø" "|"  pfx)
    (brace-subswap-norw "å" "}"  pfx)
    (brace-subswap-norw "Æ" "["  pfx)
    (brace-subswap-norw "Ø" "\\" pfx)
    (brace-subswap-norw "Å" "]"  pfx)))

(defun brace-swap-norwegian-rev()
  "brace-swap-norwegian with prefix"
  (interactive)
  (brace-swap-norwegian t))

;;;###autoload
(defun brace-swap-norwegian-win (pfx)
  "With prefix - convert from Windows to 8-bit.
Otherwise - convert from 8-bit to Windows."
  (interactive "P")
  (save-excursion
    (brace-subswap-norw "æ" "‘" pfx)
    (brace-subswap-norw "ø" "›" pfx)
    (brace-subswap-norw "å" "†" pfx)
    (brace-subswap-norw "Æ" "Æ"    pfx)
    (brace-subswap-norw "Ø" "Ø"    pfx)
    (brace-subswap-norw "Å" "Å"    pfx)))

;;;###autoload
(defun brace-swap-norwegian-html (pfx)
  "With prefix - convert from Windows to HTML entitites.
Otherwise - convert from HTML to Windows."
  (interactive "P")
  (save-excursion
    (brace-subswap-norw "¦" "&aelig;" pfx)
    (brace-subswap-norw "¸" "&oslash;" pfx)
    (brace-subswap-norw "¥" "&aring;" pfx)
    (brace-subswap-norw "†" "&Aelig;" pfx)
    (brace-subswap-norw "˜" "&Oslash;" pfx)
    (brace-subswap-norw "…" "&Aring;" pfx)
    (brace-subswap-norw "Ã¦" "&aelig;" pfx)
    (brace-subswap-norw "Ã¸" "&oslash;" pfx)
    (brace-subswap-norw "Ã¥" "&aring;" pfx)
    (brace-subswap-norw "Ã†" "&Aelig;" pfx)
    (brace-subswap-norw "Ã˜" "&Oslash;" pfx)
    (brace-subswap-norw "Ã…" "&Aring;" pfx)
    (brace-subswap-norw "‘" "&aelig;" pfx)
    (brace-subswap-norw "›" "&oslash;" pfx)
    (brace-subswap-norw "†" "&aring;" pfx)
    (brace-subswap-norw "Æ" "&Aelig;" pfx)
    (brace-subswap-norw "Ø" "&Oslash;" pfx)
    (brace-subswap-norw "Å" "&Aring;" pfx)))


(defun brace-swap-norwegian-html-rev(&optional r-beg r-end)
  "brace-swap-norwegian-html with prefix"
  (interactive "r")
  (brace-swap-norwegian-html t))


(defun brace-swap-ml (pfx &optional r-beg r-end)
  "With prefix - convert to <>& from entities, otherwise from ent. to <>&"
  (interactive "P\nr")
  (save-excursion
    (save-restriction
      (if (and r-beg r-end)
	  (narrow-to-region r-beg r-end))
      (brace-subswap-norw "&" "&amp;"  pfx)
      (brace-subswap-norw "<" "&lt;"  pfx)
      (brace-subswap-norw ">" "&gt;"  pfx))))

(defun brace-swap-ml-rev(&optional r-beg r-end)
  "brace-swap-ml with prefix"
  (interactive "r")
  (brace-swap-ml t r-beg r-end))


; ======================================================================

(provide 'brace-mode)

;;; brace-mode.el ends here
