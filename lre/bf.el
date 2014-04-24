(defun hack-ddf (bufone bufall dialog)
  (interactive
   "bBuffer with new picture \nbBuffer with old DDF \nsDialog name: ")
  (save-excursion
    (let (buftxt
	  start-p
	  no-chars-new
	  no-lines-new
	  l-delta
	  p-delta
	  end-p
	  (rexp1 (concat "start dlg '" dialog "'"))
	  (rexp2 "^ *$"))
      ;;
      ;; Copy new picture
      ;;
      (set-buffer bufone)		; Buffer with new
      (goto-char (point-min))
      (re-search-forward rexp1)		; Find start dlg
      (beginning-of-line)
      (setq start-p (point))
      (re-search-forward rexp2)		; Find next empty line
      (beginning-of-line)
      (setq buftxt (buffer-substring start-p (point)) ; Copy dialog definition
	    no-chars-new (- (point) start-p)
	    no-lines-new (count-lines start-p (point)))
      ;;
      ;; Remove old picture
      ;;
      (set-buffer bufall)		; DDF-file
      (goto-char (point-min))
      (re-search-forward rexp1)		; Start dlg
      (beginning-of-line)
      (setq start-p (point))
      (re-search-forward rexp2)
      (beginning-of-line)
      (setq end-p (point)		; Compute difference
	    p-delta (- no-chars-new (- (point) start-p))
	    l-delta (- no-lines-new (count-lines start-p end-p)))
      (kill-region start-p end-p)	; Remove old def
      (goto-char start-p)
      (insert buftxt)			; And insert new
      ;;
      ;; Recalculate offsets
      ;;
      (goto-char (point-min))		; Find this dialog in the offset list
      (setq rexp1 (concat "^ " dialog " .*,.*;"))
      (re-search-forward rexp1)
      (beginning-of-line 2)		; For each of the following dialogs
      (while (not (looking-at " start "))
	(end-of-line)
	(forward-word -2)
	(setq start-p (point))
	(forward-word 1)
	(setq buftxt (buffer-substring start-p (point)))
	(delete-region start-p (point)) ; Remove old byte offset
	(insert (format "%d" (+ (string-to-int buftxt) p-delta l-delta)))
					; Add size diff + 1 byte per line
	(forward-char 1)
	(setq start-p (point))
	(forward-word 1)
	(setq buftxt (buffer-substring start-p (point)))
	(delete-region start-p (point))	; Remove line offset
	(insert (format "%d" (+ (string-to-int buftxt) l-delta)))
					; Add line diff
	(beginning-of-line 2))))
  (switch-to-buffer bufall))
