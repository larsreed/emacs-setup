;;; commbox.el  --- create boxed comment

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author:		Lars Reed <Lars@kalars.net>
;; Last-Modified:	$Date: 2005/11/09 14:01:08 $
;; Version:		$Id: commbox.el,v 1.5 2005/11/09 14:01:08 larsr Exp $
;; Keywords:		programming comments
;; Adapted-By:
;;
;;; History:
;;    $Log: commbox.el,v $
;;    Revision 1.5  2005/11/09 14:01:08  larsr
;;    Misc
;;
;;    Revision 1.4  2005/07/05 07:15:35  larsr
;;    Fix
;;
;;    Revision 1.3  2004/09/10 19:24:51  larsr
;;    keywords
;;
;;    Revision 1.2  2004/03/15 21:30:03  larsr
;;    LF
;;
;;    Revision 1.1.1.1  2004/03/15 20:50:35  larsr
;;    Initial RCS revision
;;
;;
;;; Commentary:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Code:
;; ========================================================================

(require 'tabify)

(defsubst commbox-line-length ()
  "Return length of current line"
  (let ((p (point)))
    (end-of-line)
    (prog1
	(- (point)
	   (progn
	     (beginning-of-line)
	     (point)))
      (goto-char p))))

(fset 'commbox-buffer-substring
      (if (fboundp 'buffer-substring-no-properties)
	  'buffer-substring-no-properties
	'buffer-substring))

(defvar commbox-buffer-name "*cbox*"
  "Name of commbox temp buffer")

(defvar commbox-comm-prefix nil
  "Buffer local comment line prefix")
(make-variable-buffer-local 'commbox-comm-prefix)

(defun commbox--create-buffer (p m)
  ;; Transfer text to *cbox* buffer and prepare it (remove blanks etc)
  (let* ((apoint (if (< p m) p m))
	 (bpoint (if (< p m) m p))
	 (s (commbox-buffer-substring apoint bpoint)))
    (save-excursion
      (set-buffer (get-buffer-create commbox-buffer-name))
      (erase-buffer)
      (insert s)
      (untabify (point-min) (point-max))
      (goto-char (point-min))
      (while (re-search-forward "[ \t\r]+$" nil t)  ; Superfluous blanks
	(replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "\n\n+\\'" nil t ) ; Extra lines at eob
	(replace-match "\n"))
      (set-buffer-modified-p nil))))

(defun commbox--max-len ()
  ;;  Return length of the longest line in the temp.buffer
  (let (thislen
	maxlen)
    (save-excursion
      (set-buffer (get-buffer commbox-buffer-name))
      (goto-char (point-min))
      (setq maxlen (commbox-line-length))
      (while (< (point) (point-max))
	(end-of-line 2)
	(setq thislen (commbox-line-length))
	(if (> thislen maxlen)
	    (setq maxlen thislen)))
      maxlen)))

(defun commbox--line-prefix ()
  ;; Return prefix string.  This is calculated from the first line in
  ;; the region (currently in the temp.buffer) according to the following
  ;; 1. Copy initial blanks.
  ;; 2. Copy comment characters.  See `commbox-comm-prefix'.
  ;; 3. Copy blanks after the prefix.
  (let (w1-len
	w2-len
	pfx)
    (save-excursion
      (set-buffer (get-buffer commbox-buffer-name))
      (goto-char (point-min))
      (setq w1-len (skip-chars-forward " \t"))
      (if (and commbox-comm-prefix
	       (looking-at commbox-comm-prefix))
	  (goto-char (match-end 0))
	(skip-chars-forward "^ \t"))
      (setq pfx (commbox-buffer-substring (+ (point-min) w1-len) (point))
	    w2-len (skip-chars-forward " \t")))
    (concat (make-string w1-len ?\ )
	    pfx
	    (make-string w2-len ?\ ))))


(defun commbox (&optional prompt from-pos to-pos rebox)
  "Frame for comment.
A line is considered to consist of a non-empty prefix, a gap, then text,
then trailing blanks.  The frame is made up of the same prefix and gap,
a dash is used for each text position.
E.g.:
    ;    -----------------
    ;    This is a comment
    ;    -----------------
If a prefix is given, the function wil prompt for another character or string
from which to build the frame.  It is also possible to the delete an already
existing frame."
  (interactive "*P")
  (let ((dele nil)	    ; delete preceding and succeeding line
	(cchr "-")	    ; comment-character
	(border "")	    ; border string
	iter		    ; counter
	pfx		    ; prefix
	tmpvar              ; temp variable
	overall		    ; max line line length
	restlen		    ; length excluding prefix
	(abs-start (point)) ; first character on first line
	(abs-end   (point)) ; last character on last line
	start-marker        ; marker at abs-start
	end-marker          ; marker at abs-end
	)
    (save-excursion
      ;;  Set variables according to how the function is called
      (cond ((and (interactive-p) mark-active)
	     (setq abs-start (mark)))
	    ((and from-pos to-pos)
	     (setq abs-start from-pos
		   abs-end   to-pos)))
      ;; Correct abs-start and -end
      (if (> abs-start abs-end)
	  (setq tmpvar    abs-start
		abs-start abs-end
		abs-end   tmpvar))
      (goto-char abs-start)
      (beginning-of-line)
      (setq abs-start    (point)
	    start-marker (point-marker))
      (goto-char abs-end)
      (cond ((= (point) abs-start) (end-of-line))
	    ((bolp) (forward-char -1))
	    (t (end-of-line)))
      (setq abs-end (point)
	    end-marker (point-marker))
      ;; Copy region to temp buffer
      (commbox--create-buffer abs-start abs-end)
      ;; When called with prefix, allow user to adjust parameters
      (if rebox
	  (setq dele t)
	(if prompt
	    (progn
	      (setq cchr (read-string "Box-character(s): " "-"))
	      (setq dele (y-or-n-p    "Delete lines above & below ")))))
      ;; Delete lines above & below
      (if dele
	  (progn
	    (goto-char (+ abs-end 1))
	    (if (not (eobp)) (progn
			       (beginning-of-line)
			       (kill-line 1)))
	    (goto-char (- abs-start 1))
	    (beginning-of-line)
	    (if (not (= (point) abs-start)) (kill-line 1))))
      ;; Deduce prefix and find the longest line in the region
      (setq overall (commbox--max-len)
	    pfx     (commbox--line-prefix)
	    restlen (- overall (length pfx)))
      (if (= overall 0) ()
	;; Create rest of border
	(if (= 1 (length cchr))
	    (setq border (make-string restlen (string-to-char cchr)))
	  (setq iter (/ restlen (length cchr)))
	  (while (> iter 0)
	    (setq border (concat border cchr)
		  iter   (1- iter))))
	;; Insert border
	(goto-char start-marker)
	(insert pfx border "\n")
	(goto-char end-marker)
	(insert "\n" pfx border)))))
;;    (end-of-line 2)

(defun commbox-quick ()
  "`commbox' with default comment character, deleting lines above & below."
  (interactive "*")
  (let ((mmrk (point-marker)))
    (if mark-active
	(commbox nil (min (point) (mark)) (max (point) (mark)) t)
      (commbox nil nil nil t))
    (goto-char mmrk)))

(provide 'commbox)

;;; commbox.el ends here
