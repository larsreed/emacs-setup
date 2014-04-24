;; stig-paren.el (Exp 1.22) -- highlight (un)matching parentheses
;; Copyright (C) 1993 Free Software Foundation, Inc.
;; Copyright (C) 1993, 1994, 1995 Tinker Systems
;;
;; Original Author: rms@gnu.ai.mit.edu
;; Massively hacked by: Jonathan Stigelman <Stig@hackvan.com>
;; Maintainer: Jonathan Stigelman <Stig@hackvan.com>
;; Keywords: languages, faces

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;
;; Purpose of this package:
;;
;;   This package highlights matching parens (or whole sexps) for easier
;;   editing of source code, particularly lisp source code.
;;
;; Installation:
;;
;; (cond (window-system
;;        (require 'stig-paren)
;;	  ;; XEmacs
;;        (global-set-key [(control leftparen)] 'stig-paren-toggle-dingaling-mode)
;;        (global-set-key [(control rightparen)] 'stig-paren-toggle-sexp-mode))
;;       (t
;;        (setq blink-matching-paren t)))
;;
;; Bugs:  You find 'em, I squash 'em.
;;
;;        M-x stig-paren-submit-feedback RET
;;

;; Normally, stig-paren-command-hook gives priority to matching open parens
;; and looks to the position just before point for closing parens.  This is
;; intuitive if you frequently use forward-sexp (M-C-f) and backward-sexp
;; (M-C-b) to maneuver around in lisp code.

;; In stig-paren-dingaling-mode, priority is placed upon highlighting the
;; parenthesis matching whatever is underneath the cursor.

;; Different faces are used for color is used for mismatched parens.  If the
;; (mis)matching paren is offscreen, then a message is sent to the modeline.

;; In stig-paren-sexp-mode, entire S-expressions are highlighted instead of
;; just matching parens.

;; stig-paren.el,v
;; Revision 1.22  1996/01/02 19:31:16  stig
;; fix for escaped parens
;;
;; Revision 1.21  1995/11/30 19:31:56  stig
;; fix to deal with new fsfmacs syntax table
;;
;; Revision 1.20  1995/03/09 23:27:45  stig
;; FSF only release. For XEmacs, use paren.el in the 19.12 distribution, or send me mail.
;;
;; Revision 1.19  1995/02/04 23:48:54  stig
;; foo
;;

;;; User Options:

(defvar paren-message-offscreen t
  "* Display message if matching open paren is offscreen.")

(defvar paren-ding-unmatched nil
  "* Make noise if the cursor is at an unmatched paren.

If T, then typing or passing over an unmatched paren will ring the bell.
If NIL, then the bell will not ring even if an unmatched paren is typed.
If neither T or NIL, then the bell will not ring when the cursor moves over
unmatched parens but will ring if one is typed.")

(defvar paren-dingaling-mode nil
  "* Set to T if you'd like paren to look for closing parens after the point
instead of before the point.  Normally paren looks for close parens that
you've just typed so that you know what the paren that you've just typed
matches.

Normally priority is placed upon highlighting the opening parenthesis of the
sexp before point.  Consequently you see this behavior:

   ((   ))   or   ((   ))   or   ((   ))   or   ((   ))(   )
        P          H    P        H      P       H      P

In paren-dingaling-mode, you see this:

   ((   ))   or   ((   ))   or   ((   ))   or   ((   ))(   )
        P          H    P        H      P              P   H")

(defvar paren-sexp-mode t
  "* Non-nil causes paren-command-hook to highlight whole S-expressions.")

(defvar paren-match-face (if (or (not (eq 'x window-system))
				 (x-display-color-p))
			     'highlight
			   'underline)
  "* Matching parens (which are balanced parens) are shown in this face.  Bold
and italic faces (except for the standard ones) tend to flake out when you
change fonts...  You're responsible for maintaining the unique display
properties of this face.")

(defvar paren-mismatch-face (if (not (and (eq 'x window-system)
					  (x-display-color-p)))
				'modeline
			      (let ((fn 'paren-mismatch-face))
				(make-face fn)
				(set-face-background fn "DeepPink")
				fn))
  "* Mismatching parens (not to be confused with unbalanced parens) are shown
in this face.  Bold and italic faces (except for the standard ones) tend to
flake out when you change fonts...  You're responsible for maintaining the
unique display properties of this face.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To submit bug reports...

(eval-when-compile (require 'reporter))

(defun stig-paren-submit-feedback ()
  "Submit via mail a bug report on stig-paren"
  (interactive)
  (require 'reporter)
  (and (y-or-n-p "Do you really want to submit a report on stig-paren? ")
       (reporter-submit-bug-report
	"Jonathan Stigelman <Stig@hackvan.com>"
	"stig-paren.el (Exp 1.22)"
         '(paren-sexp-mode
	   paren-message-offscreen
	   paren-dingaling-mode
	   paren-ding-unmatched
	   paren-mismatch-face
	   paren-match-face
	   )
	 nil nil "Hey Stig, do you do anything besides hack emacs?\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code:

(defvar stig-paren-overlay nil)

(defvar paren-message-suppress nil
  "used to suppress messages from the same position so that other messages
can be seen in the modeline.")
(make-variable-buffer-local 'paren-message-suppress)

(defun stig-paren-toggle-dingaling-mode (arg)
  "Toggle paren-dingaling-mode, force off with negative arg"
  (interactive "P")
  (setq paren-dingaling-mode (if (numberp arg)
				 (> arg 0)
			       (not paren-dingaling-mode)))
  (message "Dingaling mode is %s"
	   (if paren-dingaling-mode "ON, you're a dingaling ;-)" "OFF")))

(defun stig-paren-toggle-sexp-mode (arg)
  "Toggle paren-sexp-mode, force off with negative arg"
  (interactive "P")
  (setq paren-sexp-mode (if (numberp arg)
			    (> arg 0)
			  (not paren-sexp-mode)))
  (message "Sexp mode is %s"
	   (if paren-sexp-mode "ON" "OFF")))

(defsubst pos-visible-in-window-safe (pos)
  "safe version of pos-visible-in-window-p"
  (condition-case nil
      (pos-visible-in-window-p pos)
      (args-out-of-range nil)))

(defsubst stig-paren-set-overlay (st en)
  "Move stig-paren-overlay to the region START .. END.  Create if necessary."
  (if stig-paren-overlay
      (move-overlay stig-paren-overlay st en (current-buffer))
    (setq stig-paren-overlay (make-overlay st en))))

;; Find the place to show, if there is one,
;; and show it until input arrives.
(defun stig-paren-command-hook ()
  "This highlights matching parentheses.

See the variables:
  paren-message-offscreen   use modeline when matchingparen is offscreen?
  paren-ding-unmatched	 make noise when passing over mismatched parens?
  paren-dingaling-mode	 match parens under cursor instead of before?
  paren-sexp-mode		 highlight s-expressions instead of just parens?

and
  paren-match-face, paren-mismatch-face"

  ;; I suppose I could check here to see if a keyboard macro is executing,
  ;; but I did a quick empirical check and couldn't tell that there was any
  ;; difference in performance

  (let ((oldpos (point))
	(pface nil)			; face for paren...nil kills the overlay
	pos dir mismatch)

    (save-excursion
      (if paren-dingaling-mode
	  ;; dingaling mode highlighting
	  (cond ((eq (char-syntax (following-char)) ?\))
		 (setq dir -1 oldpos (1+ oldpos))
		 (forward-char 1))
		((eq (char-syntax (following-char)) ?\()
		 (setq dir 1))
		((eq (char-syntax (preceding-char)) ?\))
		 (setq dir -1)))
	;; normal highlighting
	(cond ((eq (char-syntax (preceding-char)) ?\))
	       (setq dir -1))
	      ((eq (char-syntax (following-char)) ?\()
	       (setq dir 1))))

      ;; if the parenthesis that we noticed is escaped with a backslash,
      ;; then forward/backward-sexp will not treat it as a parenthesis and
      ;; things below will fuck up.  I hate FSF emacs!!!  Use XEmacs.
      ;; I like being able to usually figure out how regular expressions work.
      (and dir
	   (save-excursion 
	     (and (eq dir -1) (forward-char -1)) ; position on paren
	     (eq (char-syntax (preceding-char)) ?\\)) ; see if it's escaped
	   (setq dir nil))

      (if (and dir
	       (save-restriction
		 ;; Determine the range within which to look for a match.
		 (if blink-matching-paren-distance
		     (narrow-to-region
		      (max (point-min)
			   (- (point) blink-matching-paren-distance))
		      (min (point-max)
			   (+ (point) blink-matching-paren-distance))))

		 ;; Scan across one sexp within that range.
		 (condition-case nil
		     (setq pos (scan-sexps (point) dir))
		   (error (and (eq this-command 'self-insert-command)
			       (not (null paren-ding-unmatched))
			       (ding))))))

	  ;; See if the "matching" paren is the right kind of paren
	  ;; to match the one we started at.
	  (progn
	    (let* ((beg (min pos oldpos))
		   (end (max pos oldpos))
		   (mparen (matching-paren (char-after beg))))
	      (setq mismatch
    		    (and mparen
			 (/= (char-syntax (char-after beg)) ?\\)	
			 (/= (char-syntax (char-after beg)) ?\$)
			 (/= (char-after (1- end)) mparen)))
	      (if paren-sexp-mode
		  (stig-paren-set-overlay beg end)))
	    (and mismatch
		 (or paren-ding-unmatched
		     (eq this-command 'self-insert-command))
		 (ding))
	    (setq pface (and (or paren-sexp-mode
				 (pos-visible-in-window-safe pos))
			     (if mismatch
				 paren-mismatch-face
			       paren-match-face)))
	    (cond ((pos-visible-in-window-safe pos)
		   (and (not paren-sexp-mode)
			(stig-paren-set-overlay (- pos dir) pos))
		   (setq paren-message-suppress nil))
		  ((and paren-message-offscreen
			(eq dir -1)
			(not (eq paren-message-suppress (point)))
			(not (window-minibuffer-p (selected-window))))
		   (setq paren-message-suppress (point))
		   (save-excursion
		     (goto-char pos)
		     (message "%s %s"
			      (if mismatch "MISMATCH:" "Matches")
			      ;;
			      ;; if there's stuff on this line preceding the
			      ;; paren, then display text from beginning of
			      ;; line to paren.
			      ;;
			      ;; If, however, the paren is at the beginning
			      ;; of a line, then skip whitespace forward and
			      ;; display text from paren to end of the next
			      ;; line containing nonspace text.
			      ;;
			      ;; If paren-backwards-message gravity were
			      ;; implemented, then perhaps it would reverse
			      ;; this behavior and look to the previous line
			      ;; for meaningful context.
			      ;; 
			      (if (save-excursion
				    (skip-chars-backward " \t")
				    (not (bolp)))
				  (concat (buffer-substring
					   (progn (beginning-of-line) (point))
					   (1+ pos)) "...")
				(buffer-substring
				 pos (progn
				       (forward-char 1)
				       (skip-chars-forward "\n \t")
				       (end-of-line)
				       (point)))))))
		  (t (setq paren-message-suppress nil)))
	    ))
      ;; put the right face on the overlay
      (and pface (overlay-put stig-paren-overlay 'face pface))
      )))

(defun stig-paren-delete-overlay ()
  "Pre-command-hook to delete the overlay owned by stig-paren"
  (cond (stig-paren-overlay
	 (condition-case nil
	     (delete-overlay stig-paren-overlay)	     
	   (error nil))
	 (setq stig-paren-overlay nil))))

;; kill off the competition, er, uh, eliminate redundancy...
(setq blink-matching-paren nil)
(setq post-command-hook (delq 'show-paren-command-hook post-command-hook))
(setq pre-command-hook (delq 'blink-paren-pre-command pre-command-hook))
(setq post-command-hook (delq 'blink-paren-post-command post-command-hook))

(add-hook 'post-command-hook 'stig-paren-command-hook)
(add-hook 'pre-command-hook 'stig-paren-delete-overlay)

(provide 'stig-paren)
(provide 'paren)

;;; stig-paren.el ends here

; Local Variables:
; byte-optimize: t
; byte-compile-compatibility: nil
; End:
