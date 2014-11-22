;;; efc.el -- Emacs Foundation Classes
;; $Revision: 1.9 $ $Date: 2002/03/29 12:40:27 $ 

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Kinnucan
;; Keywords: lisp, tools, classes

;; Copyright (C) 2001, 2002 Paul Kinnucan.

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
;; Boston, MA 02111-1307, US
;;; Commentary:

;; This package contains a set of eieio-based foundation classes
;; for Emacs.

;; Please send bug reports and enhancement suggestions
;; to Paul Kinnucan at <paulk@mathworks.com>

;; See end of this file for change history.

;;; Code:

(require 'eieio)
(require 'wid-edit)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;; 
;; Dialog Class                                                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass efc-dialog ()
  ((title     :initarg :title
	      :type string
	      :initform "Dialog"
	      :documentation
	      "Title of dialog")
   (buf       :initarg :buf
	      :type buffer
	      :documentation
	      "Dialog buffer")
   (initbuf   :initarg :initbuf
	      :type buffer
	      :documentation
	      "Buffer from which dialog was called.")
   )
  "Super class of EFC dialogs."
  )

(defmethod initialize-instance ((this efc-dialog) &rest fields)
  "Constructor for dialog."
  ;; Call parent initializer.
  (call-next-method))


(defmethod efc-dialog-create ((this efc-dialog)))

(defmethod efc-dialog-ok ((this efc-dialog))
  "Invoked when the user clicks the dialog's okay button. The
default method kills the dialog buffer."
  (kill-buffer (current-buffer)))

(defmethod efc-dialog-cancel ((this efc-dialog))
  "Invoked when the user clicks the dialog's Cancel button. The
default method kills the dialog buffer."
  (delete-window)
  (set-buffer (oref this initbuf))
  (pop-to-buffer (oref this initbuf))
  (kill-buffer (oref this buf)))

(defmethod efc-dialog-show ((this efc-dialog))
  (oset this initbuf (current-buffer))

  (oset this buf (get-buffer-create (oref this title)))
  (set-buffer (oref this buf))

  (efc-dialog-create this)

  (widget-put
   (widget-create 
    'push-button
    :notify 
    (lambda (button &rest ignore) (efc-dialog-ok (widget-get button :dialog)))
    "Ok")
   :dialog this)

  (widget-insert "  ")

  (widget-put
   (widget-create 
    'push-button
    :notify (lambda (button &rest ignore) (efc-dialog-cancel (widget-get button :dialog)))
    "Cancel")
   :dialog this)

   (use-local-map widget-keymap)
   (widget-setup)

  ;; Position cursor over OK button.
  ;; (forward-line 0)

  (goto-char (point-min))

  (pop-to-buffer (oref this buf)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;; 
;; Option Dialog                                                              ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass efc-option-dialog (efc-dialog)
  ((options        :initarg :options
		   :documentation
		   "Options from from which to choose.")		  
   (radio-buttons  :initarg :radio-buttons
		   :documentation
		   "Buttons for selecting options.")
   (text           :initarg :text
		   :type string
		   :initform "Select option."
		   :documentation
		   "Text to be inserted at top of dialog.")
   (selection      :initarg :selection
		   :documentation
		   "Option chosen by the user."))
   "This dialog allows a user to choose one of a set of OPTIONS by clicking
a radio button next to the option. The dialog sets SELECTION to the option
chosen by the user when the user selects the OK button on the dialog. This
dialog uses recursive edit to emulate a modal dialog.")

(defmethod initialize-instance ((this efc-option-dialog) &rest fields)
  "Dialog constructor."
  (call-next-method))

(defmethod efc-dialog-create ((this efc-option-dialog))
  (widget-insert (oref this text))
  (widget-insert "\n\n")
  (oset this radio-buttons
 	(widget-create
 	 (list
	  'radio-button-choice
	  :value (car (oref this options))
	  :args (mapcar 
		 (lambda (x) 
		   (list 'item x)) 
		 (oref this options)))))
  (widget-insert "\n"))

(defmethod efc-dialog-show ((this efc-option-dialog))
  "Shows the options dialog buffer. After showing the dialog buffer,
this method invokes recursive-edit to emulate the behavior of a modal
dialog. This suspends the current command until the user has selected
an option or canceled the dialog. See `efc-dialog-ok' and
`efc-dialog-cancel' for more information."
  (call-next-method)
  (recursive-edit))


(defmethod efc-dialog-ok ((this efc-option-dialog))
  "Invoked when the user selects the OK button on the options
dialog. Sets the :selection field of THIS to the option chosen by the
user, kills the dialog buffer, and exits recursive-edit mode."
  (oset this 
	selection 
	(widget-value (oref this radio-buttons)))
  (delete-window)
  (set-buffer (oref this initbuf))
  (pop-to-buffer (oref this initbuf))
  (kill-buffer (oref this buf))
  (exit-recursive-edit))

(defmethod efc-dialog-cancel ((this efc-option-dialog))
  "Invoked when the user clicks the dialog's Cancel button.  Invokes
the default cancel method, sets the :selection field of THIS to nil,
and then exits recursive edit mode."
  (call-next-method)
  (oset this selection nil)
  (exit-recursive-edit))

(defun efc-query-options (options &optional prompt)
  "Ask user to choose among a set of options."
  (let ((dialog
	 (efc-option-dialog
	  "option dialog"
	  :text (if prompt prompt "Select option:")
	  :options options)))
    (efc-dialog-show dialog)
    (oref dialog selection)))


;; The following code is a patch that implements Richard Stallman's fix
;; for the following error that occurs only in Emacs 21.1.1.
;;
;; Debugger entered--Lisp error: (wrong-type-argument window-live-p #<window 66>)
;;      select-window(#<window 66>)
;;      exit-recursive-edit()
;; This replacement macro fixes the problem with exit-recursive-edit on Emacs 21.
;; You'll have to recompile wid-edit.el with it.
;; (defmacro save-selected-window (&rest body)
;;   "Execute BODY, then select the window that was selected before BODY.
;; However, if that window has become dead, don't get an error,
;; just refrain from switching to it."
;;   `(let ((save-selected-window-window (selected-window)))
;;      (unwind-protect
;; 	 (progn ,@body)
;;        (if (window-live-p save-selected-window-window)
;; 	   (select-window save-selected-window-window)))))


(if (and (not (featurep 'xemacs))
	 (or
	  (string-match "21.1" (emacs-version))
	  (string-match "21.2" (emacs-version))))
    (progn
      ;; Need to load wid-edit first to ensure that
      ;; it does not get loaded after this patch and
      ;; hence override the patch.
      (require 'wid-edit)

      ;; Patched version of save-selected-window.
      (defmacro save-selected-window (&rest body)
	"Execute BODY, then select the window that was selected before BODY.
However, if that window has become dead, don't get an error,
just refrain from switching to it."
	`(let ((save-selected-window-window (selected-window)))
	   (unwind-protect
	       (progn ,@body)
	     (if (window-live-p save-selected-window-window)
		 (select-window save-selected-window-window)))))

      ;; Redefine widget-button-click to use the patched 
      ;; version of save-selected-window
      (defun widget-button-click (event)
	"Invoke the button that the mouse is pointing at."
	(interactive "@e")
	(if (widget-event-point event)
	    (let* ((pos (widget-event-point event))
		   (button (get-char-property pos 'button)))
	      (if button
		  ;; Mouse click on a widget button.  Do the following
		;; in a save-excursion so that the click on the button
		  ;; doesn't change point.
		  (save-selected-window
		    (save-excursion
		      (mouse-set-point event)
		      (let* ((overlay (widget-get button :button-overlay))
			     (face (overlay-get overlay 'face))
			     (mouse-face (overlay-get overlay 'mouse-face)))
			(unwind-protect
		       ;; Read events, including mouse-movement events
		      ;; until we receive a release event.  Highlight/
		     ;; unhighlight the button the mouse was initially
			    ;; on when we move over it.
			    (let ((track-mouse t))
			      (save-excursion
				(when face ; avoid changing around image
				  (overlay-put overlay
					       'face widget-button-pressed-face)
				  (overlay-put overlay
					       'mouse-face widget-button-pressed-face))
				(unless (widget-apply button :mouse-down-action event)
				  (while (not (widget-button-release-event-p event))
				    (setq event (read-event)
					  pos (widget-event-point event))
				    (if (and pos
					     (eq (get-char-property pos 'button)
						 button))
					(when face
					  (overlay-put overlay
						       'face
						       widget-button-pressed-face)
					  (overlay-put overlay
						       'mouse-face
						       widget-button-pressed-face))
				      (overlay-put overlay 'face face)
				      (overlay-put overlay 'mouse-face mouse-face))))

			;; When mouse is released over the button, run
				;; its action function.
				(when (and pos
					   (eq (get-char-property pos 'button) button))
				  (widget-apply-action button event))))
			  (overlay-put overlay 'face face)
			  (overlay-put overlay 'mouse-face mouse-face))))

		    (unless (pos-visible-in-window-p (widget-event-point event))
		      (mouse-set-point event)
		      (beginning-of-line)
		      (recenter)))

		(let ((up t) command)
	       ;; Mouse click not on a widget button.  Find the global
		;; command to run, and check whether it is bound to an
		  ;; up event.
		  (mouse-set-point event)
		  (if (memq (event-basic-type event) '(mouse-1 down-mouse-1))
		      (cond ((setq command ;down event
				   (lookup-key widget-global-map [down-mouse-1]))
			     (setq up nil))
			    ((setq command ;up event
				   (lookup-key widget-global-map [mouse-1]))))
		    (cond ((setq command ;down event
				 (lookup-key widget-global-map [down-mouse-2]))
			   (setq up nil))
			  ((setq command ;up event
				 (lookup-key widget-global-map [mouse-2])))))
		  (when up
		    ;; Don't execute up events twice.
		    (while (not (widget-button-release-event-p event))
		      (setq event (read-event))))
		  (when command
		    (call-interactively command)))))
	  (message "You clicked somewhere weird.")))
      ))


(provide 'efc)


;; Change History
;; $Log: efc.el,v $
;; Revision 1.9  2002/03/29 12:40:27  paulk
;; Adds efc-query-option function.
;;
;; Revision 1.8  2002/03/19 12:24:47  paulk
;; Updated live-window error patch to work for Emacs 21.2.
;;
;; Revision 1.7  2002/02/21 05:35:39  paulk
;; efc-dialog class now creates the dialog buffer in
;; the efc-dialog-show method instead of in the
;; intialize-instance method. This permits reuse
;; of the dialog buffer object and hence persistance
;; of user settings in the dialog.
;;
;; Revision 1.6  2002/01/25 10:41:55  paulk
;; Fixes Lisp error: (wrong-type-argument window-live-p #<window 66>) that
;; occurs in Emacs 21.1.1 when the user clicks an efc dialog box button.
;;
;; Revision 1.5  2002/01/06 06:54:06  paulk
;; Finally found a fix for the efc dialog class that works around
;; the delete-other-windows bug in Emacs 21.
;;
;; Revision 1.4  2001/12/04 14:45:55  jslopez
;; Change jde-xemacs for (featurep 'xemacs).
;;
;; Revision 1.3  2001/12/04 12:32:34  jslopez
;; Fixes typo (efc-xemacsp to jde-xemacsp).
;;
;; Revision 1.2  2001/12/04 06:06:34  paulk
;; Remove carriage returns.
;;
;; Revision 1.1  2001/12/04 05:23:20  paulk
;; Initial revision.
;;
;;

;; End of efc.el
