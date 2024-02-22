;;; psgml-edit.el --- Editing commands for SGML-mode with parsing support
;;
;; $Id: psgml-edit.el,v 2.46 1999/10/06 16:23:33 lenst Exp $

;; Copyright (C) 1994, 1995, 1996 Lennart Staflin

;; Author: Lennart Staflin <lenst@lysator.liu.se>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;;; Commentary:

;; Part of major mode for editing the SGML document-markup language.


;;;; Code:

(provide 'psgml-edit)
(require 'psgml)
(require 'psgml-parse)

(eval-when-compile
  (setq byte-compile-warnings '(free-vars unresolved callargs redefine)))


;;;; Variables

(defvar psgml-split-level nil
  "Used by psgml-split-element")


;;;; SGML mode: structure editing

(defun psgml-last-element ()
  "Return the element where last command left point.
This either uses the save value in `psgml-last-element' or parses the buffer
to find current open element."
  (setq psgml-markup-type nil)
  (if (and (memq last-command psgml-users-of-last-element)
	   psgml-last-element)		; Don't return nil
      psgml-last-element
    (setq psgml-last-element (psgml-find-context-of (point))))  )

(defun psgml-set-last-element (&optional el)
  (if el (setq psgml-last-element el))
  (psgml-show-context psgml-last-element))

(defun psgml-beginning-of-element ()
  "Move to after the start-tag of the current element.
If the start-tag is implied, move to the start of the element."
  (interactive)
  (goto-char (psgml-element-stag-end (psgml-last-element)))
  (psgml-set-last-element (if (psgml-element-empty psgml-last-element)
			     (psgml-element-parent psgml-last-element))))

(defun psgml-end-of-element ()
  "Move to before the end-tag of the current element."
  (interactive)
  (goto-char (psgml-element-etag-start (psgml-last-element)))
  (psgml-set-last-element (if (psgml-element-empty psgml-last-element)
			     (psgml-element-parent psgml-last-element))))

(defun psgml-backward-up-element ()
  "Move backward out of this element level.
That is move to before the start-tag or where a start-tag is implied."
  (interactive)
  (goto-char (psgml-element-start (psgml-last-element)))
  (psgml-set-last-element (psgml-element-parent psgml-last-element)))

(defun psgml-up-element ()
  "Move forward out of this element level.
That is move to after the end-tag or where an end-tag is implied."
  (interactive)
  (goto-char (psgml-element-end (psgml-last-element)))
  (psgml-set-last-element (psgml-element-parent psgml-last-element)))

(defun psgml-forward-element ()
  "Move forward over next element."
  (interactive)
  (let ((next
	 (psgml-find-element-after (point) (psgml-last-element))))
    (goto-char (psgml-element-end next))
    (psgml-set-last-element (psgml-element-parent next))))

(defun psgml-backward-element ()
  "Move backward over previous element at this level.
With implied tags this is ambigous."
  (interactive)
  (let ((prev				; previous element
	 (psgml-find-previous-element (point) (psgml-last-element))))
    (goto-char (psgml-element-start prev))
    (psgml-set-last-element (psgml-element-parent prev))))

(defun psgml-down-element ()
  "Move forward and down one level in the element structure."
  (interactive)
  (let ((to
	 (psgml-find-element-after (point) (psgml-last-element))))
    (when (psgml-strict-epos-p (psgml-element-stag-epos to))
      (error "Sub-element in other entity"))
    (goto-char (psgml-element-stag-end to))
    (psgml-set-last-element (if (psgml-element-empty to)
			       (psgml-element-parent to)
			     to))))

(defun psgml-kill-element ()
  "Kill the element following the cursor."
  (interactive "*")
  (psgml-parse-to-here)
  (when psgml-markup-type
    (error "Point is inside markup"))
  (kill-region (point)
	       (psgml-element-end (psgml-find-element-after (point)))))

(defun psgml-transpose-element ()
  "Interchange element before point with element after point, leave point after."
  (interactive "*")
  (let ((pre (psgml-find-previous-element (point)))
	(next (psgml-find-element-after (point)))
	s1 s2 m2)
    (goto-char (psgml-element-start next))
    (setq m2 (point-marker))
    (setq s2 (buffer-substring (point)
			       (psgml-element-end next)))
    (delete-region (point) (psgml-element-end next))
    (goto-char (psgml-element-start pre))
    (setq s1 (buffer-substring (point) (psgml-element-end pre)))
    (delete-region (point) (psgml-element-end pre))
    (insert-before-markers s2)
    (goto-char m2)
    (insert s1)
    (psgml-message "")))

(defun psgml-mark-element ()
  "Set mark after next element."
  (interactive)
  (push-mark (psgml-element-end (psgml-find-element-after (point))) nil t))

(defun psgml-mark-current-element ()
  "Set mark at end of current element, and leave point before current element."
  (interactive)
  (let ((el (psgml-find-element-of (point))))
    (goto-char (psgml-element-start el))
    (push-mark (psgml-element-end el) nil t)))


(defun psgml-change-element-name (gi)
  "Replace the name of the current element with a new name.
Eventual attributes of the current element will be translated if
possible."
  (interactive
   (list (let ((el (psgml-find-element-of (point))))
	   (goto-char (psgml-element-start el))
	   (psgml-read-element-name
	    (format "Change %s to: " (psgml-element-name el))))))
  (when (or (null gi) (equal gi ""))
    (error "Illegal name"))
  (let* ((element (psgml-find-element-of (point)))
	 (attspec (psgml-element-attribute-specification-list element))
	 (oldattlist (psgml-element-attlist element)))
    (unless  (psgml-element-empty element)
      (goto-char (psgml-element-end element))
      (delete-char (- (psgml-element-etag-len element)))
      (insert (psgml-end-tag-of gi)))
    (goto-char (psgml-element-start element))
    (delete-char (psgml-element-stag-len element))
    (insert (psgml-delim "STAGO")
	    (psgml-general-insert-case gi))
    (let* ((newel (psgml-find-element-of (point)))
	   (newattlist (psgml-element-attlist newel))
	   (newasl (psgml-translate-attribute-specification-list
		    attspec oldattlist newattlist)))
      (psgml-insert-attributes newasl newattlist))
    (insert (if (and psgml-pxml-p (psgml-element-empty element))
		(psgml-delim "XML-TAGCE")
	      (psgml-delim "TAGC")))))


(defun psgml-translate-attribute-specification-list (values from to)
  "Translate attribute specification from one element type to another.
Input attribute values in VALUES using attlist FROM is translated into
a list using attlist TO."
  (let ((new-values nil)
	(psgml-show-warnings t)
	tem)
    (loop for attspec in values
	  as from-decl = (psgml-lookup-attdecl (psgml-attspec-name attspec) from)
	  as to-decl   = (psgml-lookup-attdecl (psgml-attspec-name attspec) to)
	  do
	  (cond
	   ;; Special case ID attribute
	   ((and (eq 'ID (psgml-attdecl-declared-value from-decl))
		 (setq tem (psgml-attribute-with-declared-value to 'ID)))
	    (push
	     (psgml-make-attspec (psgml-attdecl-name tem)
				(psgml-attspec-attval attspec))
	     new-values))
	   ;; Use attribute with same name if compatible type
	   ((equal (psgml-attdecl-declared-value from-decl)
		   (psgml-attdecl-declared-value to-decl))
	    (push attspec new-values))
	   (to-decl
	    (psgml-log-warning
	     "Attribute %s has new declared-value"
	     (psgml-attspec-name attspec))
	    (push attspec new-values))
	   (t
	    (psgml-log-warning "Can't translate attribute %s = %s"
			      (psgml-attspec-name attspec)
			      (psgml-attspec-attval attspec)))))
    new-values))

(defun psgml-untag-element ()
  "Remove tags from current element."
  (interactive "*")
  (let ((el (psgml-find-element-of (point))))
    (when (or (psgml-strict-epos-p (psgml-element-stag-epos el))
	      (psgml-strict-epos-p (psgml-element-etag-epos el)))
      (error "Current element has some tag inside an entity reference"))
    (goto-char (psgml-element-etag-start el))
    (delete-char (psgml-element-etag-len el))
    (goto-char (psgml-element-start el))
    (delete-char (psgml-element-stag-len el))))

(defun psgml-kill-markup ()
  "Kill next tag, markup declaration or process instruction."
  (interactive "*")
  (let ((start (point)))
    (psgml-with-parser-syntax
     (psgml-parse-s)
     (setq psgml-markup-start (point))
     (cond ((psgml-parse-markup-declaration 'ignore))
	   ((psgml-parse-processing-instruction))
	   ((psgml-skip-tag)))
     (kill-region start (point)))))


;;;; SGML mode: folding

(defun psgml-fold-region (beg end &optional unhide)
  "Hide (or if prefixarg unhide) region.
If called from a program first two arguments are start and end of
region. And optional third argument true unhides."
  (interactive "r\nP")
  (let ((mp (buffer-modified-p))
	(inhibit-read-only t)		;
	(buffer-read-only nil)		; should not need this, but
					; perhaps some old version of
					; emacs does not understand
					; inhibit-read-only
	(before-change-function nil)
	(after-change-function nil))
    (setq selective-display t)
    (unwind-protect
	(subst-char-in-region beg end
			      (if unhide ?\r ?\n)
			      (if unhide ?\n ?\r)
			      'noundo)
      (when psgml-buggy-subst-char-in-region
	(set-buffer-modified-p mp)))))

(defun psgml-fold-element ()
  "Fold the lines comprising the current element, leaving the first line visible.
This uses the selective display feature."
  (interactive)
  (psgml-parse-to-here)
  (cond ((and (eq psgml-current-tree psgml-top-tree) ; outside document element
	      psgml-markup-type)
	 (psgml-fold-region psgml-markup-start
			   (save-excursion
			     (psgml-parse-to (point))
			     (point))))
	((and (eq psgml-current-tree psgml-top-tree) ; outside document element
	      (looking-at " *<!"))
	 (psgml-fold-region (point)
			   (save-excursion
			     (skip-chars-forward " \t")
			     (psgml-parse-to (1+ (point)))
			     (point))))

	(t
	 (let ((el (psgml-find-element-of (point))))
	   (when (eq el psgml-top-tree)
	     (error "No element here"))
	   (save-excursion
	     (goto-char (psgml-element-end el))
	     (when (zerop (psgml-element-etag-len el))
	       (skip-chars-backward " \t\n"))
	     (psgml-fold-region (psgml-element-start el)
			       (point)))))))

(defun psgml-fold-subelement ()
  "Fold all elements current elements content, leaving the first lines visible.
This uses the selective display feature."
  (interactive)
  (let* ((el (psgml-find-element-of (point)))
	 (c (psgml-element-content el)))
    (while c
      (psgml-fold-region (psgml-element-start c)
			(psgml-element-end c))
      (setq c (psgml-element-next c)))))

(defun psgml-unfold-line ()
  "Show hidden lines in current line."
  (interactive)
  (let ((op (point)))
    (beginning-of-line)
    (push-mark)
    (end-of-line)
    (exchange-point-and-mark)
    (psgml-fold-region (point) (mark) 'unhide)
    (goto-char op)))

(defun psgml-unfold-element ()
  "Show all hidden lines in current element."
  (interactive)
  (let* ((element (psgml-find-element-of (point))))
    (psgml-fold-region (psgml-element-start element)
		      (psgml-element-end element)
		      'unfold)))

(defun psgml-expand-element ()
  "As psgml-fold-subelement, but unfold first."
  (interactive)
  (psgml-unfold-element)
  (psgml-fold-subelement))

(defun psgml-unfold-all ()
  "Show all hidden lines in buffer."
  (interactive)
  (psgml-fold-region (point-min)
		    (point-max)
		    'unfold))

;;;; SGML mode: indentation and movement

(defun psgml-indent-line (&optional col element)
  "Indent line, calling parser to determine level unless COL or ELEMENT
is given.  If COL is given it should be the column to indent to.  If
ELEMENT is given it should be a parse tree node, from which the level
is determined."
  (when psgml-indent-step
    (let ((here (point-marker)))
      (back-to-indentation)
      (unless (or col element)
	;; Determine element
	(setq element
	      (let ((psgml-throw-on-error 'parse-error))
		(catch psgml-throw-on-error
		  (if (eobp)
		      (psgml-find-context-of (point))
		    (psgml-find-element-of (point)))))))
      (when (eq element psgml-top-tree)	; not in a element at all
	(setq element nil)		; forget element
	(goto-char here))		; insert normal tab insted)
      (when element
	(psgml-with-parser-syntax
	 (let ((stag (psgml-is-start-tag))
	       (etag (psgml-is-end-tag)))
	   (cond ((and (> (point) (psgml-element-start element))
		       (< (point) (psgml-element-stag-end element)))
		  (setq col
			(+ (save-excursion
			     (goto-char (psgml-element-start element))
			     (current-column))
			   (length (psgml-element-gi element))
			   2)))
		 ((or psgml-indent-data
		      (not (psgml-element-data-p
			    (if stag
				(psgml-element-parent element)
			      element))))
		  (setq col
			(* psgml-indent-step
			   (+ (if (or stag etag) -1 0)
			      (psgml-element-level element)))))))))
      (when (and col (/= col (current-column)))
	(beginning-of-line 1)
	(delete-horizontal-space)
	(indent-to col))
      (when (< (point) here)
	(goto-char here))
      col)))


(defun psgml-next-data-field ()
  "Move forward to next point where data is allowed."
  (interactive)
  (when (eobp)
    (error "End of buffer"))
  (let ((psgml-throw-on-warning 'next-data)
	(avoid-el (psgml-last-element)))
    ;; Avoid stopping in current element, unless point is in the start
    ;; tag of the element
    (when (< (point) (psgml-element-stag-end avoid-el))
      (setq avoid-el nil))
    (catch psgml-throw-on-warning
      (while (progn
	       (psgml-parse-to (1+ (point)))
	       (setq psgml-last-element
		     (if (not (eq ?< (following-char)))
			 (psgml-find-element-of (point))
		       psgml-current-tree))
	       (or (eq psgml-last-element avoid-el)
		   (not (psgml-element-data-p psgml-last-element)))))
      (psgml-set-last-element))))

(defun psgml-next-trouble-spot ()
  "Move forward to next point where something is amiss with the structure."
  (interactive)
  (push-mark)
  (psgml-note-change-at (point))		; Prune the parse tree
  (psgml-parse-to (point))
  (let ((psgml-throw-on-warning 'trouble))
    (or (catch psgml-throw-on-warning
	  (psgml-parse-until-end-of nil t))
	(message "Ok"))))



;;;; SGML mode: information display

(defun psgml-list-valid-tags ()
  "Display a list of the contextually valid tags."
  (interactive)
  (psgml-parse-to-here)
  (let ((model (psgml-element-model psgml-current-tree))
	(smap-name (psgml-lookup-shortref-name
		    (psgml-dtd-shortmaps psgml-dtd-info)
		    psgml-current-shortmap)))
    (with-output-to-temp-buffer "*Tags*"
      (princ (format "Current element: %s  %s\n"
		     (psgml-element-name psgml-current-tree)
		     (if (psgml-eltype-defined
			  (psgml-element-eltype psgml-current-tree))
			 ""
		       "[UNDEFINED]")))
      (princ (format "Element content: %s  %s\n"
		     (cond ((or (psgml-current-mixed-p) (eq model psgml-any))
			    "mixed")
			   ((psgml-model-group-p model)
			    "element")
			   (t
			    model))
		     (if (eq model psgml-any)
			 "[ANY]" "")))

      (when smap-name
	(princ (format "Current short reference map: %s\n" smap-name)))

      (cond ((psgml-final-p psgml-current-state)
	     (princ "Valid end-tags : ")
	     (loop for e in (psgml-current-list-of-endable-eltypes)
		   do (princ (psgml-end-tag-of e)) (princ " "))
	     (terpri))
	    (t
	     (princ "Current element can not end here\n")))
;;;      (let ((s (psgml-tree-shortmap psgml-current-tree)))
;;;	(when s
;;;	  (princ (format "Current shortref map: %s\n" s))))
      (princ "Valid start-tags\n")
      (psgml-print-valid-tags "In current element:"
			     psgml-current-tree psgml-current-state))))

(defun psgml-print-valid-tags (prompt tree state &optional exclude omitted-stag)
  (if (not (psgml-model-group-p state))
      (princ (format "%s (in %s)\n" prompt state))
    (let* ((req (psgml-required-tokens state))
	   (elems (nconc req
			 (delq psgml-pcdata-token
			       (psgml-optional-tokens state))))
	   (in (psgml-tree-includes tree))
	   (ex (append exclude (psgml-tree-excludes tree))))
      ;; Modify for exceptions
      (while in
	(unless (memq (car in) elems)
	  (setq elems (nconc elems (list (car in)))))
	(setq in (cdr in)))
      (while ex
	(setq elems (delq (car ex) elems))
	(setq ex (cdr ex)))
      ;;
      (setq elems (sort elems (function string-lessp)))
      (psgml-print-list-of-tags prompt elems)
      ;; Check for omissable start-tags
      (when (and req (null (cdr req)))
	;; *** Assumes tokens are eltypes
	(let ((el (psgml-fake-open-element tree (car req))))
	  (when (psgml-element-stag-optional el)
	    (psgml-print-valid-tags
	     (format "If omitting %s:" (psgml-start-tag-of el))
	     el
	     (psgml-element-model el)
	     (append exclude elems)
	     'omitted-stag))))
      ;; Check for omissable end-tag
      (when (and (not omitted-stag)
		 (psgml-final-p state)
		 (psgml-element-etag-optional tree))
	(psgml-print-valid-tags
	 (format "If omitting %s:" (psgml-end-tag-of tree))
	 (psgml-element-parent tree)
	 (psgml-element-pstate tree)
	 (append exclude elems))))))

(defun psgml-print-list-of-tags (prompt list)
  (when list
    (princ prompt)
    (let ((col (length prompt))
	  (w   (1- (frame-width))))
      (loop for e in list
	    as str = (psgml-start-tag-of e)
	    do
	    (setq col (+ col (length str) 2))
	    (cond ((>= col w)
		   (setq col (+ (length str) 2))
		   (terpri)))
	    (princ "  ")
	    (princ str))
      (terpri))))

(defun psgml-show-context (&optional element)
  "Display where the cursor is in the element hierarchy."
  (interactive)
  (let* ((el (or element (psgml-last-element)))
	 (model (psgml-element-model el)))
    (psgml-message "%s %s"
		  (cond
		   ((and (null element)	; Don't trust psgml-markup-type if
					; explicit element is given as argument
			 psgml-markup-type))
		   ((psgml-element-mixed el)
		    "#PCDATA")
		   ((not (psgml-model-group-p model))
		    model)
		   (t ""))
		  (if (eq el psgml-top-tree)
		      "in empty context"
		    (psgml-element-context-string el)))))

(defun psgml-what-element ()
  "Display what element is under the cursor."
  (interactive)
  (let* ((pos (point))
	 (nobol (eq (point) psgml-rs-ignore-pos))
	 (sref (and psgml-current-shortmap
		    (psgml-deref-shortmap psgml-current-shortmap nobol)))
	 (el nil))
    (goto-char pos)
    (setq el (psgml-find-element-of pos))
    (assert (not (null el)))
    (message "%s %s"
	     (cond ((eq el psgml-top-tree)
		    "outside document element")
		   ((< (point) (psgml-element-stag-end el))
		    "start-tag")
		   ((>= (point) (psgml-element-etag-start el))
		    "end-tag")
		   (sref
		    "shortref")
		   (t
		    "content"))
	     (psgml-element-context-string el))))

;;;; SGML mode: keyboard inserting

(defun psgml-coerce-element-type (obj)
  (when (stringp obj)
    (setq obj (psgml-lookup-eltype (psgml-general-case obj))))
  (when nil                             ;FIXME: need predicate
    (setq obj (psgml-tree-eltype obj)))
  obj)

(defun psgml-break-brefore-stag-p (element)
  (psgml-eltype-appdata (psgml-coerce-element-type element)
		       'break-brefore-stag))

(defun psgml-break-after-stag-p (element)
  (psgml-eltype-appdata (psgml-coerce-element-type element)
		       'break-after-stag))

(defun psgml-insert-break ()
  (skip-chars-backward " \t")
  (cond ((bolp)
	 (if (looking-at "^\\s-*$")
	     (fixup-whitespace)))
	(t
	 ;; FIXME: fixup-whitespace ??
	 (insert "\n"))))


(defun psgml-insert-tag (tag &optional silent no-nl-after)
  "Insert a tag, reading tag name in minibuffer with completion.
If the variable psgml-balanced-tag-edit is t, also inserts the
corresponding end tag. If psgml-leave-point-after-insert is t, the point
is left after the inserted tag(s), unless the element has som required
content.  If psgml-leave-point-after-insert is nil the point is left
after the first tag inserted."
  (interactive
   (list
    (let ((completion-ignore-case psgml-namecase-general))
      (completing-read "Tag: " (psgml-completion-table) nil t "<" ))))
  (psgml-find-context-of (point))
  (assert (null psgml-markup-type))
  ;; Fix white-space before tag
  (unless (psgml-element-data-p (psgml-parse-to-here))
    (skip-chars-backward " \t")
    (cond ((bolp)
	   (if (looking-at "^\\s-*$")
	       (fixup-whitespace)))
	  (t
	   (insert "\n"))))
  (insert tag)
  (psgml-indent-line)
  (unless no-nl-after
    (save-excursion
      (unless (psgml-element-data-p (psgml-parse-to-here))
	(unless (eolp)
	  (save-excursion (insert "\n"))))))
  (or silent (psgml-show-context)))

(defvar psgml-new-attribute-list-function
  (function psgml-default-asl))

(defun psgml-insert-element (name &optional after silent)
  "Reads element name from minibuffer and inserts start and end tags."
  (interactive (list (psgml-read-element-name "Element: ")
		     psgml-leave-point-after-insert))
  (let (newpos				; position to leave cursor at
	element				; inserted element
	(psgml-show-warnings nil))
    (when (and name (not (equal name "")))
      (when (psgml-break-brefore-stag-p name)
	(psgml-insert-break))
      (psgml-insert-tag (psgml-start-tag-of name) 'silent)
      (if (and psgml-pxml-p (psgml-check-empty name))
	  (forward-char -2)
	(forward-char -1))
      (setq element (psgml-find-element-of (point)))
      (psgml-insert-attributes (funcall psgml-new-attribute-list-function
				       element)
			      (psgml-element-attlist element))
      (if (and psgml-pxml-p (psgml-check-empty name))
	  (forward-char 2)
	(forward-char 1))
      (when (psgml-break-after-stag-p name)
	(psgml-insert-break))
      (when (not (psgml-element-empty element))
	(when (and psgml-auto-insert-required-elements
		   (psgml-model-group-p psgml-current-state))
	  (let (tem)
	    (while (and (setq tem (psgml-required-tokens psgml-current-state))
			(null (cdr tem)))
	      (setq tem (psgml-insert-element (car tem) t t))
	      (setq newpos (or newpos tem))
	      (psgml-parse-to-here))
	    (when tem			; more than one req elem
	      (insert "\n")
	      (when psgml-insert-missing-element-comment
		(insert (format "<!-- one of %s -->" tem))
		(psgml-indent-line nil element)))))
	(setq newpos (or newpos (point)))
	(when psgml-insert-end-tag-on-new-line
	  (insert "\n"))
	(psgml-insert-tag (psgml-end-tag-of name) 'silent)
	(unless after
	  (goto-char newpos))
	(unless silent (psgml-show-context)))
      newpos)))

(defun psgml-default-asl (element)
  (loop for attdecl in (psgml-element-attlist element)
	when (psgml-default-value-type-p (psgml-attdecl-default-value attdecl)
					'REQUIRED)
	collect
	(psgml-make-attspec
	 (psgml-attdecl-name attdecl)
	 (psgml-read-attribute-value attdecl nil))))

(defun psgml-tag-region (element start end)
  "Reads element name from minibuffer and inserts start and end tags."
  (interactive
   (list
    (save-excursion (goto-char (region-beginning))
		    (psgml-read-element-name "Tag region with element: "))
    (region-beginning)
    (region-end)))
  (save-excursion
    (when (and element (not (equal element "")))
      (goto-char end)
      (insert (psgml-end-tag-of element))
      (goto-char start)
      (psgml-insert-tag (psgml-start-tag-of element)))))

(defun psgml-insert-attributes (avl attlist)
  "Insert the attributes with values AVL and declarations ATTLIST.
AVL should be a assoc list mapping symbols to strings."
  (let (name val dcl def)
    (loop for attspec in attlist do
	  (setq name (psgml-attspec-name attspec)
		val (cdr-safe (psgml-lookup-attspec name avl))
		dcl (psgml-attdecl-declared-value attspec)
		def (psgml-attdecl-default-value attspec))
	  (setq name (psgml-general-insert-case name))
	  (unless val			; no value given
	    ;; Supply the default value if a value is needed
	    (cond ((psgml-default-value-type-p 'REQUIRED def)
		   (setq val ""))
		  ((and (not (or psgml-omittag psgml-shorttag))
			(consp def))
		   (setq val (psgml-default-value-attval def)))))
	  (when val
	    (cond ((eq dcl 'CDATA))
		  ((eq dcl 'ENTITY) (setq val (psgml-entity-insert-case val)))
		  (t (setq val (psgml-general-insert-case val)))))
	  (cond
	   ((null val))			; Ignore
	   ;; Ignore attributes with default value
	   ((and (consp def)
		 (eq psgml-minimize-attributes 'max)
		 (or psgml-omittag psgml-shorttag)
		 (equal val (psgml-default-value-attval def))))
	   ;; No attribute name for token groups
	   ((and psgml-minimize-attributes psgml-shorttag
		 (member (psgml-general-case val)
			 (psgml-declared-value-token-group dcl)))
	    (insert " " val))
	   (t
	    (insert " " name "=" (psgml-quote-attribute-value val)))))
    (when auto-fill-function
      (funcall auto-fill-function))))


(defun psgml-quote-attribute-value (value)
  "Add quotes to the string VALUE unless minimization is on."
  (let ((quote ""))
	(cond ((and (not psgml-always-quote-attributes)
		    psgml-shorttag
		    (string-match "\\`[.A-Za-z0-9---]+\\'" value))
	       ) ; no need to quote
	      ((not (string-match "\"" value)) ; can use "" quotes
	       (setq quote "\""))
	      (t			; use '' quotes
	       (setq quote "'")))
	(concat quote value quote)))

(defun psgml-completion-table (&optional avoid-tags-in-cdata)
  (psgml-parse-to-here)
  (when psgml-markup-type
    (error "No tags allowed"))
  (cond ((or (psgml-model-group-p psgml-current-state)
	     (eq psgml-current-state psgml-any))
	 (append
	  (mapcar (function (lambda (x) (cons (psgml-end-tag-of x) x)))
		  (psgml-current-list-of-endable-eltypes))
	  (mapcar (function (lambda (x) (cons (psgml-start-tag-of x) x)))
		  (psgml-current-list-of-valid-eltypes))))
	(t
	 (psgml-message "%s" psgml-current-state)
	 nil)))

(defun psgml-element-endable-p ()
  (psgml-parse-to-here)
  (and (not (eq psgml-current-tree psgml-top-tree))
       (psgml-final-p psgml-current-state)))

(defun psgml-insert-end-tag ()
  "Insert end-tag for the current open element."
  (interactive "*")
  (psgml-parse-to-here)
  (cond
   ((eq psgml-current-tree psgml-top-tree)
    (psgml-error "No open element"))
   ((not (psgml-final-p psgml-current-state))
    (psgml-error "Can`t end element here"))
   (t
    (when (and psgml-indent-step
	       (not (psgml-element-data-p psgml-current-tree)))
      (delete-horizontal-space)
      (unless (bolp)
	(insert "\n")))
    (when (prog1 (bolp)
	    (insert (if (eq t (psgml-element-net-enabled psgml-current-tree))
			"/"
		      (psgml-end-tag-of psgml-current-tree))))
      (psgml-indent-line)))))

(defun psgml-insert-start-tag (name asl attlist &optional net)
  ;; Insert a start-tag with attributes
  ;; if NET is true end with NESTC unless XML then end with NESTC NET
  ;; (aka XML-TAGCE).
  (insert (psgml-delim "STAGO") (psgml-general-insert-case name))
  (psgml-insert-attributes asl attlist)
  ;; In XML, force net if element is always empty
  (when (and psgml-pxml-p (psgml-check-empty name))
    (setq net t))
  (insert (if net (if psgml-pxml-p
		      (psgml-delim "XML-TAGCE")
		    (psgml-delim "NESTC"))
	    (psgml-delim "TAGC"))))

(defun psgml-change-start-tag (element asl)
  (let ((name (psgml-element-gi element))
	(attlist (psgml-element-attlist element)))
    (assert (psgml-bpos-p (psgml-element-stag-epos element)))
    (goto-char (psgml-element-start element))
    (delete-char (psgml-element-stag-len element))
    (psgml-insert-start-tag name asl attlist
			   (if psgml-pxml-p
			       (psgml-element-empty element)
			     (eq t (psgml-element-net-enabled element))))))

(defun psgml-read-attribute-value (attdecl curvalue)
  "Return the attribute value read from user.
ATTDECL is the attribute declaration for the attribute to read.
CURVALUE is nil or a string that will be used as default value."
  (assert attdecl)
  (let* ((name (psgml-attdecl-name attdecl))
	 (dv (psgml-attdecl-declared-value attdecl))
	 (tokens (psgml-declared-value-token-group dv))
	 (notations (psgml-declared-value-notation dv))
	 (type (cond (tokens "token")
		     (notations "NOTATION")
		     (t (symbol-name dv))))
	 (prompt
	  (format "Value for %s (%s%s): "
		  name type
		  (if curvalue
		      (format " Default: %s" curvalue)
		    "")))
	 value)
    (setq value
	  (if (or tokens notations)
	      (let ((completion-ignore-case psgml-namecase-general))
		(completing-read prompt
				 (mapcar 'list (or tokens notations))
				 nil t))
	    (read-string prompt)))
    (if (and curvalue (equal value ""))
	curvalue value)))

(defun psgml-non-fixed-attributes (attlist)
  (loop for attdecl in attlist
	unless (psgml-default-value-type-p 'FIXED
					  (psgml-attdecl-default-value attdecl))
	collect attdecl))

(defun psgml-insert-attribute (name value)
  "Read attribute name and value from minibuffer and insert attribute spec."
  (interactive
   (let* ((el (psgml-find-attribute-element))
	  (name
	   (psgml-general-case
	    (let ((completion-ignore-case psgml-namecase-general))
	      (completing-read
	       "Attribute name: "
	       (mapcar (function (lambda (a) (list (psgml-attdecl-name a))))
		       (psgml-non-fixed-attributes (psgml-element-attlist el)))
	       nil t)))))
     (list name
	   (psgml-read-attribute-value
	    (psgml-lookup-attdecl name (psgml-element-attlist el))
	    (psgml-element-attval el name)))))
  ;; Body
  (assert (stringp name))
  (assert (or (null value) (stringp value)))
  (let* ((el (psgml-find-attribute-element))
	 (asl (cons (psgml-make-attspec name value)
		    (psgml-element-attribute-specification-list el)))
	 (in-tag (< (point) (psgml-element-stag-end el))))
    (psgml-change-start-tag el asl)
    (when in-tag (forward-char -1))))

(defun psgml-split-element ()
  "Split the current element at point.
If repeated, the containing element will be split before the beginning
of then current element."
  (interactive "*")
  (setq psgml-split-level
	(if (eq this-command last-command)
	    (1+ psgml-split-level)
	  0))
  (let ((u (psgml-find-context-of (point)))
	(start (point-marker)))
    (loop repeat psgml-split-level do
	  (goto-char (psgml-element-start u))
	  (setq u (psgml-element-parent u)))
    ;; Verify that a new element can be started
    (unless (and (psgml-element-pstate u) ; in case of top element
		 (psgml-get-move (psgml-element-pstate u)
				(psgml-element-name u)))

      (psgml-error "The %s element can't be split"
		  (psgml-element-name u)))
    ;; Do the split
    (psgml-insert-end-tag)
    (insert ?\n)
    (psgml-insert-tag (psgml-start-tag-of u) 'silent)
    (skip-chars-forward " \t\n")
    (psgml-indent-line)
    (when (> psgml-split-level 0)
      (goto-char start))
    (or (eq psgml-top-tree
	    (setq u (psgml-element-parent u)))
	(psgml-message
	 "Repeat the command to split the containing %s element"
	 (psgml-element-name u)))))

;;; David Megginson's custom menus for keys

(defun psgml-custom-dtd (doctype)
  "Insert a DTD declaration from the psgml-custom-dtd alist."
  (interactive
   (list (completing-read "Insert DTD: " psgml-custom-dtd nil t)))
  (let ((entry (assoc doctype psgml-custom-dtd)))
    (psgml-doctype-insert (second entry) (cddr entry))))

(defun psgml-custom-markup (markup)
  "Insert markup from the psgml-custom-markup alist."
  (interactive
   (let ((completion-ignore-case psgml-namecase-general))
     (list (completing-read "Insert Markup: " psgml-custom-markup nil t))))
  (psgml-insert-markup (cadr (assoc markup psgml-custom-markup))))


;;;; SGML mode: Menu inserting

(defun psgml-tags-menu (event)
  "Pop up a menu with valid tags and insert the choosen tag.
If the variable psgml-balanced-tag-edit is t, also inserts the
corresponding end tag. If psgml-leave-point-after-insert is t, the point
is left after the inserted tag(s), unless the element has som required
content.  If psgml-leave-point-after-insert is nil the point is left
after the first tag inserted."
  (interactive "*e")
  (let ((end (psgml-mouse-region)))
    (psgml-parse-to-here)
    (cond
     ((eq psgml-markup-type 'start-tag)
      (psgml-attrib-menu event))
     (t
      (let ((what
	     (psgml-menu-ask event (if (or end psgml-balanced-tag-edit)
				  'element 'tags))))
	(cond
	 ((null what))
	 (end
	  (psgml-tag-region what (point) end))
	 (psgml-balanced-tag-edit
	  (psgml-insert-element what))
	 (t
	  (psgml-insert-tag what))))))))

(defun psgml-element-menu (event)
  "Pop up a menu with valid elements and insert choice.
If psgml-leave-point-after-insert is nil the point is left after the first
tag inserted."
  (interactive "*e")
  (let ((what (psgml-menu-ask event 'element)))
    (and what (psgml-insert-element what))))

(defun psgml-add-element-menu (event)
  (interactive "*e")
  (let ((what (psgml-menu-ask event 'add-element)))
    (and what (psgml-add-element-to-element what nil))))

(defun psgml-start-tag-menu (event)
  "Pop up a menu with valid start-tags and insert choice."
  (interactive "*e")
  (let ((what (psgml-menu-ask event 'start-tag)))
    (and what (psgml-insert-tag what))))

(defun psgml-end-tag-menu (event)
  "Pop up a menu with valid end-tags and insert choice."
  (interactive "*e")
  (let ((what (psgml-menu-ask event 'end-tag)))
    (and what (psgml-insert-tag what))))

(defun psgml-tag-region-menu (event)
  "Pop up a menu with valid elements and tag current region with the choice."
  (interactive "*e")
  (let ((what (psgml-menu-ask event 'element)))
    (and what (psgml-tag-region what
			       (region-beginning)
			       (region-end)))))

(defun psgml-menu-ask (event type)
  (psgml-parse-to-here)
  (let (tab
	(title (capitalize (symbol-name type))))
    (cond
     ((eq type 'add-element)
      (setq tab
	    (mapcar #'psgml-eltype-name
		    (psgml--all-possible-elements
		     (psgml-find-context-of (point))))))
     (psgml-markup-type)
     ((eq type 'element)
      (setq tab
	    (mapcar (function symbol-name)
		    (psgml-current-list-of-valid-eltypes))))
     (t
      (unless (eq type 'start-tag)
	(setq tab
	      (mapcar (function psgml-end-tag-of)
		      (psgml-current-list-of-endable-eltypes))))
      (unless (eq type 'end-tag)
	(setq tab
	      (nconc tab
		     (mapcar (function psgml-start-tag-of)
			     (psgml-current-list-of-valid-eltypes)))))))
    (or tab
	(error "No valid %s at this point" type))
    (or
     (psgml-popup-menu event
		      title
		      (mapcar (function (lambda (x) (cons x x)))
			      tab))
     (message nil))))

(defun psgml-entities-menu (event)
  (interactive "*e")
  (psgml-need-dtd)
  (let ((menu
	 (mapcar (function (lambda (x) (cons x x)))
		 (sort (psgml-map-entities (function psgml-entity-name)
					  (psgml-dtd-entities psgml-dtd-info)
					  t)
		       (function string-lessp))))
	choice)
    (unless menu
      (error "No entities defined"))
    (setq choice (psgml-popup-menu event "Entities" menu))
    (when choice
      (insert "&" choice ";"))))

(defun psgml-doctype-insert (doctype vars)
  "Insert string DOCTYPE (ignored if nil) and set variables in &rest VARS.
VARS should be a list of variables and values.
For backward compatibility a singel string instead of a variable is
assigned to psgml-default-dtd-file.
All variables are made buffer local and are also added to the
buffers local variables list."
  (when doctype
    (unless (bolp)
      (insert "\n"))
    (unless (eolp)
      (insert "\n")
      (forward-char -1))
    (psgml-insert-markup doctype))
  (while vars
    (cond ((stringp (car vars))
	   (psgml-set-local-variable 'psgml-default-dtd-file (car vars))
	   (setq vars (cdr vars)))
	  ((car vars)			; Avoid nil
	   (psgml-set-local-variable (car vars) (cadr vars))
	   (setq vars (cddr vars)))))
  (setq psgml-top-tree nil))

(defun psgml-attrib-menu (event)
  "Pop up a menu of the attributes of the current element
\(or the element whith start-tag before point)."
  (interactive "e")
    (let ((menu (psgml-make-attrib-menu (psgml-find-attribute-element))))
      (psgml-popup-multi-menu event "Attributes" menu)))

(defun psgml-make-attrib-menu (el)
  (let ((attlist (psgml-non-fixed-attributes (psgml-element-attlist el))))
    (or attlist
	(error "No non-fixed attributes for element"))
    (loop for attdecl in attlist
	  for name = (psgml-attdecl-name attdecl)
	  for defval = (psgml-attdecl-default-value attdecl)
	  for tokens = (or (psgml-declared-value-token-group
			    (psgml-attdecl-declared-value attdecl))
			   (psgml-declared-value-notation
			    (psgml-attdecl-declared-value attdecl)))
	  collect
	  (cons
	   (psgml-attdecl-name attdecl)
	   (nconc
	    (if tokens
		(loop for val in tokens collect
		      (list val
			    (list 'psgml-insert-attribute name val)))
	      (list
	       (list "Set attribute value"
		     (list 'psgml-insert-attribute
			   (psgml-attdecl-name attdecl)
			   (list 'psgml-read-attribute-value
				 (list 'quote attdecl)
				 (psgml-element-attval el name))))))
	    (if (psgml-default-value-type-p 'REQUIRED defval)
		nil
	      (list "--"
		    (list (if (psgml-default-value-type-p nil defval)
			      (format "Default: %s"
				      (psgml-default-value-attval defval))
			    "#IMPLIED")
			  (list 'psgml-insert-attribute name nil))))))))
  )

;;;; SGML mode: Fill

(defun psgml-element-fillable (element)
  (and (psgml-element-mixed element)
       (not (psgml-element-appdata element 'nofill))))

(defun psgml-fill-element (element)
  "Fill bigest enclosing element with mixed content.
If current element has pure element content, recursively fill the
subelements."
  (interactive (list (psgml-find-element-of (point))))
  ;;
  (message "Filling...")
  (when (psgml-element-fillable element)
    ;; Find bigest enclosing fillable element
    (while (psgml-element-fillable (psgml-element-parent element))
      (setq element (psgml-element-parent element))))
  ;;
  (psgml-do-fill element)
  (psgml-message "Done"))

(defun psgml-do-fill (element)
  (when psgml-debug
    (goto-char (psgml-element-start element))
    (sit-for 0))
  (save-excursion
    (cond
     ((psgml-element-fillable element)
      (let (last-pos
	    (c (psgml-element-content element))
	    (agenda nil))		; regions to fill later
	(goto-char (psgml-element-stag-end element))
	(when (eolp) (forward-char 1))
	(setq last-pos (point))
	(while c
	  (cond
	   ((psgml-element-fillable c))
	   (t
	    ;; Put region before element on agenda.  Can't fill it now
	    ;; that would mangle the parse tree that is beeing traversed.
	    (push (cons last-pos (psgml-element-start c))
		  agenda)
	    (goto-char (psgml-element-start c))
	    (psgml-do-fill c)
	    ;; Fill may change parse tree, get a fresh
	    (setq c (psgml-find-element-of (point)))
	    (setq last-pos (psgml-element-end c))))
	  (setq c (psgml-element-next c)))
	;; Fill the last region in content of element,
	;; but get a fresh parse tree, if it has change due to other fills.
	(psgml-fill-region last-pos
			  (psgml-element-etag-start
			   (psgml-find-element-of
			    (psgml-element-start element))))
	(while agenda
	  (psgml-fill-region (caar agenda) (cdar agenda))
	  (setq agenda (cdr agenda)))))
     (t
      ;; If element is not mixed, fill subelements recursively
      (let ((c (psgml-element-content element)))
	(while c
	  (goto-char (psgml-element-start c))
	  (psgml-do-fill c)
	  (setq c (psgml-element-next (psgml-find-element-of (point))))))))))

(defun psgml-fill-region (start end)
  (psgml-message "Filling...")
  (save-excursion
    (goto-char end)
    (skip-chars-backward " \t\n")
    (while (progn (beginning-of-line 1)
		  (< start (point)))
      (delete-horizontal-space)
      (delete-char -1)
      (insert " "))
    (end-of-line 1)
    (let (give-up prev-column opoint)
      (while (and (not give-up) (> (current-column) fill-column))
	(setq prev-column (current-column))
	(setq opoint (point))
	(move-to-column (1+ fill-column))
	(skip-chars-backward "^ \t\n")
	(if (bolp)
	    (re-search-forward "[ \t]" opoint t))
	(setq opoint (point))
	(skip-chars-backward " \t")
	(if (bolp)
	    (setq give-up t)
	(delete-region (point) opoint)
	(newline)
	(psgml-indent-line)
	(end-of-line 1)
	(setq give-up (>= (current-column) prev-column)))))))

;;;; SGML mode: Attribute editing

(defvar psgml-start-attributes nil)
(defvar psgml-main-buffer nil)
(defvar psgml-attlist nil)

(defun psgml-edit-attributes ()
  "Edit attributes of current element.
Editing is done in a separate window."
  (interactive)
  (let ((element (psgml-find-attribute-element)))
    (unless (psgml-bpos-p (psgml-element-stag-epos element))
      (error "Element's start-tag is not in the buffer"))
    (push-mark)
    (goto-char (psgml-element-start element))
    (let* ((start (point-marker))
	   (asl (psgml-element-attribute-specification-list element))
	   (cb (current-buffer))
	   (quote psgml-always-quote-attributes)
	   (pxml-p psgml-pxml-p))
      (switch-to-buffer-other-window
       (psgml-attribute-buffer element asl))
      (psgml-edit-attrib-mode)
      (make-local-variable 'psgml-start-attributes)
      (setq psgml-start-attributes start)
      (make-local-variable 'psgml-always-quote-attributes)
      (setq psgml-always-quote-attributes quote)
      (make-local-variable 'psgml-main-buffer)
      (setq psgml-main-buffer cb)
      (make-local-variable 'psgml-pxml-p)
      (setq psgml-pxml-p pxml-p))))


(defun psgml-effective-attlist (eltype)
  (let ((effective-attlist nil)
	(attlist (psgml-eltype-attlist eltype))
	(attnames (or (psgml-eltype-appdata eltype 'attnames)
		      '(*))))
    (while (and attnames (not (eq '* (car attnames))))
      (let ((attdecl (psgml-lookup-attdecl (car attnames) attlist)))
	(if attdecl
	    (push attdecl effective-attlist)
	  (message "Attnames specefication error: no %s attribute in %s"
		   (car attnames) eltype)))
      (setq attnames (cdr attnames)))
    (when (eq '* (car attnames))
      (while attlist
	(let ((attdecl (psgml-lookup-attdecl (psgml-attdecl-name (car attlist))
					    effective-attlist)))
	  (unless attdecl
	    (push (car attlist) effective-attlist)))
	(setq attlist (cdr attlist))))
    (nreverse effective-attlist)))


(defun psgml-attribute-buffer (element asl)
  (let ((bname "*Edit attributes*")
	(buf nil)
	(inhibit-read-only t))
    (save-excursion
      (when (setq buf (get-buffer bname))
	(kill-buffer buf))
      (setq buf (get-buffer-create bname))
      (set-buffer buf)
      (erase-buffer)
      (make-local-variable 'psgml-attlist)
      (setq psgml-attlist (psgml-effective-attlist
			  (psgml-element-eltype element)))
      (psgml-insert '(read-only t)
		   "<%s  -- Edit values and finish with C-c C-c --\n"
		   (psgml-element-name element))
      (loop
       for attr in psgml-attlist do
       ;; Produce text like
       ;;  name = value
       ;;  -- declaration : default --
       (let* ((aname (psgml-attdecl-name attr))
	      (dcl-value (psgml-attdecl-declared-value attr))
	      (def-value (psgml-attdecl-default-value attr))
	      (cur-value (psgml-lookup-attspec aname asl)))
	 (psgml-insert			; atribute name
	  '(read-only t category psgml-form) " %s =" aname)
	 (cond				; attribute value
	  ((psgml-default-value-type-p 'FIXED def-value)
	   (psgml-insert '(read-only t category psgml-fixed)
			" #FIXED %s"
			(psgml-default-value-attval def-value)))
	  ((and (null cur-value)
		(or (memq def-value '(IMPLIED CONREF CURRENT))
		    (psgml-default-value-attval def-value)))
	   (psgml-insert '(read-only t category psgml-form) " ")
	   (psgml-insert '(category psgml-default rear-nonsticky (category)
				   read-only psgml-default)
			"#DEFAULT"))
	  ((not (null cur-value))
	   (psgml-insert '(read-only t category psgml-form
				    rear-nonsticky (read-only category))
			" ")
	   (psgml-insert nil "%s" (psgml-attspec-attval cur-value))))
	 (psgml-insert
	  '(read-only 1)
	  "\n\t-- %s: %s --\n"
	  (cond ((psgml-declared-value-token-group dcl-value))
		((psgml-declared-value-notation dcl-value)
		 (format "NOTATION %s"
			 (psgml-declared-value-notation dcl-value)))
		(t
		 dcl-value))
	  (cond ((psgml-default-value-attval def-value))
		(t
		 (concat "#" (upcase (symbol-name def-value))))))))
      (psgml-insert '(read-only t) ">")
      (goto-char (point-min))
      (psgml-edit-attrib-next))
    buf))


(defvar psgml-edit-attrib-mode-map (make-sparse-keymap))
(define-key psgml-edit-attrib-mode-map "\C-c\C-c" 'psgml-edit-attrib-finish)
(define-key psgml-edit-attrib-mode-map "\C-c\C-d" 'psgml-edit-attrib-default)
(define-key psgml-edit-attrib-mode-map "\C-c\C-k" 'psgml-edit-attrib-clear)

(define-key psgml-edit-attrib-mode-map "\C-a"  'psgml-edit-attrib-field-start)
(define-key psgml-edit-attrib-mode-map "\C-e"  'psgml-edit-attrib-field-end)
(define-key psgml-edit-attrib-mode-map "\t"  'psgml-edit-attrib-next)

(defun psgml-edit-attrib-mode ()
  "Major mode to edit attribute specification list.\\<psgml-edit-attrib-mode-map>
Use \\[psgml-edit-attrib-next] to move between input fields.  Use
\\[psgml-edit-attrib-default] to make an attribute have its default
value.  To abort edit kill buffer (\\[kill-buffer]) and remove window
\(\\[delete-window]).  To finsh edit use \\[psgml-edit-attrib-finish].

\\{psgml-edit-attrib-mode-map}"
  (setq mode-name "SGML edit attributes"
	major-mode 'psgml-edit-attrib-mode)
  (use-local-map psgml-edit-attrib-mode-map)
  (run-hooks 'text-mode-hook 'psgml-edit-attrib-mode-hook))


(defun psgml-edit-attrib-finish ()
  "Finish editing and insert attribute values in original buffer."
  (interactive)
  (let ((cb (current-buffer))
	(asl (psgml-edit-attrib-specification-list))
	;; save buffer local variables
	(start psgml-start-attributes))
    (when (markerp start)
      (delete-windows-on cb)
      (switch-to-buffer (marker-buffer start))
      (kill-buffer cb)
      (goto-char start)
      (let ((element (psgml-find-element-of start)))
	;; *** Should the it be verified that this element
	;; is the one edited?
	(psgml-change-start-tag element asl)))))


(defun psgml-edit-attrib-specification-list ()
  (goto-char (point-min))
  (forward-line 1)
  (psgml-with-parser-syntax
   (let ((asl nil)
	 (al psgml-attlist))
     (while (not (eq ?> (following-char)))
       (psgml-parse-s)
       (psgml-check-nametoken)		; attribute name, should match head of al
       (forward-char 3)
       (unless (memq (get-text-property (point) 'category)
		     '(psgml-default psgml-fixed))
	 (push
	  (psgml-make-attspec (psgml-attdecl-name (car al))
			     (psgml-extract-attribute-value
			      (psgml-attdecl-declared-value (car al))))
	  asl))
       (while (progn (beginning-of-line 2)
		     (or (eolp)
			 (not (get-text-property (point) 'read-only)))))

       (forward-line 1)
       (setq al (cdr al)))
     asl)))


(defun psgml-extract-attribute-value (type)
  (save-excursion
    (save-restriction
      (narrow-to-region (point)
			(progn (psgml-edit-attrib-field-end)
			       (point)))
      (unless (eq type 'CDATA)
	(subst-char-in-region (point-min) (point-max) ?\n ? )
	(goto-char (point-min))
	(delete-horizontal-space))
      (goto-char (point-min))
      (when (search-forward "\"" nil t)	; don't allow both " and '
	(goto-char (point-min))
	(while (search-forward "'" nil t) ; replace ' with char ref
	  (replace-match "&#39;")))
      (buffer-string))))

(defun psgml-edit-attrib-default ()
  "Set current attribute value to default."
  (interactive)
  (psgml-edit-attrib-clear)
  (save-excursion
    (psgml-insert '(category psgml-default read-only psgml-default)
		 "#DEFAULT"))
  (let ((inhibit-read-only t))
    (put-text-property (1- (point)) (point)
		       'rear-nonsticky '(category))))

(defun psgml-edit-attrib-clear ()
  "Kill the value of current attribute."
  (interactive)
  (let ((inhibit-read-only '(psgml-default)))
    (psgml-edit-attrib-field-start)
    (let ((end (save-excursion (psgml-edit-attrib-field-end) (point))))
      (put-text-property (point) end 'read-only nil)
      (let ((inhibit-read-only t))
	(put-text-property (1- (point)) (point)
			   'rear-nonsticky '(read-only category)))
      (kill-region (point) end))))


(defun psgml-edit-attrib-field-start ()
  "Go to the start of the attribute value field."
  (interactive)
  (let (start)
    (beginning-of-line 1)
    (while (not (eq t (get-text-property (point) 'read-only)))
      (beginning-of-line 0))
    (while (eq 'psgml-form (get-text-property (point) 'category))
      (setq start (next-single-property-change (point) 'category))
      (unless start (error "No attribute value here"))
      (assert (number-or-marker-p start))
      (goto-char start))))

(defun psgml-edit-attrib-field-end ()
  "Go to the end of the attribute value field."
  (interactive)
  (psgml-edit-attrib-field-start)
  (let ((end (if (and (eolp)
		      (get-text-property (1+ (point)) 'read-only))
		 (point)
	       (next-single-property-change (point) 'read-only))))
    (assert (number-or-marker-p end))
    (goto-char end)))

(defun psgml-edit-attrib-next ()
  "Move to next attribute value."
  (interactive)
  (or (search-forward-regexp "^ *[_.:A-Za-z0-9---]+ *= ?" nil t)
      (goto-char (point-min))))


;;;; SGML mode: Hiding tags/attributes

(defconst psgml-tag-regexp
  "\\(</?>\\|</?[_A-Za-z][---_:A-Za-z0-9.]*\\(\\([^'\"></]\\|'[^']*'\\|\"[^\"]*\"\\)*\\)/?>?\\)")

(defun psgml-operate-on-tags (action &optional attr-p)
  (let ((buffer-modified-p (buffer-modified-p))
	(inhibit-read-only t)
	(buffer-read-only nil)
	(before-change-function nil)
	(markup-index			; match-data index in tag regexp
	 (if attr-p 2 1))
	(tagcount			; number tags to give them uniq
					; invisible properties
	 1))
    (unwind-protect
	(save-excursion
	  (goto-char (point-min))
	  (while (re-search-forward psgml-tag-regexp nil t)
	    (cond
	     ((eq action 'hide)
	      (let ((tag (downcase
			  (buffer-substring-no-properties
			   (1+ (match-beginning 0))
			   (match-beginning 2)))))
		(if (or attr-p (not (member tag psgml-exposed-tags)))
		    (add-text-properties
		     (match-beginning markup-index) (match-end markup-index)
		     (list 'invisible tagcount
			   'rear-nonsticky '(invisible face))))))
	     ((eq action 'show)		; ignore markup-index
	      (remove-text-properties (match-beginning 0) (match-end 0)
				      '(invisible nil)))
	     (t (error "Invalid action: %s" action)))
	    (incf tagcount)))
      (set-buffer-modified-p buffer-modified-p))))

(defun psgml-hide-tags ()
  "Hide all tags in buffer."
  (interactive)
  (psgml-operate-on-tags 'hide))

(defun psgml-show-tags ()
  "Show hidden tags in buffer."
  (interactive)
  (psgml-operate-on-tags 'show))

(defun psgml-hide-attributes ()
  "Hide all attribute specifications in the buffer."
  (interactive)
  (psgml-operate-on-tags 'hide 'attributes))

(defun psgml-show-attributes ()
  "Show all attribute specifications in the buffer."
  (interactive)
  (psgml-operate-on-tags 'show 'attributes))


;;;; SGML mode: Normalize (and misc manipulations)

(defun psgml-expand-shortref-to-text (name)
  (let (before-change-function
	(entity (psgml-lookup-entity name (psgml-dtd-entities psgml-dtd-info))))
    (cond
     ((null entity) (psgml-error "Undefined entity %s" name))
     ((psgml-entity-data-p entity)
      (psgml-expand-shortref-to-entity name))
     (t
      (delete-region psgml-markup-start (point))
      (psgml-entity-insert-text entity)
      (setq psgml-goal (point-max))	; May have changed size of buffer
      ;; now parse the entity text
      (setq psgml-rs-ignore-pos psgml-markup-start)
      (goto-char psgml-markup-start)))))

(defun psgml-expand-shortref-to-entity (name)
  (let ((end (point))
	(re-found nil)
	before-change-function)
    (goto-char psgml-markup-start)
    (setq re-found (search-forward "\n" end t))
    (delete-region psgml-markup-start end)
    (insert "&" name (if re-found "\n" ";"))
    (setq psgml-goal (point-max))	; May have changed size of buffer
    (goto-char (setq psgml-rs-ignore-pos psgml-markup-start))))

(defun psgml-expand-all-shortrefs (to-entity)
  "Expand all short references in the buffer.
Short references to text entities are expanded to the replacement text
of the entity other short references are expanded into general entity
references.  If argument, TO-ENTITY, is non-nil, or if called
interactive with numeric prefix argument, all short references are
replaced by generaly entity references."
  (interactive "*P")
  (psgml-reparse-buffer
   (if to-entity
       (function psgml-expand-shortref-to-entity)
     (function psgml-expand-shortref-to-text))))

(defun psgml-normalize (to-entity &optional element)
  "Normalize buffer by filling in omitted tags and expanding empty tags.
Argument TO-ENTITY controls how short references are expanded as with
`psgml-expand-all-shortrefs'.  An optional argument ELEMENT can be the
element to normalize insted of the whole buffer, if used no short
references will be expanded."
  (interactive "*P")
  (unless element
    (psgml-expand-all-shortrefs to-entity))
  (let ((only-one (not (null element))))
    (setq element (or element (psgml-top-element)))
    (goto-char (psgml-element-end element))
    (let ((before-change-function nil))
      (psgml-normalize-content element only-one)))
  (psgml-note-change-at (psgml-element-start element))
  (psgml-message "Done"))

(defun psgml-normalize-element ()
  (interactive "*")
  (psgml-normalize nil (psgml-find-element-of (point))))

(defun psgml-normalize-content (element only-first)
  "Normalize all elements in a content where ELEMENT is first element.
If psgml-normalize-trims is non-nil, trim off white space from ends of
elements with omitted end-tags."
  (let ((content nil))
    (while element			; Build list of content elements
      (push element content)
      (setq element (if only-first
			nil
		      (psgml-element-next element))))
    (while content
      (setq element (car content))
      ;; Progress report
      (psgml-lazy-message "Normalizing %d%% left"
			 (/ (point) (/ (+ (point-max) 100) 100)))
      ;; Fix the end-tag
      (psgml-normalize-end-tag element)
      ;; Fix tags of content
      (psgml-normalize-content (psgml-tree-content element) nil)
      ;; Fix the start-tag
      (psgml-normalize-start-tag element)
      ;; Next content element
      (setq content (cdr content)))))

(defun psgml-normalize-start-tag (element)
  (when (psgml-bpos-p (psgml-element-stag-epos element))
    (goto-char (min (point) (psgml-element-start element)))
    (let ((name (psgml-element-gi element))
	  (attlist (psgml-element-attlist element))
	  (asl (psgml-element-attribute-specification-list element)))
      (save-excursion
	(assert (or (zerop (psgml-element-stag-len element))
		    (= (point) (psgml-element-start element))))
	(delete-char (psgml-element-stag-len element))
	(psgml-insert-start-tag name asl attlist nil)))))

(defun psgml-normalize-end-tag (element)
  (unless (psgml-element-empty element)
    (when (psgml-bpos-p (psgml-element-etag-epos element))
      (goto-char (min (point) (psgml-element-etag-start element)))
      (if (and (zerop (psgml-element-etag-len element))
	       psgml-normalize-trims)
	  (skip-chars-backward " \t\n\r"))
      (delete-char (psgml-tree-etag-len element))
      (save-excursion (insert (psgml-end-tag-of element))))))


(defun psgml-make-character-reference (&optional invert)
  "Convert character after point into a character reference.
If called with a numeric argument, convert a character reference back
to a normal character.  If called from a program, set optional
argument INVERT to non-nil."
  (interactive "*P")
  (cond
   (invert
    (or (looking-at "&#\\([0-9]+\\)[;\n]?")
	(error "No character reference after point"))
    (let ((c (string-to-int (buffer-substring (match-beginning 1)
					      (match-end 1)))))
      (delete-region (match-beginning 0)
		     (match-end 0))
      (insert c)))
   ;; Convert character to &#nn;
   (t
    (let ((c (following-char)))
      (delete-char 1)
      (insert (format "&#%d;" c))))))

(defun psgml-expand-entity-reference ()
  "Insert the text of the entity referenced at point."
  (interactive)
  (save-excursion
    (psgml-with-parser-syntax
     (setq psgml-markup-start (point))
     (or (psgml-parse-delim "ERO")
	 (progn
	   (skip-syntax-backward "w_")
	   (forward-char -1)		; @@ Really length of ERO
	   (setq psgml-markup-start (point))
	   (psgml-check-delim "ERO")))
     (let* ((ename (psgml-check-name t))
	    (entity (psgml-lookup-entity ename
					(psgml-dtd-entities
					 (psgml-pstate-dtd
					  psgml-buffer-parse-state)))))
       (unless entity
	 (error "Undefined entity %s" ename))
       (or (psgml-parse-delim "REFC")
	   (psgml-parse-RE))
       (delete-region psgml-markup-start (point))
       (psgml-entity-insert-text entity)))))

(defvar psgml-notation-handlers
  '((gif . "xv")
    (jpeg . "xv"))
  "*An alist mapping notations to programs handling them")

;; Function contributed by Matthias Clasen <clasen@netzservice.de>
(defun psgml-edit-external-entity ()
  "Open	a new window and display the external entity at the point."
  (interactive)
  (psgml-need-dtd)
  (save-excursion
    (psgml-with-parser-syntax
     (setq psgml-markup-start (point))
     (unless (psgml-parse-delim "ERO")
       (search-backward-regexp "[&>;]")
       (setq psgml-markup-start (point))
       (psgml-check-delim "ERO"))
     (psgml-parse-to-here)		; get an up-to-date parse tree
     (let* ( (parent (buffer-file-name)) ; used to be (psgml-file)
	     (ename (psgml-check-name t))
	     (entity (psgml-lookup-entity ename
					 (psgml-dtd-entities
					  (psgml-pstate-dtd
					   psgml-buffer-parse-state))))
	     (buffer nil)
	     (ppos nil))
       (unless entity
	 (error "Undefined entity %s" ename))

       (let* ((type (psgml-entity-type entity))
	      (notation (psgml-entity-notation entity))
	      (handler (cdr (assoc notation psgml-notation-handlers))))
	 (case type
	   (ndata
	    (if handler
		(progn
		  (message (format "Using '%s' to handle notation '%s'."
				   handler notation))
		  (save-excursion
		    (set-buffer (get-buffer-create "*SGML background*"))
		    (erase-buffer)
		    (let* ((file (psgml-external-file
				  (psgml-entity-text entity)
				  type
				  (psgml-entity-name entity)))
			   (process (start-process
				     (format "%s background" handler)
				     nil handler file)))
		      (process-kill-without-query process))))
	      (error "Don't know how to handle notation '%s'." notation)))
	   (text (progn

	    ;; here I try to construct a useful value for
	    ;; `psgml-parent-element'.

	    ;; find sensible values for the HAS-SEEN-ELEMENT part
	    (let ((seen nil)
		  (child (psgml-tree-content psgml-current-tree)))
	      (while (and child
			  (psgml-tree-etag-epos child)
			  (<= (psgml-tree-end child) (point)))
		(push (psgml-element-gi child) seen)
		(setq child (psgml-tree-next child)))
	      (push (nreverse seen) ppos))

	    ;; find ancestors
	    (let ((rover psgml-current-tree))
	      (while (not (eq rover psgml-top-tree))
		(push (psgml-element-gi rover) ppos)
		(setq rover (psgml-tree-parent rover))))

	    (find-file-other-window
	     (psgml-external-file (psgml-entity-text entity)
				 (psgml-entity-type entity)
				 (psgml-entity-name entity)))
	    (goto-char (point-min))
	    (psgml-mode)
	    (setq psgml-parent-document (cons parent ppos))
	    ;; update the live element indicator of the new window
	    (psgml-parse-to-here)))
	   (t (error "Can't edit entities of type '%s'." type))))))))

;;;; SGML mode: TAB completion

(defun psgml-complete ()
  "Complete the word/tag/entity before point.
If it is a tag (starts with < or </) complete with valid tags.
If it is an entity (starts with &) complete with declared entities.
If it is a markup declaration (starts with <!) complete with markup
declaration names. If it is a reserved word starting with # complete
reserved words.
If it is something else complete with ispell-complete-word."
  (interactive "*")
  (let ((tab				; The completion table
	 nil)
	(ignore-case                    ; If ignore case in matching completion
	 psgml-namecase-general)
	(pattern nil)
	(c nil)
	(here (point)))
    (skip-chars-backward "^ \n\t</!&%#")
    (setq pattern (buffer-substring (point) here))
    (setq c (char-after (1- (point))))
    (cond
     ;; entitiy
     ((eq c ?&)
      (psgml-need-dtd)
      (setq tab
	    (psgml-entity-completion-table
	     (psgml-dtd-entities (psgml-pstate-dtd psgml-buffer-parse-state)))))
     ;; start-tag
     ((eq c ?<)
      (save-excursion
	(backward-char 1)
	(psgml-parse-to-here)
	(setq tab (psgml-eltype-completion-table
		   (psgml-current-list-of-valid-eltypes)))))
     ;; end-tag
     ((eq c ?/)
      (save-excursion
	(backward-char 2)
	(psgml-parse-to-here)
	(setq tab (psgml-eltype-completion-table
		   (psgml-current-list-of-endable-eltypes)))))
     ;; markup declaration
     ((eq c ?!)
      (setq tab psgml-markup-declaration-table
	    ignore-case t))
     ;; Reserved words with '#' prefix
     ((eq c ?#)
      (setq tab '(("PCDATA") ("NOTATION") ("IMPLIED") ("REQUIRED")
		  ("FIXED") ("EMPTY"))
	    ignore-case t))
     (t
      (goto-char here)
      (ispell-complete-word)))
    (when tab
      (let* ((completion-ignore-case ignore-case)
	     (completion (try-completion pattern tab)))
	(cond ((null completion)
	       (goto-char here)
	       (message "Can't find completion for \"%s\"" pattern)
	       (ding))
	      ((eq completion t)
	       (goto-char here)
	       (message "[Complete]"))
	      ((not (string= pattern completion))
	       (delete-char (length pattern))
	       (insert completion))
	      (t
	       (goto-char here)
	       (message "Making completion list...")
	       (let ((list (all-completions pattern tab)))
		 (with-output-to-temp-buffer " *Completions*"
		   (display-completion-list list)))
	       (message "Making completion list...%s" "done")))))))


;;;; SGML mode: Options menu

(defun psgml-file-options-menu (&optional event)
  (interactive "e")
  (psgml-options-menu event psgml-file-options))

(defun psgml-user-options-menu (&optional event)
  (interactive "e")
  (psgml-options-menu event psgml-user-options))

(defun psgml-options-menu (event vars)
  (let ((var
	 (let ((maxlen
		(loop for var in vars
		      maximize (length (psgml-variable-description var)))))
	   (psgml-popup-menu
	    event "Options"
	    (loop for var in vars
		  for desc = (psgml-variable-description var)
		  collect
		  (cons
		   (format "%s%s [%s]"
			   desc
			   (make-string (- maxlen (length desc)) ? )
			   (psgml-option-value-indicator var))
		   var))))))
    (when var
      (psgml-do-set-option var event))))

(defun psgml-do-set-option (var &optional event)
  (let ((type (psgml-variable-type var))
	(val (symbol-value var)))
    (cond
     ((eq 'toggle type)
      (message "%s set to %s" var (not val))
      (set var (not val)))
     ((eq 'string type)
      (describe-variable var)
      (setq val (read-string (concat (psgml-variable-description var) ": ")))
      (when (stringp val)
	(set var val)))
     ((eq 'file-list  type)
      (describe-variable var)
      (psgml-append-to-help-buffer "\
Enter as many filenames as you want. Entering a directory
or non-existing filename will exit the loop.")
      (setq val nil)
      (while (let ((next
		    (expand-file-name
		     (read-file-name
		      (concat (psgml-variable-description var) ": ")
		      nil "" nil nil))))
	       (if (and (file-exists-p next) (not (file-directory-p next)))
		   (setq val (cons next val)))))
      (set var val))
     ((eq 'file-or-nil type)
      (describe-variable var)
      (psgml-append-to-help-buffer "\
Entering a directory or non-existing filename here
will reset the variable.")
      (setq val (expand-file-name
		 (read-file-name
		  (concat (psgml-variable-description var) ": ")
		  nil (if (stringp val) (file-name-nondirectory val))
		  nil (if (stringp val) (file-name-nondirectory val)) )))
      (if (and (file-exists-p val) (not (file-directory-p val)))
	  (set var val)
	(set var nil)))
     ((consp type)
      (let ((val
	     (psgml-popup-menu event
			      (psgml-variable-description var)
			      (loop for c in type collect
				    (cons
				     (if (consp c) (car c) (format "%s" c))
				     (if (consp c) (cdr c) c))))))
	(set var val)
	(message "%s set to %s" var val)))
     (t
      (describe-variable var)
      (setq val (read-string (concat (psgml-variable-description var)
				     " (sexp): ")))
      (when (stringp val)
	(set var (car (read-from-string val)))))))
  (force-mode-line-update))

(defun psgml-append-to-help-buffer (string)
  (save-excursion
    (set-buffer "*Help*")
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "\n" string))))

;;;; NEW

(defun psgml-trim-and-leave-element ()
  (interactive)
  (goto-char (psgml-element-etag-start (psgml-last-element)))
  (while (progn (forward-char -1)
		(looking-at "\\s-"))
    (delete-char 1))
  (psgml-up-element))

(defun psgml-position ()
  (interactive)
  (let ((el (psgml-find-context-of (point)))
	(gis nil))
    (while (not (psgml-off-top-p el))
      (push (psgml-element-gi el) gis)
      (setq el (psgml-element-parent el)))
    (message "%s" (mapconcat #'psgml-general-insert-case
			     gis "\\"))))

(define-key psgml-mode-map "\C-c\C-y" 'psgml-position)


(defun psgml--add-before-p (tok state child)
  ;; Can TOK be added in STATE followed by CHILD
  (let ((snext (psgml-get-move state tok))
	(c child))
    (when snext
      (while c
	(setq snext (psgml-get-move snext
				   (psgml-eltype-token
				    (psgml-element-eltype c))))
	(setq c (and snext (psgml-element-next c)))))
    ;; If snext is still non nill it can be inserted
    snext))

(defun psgml--all-possible-elements (el)
  (let ((c (psgml-element-content el))
	(s (psgml-element-model el))
	(found nil))
    (loop do
	  (dolist (tok (nconc (psgml-optional-tokens s)
			      (psgml-required-tokens s)))
	    (unless (memq tok found)
	      ;; tok is optional here and not already found -- check that
	      ;; it would not make the content invalid
	      (when (psgml--add-before-p tok s c)
		  (push tok found))))
	  while c do
	  (setq s (psgml-element-pstate c))
	  (setq c (psgml-element-next c)))
    (mapcar #'psgml-token-eltype found)))


(defun psgml-add-element-to-element (gi first)
  "Add an element of type GI to the current element.
The element will be added at the last legal position if FIRST is `nil',
otherwise it will be added at the first legal position."
  (interactive
   (let ((tab
	  (mapcar (lambda (et) (cons (psgml-eltype-name et) nil))
		  (psgml--all-possible-elements
		   (psgml-find-context-of (point))))))
     (cond ((null tab)
	    (error "No element possible"))
	   (t
	    (let ((completion-ignore-case psgml-namecase-general))
	      (list (completing-read "Element: " tab nil t
				     (and (null (cdr tab)) (caar tab)))
		    current-prefix-arg))))))
  (let ((el (psgml-find-context-of (point)))
	(et (psgml-lookup-eltype (psgml-general-case gi))))
    ;; First expand empty tag
    (when (and psgml-pxml-p (psgml-element-empty el))
      (save-excursion
	(goto-char (psgml-element-stag-end el))
	(delete-char -2)
	(insert ">\n" (psgml-end-tag-of psgml-current-tree))
	(psgml-indent-line))
      (setq el (psgml-find-context-of (point))))
    (let ((c (psgml-element-content el))
	  (s (psgml-element-model el))
	  (tok (psgml-eltype-token et))
	  (last nil))
      ;; Find legal position for new element
      (while (and (not (cond
			((psgml--add-before-p tok s c)
			 (setq last (if c (psgml-element-start c)
				      (psgml-element-etag-start el)))
			 first)))
		  (cond
		   (c (setq s (psgml-element-pstate c))
		      (setq c (psgml-element-next c))
		      t))))
      (cond (last
	     (goto-char last)
	     (psgml-insert-element gi))
	    (t
	     (error "A %s element is not valid in current element" gi))))))

;;; psgml-edit.el ends here
