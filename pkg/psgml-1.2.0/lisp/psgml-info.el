;;;; psgml-info.el
;;; Last edited: 1998-11-25 21:34:05 lenst
;;; $Id: psgml-info.el,v 2.8 1998/12/06 20:56:18 lenst Exp $

;; Copyright (C) 1994, 1995 Lennart Staflin

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

Next page...

;;;; Commentary:

;; This file is an addon to the PSGML package.

;; This file contains some commands to print out information about the
;; current DTD.

;; psgml-list-elements
;;    Will list all elements and the attributes declared for the element.

;; psgml-list-attributes
;;    Will list all attributes declared and the elements that use them.

;; psgml-list-terminals
;;    Will list all elements that can contain data.

;; psgml-list-occur-in-elements
;;    Will list all element types and where it can occur.

;; psgml-list-content-elements
;;    Will list all element types and the element types that can occur
;;    in its content.

;;;; Code:

(require 'psgml)
(require 'psgml-parse)

(defconst psgml-attr-col 18)


;;;; Utility functions

(defsubst psgml-add-to-table (row-index elem table)
  (let ((p (assoc row-index table)))
    (cond ((null p)
	   (cons (list row-index elem) table))
	  (t
	   (nconc p (list elem))
	   table))))

(defsubst psgml-add-last-unique (x l)
  (unless (memq x l)
    (nconc l (list x))))

(defun psgml-map-element-types (func)
  (psgml-need-dtd)
  (psgml-map-eltypes func
		    (psgml-pstate-dtd psgml-buffer-parse-state)
		    t))

(defun psgml-eltype-refrenced-elements (eltype)
  "List of element types referenced in the model of ELTYPE."
  ;; Now with cache. Uses appdata prop re-cache.
  (or
   (psgml-eltype-appdata eltype 're-cache)
   (let* ((res				; result list (eltypes)
	   nil)
	  (states			; list of states
	   (list (psgml-eltype-model eltype)))
	  (agenda			; point into states
	   states))
     (cond
      ((not (psgml-model-group-p (car states)))
       nil)
      (t
       (while agenda
	 (cond
	  ((psgml-normal-state-p (car agenda))
	   (loop for m in (append (psgml-state-opts (car agenda))
				  (psgml-state-reqs (car agenda)))
		 do
		 (pushnew (psgml-move-token m) res)
		 (psgml-add-last-unique (psgml-move-dest m) states)))

	  (t				; &-node
	   (psgml-add-last-unique (psgml-and-node-next (car agenda)) states)
	   (loop for dfa in (psgml-and-node-dfas (car agenda)) do
		 (psgml-add-last-unique dfa states))))
	 (setq agenda (cdr agenda)))
       (setq res (sort (set-difference
			(union res (psgml-eltype-includes eltype))
			(psgml-eltype-excludes eltype))
		       (function string-lessp)))
       (setf (psgml-eltype-appdata eltype 're-cache) res)
       res)))))


;;;; List elements

(defun psgml-list-elements ()
  "List the elements and their attributes in the current DTD."
  (interactive)
  (message "Creating table...")
  (psgml-display-table
   (psgml-map-element-types
    (function
     (lambda (eltype)
       (cons (psgml-eltype-name eltype)
	     (mapcar (function psgml-attdecl-name)
		     (psgml-eltype-attlist eltype))))))
   "Elements" "Element" "Attribute"))


;;;; List attributes

(defun psgml-list-attributes ()
  "List the attributes and in which elements they occur."
  (interactive)
  (let ((attributes nil))
    (message "Creating table...")
    (psgml-map-element-types
     (function
      (lambda (eltype)
	(loop for a in (psgml-eltype-attlist eltype) do
	      (setq attributes
		    (psgml-add-to-table (psgml-attdecl-name a)
				       (psgml-eltype-name eltype)
				       attributes))))))
    (psgml-display-table attributes
			"Attributes" "Attribute" "Element")))




;;;; List terminals

(defun psgml-list-terminals ()
  "List the elements that can have data in their content."
  (interactive)
  (message "Creating table...")
  (let ((data-models (list psgml-cdata psgml-rcdata psgml-any)))
    (psgml-display-table
     (delq nil
	   (psgml-map-element-types
	    (function
	     (lambda (eltype)
	       (if (or (psgml-eltype-mixed eltype)
		       (memq (psgml-eltype-model eltype) data-models))
		   (list (psgml-eltype-name eltype)
			 (symbol-name
			  (if (psgml-model-group-p (psgml-eltype-model eltype))
			      'mixed
			    (psgml-eltype-model eltype)))))))))
     "Terminals" "Element" "Content")))


;;;; Element cross reference list

(defun psgml-list-content-elements ()
  "List all element types and the element types that can occur in its content."
  (interactive)
  (message "Creating table...")
  (psgml-display-table
   (psgml-map-element-types
    (function
     (lambda (eltype)
       (cons (psgml-eltype-name eltype)
	     (mapcar (function psgml-eltype-name)
		     (psgml-eltype-refrenced-elements eltype))))))
   "Elements referenced by elements"
   "Element" "Content"))

(defun psgml-list-occur-in-elements ()
  "List all element types and where it can occur."
  (interactive)
  (message "Creating table...")
  (let ((cross nil))
    (psgml-map-element-types
     (function
      (lambda (eltype)
	(loop for ref in (psgml-eltype-refrenced-elements eltype)
	      do (setq cross (psgml-add-to-table ref
						(psgml-eltype-name eltype)
						cross))))))
    (psgml-display-table
     cross
     "Cross referenced element types" "Element" "Can occur in")))


;;;; Display table

(defun psgml-display-table (table title col-title1 col-title2
				 &optional width nosort)
  (or width
      (setq width psgml-attr-col))
  (let ((buf (get-buffer-create (format "*%s*" title))))
    (message "Preparing display...")
    (set-buffer buf)
    (erase-buffer)
    (insert col-title1)
    (indent-to width)
    (insert col-title2 "\n")
    (insert-char ?= (length col-title1))
    (indent-to width)
    (insert-char ?= (length col-title2))
    (insert "\n")
    (unless nosort
      (setq table (sort table (function (lambda (a b)
					  (string< (car a) (car b)))))))
    (loop for e in table do
	  (insert (format "%s " (car e)))
	  (loop for name in (if nosort
				(cdr e)
			      (sort (cdr e) (function string-lessp)))
		do
		(when (> (+ (length name) (current-column))
			 fill-column)
		  (insert "\n"))
		(when (< (current-column) psgml-attr-col)
		  (indent-to width))
		(insert  name " "))
	  (insert "\n"))
    (goto-char (point-min))
    (display-buffer buf)
    (message nil)))


;;;; Describe entity

(defun psgml-describe-entity (name)
  "Describe the properties of an entity as declared in the current DTD."
  (interactive
   (let (default input)
     (psgml-need-dtd)
     (save-excursion
       (psgml-with-parser-syntax
	(unless (psgml-parse-delim "ERO")
	  (skip-chars-backward "^&\"'= \t\n"))
	(setq default (or (psgml-parse-name t) ""))))
     (setq input (completing-read
		  (format "Entity name (%s): " default)
		  (psgml-entity-completion-table
		   (psgml-dtd-entities
		    (psgml-pstate-dtd psgml-buffer-parse-state)))))
     (list
      (if (equal "" input) default input))))

  (with-output-to-temp-buffer "*Help*"
    (let ((entity (psgml-lookup-entity name
				      (psgml-dtd-entities
				       (psgml-pstate-dtd
					psgml-buffer-parse-state)))))
      (or entity (error "Undefined entity"))
      (princ (format "Entity %s is %s\n"
		     name
		     (cond ((null entity)
			    "undefined")
			   (t
			    (format "a %s entity"
				    (psgml-entity-type entity))))))
      (when entity
	(let ((text (psgml-entity-text entity))
	      (notation (psgml-entity-notation entity)))
	  (cond ((stringp text)
		 (princ "Defined to be:\n")
		 (princ text))
		(t
		 (princ "With external identifier ")
		 (princ (if (car text) "PUBLIC" "SYSTEM"))
		 (when (car text)
		   (princ (format " '%s'" (car text))))
		 (when (cdr text)
		   (princ (format " '%s'" (cdr text))))
		 (when notation
		   (princ (format "\nand notation '%s'" notation))))))))))



;;;; Describe element type

(defun psgml-princ-names (names)
  (loop with col = 0
	for name in names
	do
	(when (and (> col 0) (> (+ col (length name) 1) fill-column))
	  (princ "\n")
	  (setq col 0))
	(princ " ") (princ name)
	(incf col (length name))
	(incf col 1)))

(defun psgml-describe-element-type (et-name)
  "Describe the properties of an element type as declared in the current DTD."
  (interactive
   (let (default input)
     (psgml-need-dtd)
     (save-excursion
       (psgml-with-parser-syntax
	(unless (psgml-parse-delim "STAGO")
	  (skip-syntax-backward "w_"))
	(setq default (psgml-parse-name))
	(unless (and default
		     (psgml-eltype-defined (psgml-lookup-eltype default)))
	  (setq default nil))))
     (setq input (psgml-read-element-type (if default
					     (format "Element type (%s): "
						     default)
					   "Element type: ")
					 psgml-dtd-info
					 default))

     (list
      (psgml-eltype-name input))))

  (psgml-need-dtd)
  (let ((et (psgml-lookup-eltype et-name)))
    (with-output-to-temp-buffer "*Help*"
      (princ (format "ELEMENT: %s\n\n" (psgml-eltype-name et)))
      (princ (format " Start-tag is %s.\n End-tag is %s.\n"
		     (if (psgml-eltype-stag-optional et)
			 "optional" "required")
		     (if (psgml-eltype-etag-optional et)
			 "optional" "required")))
      (princ "\nATTRIBUTES:\n")
      (loop for attdecl in (psgml-eltype-attlist et) do
	    (let ((name (psgml-attdecl-name attdecl))
		  (dval (psgml-attdecl-declared-value attdecl))
		  (defl (psgml-attdecl-default-value attdecl)))
	      (when (listp dval)
		(setq dval (concat (if (eq (first dval)
					   'NOTATION)
				       "#NOTATION (" "(")
				   (mapconcat (function identity)
					      (second dval)
					      "|")
				   ")")))
	      (cond ((psgml-default-value-type-p 'FIXED defl)
		     (setq defl (format "#FIXED '%s'"
					(psgml-default-value-attval defl))))
		    ((symbolp defl)
		     (setq defl (upcase (format "#%s" defl))))
		    (t
		     (setq defl (format "'%s'"
					(psgml-default-value-attval defl)))))
	      (princ (format " %-9s %-30s %s\n" name dval defl))))
      ;; ----
      (let ((s (psgml-eltype-shortmap et)))
	(when s
	  (princ (format "\nUSEMAP: %s\n" s))))
      ;; ----
      (princ "\nCONTENT: ")
      (cond ((symbolp (psgml-eltype-model et)) (princ (psgml-eltype-model et)))
	    (t
	     (princ (if (psgml-eltype-mixed et) "mixed\n\n"
		       "element\n\n"))
	     (psgml-princ-names
	      (mapcar #'symbol-name (psgml-eltype-refrenced-elements et)))))

      ;; ----
      (princ "\n\nOCCURS IN:\n\n")
      (let ((occurs-in ()))
	(psgml-map-eltypes
	 (function (lambda (cand)
		     (when (memq et (psgml-eltype-refrenced-elements cand))
		       (push cand occurs-in))))
	 (psgml-pstate-dtd psgml-buffer-parse-state))

	(loop with col = 0
	      for occur-et in (sort occurs-in (function string-lessp))
	      for name = (psgml-eltype-name occur-et)
	      do
	      (when (and (> col 0) (> (+ col (length name) 1) fill-column))
		(princ "\n")
		(setq col 0))
	      (princ " ") (princ name)
	      (incf col (length name))
	      (incf col 1))))))


;;;; Print general info about the DTD.

(defun psgml-general-dtd-info ()
  "Display information about the current DTD."
  (interactive)
  (psgml-need-dtd)
  (let ((elements 0)
	(entities 0)
	(parameters 0)
	(fmt "%20s %s\n")
	(hdr "")
	)
    (psgml-map-eltypes (function (lambda (e) (incf elements)))
		      psgml-dtd-info)
    (psgml-map-entities (function (lambda (e) (incf entities)))
		       (psgml-dtd-entities psgml-dtd-info))
    (psgml-map-entities (function (lambda (e) (incf parameters)))
		       (psgml-dtd-parameters psgml-dtd-info))

    (with-output-to-temp-buffer "*Help*"
      (princ (format fmt "Doctype:" (psgml-dtd-doctype psgml-dtd-info)))
      (when (psgml-dtd-merged psgml-dtd-info)
	(princ (format fmt "Compiled DTD:"
		       (car (psgml-dtd-merged psgml-dtd-info)))))
      (princ (format fmt "Element types:" (format "%d" elements)))
      (princ (format fmt "Entities:" (format "%d" entities)))
      (princ (format fmt "Parameter entities:" (format "%d" parameters)))

      (setq hdr "Files used:")
      (loop for x in (psgml-dtd-dependencies psgml-dtd-info)
	    if (stringp x)
	    do (princ (format fmt hdr x))
	    (setq hdr ""))

      (setq hdr "Undef parameters:")
      (psgml-map-entities
       (function (lambda (entity)
		   (when (psgml-entity-marked-undefined-p entity)
		     (princ (format fmt hdr (psgml-entity-name entity)))
		     (setq hdr ""))))
       (psgml-dtd-parameters psgml-dtd-info)))))

;;; psgml-info.el ends here
