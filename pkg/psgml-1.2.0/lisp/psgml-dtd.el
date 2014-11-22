;;;; psgml-dtd.el --- DTD parser for SGML-editing mode with parsing support
;; $Id: psgml-dtd.el,v 2.24 1998/11/14 12:02:15 lenst Exp $

;; Copyright (C) 1994 Lennart Staflin

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

(provide 'psgml-dtd)
(require 'psgml)
(require 'psgml-parse)


;;;; Variables

;; Variables used during doctype parsing and loading
(defvar psgml-used-pcdata nil
  "True if model group built is mixed")


;;;; Constructing basic

(defun psgml-copy-moves (s1 s2)
  "Copy all moves from S1 to S2, keeping their status."
  (let ((l (psgml-state-opts s1)))
    (while l
      (psgml-add-opt-move s2
			 (psgml-move-token (car l))
			 (psgml-move-dest (car l)))
      (setq l (cdr l)))
    (setq l (psgml-state-reqs s1))
    (while l
      (psgml-add-req-move s2
			 (psgml-move-token (car l))
			 (psgml-move-dest (car l)))
      (setq l (cdr l)))))

(defun psgml-copy-moves-to-opt (s1 s2)
  "Copy all moves from S1 to S2 as optional moves."
  (let ((l (psgml-state-opts s1)))
    (while l
      (psgml-add-opt-move s2
			 (psgml-move-token (car l))
			 (psgml-move-dest (car l)))
      (setq l (cdr l)))
    (setq l (psgml-state-reqs s1))
    (while l
      (psgml-add-opt-move s2
			 (psgml-move-token (car l))
			 (psgml-move-dest (car l)))
      (setq l (cdr l)))))


(defun psgml-some-states-of (state)
  ;; List of some states reachable from STATE, includes all final states
  (let* ((states (list state))
	 (l states)
	 s ms m)
    (while l
      (setq s (car l)
	    ms (append (psgml-state-opts s) (psgml-state-reqs s)))
      (while ms
	(setq m (psgml-move-dest (car ms))
	      ms (cdr ms))
	(unless (psgml-normal-state-p m)
	  (setq m (psgml-and-node-next m)))
	(unless (memq m states)
	  (nconc states (list m))))
      (setq l (cdr l)))
    states))

(defmacro psgml-for-all-final-states (s dfa &rest forms)
  "For all final states S in DFA do FORMS.
Syntax: var dfa-expr &body forms"
  (` (let ((L-states (psgml-some-states-of (, dfa)))
	   (, s))
       (while L-states
	 (when (psgml-state-final-p (setq (, s) (car L-states)))
	   (,@ forms))
	 (setq L-states (cdr L-states))))))

(put 'psgml-for-all-final-states 'lisp-indent-hook 2)
(put 'psgml-for-all-final-states 'edebug-form-hook '(symbolp &rest form))


;;;; Optimization for the dfa building

(defsubst psgml-empty-state-p (s)
  ;; True if S hase no outgoing moves
  (and (psgml-normal-state-p s)
       (null (psgml-state-reqs s))
       (null (psgml-state-opts s)))  )

(defun psgml-one-final-state (s)
  ;; Collaps all states that have no moves
  ;; This is a safe optimization, useful for (..|..|..)
  (psgml-debug "OPT one final: reqs %d opts %d"
	      (length (psgml-state-reqs s))
	      (length (psgml-state-opts s)))
  (let ((final nil)
	dest)
    (loop for m in (append (psgml-state-reqs s)
			   (psgml-state-opts s))
	  do
	  (setq dest (psgml-move-dest m))
	  (when (psgml-empty-state-p dest)
	    (cond ((null final)
		   (setq final dest))
		  (t
		   (setf (psgml-move-dest m) final)))))))

(defun psgml-states-equal (s1 s2)
  (and (= (length (psgml-state-opts s1))
	  (length (psgml-state-opts s2)))
       (= (length (psgml-state-reqs s1))
	  (length (psgml-state-reqs s2)))
       (loop for m in (psgml-state-opts s1)
	     always
	     (eq (psgml-move-dest m)
		 (psgml-move-dest (psgml-moves-lookup (psgml-move-token m)
						    (psgml-state-opts s2)))))
       (loop for m in (psgml-state-reqs s1)
	     always
	     (eq (psgml-move-dest m)
		 (psgml-move-dest (psgml-moves-lookup (psgml-move-token m)
						    (psgml-state-reqs s2)))))))

(defun psgml-remove-redundant-states-1 (s)
  ;; Remove states accessible from s with one move and equivalent to s,
  ;; by changing the moves from s.
  (psgml-debug "OPT redundant-1: reqs %d opts %d"
	      (length (psgml-state-reqs s))
	      (length (psgml-state-opts s)))
  (let ((yes nil)
	(no (list s))
	(l (psgml-state-reqs s))
	(nl (psgml-state-opts s))
	dest)
    (while (or l (setq l (prog1 nl (setq nl nil))))
      (cond
       ((not (psgml-normal-state-p (setq dest (psgml-move-dest (car l))))))
       ((memq dest no))
       ((memq dest yes))
       ((psgml-states-equal s dest)
	(progn (push dest yes))))
      (setq l (cdr l)))
    (setq l (psgml-state-opts s)
	  nl (psgml-state-reqs s))
    (when yes
      (psgml-debug "OPT redundant-1: sucess %s" (length yes))
      (while (or l (setq l (prog1 nl (setq nl nil))))
	(cond ((memq (psgml-move-dest (car l)) yes)
	       (setf (psgml-move-dest (car l)) s)))
	(setq l (cdr l))))))



;;;; Constructing

(defun psgml-make-opt (s1)
  (when (psgml-state-reqs s1)
    (setf (psgml-state-opts s1)
	  (nconc (psgml-state-opts s1)
		 (psgml-state-reqs s1)))
    (setf (psgml-state-reqs s1) nil))
  s1)

(defun psgml-make-* (s1)
  (setq s1 (psgml-make-+ s1))
  (when (psgml-state-reqs s1)
    (psgml-make-opt s1))
  (psgml-remove-redundant-states-1 s1)
  s1)

(defun psgml-make-+ (s1)
  (psgml-for-all-final-states s s1
    (psgml-copy-moves-to-opt s1 s))
  (psgml-remove-redundant-states-1 s1)	; optimize
  s1)

(defun psgml-make-conc (s1 s2)
  (let ((moves (append (psgml-state-reqs s1) (psgml-state-opts s1))))
    (cond
     (;; optimize the case where all moves from s1 goes to empty states
      (loop for m in moves
	    always (psgml-empty-state-p (psgml-move-dest m)))
      (loop for m in moves do (setf (psgml-move-dest m) s2))
      (when (psgml-state-final-p s1)
	(psgml-copy-moves s2 s1)))
     (t					; general case
      (psgml-for-all-final-states s s1
	(psgml-copy-moves s2 s)
	(psgml-remove-redundant-states-1 s)))))
  s1)

(defun psgml-make-pcdata ()
  (psgml-make-* (psgml-make-primitive-content-token psgml-pcdata-token)))

(defun psgml-reduce-, (l)
  (while (cdr l)
    (setcar (cdr l)
	    (psgml-make-conc (car l) (cadr l)))
    (setq l (cdr l)))
  (car l))

(defun psgml-reduce-| (l)
  (while (cdr l)			; apply the binary make-alt
    (cond ((or (psgml-state-final-p (car l))	; is result optional
	       (psgml-state-final-p (cadr l)))
	   (psgml-make-opt (car l))
	   (psgml-copy-moves-to-opt (cadr l) (car l)))
	  (t
	   (psgml-copy-moves (cadr l) (car l))))
    (setcdr l (cddr l)))
  (psgml-one-final-state (car l))	; optimization
  (car l))

(defun psgml-make-& (dfas)
  (let ((&n (psgml-make-and-node dfas (psgml-make-state)))
	(s (psgml-make-state))
	(l dfas))
    (while l				; For each si:
      ;; For m in opts(si): add optional move from s to &n on token(m).
      (loop for m in (psgml-state-opts (car l))
	    do (psgml-add-opt-move s (psgml-move-token m) &n))
      ;; For m in reqs(si): add required move from s to &n on token(m).
      (loop for m in (psgml-state-reqs (car l))
	    do (psgml-add-req-move s (psgml-move-token m) &n))
      (setq l (cdr l)))
    ;; Return s.
    s))



;(psgml-make-conc (psgml-make-primitive-content-token 'para) (psgml-make-primitive-content-token 'list))
;(psgml-make-conc (psgml-make-& (list (psgml-make-primitive-content-token 'para) (psgml-make-primitive-content-token 'list))) (psgml-make-primitive-content-token 'foo))

;(setq x  (psgml-some-states-of  (psgml-make-primitive-content-token 'para)))
;(psgml-state-final-p (car x) )
;(psgml-state-final-p (cadr x))


;;;; Parse doctype: General

(defun psgml-skip-ts ()
  ;; Skip over ts*
  ;;70  ts   = 5 s | EE | 60+ parameter entity reference
  ;;For simplicity I use ps*
  ;;65  ps   = 5 s | EE | 60+ parameter entity reference | 92 comment
  ;;*** some comments are accepted that shouldn't
  (psgml-skip-ps))

(defun psgml-parse-character-reference (&optional dofunchar)
  ;; *** Actually only numerical character references
  ;; I don't know how to handel the function character references.
  ;; For the shortrefs let's give them numeric values.
  (if (if dofunchar
	  (psgml-parse-delim "CRO" (digit nmstart))
	(psgml-parse-delim "CRO" (digit)))
      (prog1 (if (psgml-is-delim "NULL" digit)
		 (string-to-int (psgml-check-nametoken))
	       (let ((spec (psgml-check-name)))
		 (or (cdr (assoc spec '(("RE" . 10)
					("RS" . 1)
					("TAB" . 9)
					("SPACE" . 32))))
		     ;; *** What to do with other names?
		     127)))
	(or (psgml-parse-delim "REFC")
	    (psgml-parse-RE)))))

(defun psgml-parse-parameter-literal (&optional dofunchar)
  (let* (lita				; flag if lita
	 (value				; accumulates literals value
	  "")
	 (original-buffer		; Buffer (entity) where lit started
	  (current-buffer))
	 temp
	 )
    (cond
     ((or (psgml-parse-delim "LIT")
	  (setq lita (psgml-parse-delim "LITA")))
      (while (not (and (eq (current-buffer) original-buffer)
		       (if lita
			   (psgml-parse-delim "LITA")
			 (psgml-parse-delim "LIT"))))
	(cond ((eobp)
	       (or (psgml-pop-entity)
		   (psgml-error "Parameter literal unterminated")))
	      ((psgml-parse-parameter-entity-ref))
	      ((setq temp (psgml-parse-character-reference dofunchar))
	       (setq value (concat value (if (< temp 256)
					     (format "%c" temp)
					   (format "&#%d;" temp)))))
	      (t
	       (setq value
		     (concat value
			     (buffer-substring
			      (point)
			      (progn (forward-char 1)
				     (if lita
					 (psgml-skip-upto ("LITA" "PERO" "CRO"))
				       (psgml-skip-upto ("LIT" "PERO" "CRO")))
				     (point)))))))
	)
      value))))

(defun psgml-check-parameter-literal ()
  (or (psgml-parse-parameter-literal)
      (psgml-parse-error "Parameter literal expected")))

(defsubst psgml-parse-connector ()
  (psgml-skip-ps)
  (cond ((psgml-parse-delim "SEQ")
	 (function psgml-reduce-,))
	((psgml-parse-delim "OR")
	 (function psgml-reduce-|))
	((psgml-parse-delim "AND")
	 (if psgml-pxml-p
	     (psgml-error "XML forbids AND connector.")
	   (function psgml-make-&)))))

(defun psgml-parse-name-group ()
  "Parse a single name or a name group (general name case) .
Returns a list of strings or nil."
  (let (names)
    (cond
     ((psgml-parse-delim "GRPO")
      (psgml-skip-ps)
      (setq names (psgml-parse-name-group)) ; *** Allows more than it should
      (while (psgml-parse-connector)
	(psgml-skip-ps)
	(nconc names (psgml-parse-name-group)))
      (psgml-check-delim "GRPC")
      names)
     ((setq names (psgml-parse-name))
      (list names)))))

(defun psgml-check-name-group ()
  (or (psgml-parse-name-group)
      (psgml-parse-error "Expecting a name or a name group")))

(defun psgml-check-nametoken-group ()
  "Parse a name token group, return a list of strings.
Case transformed for general names."
  (psgml-skip-ps)
  (let ((names nil))
    (cond
     ((psgml-parse-delim GRPO)
      (while (progn
	       (psgml-skip-ps)
	       (push (psgml-general-case (psgml-check-nametoken)) names)
	       (psgml-parse-connector)))
      (psgml-check-delim GRPC)
      (nreverse names))			; store in same order as declared
     (t
      (list (psgml-general-case (psgml-check-nametoken)))))))

(defun psgml-check-element-type ()
  "Parse and check an element type, returns list of strings."
;;; 117  element type     =  [[30 generic identifier]]
;;;                      |  [[69 name group]]
;;;                      |  [[118 ranked element]]
;;;                      |  [[119 ranked group]]
  (cond
   ((psgml-parse-delim GRPO)
    (when psgml-pxml-p
      (psgml-error "XML forbids name groups for the element type"))
    (psgml-skip-ts)
    (let ((names (list (psgml-check-name))))
      (while (progn (psgml-skip-ts)
		    (psgml-parse-connector))
	(psgml-skip-ts)
	(nconc names (list (psgml-check-name))))
      (psgml-check-delim GRPC)
      ;; A ranked group will have a rank suffix here
      (psgml-skip-ps)
      (if (psgml-is-delim "NULL" digit)
	(let ((suffix (psgml-parse-nametoken)))
	  (loop for n in names
		collect (concat n suffix)))
	names)))
   (t					; gi/ranked element
    (let ((name (psgml-check-name)))
      (psgml-skip-ps)
      (list (if (psgml-is-delim "NULL" digit)
		(concat name (psgml-check-nametoken))
	      name))))))


(defun psgml-check-external (&optional pubid-ok)
  (or (psgml-parse-external pubid-ok)
      (psgml-parse-error "Expecting a PUBLIC or SYSTEM")))

;;;; Parse doctype: notation

(defun psgml-declare-notation ()
  ;;148  notation declaration = MDO, "NOTATION",
  ;;                        65 ps+, 41 notation name,
  ;;                        65 ps+, 149 notation identifier,
  ;;                        65 ps*, MDC
  ;;41   notation name    = 55 name
  ;;149  notation identifier = 73 external identifier
  (psgml-skip-ps)
  (psgml-check-name)
  (psgml-skip-ps)
  (psgml-check-external t))


;;;; Parse doctype: Element

(defun psgml-parse-opt ()
  (psgml-skip-ps)
  (cond ((or (psgml-parse-char ?o)
	     (psgml-parse-char ?O))
	 (if psgml-pxml-p
	      (psgml-error "XML forbids omitted tag minimization.")
	   t))
	((psgml-parse-char ?-)
	 (if psgml-pxml-p
	     (psgml-error "XML forbids omitted tag minimization.")
	   nil))))

(defun psgml-parse-modifier ()
  (cond ((psgml-parse-delim "PLUS")
	 (function psgml-make-+))
	((psgml-parse-delim "REP")
	 (function psgml-make-*))
	((psgml-parse-delim "OPT")
	 (function psgml-make-opt))))

(defun psgml-check-primitive-content-token ()
  (psgml-make-primitive-content-token
   (psgml-eltype-token
    (psgml-lookup-eltype
     (psgml-check-name)))))

(defun psgml-check-model-group ()
  (psgml-skip-ps)
  (let (el mod)
    (cond
     ((psgml-parse-delim "GRPO")
      (let ((subs (list (psgml-check-model-group)))
	    (con1 nil)
	    (con2 nil))
	(while (setq con2 (psgml-parse-connector))
	  (cond ((and con1
		      (not (eq con1 con2)))
		 (psgml-parse-error "Mixed connectors")))
	  (setq con1 con2)
	  (setq subs (nconc subs (list (psgml-check-model-group)))))
	(psgml-check-delim "GRPC")
	(setq el (if con1
		     (funcall con1 subs)
		   (car subs)))))
     ((psgml-parse-rni "PCDATA")         ; #PCDATA (FIXME: when changing case)
      (setq psgml-used-pcdata t)
      (setq el (psgml-make-pcdata)))
     ((psgml-parse-delim "DTGO")			; data tag group
      (when psgml-pxml-p
	(psgml-error "XML forbids DATATAG."))
      (psgml-skip-ts)
      (let ((tok (psgml-check-primitive-content-token)))
	(psgml-skip-ts) (psgml-check-delim "SEQ")
	(psgml-skip-ts) (psgml-check-data-tag-pattern)
	(psgml-skip-ts) (psgml-check-delim "DTGC")
	(setq el (psgml-make-conc tok (psgml-make-pcdata)))
	(setq psgml-used-pcdata t)))
     (t
      (setq el (psgml-check-primitive-content-token))))
    (setq mod (psgml-parse-modifier))
    (if mod
	(funcall mod el)
      el)))

(defun psgml-check-data-tag-pattern ()
  ;; 134  data tag pattern
  ;; template | template group
  (cond ((psgml-parse-delim GRPO)
	 (psgml-skip-ts)
	 (psgml-check-parameter-literal)	; data tag template,
	 (while (progn (psgml-skip-ts)
		       (psgml-parse-delim OR))
	   (psgml-skip-ts)
	   (psgml-check-parameter-literal)) ; data tag template
	 (psgml-skip-ts)
	 (psgml-check-delim GRPC))
	(t
	 (psgml-check-parameter-literal))) ; data tag template
  (psgml-skip-ts)
  (when (psgml-parse-delim SEQ)
    (psgml-check-parameter-literal)))	; data tag padding template

(defun psgml-check-content-model ()
  (psgml-check-model-group))

(defun psgml-check-content ()
  (psgml-skip-ps)
  (cond ((psgml-is-delim GRPO)
	 (psgml-check-content-model))
	(t
	 ;; ANY, CDATA, RCDATA or EMPTY
	 (let ((dc (intern (psgml-check-name))))
	   (cond ((eq dc 'ANY)
		  (setq psgml-used-pcdata t))
		 ((eq dc 'CDATA)
		  (when psgml-pxml-p
		    (psgml-error "XML forbids CDATA declared content.")))
		 ((eq dc 'RCDATA)
		  (when psgml-pxml-p
		    (psgml-error "XML forbids RCDATA declared content")))
		 ((eq dc 'EMPTY))
		 (t
		  (psgml-error "Exptected content model group or one of %s"
			      (if psgml-pxml-p
				  "ANY or EMPTY"
				  "ANY, CDATA, RCDATA or EMPTY"))))
	   dc))))

(defun psgml-parse-exeption (type)
  (psgml-skip-ps)
  (if (psgml-parse-char type)
      (if psgml-pxml-p
	   (psgml-error "XML forbids inclusion and exclusion exceptions.")
	(mapcar (function psgml-lookup-eltype)
		(psgml-check-name-group)))))

(defun psgml-before-eltype-modification ()
;;;  (let ((merged (psgml-dtd-merged psgml-dtd-info)))
;;;    (when (and merged
;;;	       (eq (psgml-dtd-eltypes psgml-dtd-info)
;;;		   (psgml-dtd-eltypes (cdr merged))))
;;;      (setf (psgml-dtd-eltypes psgml-dtd-info)
;;;	    (psgml-merge-eltypes (psgml-make-eltypes-table)
;;;				(psgml-dtd-eltypes psgml-dtd-info)))))
  )

(defun psgml-declare-element ()
  (let* ((names (psgml-check-element-type))
	 (stag-opt (psgml-parse-opt))
	 (etag-opt (psgml-parse-opt))
	 (psgml-used-pcdata nil)
	 (model (psgml-check-content))
	 (exclusions (psgml-parse-exeption ?-))
	 (inclusions (psgml-parse-exeption ?+)))
    (psgml-before-eltype-modification)
    (while names
      (psgml-debug "Defining element %s" (car names))
      (let ((et (psgml-lookup-eltype (car names))))
	(setf (psgml-eltype-stag-optional et) stag-opt
	      (psgml-eltype-etag-optional et) etag-opt
	      (psgml-eltype-model et) model
	      (psgml-eltype-mixed et) psgml-used-pcdata
	      (psgml-eltype-excludes et) exclusions
	      (psgml-eltype-includes et) inclusions))
      (setq names (cdr names)))
    (psgml-lazy-message "Parsing doctype (%s elements)..."
		       (incf psgml-no-elements))))

;;;; Parse doctype: Entity

(defun psgml-declare-entity ()
  (let (name				; Name of entity
	dest				; Entity table
	(type 'text)			; Type of entity
	(notation nil)                  ; Notation of entity
	text				; Text of entity
	extid				; External id
	)
    (cond
     ((psgml-parse-delim "PERO")		; parameter entity declaration
      (psgml-skip-ps)
      (setq name (psgml-check-name t))
      (setq dest (psgml-dtd-parameters psgml-dtd-info)))
     (t					; normal entity declaration
      (or (psgml-parse-rni "DEFAULT")
	  (setq name (psgml-check-name t)))
      (setq dest (psgml-dtd-entities psgml-dtd-info))))
    (psgml-skip-ps)
    ;;105  entity text  = 66 parameter literal
    ;;                 | 106 data text
    ;;                 | 107 bracketed text
    ;;                 | 108 external entity specification
    (setq extid (psgml-parse-external))
    (setq text
	  (cond
	   (extid			; external entity specification =
					; 73 external identifier,
					; (65 ps+, 109+ entity type)?
	    (psgml-skip-ps)
	    (let ((tn (psgml-parse-entity-type)))
	      (setq type (or (car tn) 'text))
	      (unless (eq (cdr tn) "")
		(setq notation (cdr tn))))
	    extid)
	   ((psgml-startnm-char-next)
	    (let ((token (intern (psgml-check-case (psgml-check-name)))))
	      (psgml-skip-ps)
	      (when (and psgml-pxml-p
			 (memq token '(cdata sdata pi starttag endtag ms md)))
		(psgml-error "XML forbids %s entities."
			    (upcase (symbol-name token))))
	      (cond
	       ((memq token '(CDATA SDATA)) ; data text ***
		(setq type token)
		(psgml-check-parameter-literal))
	       ((eq token 'PI)
		(concat "<?" (psgml-check-parameter-literal) ">"))
	       ((eq token 'STARTTAG)
		(psgml-start-tag-of (psgml-check-parameter-literal)))
	       ((eq token 'ENDTAG)
		(psgml-end-tag-of (psgml-check-parameter-literal)))
	       ((eq token 'MS)		; marked section
		(concat "<![" (psgml-check-parameter-literal) "]]>"))
	       ((eq token 'MD)		; Markup declaration
		(concat "<!" (psgml-check-parameter-literal) ">")))))
	   ((psgml-check-parameter-literal))))
    (when dest
      (psgml-entity-declare name dest type text notation))))


(defun psgml-parse-entity-type ()
  ;;109+ entity type      = "SUBDOC"
  ;;                      | (("CDATA" | "NDATA" | "SDATA"),
  ;;                             65 ps+,
  ;;                             41 notation name,
  ;;                             149.2+ data attribute specification?)
  (let ((type (psgml-parse-name))
	(notation nil))
    (when type
      (setq type (intern (downcase (psgml-check-case type))))
      (when (and psgml-pxml-p (memq type '(subdoc cdata sdata)))
	(psgml-error "XML forbids %s entities."
		    (upcase (symbol-name type))))
      (cond ((eq type 'subdoc))
	    ((memq type '(cdata ndata sdata))
	     (psgml-skip-ps)
	     (setq notation (psgml-parse-name))
	     (when notation
	       (setq notation (intern (downcase (psgml-check-case notation)))))
	     ;;149.2+ data attribute specification
	     ;;                      = 65 ps+, DSO,
	     ;;                        31 attribute specification list,
	     ;;                        5 s*, DSC
	     (psgml-skip-ps)
	     (when (psgml-parse-delim DSO)
	       (psgml-parse-attribute-specification-list)
	       (psgml-parse-s)
	       (psgml-check-delim DSC)))
	    (t (psgml-error "Illegal entity type: %s" type))))
    (cons type notation)
    ))


;;;; Parse doctype: Attlist

(defun psgml-declare-attlist ()
  (let* ((assnot (cond ((psgml-parse-rni "NOTATION")
			(when psgml-pxml-p
			  (psgml-error "XML forbids data attribute declarations"))
			(psgml-skip-ps)
			t)))
	 (assel (psgml-check-name-group))
	 (attlist nil)
	 (attdef nil))
    (when (and psgml-pxml-p (> (length assel) 1))
      (psgml-error "XML forbids name groups for an associated element type."))
    (while (setq attdef (psgml-parse-attribute-definition))
      (push attdef attlist))
    (setq attlist (nreverse attlist))
    (unless assnot
      (psgml-before-eltype-modification)
      (loop for elname in assel do
	    (setf (psgml-eltype-attlist (psgml-lookup-eltype elname))
		  (psgml-merge-attlists
		   (psgml-eltype-attlist
		    (psgml-lookup-eltype elname))
		   attlist))))))

(defun psgml-merge-attlists (old new)
  (setq old (nreverse (copy-list old)))
  (loop for att in new do
	(unless (assoc (car att) old)
	  (setq old (cons att old))))
  (nreverse old))

(defun psgml-parse-attribute-definition ()
  (psgml-skip-ps)
  (if (psgml-is-delim "MDC") ; End of attlist?
      nil
    (psgml-make-attdecl (psgml-check-name)
		       (psgml-check-declared-value)
		       (psgml-check-default-value))))

(defun psgml-check-declared-value ()
  (psgml-skip-ps)
  (let ((type 'name-token-group)
	(names nil))
    (unless (eq (following-char) ?\()
      (setq type (intern (psgml-check-case (psgml-check-name))))
      (psgml-validate-declared-value type)
      (psgml-skip-ps))
    (when (memq type '(name-token-group NOTATION))
      (setq names (psgml-check-nametoken-group)))
    (psgml-make-declared-value type names)))

(defun psgml-validate-declared-value (type)
  (unless (memq type
		'(CDATA
		  ENTITY
		  ENTITIES
		  ID
		  IDREF
		  IDREFS
		  NAME
		  NAMES
		  NMTOKEN
		  NMTOKENS
		  NOTATION
		  NUMBER
		  NUMBERS
		  NUTOKEN
		  NUTOKENS))
    (psgml-error "Invalid attribute declared value: %s" type))
  (when (and psgml-pxml-p (memq type
			      '(NAME NAMES NUMBER NUMBERS NUTOKEN NUTOKENS)))
    (psgml-error "XML forbids %s attributes." (upcase (symbol-name type)))))

(defun psgml-check-default-value ()
  (psgml-skip-ps)
  (let* ((rni (psgml-parse-rni))
	 (key (if rni (intern (psgml-check-case (psgml-check-name))))))
    (if rni (psgml-validate-default-value-rn key))
    (psgml-skip-ps)
    (psgml-make-default-value
     key
     (if (or (not rni) (eq key 'FIXED))
	 (psgml-check-attribute-value-specification)))))

(defun psgml-validate-default-value-rn (rn)
  (unless (memq rn '(REQUIRED FIXED CURRENT CONREF IMPLIED))
    (psgml-error "Unknown reserved name: %s."
		(upcase (symbol-name rn))))
  (when (and psgml-pxml-p (memq rn '(CURRENT CONREF)))
    (psgml-error "XML forbids #%s attributes."
		(upcase (symbol-name rn)))))



;;;; Parse doctype: Shortref

;;;150  short reference mapping declaration = MDO, "SHORTREF",
;;;                        [[65 ps]]+, [[151 map name]],
;;;                        ([[65 ps]]+, [[66 parameter literal]],
;;;                        [[65 ps]]+, [[55 name]])+,
;;;                        [[65 ps]]*, MDC

(defun psgml-declare-shortref ()
  (let ((mapname (psgml-check-name))
	mappings literal name)
    (while (progn
	     (psgml-skip-ps)
	     (setq literal (psgml-parse-parameter-literal 'dofunchar)))
      (psgml-skip-ps)
      (setq name (psgml-check-name t))
      (push (cons literal name) mappings))
    (psgml-add-shortref-map
     (psgml-dtd-shortmaps psgml-dtd-info)
     mapname
     (psgml-make-shortmap mappings))))

;;;152  short reference use declaration = MDO, "USEMAP",
;;;                        [[65 ps]]+, [[153 map specification]],
;;;                        ([[65 ps]]+, [[72 associated element type]])?,
;;;                        [[65 ps]]*, MDC

(defun psgml-do-usemap-element (mapname)
  ;; This is called from psgml-do-usemap with the mapname
  (psgml-before-eltype-modification)
  (loop for e in (psgml-parse-name-group) do
	(setf (psgml-eltype-shortmap (psgml-lookup-eltype e psgml-dtd-info))
	      (if (null mapname)
		  'empty
		mapname))))


;;;; Parse doctype

(defun psgml-check-dtd-subset ()
  (let ((psgml-parsing-dtd t)
	(eref psgml-current-eref))
    (while
	(progn
	  (setq psgml-markup-start (point))
	  (cond
	   ((and (eobp) (eq psgml-current-eref eref))
	    nil)
	   ((psgml-parse-ds))
	   ((psgml-parse-markup-declaration 'dtd))
	   ((psgml-parse-delim "MS-END")))))))


;;;; Save DTD: compute translation

(defvar psgml-translate-table nil)

(defun psgml-translate-node (node)
  (assert (not (numberp node)))
  (let ((tp (assq node psgml-translate-table)))
    (unless tp
      (setq tp (cons node (length psgml-translate-table)))
      (nconc psgml-translate-table (list tp)))
    (cdr tp)))

(defun psgml-translate-moves (moves)
  (while moves
    (psgml-translate-node (psgml-move-dest (car moves)))
    (setq moves (cdr moves))))

(defun psgml-translate-model (model)
  (let* ((psgml-translate-table (list (cons model 0)))
	 (p psgml-translate-table))
    (while p
      (cond ((psgml-normal-state-p (caar p))
	     (psgml-translate-moves (psgml-state-opts (caar p)))
	     (psgml-translate-moves (psgml-state-reqs (caar p))))
	    (t
	     (psgml-translate-node (psgml-and-node-next (caar p)))))
      (setq p (cdr p)))
    psgml-translate-table))

;;;; Save DTD: binary coding

(defvar psgml-code-token-numbers nil)
(defvar psgml-code-xlate nil)

(defsubst psgml-code-xlate (node)
  ;;(let ((x (cdr (assq node psgml-code-xlate)))) (assert x) x)
  (cdr (assq node psgml-code-xlate)))

(defun psgml-code-number (num)
  (if (> num psgml-max-single-octet-number)
      (insert (+ (lsh (- num psgml-max-single-octet-number) -8)
		 psgml-max-single-octet-number 1)
	      (logand (- num psgml-max-single-octet-number) 255))
    (insert num)))

(defun psgml-code-token-number (token)
  (let ((bp (assq token psgml-code-token-numbers)))
    (unless bp
      (setq psgml-code-token-numbers
	    (nconc psgml-code-token-numbers
		   (list (setq bp (cons token
					(length psgml-code-token-numbers)))))))
    (cdr bp)))

(defun psgml-code-token (token)
  (psgml-code-number (psgml-code-token-number token)))

(defmacro psgml-code-sequence (loop-c &rest body)
  "Produce the binary coding of a counted sequence from a list.
Syntax: (var seq) &body forms
FORMS should produce the binary coding of element in VAR."
  (let ((var (car loop-c))
	(seq (cadr loop-c)))
    (` (let ((seq (, seq)))
	 (psgml-code-number (length seq))
	 (loop for (, var) in seq
	       do (,@ body))))))

(put 'psgml-code-sequence 'lisp-indent-hook 1)
(put 'psgml-code-sequence 'edbug-forms-hook '(sexp &rest form))

(defun psgml-code-sexp (sexp)
  (let ((standard-output (current-buffer)))
    (prin1 sexp)
    (terpri)))

(defun psgml-code-tokens (l)
  (psgml-code-sequence (x l)
    (psgml-code-token x)))

(defsubst psgml-code-move (m)
  (psgml-code-token (psgml-move-token m))
  (insert (psgml-code-xlate (psgml-move-dest m))))

(defun psgml-code-model (m)
  (let ((psgml-code-xlate (psgml-translate-model m)))
    (psgml-code-sequence (s psgml-code-xlate)		; s is (node . number)
      (setq s (car s))			; s is node
      (cond
       ((psgml-normal-state-p s)
	(assert (and (< (length (psgml-state-opts s)) 255)
		     (< (length (psgml-state-reqs s)) 256)))
	(psgml-code-sequence (x (psgml-state-opts s))
	  (psgml-code-move x))
	(psgml-code-sequence (x (psgml-state-reqs s))
	  (psgml-code-move x)))
       (t				; s is a &-node
	(insert 255)			; Tag &-node
	(insert (psgml-code-xlate (psgml-and-node-next s)))
	(psgml-code-sequence (m (psgml-and-node-dfas s))
	  (psgml-code-model m)))))))

(defun psgml-code-element (et)
  (psgml-code-sexp (psgml-eltype-all-miscdata et))
  (cond
   ((not (psgml-eltype-defined et))
    (insert 128))
   (t
    (insert (psgml-eltype-flags et))
    (let ((c (psgml-eltype-model et)))
      (cond ((eq c psgml-cdata) (insert 0))
	    ((eq c psgml-rcdata) (insert 1))
	    ((eq c psgml-empty) (insert 2))
	    ((eq c psgml-any) (insert 3))
	    ((null c) (insert 4))
	    (t
	     (assert (psgml-model-group-p c))
	     (insert 128)
	     (psgml-code-model c))))
    (psgml-code-tokens (psgml-eltype-includes et))
    (psgml-code-tokens (psgml-eltype-excludes et)))))


(defun psgml-code-dtd (dtd)
  "Produce the binary coding of the current DTD into the current buffer."
  (psgml-code-sexp (psgml-dtd-dependencies dtd))
  (psgml-code-sexp (psgml-dtd-parameters dtd))
  (psgml-code-sexp (psgml-dtd-doctype dtd))
  (let ((done 0)			; count written elements
	tot)
    (setq psgml-code-token-numbers nil)
    (psgml-code-token-number psgml-pcdata-token) ; Make #PCDATA token 0
    (psgml-map-eltypes			; Assign numbers to all tokens
     (function (lambda (et)
		 (psgml-code-token-number (psgml-eltype-token et))))
     dtd nil t)
    (setq tot (length psgml-code-token-numbers))
    ;; Produce the counted sequence of element type names
    (psgml-code-sequence (pair (cdr psgml-code-token-numbers))
      (psgml-code-sexp (psgml-eltype-name (car pair))))
    ;; Produce the counted sequence of element types
    (psgml-code-sequence (pair (cdr psgml-code-token-numbers))
      (setq done (1+ done))
      (psgml-code-element (car pair))
      (psgml-lazy-message "Saving DTD %d%% done" (/ (* 100 done) tot)))
    (psgml-code-sexp (psgml-dtd-entities dtd))
    (psgml-code-sexp (psgml-dtd-shortmaps dtd))
    (psgml-code-sexp (psgml-dtd-notations dtd))))


;;;; Save DTD

(defun psgml-save-dtd (file)
  "Save the parsed dtd on FILE."
  (interactive
   (let* ((tem (expand-file-name
		(or psgml-default-dtd-file
		    (psgml-default-dtd-file))))
	  (dir (file-name-directory tem))
	  (nam (file-name-nondirectory tem)))
     (list
      (read-file-name "Save DTD in: " dir tem nil nam))))
  (setq file (expand-file-name file))
  (when (equal file (buffer-file-name))
    (error "Would clobber current file"))
  (psgml-need-dtd)
  (psgml-push-to-entity (psgml-make-entity "#SAVE" nil ""))
  (psgml-write-dtd psgml-dtd-info file)
  (psgml-pop-entity)
  (setq psgml-default-dtd-file
	(if (equal (expand-file-name default-directory)
		   (file-name-directory file))
	    (file-name-nondirectory file)
	  file))
  (setq psgml-loaded-dtd file))

(defun psgml-write-dtd (dtd file)
  "Save the parsed dtd on FILE.
Construct the binary coded DTD (bdtd) in the current buffer."
  (when (fboundp 'set-buffer-multibyte)
    (setq buffer-file-coding-system 'no-conversion)
    (set-buffer-multibyte nil))
  (insert
   ";;; This file was created by psgml on " (current-time-string) "\n"
   "(psgml-saved-dtd-version 7)\n")
  (psgml-code-dtd dtd)
  (set 'file-type 1)
  (write-region (point-min) (point-max) file))


;;; psgml-dtd.el ends here
