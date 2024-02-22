;;;;\filename dump.el
;;;\Last edited: 1999-08-09 23:38:43 lenst
;;;\RCS $Id: psgml-debug.el,v 2.12 1999/08/12 06:23:11 lenst Exp $
;;;\author {Lennart Staflin}
;;;\maketitle

;;\begin{codeseg}
(provide 'psgml-debug)
(require 'psgml)
(require 'psgml-parse)
(require 'psgml-edit)
(require 'psgml-dtd)
(autoload 'psgml-translate-model "psgml-dtd" "" nil)

;;;; Debugging

(define-key psgml-mode-map "\C-c," 'psgml-goto-cache)
(define-key psgml-mode-map "\C-c\C-x" 'psgml-dump-tree)
(define-key psgml-mode-map "\C-c."   'psgml-shortref-identify)

(defun psgml-this-element ()
  (interactive)
  (let ((tree (psgml-find-element-of (point))))
    (psgml-dump-rec tree)))

(defun psgml-goto-cache ()
  (interactive)
  (setq psgml-dtd-info (psgml-pstate-dtd psgml-buffer-parse-state)
	psgml-top-tree (psgml-pstate-top-tree psgml-buffer-parse-state))
  (psgml-find-start-point (point))
  (message "%s" (psgml-dump-node psgml-current-tree)))

(defun psgml-dump-tree (arg)
  (interactive "P")
  (when arg
    (psgml-parse-to-here))
  (with-output-to-temp-buffer "*Dump*"
    (psgml-dump-rec (psgml-pstate-top-tree psgml-buffer-parse-state))))

(defun psgml-auto-dump ()
  (let ((standard-output (get-buffer-create "*Dump*"))
	(cb (current-buffer)))

    (when psgml-buffer-parse-state
      (unwind-protect
	  (progn (set-buffer standard-output)
		 (erase-buffer))
	(set-buffer cb))

      (psgml-dump-rec (psgml-pstate-top-tree psgml-buffer-parse-state))

      ))
  )

(defun psgml-start-auto-dump ()
  (interactive)
  (add-hook 'post-command-hook
	    (function psgml-auto-dump)
	    'append))

(defun psgml-comepos (epos)
  (if (psgml-strict-epos-p epos)
      (format "%s:%s"
	      (psgml-entity-name (psgml-eref-entity (psgml-epos-eref epos)))
	      (psgml-epos-pos epos))
    (format "%s" epos)))

(defun psgml-dump-node (u)
  (format
   "%s%s start:%s(%s) end:%s(%s) epos:%s/%s net:%s\n"
   (make-string (psgml-tree-level u) ?. )
   (psgml-element-gi u)
   (psgml-element-start u) (psgml-tree-stag-len u)
   (if (psgml-tree-etag-epos u) (psgml-tree-end u)) (psgml-tree-etag-len u)
   (psgml-comepos (psgml-tree-stag-epos u))
   (psgml-comepos (psgml-tree-etag-epos u))
   (psgml-tree-net-enabled u)))

(defun psgml-dump-rec (u)
  (while u
    (princ (psgml-dump-node u))
    (psgml-dump-rec (psgml-tree-content u))
    (setq u (psgml-tree-next u))))

(defun psgml-shortref-identify ()
  (interactive)
  (psgml-find-context-of (point))
  (let* ((nobol (eq (point) psgml-rs-ignore-pos))
	 (tem (psgml-deref-shortmap psgml-current-shortmap nobol)))
    (message "%s (%s)" tem nobol)))

(defun psgml-lookup-shortref-name (table map)
  (car (rassq map (cdr table))))

(defun psgml-show-current-map ()
  (interactive)
  (psgml-find-context-of (point))
  (let ((name (psgml-lookup-shortref-name
	       (psgml-dtd-shortmaps psgml-dtd-info)
	       psgml-current-shortmap)))
    (message "Current map: %s"
	     (or name "#EMPTY"))))

;;;; For edebug

;;(put 'when 'edebug-form-hook t)
;;(put 'unless 'edebug-form-hook t)
;;(put 'push 'edebug-form-hook '(form sexp))
;;(put 'setf 'edebug-form-hook '(sexp form))

(setq edebug-print-level 3
      edebug-print-length 5
      edebug-print-circle nil
)

(eval-when (load)
  (unless psgml-running-lucid
    (def-edebug-spec psgml-with-parser-syntax (&rest form))
    (def-edebug-spec psgml-skip-upto (sexp))
    (def-edebug-spec psgml-check-delim (sexp &optional sexp))
    (def-edebug-spec psgml-parse-delim (sexp &optional sexp))
    (def-edebug-spec psgml-is-delim (sexp &optional sexp sexp sexp))))

;;;; dump

(defun psgml-dump-dtd (&optional dtd)
  (interactive )
  (unless dtd
    (setq dtd (psgml-pstate-dtd psgml-buffer-parse-state)))
  (with-output-to-temp-buffer "*DTD dump*"
    (princ (format "Dependencies: %S\n"
		   (psgml-dtd-dependencies dtd)))
    (loop for et being the symbols of (psgml-dtd-eltypes dtd)
	  do (psgml-dp-element et))))

(defun psgml-dump-element (el-name)
  (interactive
   (list (completing-read "Element: "
			  (psgml-dtd-eltypes
			   (psgml-pstate-dtd psgml-buffer-parse-state))
			  nil t)))
  (with-output-to-temp-buffer "*Element dump*"
    (psgml-dp-element (psgml-lookup-eltype el-name))))

(defun psgml-dp-element (el)
  (cond
   ((psgml-eltype-defined el)
    (princ (format "Element %s %s %s%s:\n"
		   (psgml-eltype-name el)
		   (if (psgml-eltype-stag-optional el) "O" "-")
		   (if (psgml-eltype-etag-optional el) "O" "-")
		   (if (psgml-eltype-mixed el) " mixed" "")))
    (cond
     ((psgml-model-group-p (psgml-eltype-model el))
      (psgml-dp-model (psgml-eltype-model el)))
     (t
      (prin1 (psgml-eltype-model el))
      (terpri)))
    (princ (format "Exeptions: +%S -%S\n"
		   (psgml-eltype-includes el)
		   (psgml-eltype-excludes el)))
    (princ (format "Attlist: %S\n" (psgml-eltype-attlist el)))
    (princ (format "Plist: %S\n" (symbol-plist el))))
   (t
    (princ (format "Undefined element %s\n" (psgml-eltype-name el)))))
  (terpri))


(defun psgml-dp-model (model &optional indent)
  (or indent (setq indent 0))
  (let ((psgml-code-xlate (psgml-translate-model model)))
    (loop
     for i from 0
     for x in psgml-code-xlate do
     (cond ((psgml-normal-state-p (car x))
	    (princ (format "%s%d: opts=%s reqs=%s\n"
			   (make-string indent ? ) i
			   (psgml-untangel-moves (psgml-state-opts (car x)))
			   (psgml-untangel-moves (psgml-state-reqs (car x))))))
	   (t				; and-node
	    (princ (format "%s%d: and-node next=%d\n"
			   (make-string indent ? ) i
			   (psgml-code-xlate (psgml-and-node-next (car x)))))
	    (loop for m in (psgml-and-node-dfas (car x))
		  do (psgml-dp-model m (+ indent 2))))))))

(defun psgml-untangel-moves (moves)
  (loop for m in moves
	collect (list (psgml-move-token m)
		      (psgml-code-xlate (psgml-move-dest m)))))


;;;; Dump state

(defun psgml-dump-state ()
  (interactive)
  (with-output-to-temp-buffer "*State dump*"
    (psgml-dp-state psgml-current-state)))

(defun psgml-dp-state (state &optional indent)
  (or indent (setq indent 0))
  (cond
   ((psgml-normal-state-p state)
    (psgml-dp-model state indent))
   (t
    (princ (format "%sand-state\n" (make-string indent ? )))
    (psgml-dp-state (psgml-and-state-substate state) (+ 2 indent))
    (princ (format "%s--next\n" (make-string indent ? )))
    (psgml-dp-state (psgml-and-state-next state)     (+ 2 indent))
    (princ (format "%s--dfas\n" (make-string indent ? )))
    (loop for m in (psgml-and-state-dfas state)
	  do (psgml-dp-model m (+ indent 2))
	  (princ (format "%s--\n" (make-string indent ? )))))))


;;;; Build autoloads for all interactive functions in psgml-parse

(defun psgml-build-autoloads ()
  (interactive)
  (with-output-to-temp-buffer "*autoload*"
    (loop
     for file in '("psgml-parse" "psgml-edit" "psgml-dtd"
		   "psgml-info" "psgml-charent")
     do
     (set-buffer (find-file-noselect (concat file ".el")))
     (goto-char (point-min))
     (while (and
	     (not (eobp))
	     (re-search-forward "^(defun +\\([^ ]+\\)" nil t))
       (let ((name (buffer-substring (match-beginning 1)
				     (match-end 1)))
	     doc)
	 (forward-sexp 1)		; skip argument list
	 (skip-chars-forward " \n\t")
	 (when (eq ?\" (following-char)) ; doc string
	       (setq doc (buffer-substring (point)
					   (progn (forward-sexp 1)
						  (point)))))
	 (skip-chars-forward " \n\t")
	 (when (looking-at "(interactive")
	       (if (null doc)
		   (message "No doc for %s" name))
	       (princ (format
		       "(autoload '%s \"%s\" %s t)\n"
		       name file doc))))))))

;;;; Test psgml with psgmls test cases

(defun test-psgml (start)
  (interactive "p")
  (let (file
	(psgml-show-warnings t))
    (with-output-to-temp-buffer "*Testing psgml*"
      (while
	  (progn
	    (setq file (format "/ni/src/psgmls-1.1/test/test%03d.sgm"
			       start))
	    (file-exists-p file))
	(princ (format "*** File test%03d ***\n" start))
	(find-file file)
	(condition-case errcode
	    (progn
	      (psgml-parse-prolog)
	      ;;(psgml-next-trouble-spot)
	      (psgml-parse-until-end-of nil)
	      )
	  (error
	   (princ errcode)
	   (terpri)))
	(if (get-buffer psgml-log-buffer-name)
	    (princ (save-excursion
		     (set-buffer psgml-log-buffer-name)
		     (buffer-string))))
	(terpri)
	(terpri)
	(sit-for 0)
	(kill-buffer (current-buffer))
	(setq start (1+ start))))))


;;;; Profiling

(defun profile-psgml (&optional file)
  (interactive)
  (or file (setq file (expand-file-name "~/work/config/metaspec.xml")))
  (find-file file)
  (psgml-need-dtd)
  (psgml-instrument-parser)
  (elp-reset-all)
  (dotimes (i 10)
    (garbage-collect)
    (psgml-reparse-buffer (function psgml-handle-shortref)))
  (elp-results))

(defun psgml-instrument-parser ()
  (interactive)
  (require 'elp)
  (setq elp-function-list nil)
  (elp-restore-all)
  (setq elp-function-list
	'(
	  psgml-parse-to
	  psgml-parser-loop
	  psgml-parse-markup-declaration
	  psgml-do-processing-instruction
	  psgml-pop-entity
	  psgml-tree-net-enabled
	  psgml-do-end-tag
	  psgml-do-data
	  psgml-deref-shortmap
	  psgml-handle-shortref
	  psgml-do-start-tag
	  psgml-do-general-entity-ref
	  psgml-set-face-for
	  psgml-pcdata-move
	  psgml-shortmap-skipstring
	  ;;
	  psgml-parse-attribute-specification-list
	  psgml-check-tag-close
	  psgml-do-move
	  psgml-open-element
	  psgml-list-implications
	  psgml-move-current-state
	  psgml-do-empty-start-tag
	  psgml-lookup-eltype
	  psgml-startnm-char-next
	  psgml-eltype-defined
	  psgml-execute-implied
	  psgml-next-sub-and
	  psgml-get-and-move
	  format
	  ))
  (elp-instrument-list))


(defun psgml-instrument-dtd-parser ()
  (interactive)
  (require 'elp)
  (setq elp-function-list nil)
  (elp-restore-all)
  (setq elp-function-list
	'(
	  psgml-parse-prolog
	  psgml-skip-ds
	  psgml-parse-markup-declaration
	  psgml-check-doctype-body
	  ;;
	  psgml-check-dtd-subset
	  psgml-parse-ds
	  psgml-declare-attlist
	  psgml-declare-entity
	  psgml-declare-element
	  psgml-declare-shortref
	  ;;
	  psgml-parse-parameter-literal
	  psgml-check-element-type
	  psgml-check-primitive-content-token
	  psgml-check-model-group
	  ;; In psgml-check-model-group
	  psgml-parse-modifier
	  psgml-make-pcdata
	  psgml-skip-ts
	  psgml-make-opt
	  psgml-make-*
	  psgml-make-+
	  psgml-reduce-,
	  psgml-reduce-|
	  psgml-make-&
	  psgml-make-conc
	  psgml-copy-moves
	  ;; is ps*
	  psgml-do-parameter-entity-ref
	  ;;
	  psgml-make-primitive-content-token
	  psgml-push-to-entity
	  psgml-lookup-entity
	  psgml-lookup-eltype
	  psgml-one-final-state
	  psgml-remove-redundant-states-1
	  ))
  (elp-instrument-list))

;;;; Structure Viewing and Navigating

(require 'psgml-api)

(defvar show-structure-buffer nil)
(defvar show-structure-positions nil)
(defvar show-structure-source-buffer nil)

(defun show-structure ()
  (interactive)
  (let* ((source (current-buffer))
	 (result (get-buffer-create "*Struct*"))
	 (show-structure-buffer result))
    (set-buffer result)
    (erase-buffer)
    (make-local-variable 'show-structure-positions)
    (setq show-structure-positions nil)
    (make-local-variable 'show-structure-source-buffer)
    (setq show-structure-source-buffer source)
    (use-local-map (make-sparse-keymap))
    (local-set-key "\C-c\C-c" 'show-structure-goto)
    (set-buffer source)
    (show-element (psgml-top-element))
    (display-buffer result)))


(defun show-structure-goto ()
  (interactive)
  (beginning-of-line)
  (let ((pos-pair (assoc (point) show-structure-positions)))
    (when pos-pair
      (switch-to-buffer show-structure-source-buffer)
      (goto-char (cdr pos-pair)))))


(defun show-struct-element-p (element)
  (or (and (not (psgml-element-data-p element))
	   (not (psgml-element-empty element)))
      (psgml-element-appdata element 'structure)))


(defun show-element (element)
  (cond ((show-struct-element-p element)
	 (let ((gi (psgml-element-gi element))
	       (level (psgml-element-level element)))
	   (save-excursion
	     (set-buffer show-structure-buffer)
	     (if (not (bolp))
		 (insert "\n"))
	     (push (cons (point) (psgml-element-start element))
		   show-structure-positions)
	     (insert (format "%s[%15s] " (make-string (- level 1) ? ) gi))))
	 (catch 'show-data-stop
	     (show-element-data element))
	 (psgml-map-content element #'show-element))))

(defun show-element-data (element)
  (psgml-map-content element #'show-element-data #'show-data)
  (throw 'show-data-stop nil))

(defun show-data (data)
  (save-excursion
    (set-buffer show-structure-buffer)
    (let ((start (point)))
      (insert data)
      (let ((end (point)))
	(subst-char-in-region start end ?\n ? )
	(when (> (current-column) fill-column)
	  (move-to-column fill-column)
	  (delete-region (point) end)
	  (throw 'show-data-stop nil))))))

;¤¤\end{codeseg}
