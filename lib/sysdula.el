(defun sysdula-inc (op)
  "Sets preceding_var = preceding_var op prefix"
  (let (var-name
	cpos)
    (forward-word -1)
    (setq cpos (point))
    (forward-word 1)
    (setq var-name (buffer-substring cpos (point)))
    (insert " = " var-name " " op " "
	    (number-to-string (prefix-numeric-value
			       current-prefix-arg)))))

(if sysdul-abbrev-list
    ()
  (setq sysdul-abbrev-list
	(append
	 (if sysdul-local-abbs (funcall sysdul-local-abbs))
	 '(
	   ("++" (sysdula-inc "+"))
	   ("--" (sysdula-inc "-"))
	   ("fi" "end if" *)
	   ("if" "if " | / (= . "fi") (help . "if"))
	   ("wh" "while " | / "end while" * (help . "while"))
	   ("ida" "identify all " ("Entity/role") " with " | "KEY" (: . 4)
	    "that RELATES" (:) "select LIST" (:) "sorted by SORTKEY"
	    (help . "identify all"))
	   ("idn" "identify next " ("Entity/role") (: . 3) "and get " |
	    "LIST" (help . "identify next"))
	   ))))

	   ("2D" . sysdul-macro-def)
	   ("2E" . sysdul-end-include)
	   ("2F" . sysdul-file-include)
	   ("2I" . sysdul-include-if)
	   ("2M" . sysdul-macro-pair)
	   ("2O" . sysdul-or-include)
	   ("2R" . sysdul-svapp-error)
	   ("2S" . sysdul-else-include)
	   ("AP" . sysdul-acitvate-pic)
	   ("AU" . sysdul-automark)
	   ("CA" . sysdul-call)
	   ("CC" . sysdul-commit-changes)
	   ("CD" . sysdul-close-db)
	   ("CE" . sysdul-close-error-file)
	   ("CF" . sysdul-close-file)
	   ("CH" . sysdul-control-handling)
	   ("CI" . sysdul-if-control)
	   ("CL" . sysdul-continue-loop)
	   ("CM" . sysdul-close-mess-file)
	   ("CP" . sysdul-control-procedure)
	   ("DA" . sysdul-decl-array)
	   ("DB" . sysdul-define-button)
	   ("DC" . sysdul-declare)
	   ("DD" . sysdul-display-field)
	   ("DE" . sysdul-delete)
	   ("DH" . sysdul-default-err)
	   ("DI" . sysdul-display)
	   ("DIB" . sysdul-disable-button)
	   ("DIE" . sysdul-disable-editor)
	   ("DIF" . sysdul-disable-field)
	   ("DIM" . sysdul-disable-menu)
	   ("DIP" . sysdul-disable-pict)
	   ("DO" . sysdul-define-record)
	   ("DR" . sysdul-decl-ref)
	   ("DU" . sysdul-display-user-message)
	   ("EA" . sysdul-employs-automark)
	   ("EI" . sysdul-end-if)
	   ("ENB" . sysdul-enable-button)
	   ("ENE" . sysdul-enable-editor)
	   ("ENF" . sysdul-enable-field)
	   ("ENM" . sysdul-enable-menu)
	   ("ENP" . sysdul-enable-pict)
	   ("ES" . sysdul-else-test)
	   ("EX" . sysdul-exit-levels)
	   ("F1" . sysdul-find-first)
	   ("FA" . sysdul-for-all)
	   ("FE" . sysdul-finish-ent)
	   ("FI" . sysdul-find-equal)
	   ("FL" . sysdul-find-last)
	   ("FO" . sysdul-forget)
	   ("FR" . sysdul-for-all-rep)
	   ("FS" . sysdul-finish-screen)
	   ("FT" . sysdul-find-that)
	   ("FU" . sysdul-function)
	   ("GE" . sysdul-get)
	   ("I1" . sysdul-identify-first)
	   ("IA" . sysdul-identify-all)
	   ("IC" . sysdul-identify-curr)
	   ("ID" . sysdul-is-defined)
	   ("IF" . sysdul-if-test)
	   ("II" . sysdul-is-activated)
	   ("IL" . sysdul-identify-last)
	   ("IN" . sysdul-identify-next)
	   ("IP" . sysdul-identify-prev)
	   ("LA" . sysdul-lock-all)
	   ("LI" . sysdul-lineno)
	   ("LO" . sysdul-lock)
	   ("MA" . sysdul-maxno)
	   ("MF" . sysdul-move-from)
	   ("MT" . sysdul-move-to)
	   ("NO" . sysdul-no-of)
	   ("NTP" . sysdul-note-pic)
	   ("NTR" . sysdul-note-ref)
	   ("NTS" . sysdul-note-selection)
	   ("NTU" . sysdul-note-undef)
	   ("NU" . sysdul-internal-number)
	   ("OB" . sysdul-obtain)
	   ("OC" . sysdul-obtain-curr)
	   ("OD" . sysdul-open-db)
	   ("OE" . sysdul-open-error-file)
	   ("OF" . sysdul-open-file)
	   ("OI" . sysdul-or-if-test)
	   ("OM" . sysdul-open-mess-file)
	   ("OP" . sysdul-obtain-prev)
	   ("OV" . sysdul-obtain-val)
	   ("PB" . sysdul-place-button)
	   ("PC" . sysdul-passivate-curr)
	   ("PE" . sysdul-perform)
	   ("PG" . sysdul-program)
	   ("PP" . sysdul-passivate-picture)
	   ("PR" . sysdul-procedure)
	   ("RA" . sysdul-read-all)
	   ("RC" . sysdul-rollback-changes)
	   ("RD" . sysdul-read-file)
	   ("RE" . sysdul-remember)
	   ("REC" . sysdul-return-cont)
	   ("REE" . sysdul-return-error)
	   ("REO" . sysdul-return-obtain)
	   ("RET" . sysdul-return-terminate)
	   ("RF" . sysdul-restart-from)
	   ("RG" . sysdul-ready-group)
	   ("RL" . sysdul-restart-label)
	   ("RM" . sysdul-refresh-menu)
	   ("RN" . sysdul-refresh-screen)
	   ("RP" . sysdul-ready-pict)
	   ("RS" . sysdul-read-str)
	   ("RT" . sysdul-read-term)
	   ("RY" . sysdul-ready-ent)
	   ("SE" . sysdul-start-event)
	   ("SEAC" . sysdul-auto-commit)
	   ("SEAS" . sysdul-auto-skip)
	   ("SEBF" . sysdul-blocking-factor)
	   ("SECE" . sysdul-clear-edit)
	   ("SECH" . sysdul-note-change)
	   ("SECI" . sysdul-commit-interval)
	   ("SECS" . sysdul-copy-skip)
	   ("SECS" . sysdul-note-curr)
	   ("SEDE" . sysdul-deletion)
	   ("SEDG" . sysdul-defaulting)
	   ("SEDN" . sysdul-db-no)
	   ("SEIN" . sysdul-insertion)
	   ("SELC" . sysdul-lowercase)
	   ("SELK" . sysdul-locked-field)
	   ("SESC" . sysdul-scrolling)
	   ("SESP" . sysdul-prn-for-screen)
	   ("SEST" . sysdul-sibas-trans)
	   ("SEUC" . sysdul-uppercase)
	   ("SEUK" . sysdul-user-key)
	   ("SEVI" . sysdul-video-on-off)
	   ("SEVK" . sysdul-visible-key)
	   ("SEVO" . sysdul-video-on)
	   ("SL" . sysdul-start-loop)
	   ("SN" . sysdul-start-new-inc)
	   ("ST" . sysdul-store)
	   ("TA" . sysdul-abort-trans)
	   ("TE" . sysdul-end-trans)
	   ("TF" . sysdul-terminate-from)
	   ("TL" . sysdul-terminate-label)
	   ("TO" . sysdul-terminate-loop)
	   ("TS" . sysdul-start-trans)
	   ("UC" . sysdul-undo-chg)
	   ("UL" . sysdul-unlock-all)
	   ("UN" . sysdul-univ)
	   ("WD" . sysdul-when-db-err)
	   ("WE" . sysdul-write-trace)
	   ("WF" . sysdul-write-file)
	   ("WH" . sysdul-while)
	   ("WI" . sysdul-when-io-err)
	   ("WM" . sysdul-write-mess)
	   ("WS" . sysdul-when-ss-err)
	   ("WT" . sysdul-write-term)
	   ("WX" . sysdul-write-str)
	   ))))





;;; sysdul-abbrev.el --- abbreviations for sysdul-mode

;;; Copyright (C) 1995 Lars Reed

;; Author:		Lars Reed <lre@sysdeco.no>
;; Last-Modified:	95/10/20
;; Version:		1.7
;; Keywords:		languages programming Sysdul abbrevs
;; Adapted-By:

;;; Commentary:

;; ....

;;; Code:

; ======================================================================


(defvar ent-list-hist nil
  "History of entity class / role names accessed in sysdul-mode.")
(defvar pic-list-hist nil
  "History of picture names in Sysdul mode.")
(defvar label-list-hist nil
  "History of labels used in Sysdul mode.")
(defvar arr-list-hist nil
  "History of arrays & records used in Sysdul mode.")
(defvar fno-list-hist nil
  "History of file numbers used in Sysdul mode.")

(defvar sysdul-tbf "SMO" "Last TBF accessed in sysdul-mode.")
(defvar sysdul-last-ent "@L_ROLE@" "Last entity accessed in sysdul-mode.")
(defvar sysdul-last-label "3990" "Last label accessed in sysdul-mode.")
(defvar sysdul-last-arr "ZZCONFIG"
  "Last array/record accessed in sysdul-mode.")
(defvar sysdul-last-pic "@L_PICTURE@" "Last picture accessed in sysdul-mode.")
(defvar sysdul-last-fno "10" "Last file number used in Sysdul.")

; ======================================================================



; ****************************************************

; Function to set sysdul-last-pic
(defun sysdul-get-pic ()
  "Function to prompt for and set sysdul-last-pic."
  (setq sysdul-last-pic
	(sub-sysdul-get "Picture" sysdul-last-pic 'pic-list-hist)))

; Function to set sysdul-last-arr
(defun sysdul-get-arr ()
  "Function to prompt for and set sysdul-last-arr."
  (setq sysdul-last-arr
	(sub-sysdul-get "Array/record" sysdul-last-arr 'arr-list-hist)))

; Function to set sysdul-last-label
(defun sysdul-get-label ()
  "Function to prompt for and set sysdul-last-label."
  (setq sysdul-last-label
	(sub-sysdul-get "Label" sysdul-last-label 'label-list-hist)))

; Function to set sysdul-last-fno
(defun sysdul-get-fno ()
  "Function to prompt for and set sysdul-last-fno."
  (setq sysdul-last-fno
	(sub-sysdul-get "File no." sysdul-last-fno 'fno-list-hist)))

; Function to set sysdul-last-ent & sysdul-tbf
(defun sysdul-get-ent ()
  "Function to prompt for and set sysdul-last-ent & sysdul-tbf."
  (setq sysdul-last-ent
	(sub-sysdul-get "Entity" sysdul-last-ent 'ent-list-hist))
  (setq sysdul-tbf
	(substring sysdul-last-ent 0 (min 3 (length sysdul-last-ent)))))

; ----------------------------------------------------------------------


; Std. OR IF-test
(defun sysdul-or-if-test (&optional noof)
  "Standard Sysdul OR IF+indent.
Prefix with number, default 1, to get more than one branch."
  (interactive "p")
  (let ((cnt (or noof 1)))
    (save-excursion
      (while (> cnt 0)
        (insert "OR IF ...")
        (sysdul-indent-newline 1)
        (if (> cnt 1) (sysdul-indent-newline -1))
        (setq cnt (- cnt 1))))
    (end-of-line)
    (forward-word -1)
    (forward-word 1)
    (forward-char 1)))

; Std. ELSE
(defun sysdul-else-test ()
  "Standard Sysdul ELSE+indent."
  (interactive)
  (insert "ELSE")
  (sysdul-indent-newline 1))

; Std. END-IF
(defun sysdul-end-if ()
  "Standard Sysdul END IF."
  (interactive)
  (insert "END IF"))

; Std. IF-test
(defun sysdul-if-test (&optional count)
  "Standard Sysdul IF/END IF, with `count' OR IFs and one ELSE.
Prefix argument (default 1) determines no. of OR-IFs - use C-u 0 to get
a plain IF/END IF."
  (interactive "p")
  (insert "IF ")
  (save-excursion
    (sub-sysdul-in-out)
    (if (> count 0) (progn (sysdul-else-test)
                           (sysdul-indent-newline -1)))
    (insert "END IF\n"))
  (if (> count 0)
      (save-excursion
        (end-of-line 2)
        (sysdul-indent-newline -1)
        (sysdul-or-if-test count))))

; DB-test body
(defun sysdul-db-test (testword msgno &optional etxt dberr eent)
  "Test for db-error TESTWORD, if DBERR is non-nil, start by testing
for DB-error.  Call sysdul-msg-function with parameters `msgno etxt eent'
on error."
  (save-excursion
    (let (dbtest
          (eetxt (or etxt testword)))
      (if dberr (setq dbtest dberr)
        (setq dbtest 0))
      (if (> dbtest 0) (progn (insert "IF DB-ERROR")
                              (sysdul-indent-newline 1)
                              (funcall sysdul-msg-function 2000 "DB" eent)
                              (indent-relative)
                              (sysdul-indent-func -1)
                              (if (< dbtest 2) (insert "OR "))))
      (if (< dbtest 2) (progn
                         (insert "IF NOT " testword)
                         (sysdul-indent-newline 1)
                         (funcall sysdul-msg-function msgno eetxt eent)
                         (indent-relative)
                         (sysdul-indent-func -1)))
      (insert "END IF"))))

; IO-test body
(defun sysdul-io-test (msgno &optional etxt)
  "Test for IO-error.  Call sysdul-ioerr-function with parameters
`msgno etxt' on error."
  (insert "IF IO-ERROR")
  (sysdul-indent-newline 1)
  (funcall sysdul-ioerr-function msgno etxt)
  (indent-relative)
  (sysdul-indent-func -1)
  (insert "END IF"))


; ----------------------------------------------------------------------

(defun sub-sysdul-find (flag)
  "Sysdul FIND-statements."
  (let (currc downl lastent)
    (setq currc (current-column))
    (sysdul-get-ent)
    (insert "FIND " sysdul-last-ent " ")
    (if (= flag 0) (progn (insert "WITH ")
                          (save-excursion
                            (insert " EQUAL TO ... :")
                            (sysdul-indent-newline 0 3)
                            (insert "AND GET ...\n")
                            (indent-to currc)
                            (setq downl 2)))
      (progn (insert "THAT ")
             (save-excursion
               (setq lastent sysdul-last-ent)
               (sysdul-get-ent)
               (insert " " sysdul-last-ent)
               (setq sysdul-last-ent lastent)
               (sysdul-indent-newline 0)
               (setq downl 1))))
    (save-excursion
      (end-of-line (+ downl 1))
      (sysdul-db-test "FOUND" 2100 "FIND" 1 sysdul-last-ent))))

(defun sysdul-find-equal ()
  "Sysdul FIND entity, EQUAL key, followed by test."
  (interactive)
  (sub-sysdul-find 0))

(defun sysdul-find-that ()
  "Sysdul FIND entity, THAT relates, followed by test."
  (interactive)
  (sub-sysdul-find 1))

; ----------------------------------------------------------------------

; Sysdul DB-statement
(defun sub-sysdul-loop (stmnt flag)
  "Sysdul LOOP-statements."
  (sysdul-get-ent)
  (let (currc cntr)
    (setq currc (current-column))
    (insert stmnt " " sysdul-last-ent " WITH ")
    (save-excursion
      (insert "<" sysdul-tbf "-id> FROM low  :")
      (sysdul-indent-newline 0)
      (setq cntr 4)
      (if (> currc 1) (setq cntr (1+ cntr)))
      (if (= flag 3)  (setq cntr (1+ cntr)))
      (while (> cntr 0) (progn (indent-relative)
                               (setq cntr (1- cntr))))
      (insert "  TO high")
      (cond ((= flag 2)
	     (insert
	      " ; IN REVERSE ORDER ;  WITHOUT MARKING ; SCROLLABLE ; AND LOCK"))
	    ((= flag 4)
	     (insert " :")
	     (sysdul-indent-newline 0)
	     (backward-delete-char-untabify 16 nil)
	     (insert "SELECT <ALL|list> :")
	     (sysdul-indent-newline 0)
	     (insert "SORTED ON <element> <A|DE>SCENDING :")
	     (sysdul-indent-newline 0)
	     (insert "[SCROLLABLE] [AND LOCK]")))
      (if (= flag 3) ()
	(insert "\n")
	(indent-to currc)
	(cond
	 ((= flag 1) (progn ; FIND FIRST/LAST
		       (sysdul-db-test "FOUND" 2100  "FIND"
				       nil sysdul-last-ent)))
	 ((= flag 2) (progn ; FOR ALL
		       (sysdul-indent-func 1)
		       (insert "GET " sysdul-last-ent " <list>")
		       (sysdul-indent-newline 0)
		       (sysdul-db-test "GOT" 2003 "GET" nil sysdul-last-ent)
		       (end-of-line (+ 2 sysdul-msg-lines))
		       (sysdul-indent-newline -1)
		       (insert "END FOR")
		       (sysdul-indent-newline 0)
		       (sysdul-db-test "ANY OCCURRENCES" 2100 "FIND"
				       nil sysdul-last-ent)))
	 ((= flag 3) (progn ; NUMBER OF
		       (sysdul-db-test "" 0 "" 2 nil))))))))


(defun sysdul-find-first ()
  "Sysdul FIND FIRST entity with key, followed by test."
  (interactive)
  (sub-sysdul-loop "FIND FIRST" 1))

(defun sysdul-find-last ()
  "Sysdul FIND LAST entity with key, followed by test."
  (interactive)
  (sub-sysdul-loop "FIND LAST" 1))

(defun sysdul-for-all ()
  "Sysdul FOR ALL-loop on entity with GET inside, followed by test."
  (interactive)
  (sub-sysdul-loop "FOR ALL" 2))

(defun sysdul-identify-all ()
  "Sysdul IDENTIFY ALL."
  (interactive)
  (sub-sysdul-loop "IDENTIFY ALL" 4))

; ----------------------------------------------------------------------

(defun sub-sysdul-func (flag)
  "General x = ... function."
; Looking-at???
  (if (> flag 1) (sysdul-get-pic))
  (if (= flag 1) (sysdul-get-sub))
  (insert " = ")
  (sysdul-indent-newline 0)
  (save-excursion
    (cond ((= flag 0) (sub-sysdul-loop "NUMBER OF" 3))
	  ((= flag 1) (insert "INTERNAL NUMBER FOR CONTROL-PROCEDURE "
			      sysdul-last-sub))
	  ((= flag 2) (insert "MAXNO FOR  ON " sysdul-last-pic " ")
	              (sysdul-when-ss-err))
	  ((= flag 3) (insert "LINE NUMBER FOR  ON " sysdul-last-pic))))
  (delete-indentation)
  (if (= flag 1) (end-of-line))
  (if (> flag 1) (progn
		   (forward-word flag)
		   (forward-char 1))))

(defun sysdul-no-of ()
  "Sysdul NUMBER OF."
  (interactive)
  (sub-sysdul-func 0))

(defun sysdul-internal-number ()
  "Sysdul INTERNAL NUMBER."
  (interactive)
  (sub-sysdul-func 1))

(defun sysdul-maxno ()
  "Sysdul MAXNO."
  (interactive)
  (sub-sysdul-func 2))

(defun sysdul-lineno ()
  "Sysdul LINE NUMBER."
  (interactive)
  (sub-sysdul-func 3))

; ----------------------------------------------------------------------

(defun sysdul-program ()
  "Sysdul PROGRAM /END PROGRAM."
  (interactive)
  (sysdul-get-sub)
  (if (bolp) (insert " "))
  (insert "PROGRAM " sysdul-last-sub " EMPLOYS ")
  (save-excursion
    (sub-sysdul-in-out)
    (insert "END PROGRAM")))

(defun sysdul-control-procedure ()
  "Sysdul CONTROL-PROCEDURE."
  (interactive)
  (sysdul-get-sub)
  (if (< (current-column) 2) (insert " "))
  (insert "CONTROL-PROCEDURE " sysdul-last-sub " EMPLOYS ")
  (save-excursion
    (insert "AUTOMARKING")
    (sub-sysdul-in-out)
    (insert "END CONTROL-PROCEDURE")))

(defun sysdul-control-handling ()
  "Sysdul CONTROL-HANDLING."
  (interactive)
  (if (< (current-column) 2) (insert " "))
  (insert "CONTROL-HANDLING")
  (save-excursion
    (sub-sysdul-in-out)
    (insert "END CONTROL-HANDLING")))

(defun sysdul-procedure ()
  "Sysdul PROCEDURE / END PROCEDURE."
  (interactive)
  (let (currc)
    (setq currc (+ (current-column) sysdul-indent-level))
    (sysdul-get-sub)
    (insert "PROCEDURE " sysdul-last-sub " IMPORTS ")
    (save-excursion
      (insert "...       :")
      (sysdul-indent-newline 0 3)
      (insert "EXPORTS ...       :")
      (sysdul-indent-newline 0)
      (insert "EMPLOYS AUTOMARKING\n")
      (indent-to currc)
      (sysdul-indent-newline -1)
      (insert "END PROCEDURE"))))

(defun sysdul-function ()
  "Sysdul DECLARE AS FUNCTION."
  (interactive)
  (sysdul-get-sub)
  (insert "DECLARE " sysdul-last-sub " AS FUNCTION"))


(defun sysdul-define-record ()
  "Sysdul DEFINE AS RECORD."
  (interactive)
  (sysdul-get-arr)
  (insert "DEFINE " sysdul-last-arr)
  (insert " AS RECORD OF "))

(defun sysdul-decl-array ()
  "Sysdul DECLARE ... AS ... NUMBERED FROM ... TO ... GLOBAL."
  (interactive)
  (insert "DECLARE ")
  (sysdul-get-arr)
  (insert sysdul-last-arr " AS ")
  (sysdul-get-arr)
  (insert sysdul-last-arr " NUMBERED FROM ")
  (save-excursion
    (insert " TO ... GLOBAL")))

(defun sysdul-declare ()
  "Sysdul DECLARE ... AS ."
  (interactive)
  (insert "DECLARE ")
  (save-excursion
    (insert " AS ")))

(defun sysdul-decl-ref ()
  "Sysdul DECLARE ... AS REFERENCE TO."
  (interactive)
  (insert "DECLARE ")
  (save-excursion
    (insert " AS REFERENCE TO " sysdul-last-ent)))

(defun sysdul-univ ()
  "Sysdul UNIVERSAL."
  (interactive)
  (sysdul-get-ent)
  (insert "UNIVERSAL " sysdul-last-ent))

(defun sysdul-automark ()
  "Sysdul AUTOMARK."
  (interactive)
  (sysdul-get-ent)
  (insert "AUTOMARK " sysdul-last-ent))

(defun sysdul-employs-automark ()
  "Sysdul EMPLOYS AUTOMARKING."
  (interactive)
  (insert "EMPLOYS AUTOMARKING"))

(defun sysdul-is-defined ()
  "Sysdul IF ... IS DEFINED."
  (interactive)
  (insert "IF NOT ")
  (save-excursion
    (insert "IS DEFINED")
    (sub-sysdul-in-out)
    (insert "END IF")))

(defun sysdul-while ()
  "Sysdul WHILE-loop."
  (interactive)
  (insert "WHILE ")
  (save-excursion
    (sub-sysdul-in-out)
    (insert "END WHILE")))

(defun sysdul-call ()
  "Sysdul CALL procedure."
  (interactive)
  (sysdul-get-sub)
  (insert "CALL " sysdul-last-sub " EXPORTING ")
  (save-excursion
    (insert "... :")
    (sysdul-indent-newline 0 3)
    (insert "IMPORTING ...")))

; ----------------------------------------------------------------------

(defun sub-sysdul-goto (dir lab)
  "General Sysdul RESTART/TERMINATE -LABEL/FROM."
  (sysdul-get-label)
  (if (< dir 0) (insert "RESTART") (insert "TERMINATE"))
  (if (= lab 1) (insert "-LABEL") (insert " FROM"))
  (insert " " sysdul-last-label)
  (message "%s" "Spaghetti!"))

(defun sysdul-restart-from ()
  "Sysdul RESTART FROM."
  (interactive)
  (sub-sysdul-goto -1 0))

(defun sysdul-restart-label ()
  "Sysdul RESTART-LABEL."
  (interactive)
  (sub-sysdul-goto -1 1))

(defun sysdul-terminate-from ()
  "Sysdul TERMINATE FROM."
  (interactive)
  (sub-sysdul-goto 1 0))

(defun sysdul-terminate-label ()
  "Sysdul TERMINATE-LABEL."
  (interactive)
  (sub-sysdul-goto 1 1))

; ----------------------------------------------------------------------

(defun sub-sysdul-array (dir)
  "General Sysdul MOVE TO/FROM ARRAY."
  (sysdul-get-arr)
  (insert "MOVE ")
  (if (< dir 0) (insert "FROM") (insert "TO"))
  (insert " NUMBER ")
  (save-excursion
    (insert " IN " sysdul-last-arr " ...")))

(defun sysdul-move-to ()
  "Sysdul MOVE TO ARRAY."
  (interactive)
  (sub-sysdul-array 1))

(defun sysdul-move-from ()
  "Sysdul MOVE FROM ARRAY."
  (interactive)
  (sub-sysdul-array -1))

; ----------------------------------------------------------------------

(defun sysdul-default-err ()
  "Sysdul DEFAULT ERROR-HANDLING."
  (interactive)
  (insert "DEFAULT ERROR-HANDLING WHEN SYSSCREEN")
  (save-excursion
    (insert "-ERROR TERMINATE FROM ...")))

(defun sysdul-close-db ()
  "Sysdul CLOSE DATABASE."
  (interactive)
  (let (currc)
    (setq currc (current-column))
    (insert "CLOSE DATABASE ")
    (save-excursion
      (insert " ON SERVER ...")
      (sysdul-indent-newline 0)
      (sysdul-db-test "CLOSED" 141  "CLOSE DB" nil nil))))

(defun sysdul-open-db ()
  "Sysdul OPEN DATABASE."
  (interactive)
  (let (currc)
    (setq currc (current-column))
    (insert "OPEN DATABASE ")
    (save-excursion
      (insert " ON SERVER ... :")
      (sysdul-indent-newline 0 3)
      (insert "FOR UPDATE :")
      (sysdul-indent-newline 0)
      (insert "AS ... :")
      (sysdul-indent-newline 0)
      (insert "GIVING PASSWORD ... :")
      (sysdul-indent-newline 0)
      (insert "ON ...\n")
      (indent-to currc)
      (sysdul-db-test "OPENED" 140  "OPEN DB" nil nil))))

(defun sysdul-ready-ent ()
  "Sysdul READY-ENTITY."
  (interactive)
  (let (currc)
    (setq currc (current-column))
    (insert "READY ENTITY ")
    (save-excursion
      (insert " :")
      (sysdul-indent-newline 0 2)
      (sysdul-indent-newline 0)
      (insert "FOR UPDATE :")
      (sysdul-indent-newline 0)
      (insert "AS VIEW    WITH EXCLUSIVE UPDATE\n")
      (indent-to currc)
      (sysdul-db-test "READIED" 160  "READY ENTITY" nil nil))))

(defun sysdul-finish-ent ()
  "Sysdul FINISH ENTITY."
  (interactive)
  (let (currc)
    (setq currc (current-column))
    (insert "FINISH ENTITY ")
    (save-excursion
      (insert " :")
      (sysdul-indent-newline 0 2)
      (indent-to currc)
      (sysdul-db-test "FINISHED" 161  "FINISH ENTITY" nil nil))))

; ----------------------------------------------------------------------

(defun sub-sysdul-note (flag)
  "General Sysdul NOTE THAT."
  (if (< flag 3) (sysdul-get-ent))
  (if (and (>= flag 4) (<= flag 7)) (sysdul-get-pic))
  (insert "NOTE THAT ")
  (cond ((= flag 0) (save-excursion
                      (insert " REFERS TO " sysdul-last-ent)
                      (sysdul-indent-newline 0)))
        ((= flag 1) (progn
                      (insert sysdul-last-ent " IS UNDEFINED")
                      (sysdul-indent-newline 0)))
        ((= flag 2) (progn
                      (insert "SELECTION FOR " sysdul-last-ent " IS UNDEFINED")
                      (sysdul-indent-newline 0)))
        ((= flag 3) (progn
                      (insert "CURRENCY-SET ")
                      (save-excursion
                        (insert " IS UNDEFINED")
                        (sysdul-indent-newline 0))))
	((or (= flag 4)
	     (= flag 5)) (save-excursion
			   (insert " ON " sysdul-last-pic " [LINE n] IS ")
			   (insert (if (= flag 4) "UN") "CHANGED")))
	((or (= flag 6)
	     (= flag 7)) (save-excursion
			   (insert " PICTURE " sysdul-last-pic " IS ")
			   (insert (if (= flag 6) "UN") "CHANGED"))))
;  (save-excursion
;    (end-of-line 2)
;    (sysdul-db-test "" 0 "" 2 nil)))
  )

(defun sysdul-note-ref ()
  "Sysdul NOTE THAT REFERS TO."
  (interactive)
  (sub-sysdul-note 0))

(defun sysdul-note-undef ()
  "Sysdul NOTE THAT ... IS UNDEFINED."
  (interactive)
  (sub-sysdul-note 1))

(defun sysdul-note-selection ()
  "Sysdul NOTE THAT SELECTION."
  (interactive)
  (sub-sysdul-note 2))

(defun sysdul-note-curr ()
  "Sysdul NOTE THAT REFERS TO."
  (interactive)
  (sub-sysdul-note 3))

(defun sysdul-note-change (&optional chg)
  "Sysdul NOTE THAT field ON picture IS (UN)CHANGED."
  (interactive "P")
  (sub-sysdul-note (if chg 4 5)))

(defun sysdul-note-pic (&optional chg)
  "Sysdul NOTE THAT  PICTURE IS (UN)CHANGED."
  (interactive "P")
  (sub-sysdul-note (if chg 6 7)))

; ----------------------------------------------------------------------

(defun sysdul-exit-levels (num)
  "Sysdul EXIT n LEVELS (uses prefix argument)."
  (interactive "p")
  (insert "EXIT " (int-to-string num) " LEVEL")
  (if (> num 1) (insert "S")))

(defun sysdul-db-no ()
  "Sysdul SET DATABASE-NUMBER."
  (interactive)
  (insert "SET DATABASE-NUMBER "))

; ----------------------------------------------------------------------

(defun sub-sysdul-db (cmd)
  "Sysdul DB-statements."
  (sysdul-get-ent)
  (let ((dberr nil)
        testword
        msgno
        etxt
        (eent nil))
    (cond ((= cmd 0)
           (insert "GET " sysdul-last-ent " ")
           (setq testword "GOT"
		 msgno 2003
		 etxt "GET"))
          ((= cmd 1)
           (insert "STORE " sysdul-last-ent " WITH ")
           (setq testword "STORED"
		 msgno 2001
		 etxt "STORE"))
          ((= cmd 2)
           (insert "DELETE " sysdul-last-ent)
           (setq testword "DELETED"
		 msgno    2005
		 dberr    1
		 etxt     "DELETE"))
          ((= cmd 3)
           (insert "REMEMBER THAT " sysdul-last-ent " HAS ")
           (setq testword "REMEMBERED"
		 msgno    2002
		 etxt     "REMEMBER"))
          ((= cmd 4)
           (insert "FORGET THAT " sysdul-last-ent " HAS ")
           (setq testword "FORGOT"
		 msgno    2006
		 etxt     "FORGET")))
    (save-excursion
      (if (= cmd 2) (insert " DELAYED") (insert "<list>"))
      (sysdul-indent-newline 0)
      (sysdul-db-test testword msgno etxt dberr sysdul-last-ent))))

(defun sysdul-get ()
  "Sysdul GET from DB."
  (interactive)
  (sub-sysdul-db 0))

(defun sysdul-store ()
  "Sysdul STORE."
  (interactive)
  (sub-sysdul-db 1))

(defun sysdul-delete ()
  "Sysdul DELETE."
  (interactive)
  (sub-sysdul-db 2))

(defun sysdul-remember ()
  "Sysdul REMEMBER."
  (interactive)
  (sub-sysdul-db 3))

(defun sysdul-forget ()
  "Sysdul FORGET."
  (interactive)
  (sub-sysdul-db 4))

; ----------------------------------------------------------------------

(defun sub-sysdul-lock (pfx &optional name)
  "General Sysdul (UN)LOCK."
  (let ((nam))
    (if name (setq nam name) (setq nam sysdul-last-ent))
    (insert pfx "LOCK " nam)
    (sysdul-indent-newline 0)
    (sysdul-db-test (concat pfx "LOCKED") 515 (concat pfx "LOCK") nil)
    (end-of-line (+ 1 sysdul-msg-lines))))

(defun sysdul-lock ()
  "Sysdul LOCK."
  (interactive)
  (sysdul-get-ent)
  (sub-sysdul-lock ""))

(defun sysdul-lock-all ()
  "Sysdul LOCK ALL REFERENCES."
  (interactive)
  (sub-sysdul-lock "" "ALL REFERENCES"))

(defun sysdul-unlock-all ()
  "Sysdul UNLOCK."
  (interactive)
  (sub-sysdul-lock "UN" "ALL REFERENCES"))

; ----------------------------------------------------------------------

(defun sub-sysdul-trans (pfx errno)
  "General SYSDUL xxx TRANSACTION."
  (insert pfx " TRANSACTION")
  (sysdul-indent-newline 0)
  (sysdul-db-test (concat pfx "ED") errno (concat pfx "TRANSACTION") nil)
  (end-of-line (+ 1 sysdul-msg-lines)))

(defun sysdul-start-trans ()
  "Sysdul START TRANSACTION."
  (interactive)
  (sub-sysdul-trans "START" 511))

(defun sysdul-abort-trans ()
  "Sysdul ABORT TRANSACTION."
  (interactive)
  (sub-sysdul-trans "ABORT" 510))

(defun sysdul-end-trans ()
  "Sysdul END TRANSACTION."
  (interactive)
  (sub-sysdul-trans "END" 512))

(defun sysdul-commit-changes ()
  "Sysdul COMMIT CHANGES."
  (interactive)
  (insert "COMMIT CHANGES"))

(defun sysdul-rollback-changes ()
  "Sysdul ROLLBACK CHANGES."
  (interactive)
  (insert "ROLLBACK CHANGES"))

; ----------------------------------------------------------------------

(defun sub-sysdul-when-error (typerr)
  "General Sysdul WHEN xxx-ERROR."
  (insert "WHEN "typerr"-ERROR ")
  (sysdul-terminate-from))

(defun sysdul-when-db-err ()
  "Sysdul WHEN DB-ERROR."
  (interactive)
  (sub-sysdul-when-error "DB"))

(defun sysdul-when-io-err ()
  "Sysdul WHEN IO-ERROR."
  (interactive)
  (sub-sysdul-when-error "IO"))

(defun sysdul-when-ss-err ()
  "Sysdul WHEN SYSSCREEN-ERROR."
  (interactive)
  (sub-sysdul-when-error "SYSSCREEN"))

; ----------------------------------------------------------------------

(defun sub-sysdul-identify (pfx)
  "General Sysdul IDENTIFY xxx."
  (sysdul-get-ent)
  (insert "IDENTIFY " pfx " " sysdul-last-ent " AND GET ")
  (save-excursion
    (insert "<list>")
    (sysdul-indent-newline 0)
    (sysdul-db-test "IDENTIFIED" 2008 "IDENTIFY" 1 sysdul-last-ent)))

(defun sysdul-identify-first ()
  "Sysdul IDENTIFY FIRST."
  (interactive)
  (sub-sysdul-identify "FIRST"))

(defun sysdul-identify-last ()
  "Sysdul IDENTIFY LAST."
  (interactive)
  (sub-sysdul-identify "LAST"))

(defun sysdul-identify-next ()
  "Sysdul IDENTIFY NEXT."
  (interactive)
  (sub-sysdul-identify "NEXT"))

(defun sysdul-identify-prev ()
  "Sysdul IDENTIFY PREVIOUS."
  (interactive)
  (sub-sysdul-identify "PREVIOUS"))

(defun sysdul-identify-curr ()
  "Sysdul IDENTIFY CURRENT."
  (interactive)
  (sub-sysdul-identify "CURRENT"))

; ----------------------------------------------------------------------

(defun sub-sysdul-open-file (flag)
  "General Sysdul OPEN FILE."
  (let (ans res msgn msgt)
    (insert "OPEN ")
    (if (= flag 0)
	(setq ans
	      (read-from-minibuffer "(N)ew (O)ld or neither (default): "
				    nil nil nil nil))
      (setq ans "E"))
    (cond ((or (string= ans "N") (string= ans "n")) (insert "NEW "))
	  ((or (string= ans "O") (string= ans "o")) (insert "OLD "))
	  ((or (string= ans "E") (string= ans "e")) (insert "ERROR-")))
    (insert "FILE ")
    (save-excursion
      (if (= flag 0)
	  (progn
	    (insert " WITH UNIT " sysdul-last-fno " FOR ")
	    (setq ans
		  (read-from-minibuffer "(R)ead (W)rite or (A)ppend: "
					"R" nil nil nil)))
	(setq ans (read-from-minibuffer "(W)rite or (A)ppend: "
					"A" nil nil nil)))
      (if (= flag 0) (setq res "READ")
	(setq res "WRITE APPEND"))
      (cond ((or (string= ans "W") (string= ans "w"))
	     (setq res "WRITE"))
	    ((or (string= ans "A") (string= ans "a"))
	     (setq res "WRITE APPEND")))
      (if (= flag 0) (insert res "; [WITH RECORD-LENGTH nnn]"))
      (sysdul-indent-newline 0)
      (if (= flag 0)
	  (setq msgn 1001
		msgt "open file")
	(setq msgn 120
	      msgt "open trace file"))
      (sysdul-io-test msgn msgt))))

(defun sysdul-open-file ()
  "Sysdul OPEN FILE."
  (interactive)
  (sysdul-get-fno)
  (sub-sysdul-open-file 0))

(defun sysdul-open-error-file ()
  "Sysdul OPEN ERROR-FILE."
  (interactive)
  (sub-sysdul-open-file 1))

(defun sysdul-close-file ()
  "Sysdul CLOSE FILE."
  (interactive)
  (sysdul-get-fno)
  (insert "CLOSE FILE WITH UNIT " sysdul-last-fno)
  (save-excursion
    (sysdul-indent-newline 0)
    (sysdul-io-test 1002 "close file")))

(defun sysdul-close-error-file ()
  "Sysdul CLOSE ERROR-FILE."
  (interactive)
  (insert "CLOSE ERROR-FILE")
  (sysdul-indent-newline 0)
  (sysdul-io-test 121 "close trace file"))

(defun sysdul-open-mess-file ()
  "Sysdul OPEN MESSAGE FILE."
  (interactive)
  (insert "OPEN MESSAGE FILE ")
  (save-excursion
    (sysdul-indent-newline 0)
    (sysdul-io-test 1002 "close file")))

(defun sysdul-close-mess-file ()
  "Sysdul CLOSE MESSAGE FILE."
  (interactive)
  (insert "CLOSE MESSAGE FILE")
  (sysdul-indent-newline 0)
  (sysdul-io-test 1002 "close file"))

; ----------------------------------------------------------------------

(defun sub-sysdul-write (str out msgno msgt)
  (insert "WRITE " str)
  (save-excursion
    (insert " TO " out)
    (sysdul-indent-newline 0)
    (sysdul-io-test msgno msgt)))

(defun sysdul-write-file ()
  "Sysdul WRITE TO FILE."
  (interactive)
  (sysdul-get-fno)
  (sub-sysdul-write "" sysdul-last-fno 1101 "write to file"))

(defun sysdul-write-trace ()
  "Sysdul WRITE TO ERROR-FILE."
  (interactive)
  (sub-sysdul-write "" "ERROR-FILE" 122 "write to error-file"))

(defun sysdul-write-term ()
  "Sysdul WRITE TO TERMINAL."
  (interactive)
  (sub-sysdul-write "" "TERMINAL" 1000 "write to terminal"))

(defun sysdul-write-mess (mno)
  "Sysdul WRITE MESSAGE."
  (interactive "p")
  (sub-sysdul-write
   (concat "MESSAGE NUMBER " (int-to-string mno) " WITH (...)" )
   "..." 1000 "write"))

(defun sysdul-write-str ()
  "Sysdul WRITE TO STRING."
  (interactive)
  (sub-sysdul-write "" "<string>" 1000 "write..."))

; ----------------------------------------------------------------------

(defun sub-sysdul-read (out msgno msgt)
  "Sysdul general READ."
  (insert "READ ")
  (save-excursion
    (insert " FROM " out " ; [AT END TERMINATE FROM ...] ")
    (sysdul-indent-newline 0)
    (sysdul-io-test msgno msgt)))

(defun sysdul-read-file ()
  "Sysdul READ FROM FILE."
  (interactive)
  (sysdul-get-fno)
  (sub-sysdul-read sysdul-last-fno 1100 "read from file"))

(defun sysdul-read-term ()
  "Sysdul READ FROM TERMINAL."
  (interactive)
  (sub-sysdul-read "TERMINAL" 1000 "read from terminal"))

(defun sysdul-read-str ()
  "Sysdul READ FROM STRING."
  (interactive)
  (sub-sysdul-read "<string>" 1000 "read..."))

(defun sysdul-read-all ()
  "Sysdul READ ALL."
  (interactive)
  (sysdul-get-fno)
  (insert "READ ALL ")
  (save-excursion
    (insert " FROM " sysdul-last-fno)
    (sub-sysdul-in-out)
    (insert "END READ")))

; ----------------------------------------------------------------------

(defun sysdul-ready-pict ()
  "Sysdul READY PICTUREFILE."
  (interactive)
  (insert "READY PICTUREFILE "))

(defun sysdul-finish-screen ()
  "Sysdul FINISH SYSSCREEN."
  (interactive)
  (insert "FINISH SYSSCREEN"))

(defun sysdul-activate-pic ()
  "Sysdul ACTIVATE PICTURE."
  (interactive)
  (sysdul-get-pic)
  (insert "ACTIVATE FREE PICTURE " sysdul-last-pic "ON LINE ")
  (save-excursion
    (insert " COLUMN ... :  [WITH BOX]")
    (sysdul-indent-newline 0 5)
    (insert "UPON CLOSE ... :")
    (sysdul-indent-newline 0)
    (sysdul-when-ss-err)))

(defun sysdul-is-activated ()
  "Sysdul IF PICTURE IS ACTIVATED."
  (interactive)
  (sysdul-get-pic)
  (insert "IF PICTURE " sysdul-last-pic " IS ACTIVATED")
  (sysdul-indent-newline 1)
  (save-excursion
    (sysdul-indent-newline -1)
    (insert "END IF")))

(defun sysdul-if-control ()
  "Sysdul IF CONTROL-PROCEDURE IS."
  (interactive)
  (sysdul-get-sub)
  (insert "IF CONTROL-PROCEDURE IS " sysdul-last-sub)
  (sysdul-indent-newline 1)
  (save-excursion
    (sysdul-indent-newline -1)
    (insert "END IF")))

(defun sysdul-passivate-picture (&optional pic)
  "Sysdul PASSIVATE PICTURE."
  (interactive)
  (let (picn)
    (if pic
	(setq picn pic)
      (sysdul-get-pic)
      (setq picn sysdul-last-pic))
    (insert "PASSIVATE " picn " ")
    (sysdul-when-ss-err)))

(defun sysdul-passivate-curr ()
  "Sysdul PASSIVATE CURRENT PICTURE."
  (interactive)
  (sysdul-passivate-picture "CURRENT PICTURE"))

(defun sysdul-display ()
  "Sysdul DISPLAY TO PICTURE."
  (interactive)
  (sysdul-get-pic)
  (insert "DISPLAY ")
  (save-excursion
    (insert " TO " sysdul-last-pic " [LINE ...] :")
    (sysdul-indent-newline 0 2)
    (backward-delete-char-untabify 1 nil)
    (sysdul-when-ss-err)))

(defun sysdul-display-current ()
  "Sysdul DISPLAY CURRENT FIELD."
  (interactive)
  (insert "DISPLAY CURRENT FIELD EQUAL TO ")
  (save-excursion
    (sysdul-when-ss-err)))

(defun sub-sysdul-obtain (flag)
  "General Sysdul OBTAIN."
  (if (< flag 2) (sysdul-get-pic))
  (insert "OBTAIN ")
  (if (= flag 2) (insert "PREVIOUS "))
  (if (> flag 0) (insert "VALUE OF "))
  (if (> flag 1) (insert "CURRENT FIELD INTO "))
  (save-excursion
    (insert " FROM " sysdul-last-pic)
    (if (> flag 0) (insert " [LINE ...]"))
    (sysdul-indent-newline 0)
    (sysdul-if-test 0)
    (if (= flag 0) (progn
		     (insert "RETURNED WITH ERROR")
		     (end-of-line 3)
		     (sysdul-indent-newline 0)
		     (sysdul-if-test 0)))
    (insert "OBTAINED FIELD IS ALTERED")
    (end-of-line 3)
    (sysdul-indent-newline 0)
    (sysdul-if-test 1)
    (insert "EXIT IS GIVEN")))

(defun sysdul-obtain ()
  "Sysdul OBTAIN FROM PICTURE."
  (interactive)
  (sub-sysdul-obtain 0))

(defun sysdul-obtain-val ()
  "Sysdul OBTAIN VALUE OF."
  (interactive)
  (sub-sysdul-obtain 1))

(defun sysdul-obtain-prev ()
  "Sysdul OBTAIN PREVIOUS VALUE."
  (interactive)
  (sub-sysdul-obtain 2))

(defun sysdul-obtain-curr ()
  "Sysdul OBTAIN VALUE OF CURRENT FIELD."
  (interactive)
  (sub-sysdul-obtain 3))

(defun sysdul-display-user-message ()
  "Sysdul DISPLAY USER MESSAGE."
  (interactive)
  (let (ans typ currc)
    (setq currc (current-column))
    (insert "DISPLAY USER ")
    (setq ans
	  (read-from-minibuffer "(I)nfo (W)arning (F)ield (A)cknowledge: "
				"A" nil nil nil))
    (setq typ "ACKNOWLEDGE")
    (cond ((or (string= ans "I") (string= ans "i")) (setq typ "INFO"))
	  ((or (string= ans "W") (string= ans "w")) (setq typ "WARNING"))
	  ((or (string= ans "F") (string= ans "f")) (setq typ "FIELD")))
    (insert typ " MESSAGE :")
    (sysdul-indent-newline 0 2)
    (save-excursion
      (insert "[NUMBER ... WITH ...] [REPLY FROM ...] [WITH BELL]\n")
      (indent-to currc)
      (sysdul-if-test 0)
      (insert "REPLY IS ..."))))

(defun sysdul-start-new-inc ()
  "Sysdul START NEW INCARNATION."
  (interactive)
  (sysdul-get-pic)
  (insert "START NEW INCARNATION OF " sysdul-last-pic)
  (sysdul-indent-newline 0)
  (sysdul-when-ss-err))

(defun sysdul-ready-group ()
  "Sysdul READY GROUP."
  (interactive)
  (sysdul-get-pic)
  (insert "READY GROUP ")
  (save-excursion
    (insert " FOR OUTPUT ON "sysdul-last-pic)
    (sysdul-indent-newline 0)
    (sysdul-when-ss-err)))

(defun sysdul-for-all-rep ()
  "Sysdul FOR ALL REPETIONS."
  (interactive)
  (insert "FOR ALL REPETIONS OF ")
  (save-excursion
    (sysdul-indent-newline 1)
    (insert "<obtain>")
    (sysdul-indent-newline 0)
    (insert "IF INCARNATION IS NEW")
    (sub-sysdul-in-out)
    (insert "OR IF INCARNATION IS OLD")
    (sub-sysdul-in-out)
    (insert "OR IF INCARNATION IS DELETED")
    (sub-sysdul-in-out)
    (insert "ELSE")
    (sub-sysdul-in-out)
    (insert "END IF")
    (sysdul-indent-newline -1)
    (insert "END FOR")))

(defun sysdul-undo-chg ()
  "Sysdul UNDO CHANGES."
  (interactive)
  (insert "UNDO CHANGES IN CURRENT GROUP"))

; ----------------------------------------------------------------------

(defun sub-sysdul-on-off (txt negsp)
  "General Sysdul boolean->ON/OFF."
  (insert txt " ")
  (if (>= negsp 0) (insert "ON") (insert "OFF")))

(defun sysdul-sibas-trans (negs)
  "Sysdul SET SIBAS-TRANSACTIONS ON/OFF.
Use M-- for OFF, default is ON."
  (interactive "p")
  (sub-sysdul-on-off "SET SIBAS-TRANSACTIONS" negs))

(defun sysdul-auto-commit (negs)
  "Sysdul SET AUTOCOMMIT ON/OFF.
Use M-- for OFF, default is ON."
  (interactive "p")
  (sub-sysdul-on-off "SET AUTOCOMMIT" negs))

(defun sysdul-commit-interval (cnt)
  "Sysdul SET COMMIT-INTERVAL TO #.
Use numeric prefix to specify number."
  (interactive "p")
  (insert "SET COMMIT-INTERVAL TO " (int-to-string cnt)))

(defun sysdul-blocking-factor (cnt)
  "Sysdul SET BLOCKING-FACTOR # FOR.
Use numeric prefix to specify number."
  (interactive "p")
  (sysdul-get-ent)
  (insert "SET BLOCKING-FACTOR " (int-to-string cnt)
	  " FOR " sysdul-last-ent))

; ----------------------------------------------------------------------

(defun sysdul-prn-for-screen ()
  "Sysdul SET PRINTER FOR SCREEN."
  (interactive)
  (insert "SET PRINTER FOR SCREEN TO "))

(defun sysdul-refresh-screen ()
  "Sysdul REFRESH SCREEN."
  (interactive)
  (insert "REFRESH SCREEN"))

(defun sysdul-refresh-menu ()
  "Sysdul REFRESH MENU."
  (interactive)
  (insert "REFRESH MENU "))

(defun sysdul-insertion (negs)
  "Sysdul SET INSERTION ON/OFF.
Use M-- for OFF, default is ON."
  (interactive "p")
  (sub-sysdul-on-off "SET INSERTION" negs)
  (insert " FOR "))

(defun sysdul-deletion (negs)
  "Sysdul SET DELETION ON/OFF.
Use M-- for OFF, default is ON."
  (interactive "p")
  (sub-sysdul-on-off "SET DELETION" negs)
  (insert " FOR "))

(defun sysdul-user-key (negs)
  "Sysdul SET user-key ON/OFF.
Use M-- for OFF, default is ON."
  (interactive "p")
  (insert "SET U")
  (save-excursion
    (sub-sysdul-on-off "" negs)
    (insert " WITH ...")))

(defun sysdul-auto-skip (negs)
  "Sysdul SET AUTOSKIP ON/OFF.
Use M-- for OFF, default is ON."
  (interactive "p")
  (sub-sysdul-on-off "SET AUTOSKIP" negs))

(defun sysdul-copy-skip (negs)
  "Sysdul SET COPY-SKIP ON/OFF.
Use M-- for OFF, default is ON."
  (interactive "p")
  (sub-sysdul-on-off "SET COPY-SKIP" negs))

(defun sysdul-clear-edit (negs)
  "Sysdul SET CLEAR-EDIT ON/OFF.
Use M-- for OFF, default is ON."
  (interactive "p")
  (sub-sysdul-on-off "SET CLEAR-EDIT" negs))

(defun sysdul-uppercase (negs)
  "Sysdul SET UPPERCASE ON/OFF.
Use M-- for OFF, default is ON."
  (interactive "p")
  (sub-sysdul-on-off "SET UPPERCASE" negs))

(defun sysdul-lowercase (negs)
  "Sysdul SET LOWERCASE ON/OFF.
Use M-- for OFF, default is ON."
  (interactive "p")
  (sub-sysdul-on-off "SET LOWERCASE" negs))

(defun sysdul-defaulting (negs)
  "Sysdul SET DEFAULTING ON/OFF.
Use M-- for OFF, default is ON."
  (interactive "p")
  (sub-sysdul-on-off "SET DEFAULTING" negs))

(defun sysdul-visible-key (negs)
  "Sysdul SET VISIBLE KEY ON/OFF FOR ....
Use M-- for OFF, default is ON."
  (interactive "p")
  (sysdul-get-pic)
  (sub-sysdul-on-off "SET VISIBLE KEY" negs)
  (insert " FOR ")
  (save-excursion
    (insert " ON " sysdul-last-pic)))

(defun sysdul-locked-field (negs)
  "Sysdul SET LOCKED FIELD ON/OFF FOR ....
Use M-- for OFF, default is ON."
  (interactive "p")
  (sysdul-get-pic)
  (sub-sysdul-on-off "SET LOCKED FIELD" negs)
  (insert " FOR ")
  (save-excursion
    (insert " ON " sysdul-last-pic)))

(defun sysdul-video-on-off (negs)
  "Sysdul SET VIDEO ... ON/OFF.
If prefix is negative - set OFF, otherwise on.
Numeric prefix value sets video mode."
  (interactive "p")
  (sub-sysdul-on-off (concat "SET VIDEO " (int-to-string (abs negs))) negs)
  (insert " FOR CURRENT FIELD"))

(defun sysdul-video-on (negs)
  "Sysdul SET VIDEO ... ON FOR ....
Use prefix to number video mode."
  (interactive "p")
  (sysdul-get-pic)
  (insert "SET VIDEO " (int-to-string (abs negs)) " ON FOR ")
  (save-excursion
    (insert " ON " sysdul-last-pic)))

(defun sysdul-scrolling (negs)
  "Sysdul SET SCROLLING ON/OFF
Use M-- for OFF, default is ON."
  (interactive "p")
  (sub-sysdul-on-off "SET SCROLLING" negs))

; ----------------------------------------------------------------------


(defun sysdul-return ()
  "Sysdul RETURN."
  (interactive)
  (insert "RETURN"))

(defun sysdul-return-obtain ()
  "Sysdul RETURN AND OBTAIN AGAIN."
  (interactive)
  (sysdul-return)
  (insert " AND OBTAIN AGAIN"))

(defun sysdul-return-cont ()
  "Sysdul RETURN AND CONTINUE FROM."
  (interactive)
  (sysdul-return)
  (insert " AND CONTINUE FROM "))

(defun sysdul-return-error ()
  "Sysdul RETURN WITH ERROR."
  (interactive)
  (sysdul-return)
  (insert " WITH ERROR"))

(defun sysdul-return-terminate ()
  "Sysdul RETURN AND TERMINATE EVENT-PROCESSING."
  (interactive)
  (sysdul-return)
  (insert " AND TERMINATE EVENT-PROCESSING"))

(defun sysdul-perform ()
  "Sysdul PERFORM CONDITIONALLY."
  (interactive)
  (insert "PERFORM CONDITIONALLY ")
  (save-excursion
    (sysdul-indent-newline 0)
    (insert "IF PERFORMED")
    (sub-sysdul-in-out)
    (insert "END IF")))

; ======================================================================

(defun sysdul-start-event ()
  "Sysdul START EVENT-PROCESSING."
  (interactive)
  (sysdul-get-sub)
  (insert "START EVENT-PROCESSING UPON CLOSE " sysdul-last-sub))

(defun sysdul-field-seq ()
  "Sysdul SET FIELD SEQUENCE."
  (interactive)
  (sysdul-get-pic)
  (insert "SET FIELD SEQUENCE ")
  (save-excursion
    (insert " ON " sysdul-last-pic)))

; ======================================================================

(defun sub-sysdul-enable (field on-off)
  "Sysdul DISABLE/ENABLE ...
field: 0=field, 1=button, 2=picture, 3=editor, 4=menu"
  (sysdul-get-pic)
    (insert (if on-off "EN" "DIS") "ABLE ")
    (cond ((= field 1) (insert "BUTTON "))
	  ((= field 4) (insert "MENU"))
	  ((= field 3) (insert "EDITOR")))
    (if (/= field 3)
	(let ((mp (point)))
	  (insert (if (or (= field 0)
			  (= field 1))
		      (insert " ON"))
		  (if (< field 3)
		      " PICTURE " )
		  sysdul-last-pic)
	  (if (= field 2) (setq mp (point)))
	  (goto-char mp))))

(defun sysdul-enable-menu ()
  "Sysdul ENABLE MENU."
  (interactive)
  (sub-sysdul-enable 4 t))

(defun sysdul-disable-menu ()
  "Sysdul DISABLE MENU."
  (interactive)
  (sub-sysdul-enable 4 nil))

(defun sysdul-enable-field ()
  "Sysdul ENABLE field ON PICTURE."
  (interactive)
  (sub-sysdul-enable 0 t))

(defun sysdul-disable-field ()
  "Sysdul DISABLE field ON PICTURE."
  (interactive)
  (sub-sysdul-enable 0 nil))

(defun sysdul-disable-editor()
  "Sysdul DISABLE EDITOR."
  (interactive)
  (sub-sysdul-enable 3 nil))

(defun sysdul-enable-editor()
  "Sysdul ENABLE EDITOR."
  (interactive)
  (sub-sysdul-enable 3 t))

(defun sysdul-enable-pict ()
  "Sysdul ENABLE PICTURE."
  (interactive)
  (sub-sysdul-enable 2 t))

(defun sysdul-disable-pict ()
  "Sysdul DISABLE PICTURE."
  (interactive)
  (sub-sysdul-enable 2 nil))

(defun sysdul-enable-button ()
  "Sysdul ENABLE BUTTON ON PICTURE."
  (interactive)
  (sub-sysdul-enable 1 t))

(defun sysdul-disable-button ()
  "Sysdul DISABLE BUTTON ON PICTURE."
  (interactive)
  (sub-sysdul-enable 1 nil))

(defun sysdul-define-button ()
  "Sysdul DEFINE BUTTON."
  (interactive)
  (insert "DEFINE BUTTON ")
  (save-excursion
    (insert " WITH TEXT ")))


(defun sysdul-place-button ()
  "Sysdul PLACE BUTTON."
  (interactive)
  (sysdul-get-pic)
  (sysdul-get-sub)
  (insert "PLACE BUTTON ")
  (save-excursion
    (insert " ON PICTURE or-menu... :")
    (sysdul-indent-newline 0 3)
    (insert "WITH PROCEDURE")))


; ======================================================================

(defun sysdul-start-loop ()
  "Sysdul START LOOP FOR UPDATE."
  (interactive)
  (sysdul-get-ent)
  (sysdul-get-pic)
  (insert "START LOOP FOR UPDATE OF " sysdul-last-ent " ON " sysdul-last-pic)
  (sub-sysdul-in-out)
  (forward-line -1))

(defun sysdul-continue-loop ()
  "Sysdul CONTINUE LOOP"
  (interactive)
  (insert "CONTINUE LOOP"))

(defun sysdul-terminate-loop ()
  "Sysdul TERMINATE LOOP"
  (interactive)
  (insert "TERMINATE LOOP"))

; ======================================================================

; ----------------------------------------------------------------------

(defun sub-sysdul-macro (&optional pfx ret sfx ind)
  "Insert svapp macro statements"
  (if pfx (insert pfx))
  (if ind (indent-relative))
  (if sfx (insert sfx))
  (if ret (forward-char (- ret))))

(defun sysdul-macro-pair ()
  "Insert @@"
  (interactive)
  (sub-sysdul-macro "@@" 1))

(defun sysdul-macro-def ()
  "Insert *V MACRO"
  (interactive)
  (sub-sysdul-macro "*V MACRO  = " 3))

(defun sysdul-end-include ()
  "Insert *V END-INCLUDE"
  (interactive)
  (sub-sysdul-macro "*V" 0 "END-INCLUDE" t))

(defun sysdul-else-include ()
  "Insert *V ELSE-INCLUDE"
  (interactive)
  (sub-sysdul-macro "*V" 0 "ELSE-INCLUDE" t))

(defun sysdul-or-include ()
  "Insert *V OR-INCLUDE"
  (interactive)
  (sub-sysdul-macro "*V" 3 "OR-INCLUDE IF  = " t))

(defun sysdul-file-include ()
  "Insert *V FILE-INCLUDE"
  (interactive)
  (sub-sysdul-macro "*V" 0 "FILE-INCLUDE " t))

(defun sysdul-svapp-error ()
  "Insert *V ERROR"
  (interactive)
  (sub-sysdul-macro "*V" 0 "ERROR ;" t))

(defun sysdul-include-if ()
  "Insert *V INCLUDE IF"
  (interactive)
  (sub-sysdul-macro "*V" 3 "INCLUDE IF  = " t))

; ----------------------------------------------------------------------

(defun sysdul-plus-plus (&optional size)
  "Sets preceding var = preceding var + <numarg>"
  (interactive "p")
  (let (var-na
	cpos)
    (forward-word -1)
    (setq cpos (point))
    (forward-word 1)
    (setq var-na (buffer-substring cpos (point)))
    (insert " = " var-na " " (if (> size 0) "+" "-") " "
	    (number-to-string (abs size)))))

(defun sysdul-minus-minus (&optional size)
  "Sets preceding var = preceding var - <numarg>"
  (interactive "p")
  (sysdul-plus-plus (- size)))

; abbrev: previous/next/leave/delete-incarnation


;;; sysdul-abbrev.el ends here
