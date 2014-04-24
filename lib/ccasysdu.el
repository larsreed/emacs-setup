;;; ccasysdu.el --- edit Sysdul source code

;;; Copyright (C) 1995 Lars Reed

;; Author:		Lars Reed <lre@sysdeco.no>
;; Last-Modified:	95/10/19
;; Version:		1.7
;; Keywords:		languages Sysdul CCAS
;; Adapted-By:

;;; Commentary:

;; ....

;;; Code:
;;; ========================================================================



(setq sysdul-msg-function 'ccas-sys-msg
      sysdul-ioerr-function 'ccas-io-error
      sysdul-local-abbs 'ccas-abb-bind)
(setq sysdul-msg-lines 5)

; ======================================================================

(defun ccas-abb-bind ()
; CCAS Sysdul abbreviations."
  '(
    ("3D" . ccas-macro-def)
    ("3F" . ccas-file-include)
    ("3I" . ccas-macro-dir)
    ("3M" . ccas-macro-pair)
    ("3S" . ccas-macro-suff)
    ("4B" . ccas-boolean)
    ("4D" . ccas-ddname)
    ("4F" . ccas-false)
    ("4I" . ccas-int)
    ("4N" . ccas-file)
    ("4T" . ccas-true)
    ("SM" . ccas-sys-msg)
    ("5TS" . ccas-start-trans)
    ("5TE" . ccas-end-trans)))


; ======================================================================

; Make SYSTEM message
(defun ccas-sys-msg (msgno &optional etxt eent)
  "Create std. @SYS_MSG@ body."
  (interactive "nMessage no: \nsError description: \nsEntity: ")
  (insert "@SYS_MSG@\n")
  (indent-relative)
  (insert "WERRNO = " (int-to-string msgno))
  (if etxt (insert "  ; " etxt "-error\n")
           (insert " ; ???\n"))
  (indent-relative)
  (if eent (progn (insert "WMENTA = \'" eent "\'\n")
                  (indent-relative)))
  (insert "WRITE " eent ".<" sysdul-tbf "-id> TO WKEY\n")
  (indent-relative)
  (insert "@SHOW_MSG@\n"))

(defun ccas-io-error (msgno &optional etxt)
  "Create std. @SYS_MSG@ body for IO-error."
  (interactive "nMessage no: \nsError description: ")
  (insert "@SYS_MSG@\n")
  (indent-relative)
  (insert "WERRNO = " (int-to-string msgno))
  (if etxt (insert "  ; Cannot " etxt "\n")
           (insert " ; ???\n"))
  (indent-relative)
  (insert "READ WMVAR1, WMVAR2, WMVAR3 FROM WFILENAM\n")
  (indent-relative)
  (insert "@SHOW_MSG@\n"))

; ======================================================================

(defun ccas-macro-def ()
  "Insert @LOCAL@"
  (interactive)
  (sub-sysdul-macro "@LOCAL@ = " 3))

(defun ccas-macro-dir ()
  "Insert @SVPDIR@."
  (interactive)
  (sub-sysdul-macro "@SVPDIR@" 1))

(defun ccas-macro-pair ()
  "Insert @L_@"
  (interactive)
  (sub-sysdul-macro "@L_@" 1))

(defun ccas-macro-suff ()
  "Insert @SUFF_@"
  (interactive)
  (sub-sysdul-macro "@SUFF_@" 1))

(defun ccas-file-include ()
  "Insert @INT@"
  (interactive)
  (sub-sysdul-macro "@FI@ "))

(defun ccas-int ()
  "Insert @INT@"
  (interactive)
  (sub-sysdul-macro "@INT@"))

(defun ccas-file ()
  "Insert @FILENAME@"
  (interactive)
  (sub-sysdul-macro "@FILENAME@"))

(defun ccas-ddname ()
  "Insert @DD_NAME@"
  (interactive)
  (sub-sysdul-macro "@DD_NAME@"))

(defun ccas-boolean ()
  "Insert @BOOLEAN@"
  (interactive)
  (sub-sysdul-macro "@BOOLEAN@"))

(defun ccas-true ()
  "Insert @TRUE@"
  (interactive)
  (sub-sysdul-macro "@TRUE@"))

(defun ccas-false ()
  "Insert @FALSE@"
  (interactive)
  (sub-sysdul-macro "@FALSE@"))

(defun ccas-start-trans ()
  "Sysdul START TRANSACTION."
  (interactive)
  (insert "@START_TRANS@")
  (sysdul-indent-newline 0)
  (insert "IF SYSFLG.TRANSLCK <> @TRUE@")
  (sysdul-indent-newline 1)
  (insert "TERMINATE FROM ")
  (sysdul-get-label)
  (insert sysdul-last-label)
  (sysdul-indent-newline -1)
  (insert "END IF\n"))

(defun ccas-end-trans ()
  "Sysdul END TRANSACTION."
  (interactive)
    (insert "@END_TRANS_TEST@")
    (sysdul-indent-newline 0)
    (insert "IF WDBSTAT EQUAL TO 1    \; Concurrency")
    (sysdul-indent-newline 1)
    (insert "RESTART FROM ")
    (sysdul-get-label)
    (insert sysdul-last-label)
    (sysdul-indent-newline -1)
    (insert "OR IF WDBSTAT EQUAL TO 0 \; Other error")
    (sysdul-indent-newline 1)
    (insert "@END_TRANS_ERR@")
    (sysdul-indent-newline 0)
    (insert "TERMINATE FROM ")
    (sysdul-get-label)
    (insert sysdul-last-label)
    (sysdul-indent-newline -1)
    (insert "END IF"))

;;; ccasysdu.el ends here
