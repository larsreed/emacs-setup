; Mode for fontifying JBoss and WebLogic log files.
; Addition of log4j from Ken Gold <kgold@watson.ibm.com>
; Warning lines are displayed in bold orange,
; error lines in bold red, and 'Caused by' lines in exception traces are
; reverse videoed in pink
;
; Author:   Glen Cordrey 10/29/2004 glen@oojava.com
; Version:  1.0
;
; Tested against logs from JBoss 3.0.7 and WebLogic 7.0.4.
; Log lines to fontify are
; selected via regexps, so it should be easy to accomodate changes by simply
; modifying the regexps.
;
; To use, edit your .emacs to contain
;      (add-to-list 'auto-mode-alist '("\\.log\\'" . websrv-log-mode))
;      (autoload 'websrv-log-mode "websrv-log"
;	"View JBoss/WebLogic error logs")
; Any file with a .log extension will then open in this mode

; allow hooks if anyone wants to add them
(defvar websrv-log-mode-hook nil
  "Hooks for websrv-log-mode")

; define some faces
(defgroup log-faces nil
  "Faces for log mode"
  :group 'websrv-log)

(defface websrv-log-caused-by-face '((t (:background "Pink")))
  "Face used for Caused by lines in exceptions"
  :group 'websrv-log-faces)

(defface websrv-log-warning-face '((t (:foreground "Orange"t) ))
  "Face used for lines containing warnings"
  :group 'websrv-log-faces)

(defface websrv-log-error-face '((t (:foreground "Red") ))
  "Face used for lines containing errors"
  :group 'websrv-log-faces)

(defface websrv-log-fatal-face '((t (:foreground "Red" :bold t) ))
  "Face used for lines containing errors"
  :group 'websrv-log-faces)

(defface websrv-log-debug-face '((t (:foreground "Orange" :bold t) ))
  "Face used for lines containing errors"
  :group 'websrv-log-faces)


(defvar websrv-log-caused-by-face 'websrv-log-caused-by-face)
(defvar websrv-log-warning-face	  'websrv-log-warning-face)
(defvar websrv-log-error-face	  'websrv-log-error-face)
(defvar websrv-log-fatal-face	  'websrv-log-fatal-face)
(defvar websrv-log-debug-face	  'websrv-log-debug-face)

(define-generic-mode 'websrv-log-mode
  '("#")
  '("WARN" "INFO" "DEBUG" "FATAL" "Caused by")
  '(
    (".*<Error>.*" . websrv-log-error-face)
    (".*<Warning>.*" . websrv-log-warning-face)
    (".*ERROR.*" . websrv-log-error-face)
    (".*WARN.*" . websrv-log-warning-face)
    (".*FATAL.*" . websrv-log-fatal-face)
    (".*DEBUG.*" . websrv-log-debug-face)
    ("Caused by.*" . websrv-log-caused-by-face)
    )
  nil nil
  "Major mode for LOG4J, JBOSS, WebLogic log files")

(provide 'websrv-log-mode)
