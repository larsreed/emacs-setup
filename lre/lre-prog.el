;;; lre-prog.el --- Lars Reed - Emacs init file
;; Some less frequently used programming utilities

;; Copyright (C) 2002-2014 Lars Reed
;;   See lresetup.el
;; Author:              Lars Reed <Lars@kalars.net>
; Hi-lock: (("^;;; lre.*\\.el[^\n]*" (0 (quote hi-black-hb) t)))
; Hi-lock: (("Lars.*R[e]ed" (0 (quote hi-blue) t)))
; Hi-lock: (("^;;; [^\n]+" (0 (quote hi-green) t)))

;;; Code

(eval-when-compile
  (lre-safe-require 'comint)
  (lre-safe-require (if (lre-memb 'e21+) 'sql 'sql-mode)))

(defalias 'lre--compiler (if (fboundp 'compilation-start) 'compilation-start
                           'compile-internal))


;;; ---------------------------------------------------------------------
;;; /// Misc ///

(defun lre-choose-java-mode()
  "Select the right java-mode"
  (interactive)
  (if (lre-memb 'jde)
      (jde-mode)
    (java-mode)))

(defun lre-insert-code-doc (pfx)
  "Insert code documentation."
  (interactive "P")
  (let (p1 name g-point)
    ;; navigate & get name
    (save-excursion
      (cond ((eq major-mode 'cf-mode)
             (beginning-of-line)
             (when (looking-at
                    (concat "\\(.\\(define\\|ifn?def\\)\\)\\|"
                            "\\(\\s-*\\("
                            "declare\\|call\\|execute\\|exporting\\|importing"
                            "\\)\\)"))
               (forward-word 1)
               (forward-char))
             (setq p1 (point))
             (end-of-line)
             (setq name
                   (lre-replace-many-regexps (buffer-substring p1 (point))
                          '(("\\s-*\\(@@\\)?[\\]"  "")
                            ("\\s-*[:]$"  "")
                            ("[|]DialogShortName[|]"  "_?DSN?_")
                            ("[|]Fla?g[|]"  "_?flg?_")
                            ("[|]Entity"  "_?Klasse?")
                            ("^\\s-*[;]\\s-*" "")
                            ("^\\s-*" "")
                            ("\\([(].*[)]\\)"  " \\1")
                            ("[|]" "_"))))
             )))
    ;; navigate & insert comment
    (save-excursion
      (cond ((eq major-mode 'cf-mode)
             (beginning-of-line 0)
             (if (not (looking-at ".ifn?def"))
                 (beginning-of-line 2))
             (insert "/* @doc-"
                     (cond ((string-match "[Hh]ook" name)
                            "hook")
                           ((string-match "[%]M[%]" name)
                            "file")
                           ((string-match "^[zxZXW]" name)
                            "var")
                           ((string-match "^P[DFG]" name)
                            "fun")
                           ((string-match "^[CS][DJFG]" name)
                            "proc")
                           ((string-match "[@][^@]+[@]" name)
                            "svp")
                           (t "mac"))
                     " " name)
             (setq g-point (point))
             (insert
                     (if (not pfx) "\n *    " "")
                     "*/\n"))
            ((eq major-mode 'vsq-mode)
             (insert "/* @doc-sql navn IUD x */")))
      )
    (if g-point (goto-char g-point))
    )
  )



;;; ---------------------------------------------------------------------
;;; /// Sysdul adm ///

(defun lre-hack-ddf (bufone bufall dialog)
  "Merge compiled picture."
  (interactive
   "bBuffer with new picture \nbBuffer with old DDF \nsDialog name: ")
  (save-excursion
    (let (buftxt
          start-p
          no-chars-new
          no-lines-new
          l-delta
          p-delta
          end-p
          (rexp1 (concat "start dlg '" dialog "'"))
          (rexp2 "^\\s-*$"))
      ;;
      ;; Copy new picture
      ;;
      (set-buffer bufone)               ; Buffer with new
      (goto-char (point-min))
      (re-search-forward rexp1)         ; Find start dlg
      (beginning-of-line)
      (setq start-p (point))
      (re-search-forward rexp2)         ; Find next empty line
      (beginning-of-line)
      (setq buftxt (buffer-substring start-p (point)) ; Copy dialog definition
            no-chars-new (- (point) start-p)
            no-lines-new (count-lines start-p (point)))
      ;;
      ;; Remove old picture
      ;;
      (set-buffer bufall)               ; DDF-file
      (goto-char (point-min))
      (re-search-forward rexp1)         ; Start dlg
      (beginning-of-line)
      (setq start-p (point))
      (re-search-forward rexp2)
      (beginning-of-line)
      (setq end-p (point)               ; Compute difference
            p-delta (- no-chars-new (- (point) start-p))
            l-delta (- no-lines-new (count-lines start-p end-p)))
      (kill-region start-p end-p)       ; Remove old def
      (goto-char start-p)
      (insert buftxt)                   ; And insert new
      ;;
      ;; Recalculate offsets
      ;;
      (goto-char (point-min))           ; Find this dialog in the offset list
      (setq rexp1 (concat "^ " dialog " .*,.*;"))
      (re-search-forward rexp1)
      (beginning-of-line 2)             ; For each of the following dialogs
      (while (not (looking-at " start "))
        (end-of-line)
        (forward-word -2)
        (setq start-p (point))
        (forward-word 1)
        (setq buftxt (buffer-substring start-p (point)))
        (delete-region start-p (point)) ; Remove old byte offset
        (insert (format "%d" (+ (To-int buftxt) p-delta l-delta)))
                                        ; Add size diff + 1 byte per line
        (forward-char 1)
        (setq start-p (point))
        (forward-word 1)
        (setq buftxt (buffer-substring start-p (point)))
        (delete-region start-p (point)) ; Remove line offset
        (insert (format "%d" (+ (To-int buftxt) l-delta)))
                                        ; Add line diff
        (beginning-of-line 2))))
  (switch-to-buffer bufall))

(defun lre-grape-ready-file ()
  "Klargjør fil for innsjekking.
Kjøres fra vsd-fila - fjerner del 2, og henter inn cf-filene."
  (interactive)
  (goto-char (point-min))
  (let ((b-base (substring (buffer-file-name) 0 (- (length (buffer-file-name))
                                                   4))))
    (message "Fjerner seksjon 2..4")
    (lre-grape-kill-part-4)
    (lre-grape-kill-part-3)
    (lre-grape-kill-part-2)
    (message "Legger inn seksjon 3 (PROC)")
    (search-forward "INCLUDE IF SRCTYPE = PROC")
    (beginning-of-line 2)
    (insert-file-contents (concat b-base "_proc.cf"))
    (message "Legger inn seksjon 4 (DEF)")
    (search-forward "INCLUDE IF SRCTYPE = DEF")
    (beginning-of-line 2)
    (insert-file-contents (concat b-base "_def.cf")))
  (sysdul-fix-white))

(defun lre-grape-auto-ready-file (fna)
  "For evt kall via rundoit."
  (if (< (length fna) 1)
      (error "Mangler filnavn")
    (save-excursion
      (find-file fna)
      (lre-grape-ready-file)
      (save-buffer)
      (kill-buffer (current-buffer)))))

(defun lre-grape-update-events ()
  "Change x_mXXX_events.svp."
  (interactive)
  (goto-char (point-min))
  (search-forward "AVVIK FRA STANDARD")
  (beginning-of-line 0)
  (insert "*V ; ")
  (lre-todays-date 3 t)
  (insert "    Oppdatert\n")
  (search-forward "@GRAPE_COADM_BEGIN@")
  (forward-char 1)
  (let ((s-p (point)))
    (search-forward "@GRAPE_COADM_END@")
    (forward-char 1)
    (delete-region s-p (point)))
  (let ((s (concat (substring (file-name-nondirectory
                               (buffer-file-name)) 3 6) ".coadm")))
    (insert-file-contents
     (read-file-name "Filnavn, ny eventhandler: " nil s t s)))
  (sysdul-fix-white))

(defun lre-grape-update-main ()
  "Change mXXX.vsd."
  (interactive)
  (lre-grape-kill-part-2)
  (let* ((s0 (substring (file-name-nondirectory (buffer-file-name)) 1 4))
         (s1 (concat s0 ".new"))
         (s2 (concat s0 ".diff")))
    (insert-file-contents
     (read-file-name "Filnavn, ny del 2: " nil s1 t s1))
    (goto-char (point-min))
    (if (not (search-forward "INCLUDE IF SRCTYPE = MAN")) ()
      (beginning-of-line)
      (insert "*V ; Manuelt oppdatert: ")
      (lre-todays-date 4 t)
      (insert "\n"))
    (sysdul-fix-white)
    (find-file-other-frame
     (read-file-name "Filnavn, diffil: " nil s2 t s2))))

(defun lre-grape-open-non-diff (&optional new-frame)
  "Åpne fil svarende til diff-fil, posisjoner cursor ved endring om mulig."
  (interactive)
  (let ((fnam (lre-replace-in-string buffer-file-name "org/diff." ""))
        go-line)
    (save-excursion
      (if (= (point) (point-min))
          (forward-line 1))
      (if (re-search-backward
           "^\\([0-9]+\\)\\(,[0-9]+\\)?[a-z]\\([0-9]+\\)?" nil t)
          (setq go-line (string-to-number (or (match-string 3)
                                              (match-string 1)
                                              "0")))))
    (if new-frame
        (find-file-other-frame fnam)
      (find-file-other-window fnam))
    (if go-line (goto-line go-line))))

(defun lre-grape-open-non-diff-new-frame()
  "Åpne fil svarende til diff i nytt vindu,
posisjoner cursor ved endring om mulig"
  (interactive)
  (lre-grape-open-non-diff t))

(defun lre-load-sysdul-stereo (file1 file2 &optional dir)
  "Åpner filene FILE1 og FILE2, cder evt. til DIR først."
  (if dir (cd dir))
  (server-find-file file1)
  (sysdul-next-error nil)
  (find-file-other-window file2))

(defsubst lre-grape-procify-sub-1 (rx sub-s &optional delim)
  (goto-char (point-min))
  (query-replace-regexp rx sub-s delim))

(defun lre-grape-procify (&optional dlg-name shrt-name)
  "Konverterer vsd-fil til proc.cf-format
\(eller hjelper i hvert fall litt til med det...\)"
  (interactive
   (list (read-from-minibuffer "Dialognavn: " (substring
                                               (buffer-name)
                                               0
                                               (- (length (buffer-name))
                                                  7)))
         (read-string "SE-navn: ")))
  (lre-grape-procify-sub-1 "if\\s-+not\\s-+zzqok"  "If_Fail")
  (lre-grape-procify-sub-1 "if\\s-+zzqok"       "If_Success")
  (lre-grape-procify-sub-1 "if\\s-+zzqdeleted"  "If_Deleted")
  (lre-grape-procify-sub-1 "if\\s-+zzqstored"   "If_Stored")
  (lre-grape-procify-sub-1 "zzqok\\s-+=\\s-+true"  "Success")
  (lre-grape-procify-sub-1 "zzqok\\s-+=\\s-+false" "Fail")
  (lre-grape-procify-sub-1 "@TRANS_BEGIN@"   "TadBeginTrans")
  (lre-grape-procify-sub-1 "@TRANS_ABORT@"   "TadAbortTrans")
  (lre-grape-procify-sub-1 "@TRANS_END@"     "TadEndTrans")
  (lre-grape-procify-sub-1 "@BLANK@"         "")
  (lre-grape-procify-sub-1 (concat "CUR_" (regexp-quote dlg-name) "\\>")
                           "Cur" )
  (lre-grape-procify-sub-1 (concat " " (regexp-quote dlg-name) "\\>")
                           " DialogName" )
  (lre-grape-procify-sub-1 (concat "_" (regexp-quote dlg-name))
                           "|DialogName")
  (lre-grape-procify-sub-1 (concat "_" shrt-name "_")
                           "|DialogShortName|")
  (lre-grape-procify-sub-1 (concat "_" shrt-name)
                           "|DialogShortName")
  (lre-grape-procify-sub-1 shrt-name "DialogShortName")
  (lre-grape-procify-sub-1 (concat
                            ";\\s-+-+\n;\\s-+Feilh.ndtering\n;\\s-+-+\n"
                            "\\(\\s-*default\\s-*error-handling\\s-*"
                            "when\\s-*db-error\\s-+terminate\\s-*from"
                            "\\s-*@LABEL_DB@\n"
                            "\\s-*@HOOK_DEF_DBERR@\n\\)?"
                            "\\(\\s-*default\\s-*error-handling\\s-*"
                            "when\\s-*gui-error\\s-+terminate\\s-*from"
                            "\\s-*@LABEL_GUI@\n"
                            "\\s-*@HOOK_DEF_GUIERR@\n*\\)?")
                           "")
  (lre-grape-procify-sub-1 (concat ";\\s-+%%%%%%%%%%%%%%%%+\n"
                                   ";\\s-+[a-zA-Z0-9_]*\n"
                                   ";\\s-+%%%%%%%%%%%%%%%%+\n+")
                           "")
  (lre-grape-procify-sub-1 (concat ";\\s-+-+\n;\\s-+Versjonsstreng\n;\\s-+-+\n+"
                                   "\\s-*W_SCCS_ID\\s-*=\\s-*.*>.\n")
                           "")
  (lre-grape-procify-sub-1 (concat "\\s-*"
                                   "\\(@HOOK_DEF_LEAVE@[\n \t]+\\)?"
                                   "\\(terminate from @LABEL_END@[\n \t]+\\)?"
                                   "\\(terminate-label @LABEL_DB@[\n \t]+\\)?"
                                   "\\(@ERR_DB@[\n \t]+\\)?"
                                   "\\(terminate from @LABEL_END@[\n \t]+\\)?"
                                   "\\(terminate-label @LABEL_GUI@[\n \t]+\\)?"
                                   "\\(@ERR_GUI@[\n \t]+\\)?"
                                   "\\(terminate from @LABEL_END@[\n \t]+\\)?"
                                   "\\(terminate-label @LABEL_END@[\n \t]+\\)?"
                                   "\\(@HOOK_LEAVE@[\n \t]+\\)?"
                                   "\\(terminate-label @LABEL_EXIT@[\n \t]+\\)?"
                                   "@PEND_[A-Z]+@[\n]+")
                           "")
  (lre-grape-procify-sub-1 (concat "^\\s-*@\\("
                                   "PBEGIN_[A-Z]+\\|"
                                   "UNIVERSAL\\|"
                                   "GRAPE_.*VARS"
                                   "\\)@\n+")
                           "")
  (lre-grape-procify-sub-1 "^\\s-*procedure\\s-+\\([^ \n]+\\)\\(\\s-+employs.*\\)"
                           "TadHVProcStart(\\1,???)")
  (lre-grape-procify-sub-1 "^\\s-*end\\s-+procedure\\s-+;\\s-*\\([a-zA-Z0-9_]+\\)?.*$"
                           "TadHVProcEnd(\\1)")
  (lre-grape-procify-sub-1 "^\\s-*control-procedure\\s-+\\([^ \n]+\\)\\(\\s-+employs.*\\)$"
                           "TadHVCpStart(\\1,???)")
  (lre-grape-procify-sub-1 "^\\s-*end\\s-+control-procedure\\s-+;\\s-*\\([a-zA-Z0-9_]+\\)?.*$"
                           "TadHVCpEnd(\\1)")
  (lre-grape-procify-sub-1 "^\\s-*end\\s-+control-procedure\\s-*;\\(.*\\))"
                           "TadHVCpEnd(\\1)")
  (lre-grape-procify-sub-1 ";\\s-+\\(PROSEDYRE\\|TYPE\\):\\s-+.*\n" "")
  (lre-grape-procify-sub-1 ";\\s-+-+\n;\\s-+Grapedeklarasjoner\n;\\s-+-+\n+" "")
  (lre-grape-procify-sub-1 "terminate\\s-+from\\s-+@LABEL_E\\(XIT\\|ND\\)@"
                           "TadExitProc")
  (lre-grape-procify-sub-1 "\\<return\\>" "TadExitProc" t)
  (lre-grape-procify-sub-1 (concat "If_Fail[ @\\\n\t]+"
                                   "\\(return\\|TadExit\\(Fail\\|Proc\\)\\)"
                                   "[ @\\\t\n]+\\(end if\\|Fi_Fail\\)")
                           "Return_If_Failed")
  (lre-grape-procify-sub-1 (concat "call\\s-+\\([a-zA-Z|0-9_]*\\)"
                                   "\\(\\s-*;[^\n]*\\)?\\(\\s-*@@\\\\\\)?\n"
                                   "\\(\\s-*Tad[_]+Empty[ \t\n]*"
                                   "\\(@@\\\\\\)?\n\\)*"
                                   "[ \t\n]*Return_If_Failed")
                           "CallAndTest(\\1)")
  (lre-grape-procify-sub-1 "^\\(\\s-*if\\s-+[^\n]+\\)\\s-+=\\s-+"
                           "\\1 equal to ")
  (lre-grape-procify-sub-1 "^\\(\\s-*or\\s-+if\\s-+[^\n]+\\)\\s-+=\\s-+"
                           "\\1 equal to ")
  (lre-grape-procify-sub-1 "zzqstored\\s-+=\\s-+false" "Fail_Stored")
  (lre-grape-procify-sub-1 "zzqstored\\s-+=\\s-+true"  "Success_Stored")
  (lre-grape-procify-sub-1 "zzqdeleted\\s-+=\\s-+false" "Fail_Deleted")
  (lre-grape-procify-sub-1 "zzqdeleted\\s-+=\\s-+true"  "Success_Deleted")
  (lre-grape-procify-sub-1 "Fail[ @\\\t\n]+TadExitProc" "TadExitFail")
  (lre-grape-procify-sub-1 "Success[ @\\\t\n]+TadExitProc" "TadExitSuccess")
  (lre-grape-procify-sub-1 "Fail_Stored[ @\\\t\n]+TadExitFail"
                           "TadExitFailStored")
  (lre-grape-procify-sub-1 "Fail[ @\\\t\n]+Fail_Stored[ @\\\t\n]+TadExitProc"
                           "TadExitFailStored")
  (lre-grape-procify-sub-1 "disable\\s-+\\([^\n]+\\)\\s-+in\\s-+DialogName"
                           "DisableObject(\\1)")
  (lre-grape-procify-sub-1 "enable\\s-+\\([^\n]+\\)\\s-+in\\s-+DialogName"
                           "EnableObject(\\1)")
  (lre-grape-procify-sub-1 "show\\s-+\\([^\n]+\\)\\s-+in\\s-+DialogName"
                           "ShowObject(\\1)")
  (lre-grape-procify-sub-1 "hide\\s-+\\([^\n]+\\)\\s-+in\\s-+DialogName"
                           "HideObject(\\1)")
  (lre-grape-procify-sub-1 "note\\s-+that\\s-+selection\\s-+for\\s-+\\([^\n \t]+\\)\\s-+\\(is\\|are\\)\\s-+undefined"
                           "TadNoSel(\\1)")
  (lre-grape-procify-sub-1 "note\\s-+that\\s-+\\([^\n \t]+\\)\\s-+\\(is\\|are\\)\\s-+undefined"
                           "TadNoCurr(\\1)")
  (lre-grape-procify-sub-1 "display\\s-+value\\s-+list\\s-+of\\s-+\\([^\n]+\\)\\s-+in\\s-+DialogName"
                           "TadDispVal(\\1)")
  (lre-grape-procify-sub-1 "clear\\s-+value\\s-+list\\s-+of\\s-+\\([^\n]+\\)\\s-+in\\s-+DialogName"
                           "TadClearVal(\\1)")
  (lre-grape-procify-sub-1 "clear\\s-+\\([^\n]+\\)\\s-+in\\s-+DialogName"
                           "ClearObject(\\1)")
  (lre-grape-procify-sub-1 "\\((\\s-+\\)?value\\s-+of\\s-+\\(.+\\)\\s-+in\\s-+DialogName\\s-+\\(is\\|are\\)\\s-+changed\\(\\s-+)\\)?"
                           "TadIsChanged(\\2)")
;; >>>  (goto-char (point-min))
;; >>>  (replace-regexp "Tad__+Empty" "")
  (goto-char (point-min))
  (sysdul-fix-white)
  (goto-char (point-min))
  (replace-regexp "\n\n\n\n\n+" "\n\n\n")
;; >>>   (lre-grape-procify-sub-1 "^$" (concat (make-string 60 ?\ )
;; >>>                                 "Tad______Empty"))
  (lre-grape-procify-sub-1 (concat "\\(\\s-+Tad______Empty\n\\)+"
                                   "\\(TadHV.+Start\\|procedure\\)")
                           "\n\n\n\\2")
  (goto-char (point-min))
  (lre-next-long-line)
  (message (concat "Endring ferdig - sjekk lange linjer med "
                   (substitute-command-keys "'\\[lre-next-long-line]'")))
  (sit-for 1))

(defun lre-raw-sysdul(&optional words)
  "Remove comments, empty lines, multi-lines etc from Sysdul.
With prefix, also replace punctuation & operators with space."
  (interactive "P")
  (goto-char (point-min))
  (save-excursion
    (while (not (eobp))
      (end-of-line)
      (if (bolp)
          (forward-line 1)
        (backward-char 1)
        (if (looking-at ":")
            (delete-char 2)
          (forward-line 1)))))
  (save-excursion (replace-regexp "\t" " "))
  (save-excursion (replace-regexp "  +" " "))
  (save-excursion (delete-matching-lines "^ ?;"))
  (save-excursion (replace-regexp "^ " ""))
  (save-excursion (replace-regexp "\n\n+" "\n"))
  (save-excursion (replace-regexp " *;[^'\"\n]*$" ""))
  (when words
    (save-excursion (replace-regexp "[-!()=+,*/<>]" " "))
    (save-excursion (replace-regexp "  +" " "))
    (save-excursion (replace-regexp "^ " ""))
    ))


;;; ------------------------------------------------------------------
;;; /// SQL ///

(defvar sql-mode-syntax-tbl nil
  "Syntax table for SQL-mode.")

(defconst sql-mode-font-lock-keywords
  (append
   (list
    (cons "^\\s-*go\\s-*$" (lre-font-lock-ref-face)))
   lre-font-lock-specials
   '(
     ("\\b\\(begin\\|commit\\|rollback\\|save\\|prepare\\)\\s-+transaction"
      . font-lock-variable-name-face)
     ("^\\s-*--.*" . font-lock-comment-face)
     ("\\b\\(select\\(\\s-+\\(all\\|distinct\\)\\)?\\|update\\(\\s-+statistics\\)?\\|delete\\(\\s-+from\\)?\\|grant\\|insert\\(\\s-+into\\)?\\)\\b"
      . font-lock-function-name-face)
     ("\\balter\\s-+\\(database\\|table\\)" . font-lock-function-name-face)
     ("\\bcreate\\s-+\\(unique\\s-+\\)?\\(\\(non\\)?clustered\\s-+\\)?index"
      . font-lock-function-name-face)
     ("\\bcreate\\s-+\\(or\\s-+replace\\)?\\(database\\|table\\|default\\|proc\\(edure\\)?\\|function\\|package\\|rule\\|trigger\\|view\\|exception\\)"
      . font-lock-function-name-face)
     ("\\btruncate\\s-+table\\b" . font-lock-function-name-face)
     ("\\bdrop\\s-+\\(database\\|table\\|default\\|index\\|proc\\(edure\\)?\\|rule\\|trigger\\|view\\)"
      . font-lock-function-name-face)
     ("^\\s-*\\(\\(disk\\|dump\\|execute\\|load\\|set\\|use\\)\\s-+\\w+\\)"
      1 font-lock-function-name-face)
     ("^\\s-*\\(if\\|while\\|else\\|elsif\\|begin\\|end\\(\\s-*\\(if\\|loop\\)\\)?\\|break\\|continue\\|return\\|goto\\|raiserror\\|cursor\\|disk\\|dump\\|execute\\|print\\|shutdown\\|waitfor\\|writetext\\|readtext\\|checkpoint\\)\\b"
      1 font-lock-function-name-face)
     ("\\b\\(between\\|and\\|or\\|group\\s-+by\\(\\s-+all\\)?\\|having\\|convert\\|max\\|min\\|count\\|avg\\|sum\\|where\\|from\\|distinct\\)\\b"
      . 1)
     "\\b\\(is\\s-+null\\|order\\s-+by\\|then\\|dup_val_on_index\\|too_many_rows\\|no_data_found\\|holdlock\\|for\\s-+browse\\|union\\(\\s-+all\\)?\\|\\(primary\\|foreign\\)\\s-+key\\|add\\s-+constraint\\|\\(not\\s-+\\)?\\(like\\|exists\\)\\)\\b"
     ("\\b\\(not\\s-+\\)?null\\b" . font-lock-variable-name-face)
     ("^\\s-*declare" . font-lock-keyword-face)
     ("\\bsp_\\w+" . font-lock-function-name-face)
     "\\b\\(varchar\\(2\\)?\\|char\\|datetime\\|smallint\\|tinyint\\|numeric\\|timestamp\\|bit\\|int\\|money\\|number\\|rowid\\)\\b"
     "@@?\\w+")))

(defconst lre-sql-mode-font-lock-keywords-21
  (append lre-font-lock-specials
  (list
   (cons
    "^\\s-*go\\s-*$" 'font-lock-warning-face)
   (cons
    "\\b\\(begin\\|commit\\|prepare\\|rollback\\|save\\)\\s-+trans\\(action\\)?"
    'font-lock-function-name-face)
   (cons
    "^\\s-*--.*" 'font-lock-comment-face)
   (list
    (concat
     "^\\s-*\\("
     (lre-replace-in-string
      (mapconcat 'identity
                 '(
                   "alter database"
                   "alter table"
                   "create clustered index"
                   "create database"
                   "create default"
                   "create function"
                   "create index"
                   "create proc"
                   "create procedure"
                   "create table"
                   "create trigger"
                   "create unique clustered index"
                   "create unique index"
                   "create unique nonclustered index"
                   "create view"
                   "drop database"
                   "drop default"
                   "drop index"
                   "drop function"
                   "drop proc"
                   "drop procedure"
                   "drop table"
                   "drop trigger"
                   "drop view"
                   "set"
                   "use"
                   ) "\\|")
      " " "\\\\s-+")
     "\\)\\b\\s-+\\(\\(\\s_\\|\\sw\\)+\\)")
    '(1 font-lock-keyword-face nil t)
    '(2 font-lock-variable-name-face nil t))
   (concat
    "\\b"
    (lre-replace-in-string
     (lre-regexp-opt
      '(
        "begin"
        "break"
        "case"
        "checkpoint"
        "continue"
        "compute"
        "delete from"
        "delete"
        "deterministic"
        "disable trigger"
        "disk"
        "dump"
        "dump database"
        "dump transaction"
        "else"
        "elseif"
        "enable trigger"
        "end"
        "exec"
        "execute"
        "for browse"
        "goto"
        "grant"
        "if"
        "inout"
        "insert into"
        "insert"
        "left join"
        "load"
        "lock table"
        "output"
        "outer join"
        "print"
        "raiserror"
        "readpast"
        "readtext"
        "return"
        "returns"
        "right join"
        "select all"
        "select distinct"
        "select"
        "set"
        "shutdown"
        "syb_terminate"
        "truncate table"
        "update statistics"
        "update"
        "values"
        "waitfor"
        "when"
        "while"
        "writetext"
        ) t)
     " " "\\\\s-+")
    "\\b")
   (concat
    "\\b"
    (lre-replace-in-string
     (lre-regexp-opt
      '(
        "add constraint"
        "and"
        "between"
        "distinct"
        "exists"
        "for browse"
        "foreign key"
        "from"
        "group by all"
        "group by"
        "having"
        "holdlock"
        "is null"
        "like"
        "not exists"
        "not like"
        "not null"
        "null"
        "or"
        "order by"
        "primary key"
        "union all"
        "union"
        "where"
        ) t)
     " " "\\\\s-+")
    "\\b")
   (list
    "^\\s-*\\(declare\\)\\b\\s-+\\(@?\\(\\s_\\|\\sw\\)+\\)"
    '(1 font-lock-keyword-face nil t)
    '(2 font-lock-variable-name-face nil t))
   (list
    (concat
     "\\b"
     (lre-regexp-opt
      '(
        "avg"
        "convert"
        "count"
        "dateadd"
        "datediff"
        "getdate"
        "isnull"
        "max"
        "min"
        "sum"
        )
      t)
     "\(")
    1
    font-lock-function-name-face)
   '("\\bsp_\\s_+" . font-lock-function-name-face)
   (cons
    (concat "\\b\\(\\("
            (lre-regexp-opt '(
                              "varchar"
                              "nvarchar"
                              "varchar2"
                              "char"
                              "numeric"
                              "numericn"
                              "decimal"
                              "decimaln"
                              "nchar"
                              "binary"
                              "varbinary"
                              ) t)
            "\\(\([0-9]+\\(,[0-9]+\\)?\)\\)?\\)\\|\\("
            (lre-regexp-opt '(
                              "datetime"
                              "smalldatetime"
                              "datetimn"
                              "smallint"
                              "sysname"
                              "tinyint"
                              "timestamp"
                              "bit"
                              "int"
                              "intn"
                              "image"
                              "text"
                              "money"
                              "smallmoney"
                              "moneyn"
                              "rowid"
                              "float"
                              "floatn"
                              "real"
                              "smallmoney"
                              ))
            "\\)\\)\\(\\s-+"
            (lre-regexp-opt '(
                              "identity"
                              "not null"
                              "null"
                              ))
            "\\)*\\b")
    font-lock-type-face)
   ))
  "Emacs 21+ SQL keywords")

(defun lre-sql-feed (p)
  "Insert \§\§, and line-feed (if no prefix)."
  (interactive "P")
  (if p
      (insert "\§\§\\")
    (insert " \§\§\\\n")
    (indent-for-tab-command)))

(defun lre-sql-go (p)
  "Insert linefeed, go, linefeed."
  (interactive "P")
  (let ((opo (point)))
    (insert "\ngo")
    (or p (insert "\n"))
    (and p (goto-char opo))))

(defun lre--sql-keys (map)
  "Redefine keys"
  (when (lre-memb 'keys)
    (define-key map [kp-enter] 'lre-sql-feed)
    (define-key map [backspace] 'backward-delete-char-untabify)
    (define-key map (kbd "C-c ;") 'lre-c-comment)))


(defun lre--sql-mode()
  "Common sql-mode additions"
  (lre-set-local indent-line-function 'indent-relative-maybe)
  (abbrev-mode 0)
  (lre--sql-keys sql-mode-map)
  (lre-colors)
  (lre-turn-on-font-lock)
  (tplsub-mode 1)
  )

(defun lre-comint-go (arg)
  "Insert go and call `comint-send-input'."
  (interactive "P")
  (end-of-line)
  (insert "\ngo")
  (comint-send-input))

(defvar lre-sql-proc-list
  '("Procedures"
    (submenu "Help"
             ("spe_elems" "Table" ("Multiple lines (0/1)" . "0")
                          "Prefix/empty" "Suffix/empty"
                          ("Declarations (0/1)" . "0")  ("Nulls (0/1)" . "0"))
             ("spe_print" "Name")
             "---"
             ("sp_help" "Object")
             ("sp_helptext" "Object")
             ("sp_depends" "Object")
             ("sp_helpindex" "Table")
             ("sp_helpkey" "Object/empty")
             "---"
             ("sp_helpjoins" "Table 1" "Table 2")
             ("sp_helpjava" "Args...")
             ("sp_syntax" "Command/fragment/empty" "Module/empty")
             ("sp_helprotect" "Object" "User/empty")
             )
    (submenu "Catalog"
             ("spe_find" "String" "Procname pattern/empty")
             ("spe_procs" "Prefix/empty")
             ("spe_indexes" "Prefix/empty")
             ("spe_views" "Prefix/empty")
             ("spe_tables" "Prefix/empty")
             ("spe_defaults" "Prefix/empty")
             ("spe_constrs" "Prefix/empty")
             ("spe_fkeys" "Prefix/empty")
             "---"
             ("sp_column_privileges" "Table" "Owner/empty" "Qualifier/empty"
              "Column/empty")
             ("sp_columns" "Table" "Owner/empty" "Qualifier/empty"
              "Column/empty")
             ("sp_databases")
             ("sp_datatype_info" "Type/empty")
             ("sp_fkeys" "PK-table" "PK-Owner/empty" "PK-Qualifier/empty"
              "FK-table" "FK-Owner/empty" "QFK-ualifier/empty")
             ("sp_pkeys" "Table" "Owner/empty" "Qualifier/empty")
             ("sp_server_info" "AttrID/empty")
             ("sp_sproc_columns" "Procname" "Owner/empty" "Qualifier/empty"
              "Column/empty")
             ("sp_statistics" "Table"  "Owner/empty" "Qualifier/empty"
              "Index/empty"
              "Unique Y/blank")
             ("sp_stored_procedures" "Substring/empty" "Owner/empty"
              "Qualifier/empty")
             ("sp_table_privileges" "Table" "Owner/empty" "Qualifier/empty")
             ("sp_tables" "Substring/empty" "Owner/empty" "Qualifier/empty"
              "Type (TABLE/VIEW/SYSTEM TABLE/empty")
             )
    (submenu "Env"
             ("spe_ps_act" "DB/empty")
             ("spe_ps_load" "DB/empty")
             ("spe_ps" "DB/empty")
             ("sp_who" "Login/SPID/empty")
             ("spe_usercount")
             ("spe_threads" "SPID")
             )
    (submenu "Trace"
             ("set showplan on")
             ("set noexec"   ("on/off" . "on"))
             ("set showplan" ("on/off" . "on"))
             ("spe_plan" "SPID")
             ("spe_locks" "SPID/empty")
             ("sp_showplan" "Args...")
             ("sp_lock" "SPID 1/empty" "SPID 2/empty")
             )
    (submenu "TVIST_F (FEL)"
             ("spe_syberr" "Errno")
             ("lre_mld_id" "From no" "To/blank")
             ("lre_dbver")
             ("lre_acc" "Func prefix/blank")
             ("lre_del_trans" "Batch ID")
             ("lre_kv" "#")
             ("lre_kv_finn" "Substring")
             ("spe_jobbstat" "Batch ID")
             ("lre_bmy" "BRUKER_ID" "MYNDIGHET_ID")
             ("spe_transbuff")
     )
    (submenu "DBA"
             ("sp_recompile" "Table")
             ("sp_rename" "From" "To")
             ("sp_helpuser" "User/empty")
             ("sp_spaceused" "Object/empty")
             ("spe_space" "Prefix/empty")
             "---"
             ("sp_configure")
             ("sp_monitor")
             ("sp_transactions")
             ("sp_dboption")
             ("sp_serveroption")
             "---"
             ("sp_helplog")
             ("sp_helpsort")
             ("sp_helpserver" "Server/empty")
             ("sp_helpdb" "DB/empty")
             ("sp_helpdevice" "Device/empty")
             ("sp_helpsegment" "Segment/empty")
             ("sp_helpgroup" "Group/empty")
             ("sp_helplanguage" "Language/empty")
     )
    ("drop proc" "Name")
    ("describe" "Table")
    )
  "List of Sybase procedures with prompts")

(defun lre-sql-mode-menu-base-item (item)
  "One menu item..."
  (if (stringp item) item
    (if (eq (car item) 'submenu)
        (append (list (cadr item))
                (mapcar 'lre-sql-mode-menu-base-item (cddr item)))
      (vector (car item)
              (list 'lre-sql-proc (car item))
              t))))

(defvar lre--sql-flat)

(defun lre-sql-mode-menu-flatten-item (item)
  "One menu item..."
  (if (stringp item) nil
    (if (eq (car item) 'submenu)
        (mapcar 'lre-sql-mode-menu-flatten-item (cddr item))
      (setq lre--sql-flat (cons item lre--sql-flat)))))

(defsubst lre-sql-mode-menu-base ()
  "Convert `lre-sql-proc-list' to a menu definition."
  (mapcar 'lre-sql-mode-menu-base-item lre-sql-proc-list))

(defun lre-sql-mode-menu-flatten ()
  "Convert `lre-sql-proc-list' to a flat list."
  (let ((lre--sql-flat nil))
    (mapc 'lre-sql-mode-menu-flatten-item lre-sql-proc-list)
    lre--sql-flat))

(defvar lre-sql-hist nil
  "History for SQL commands")

(defun lre-sql-proc (&optional cmd)
  "Insert SQL-command"
  (interactive "sCommand name: ")
  (let ((dfl (word-at-point))
        (item (assoc cmd (lre-sql-mode-menu-flatten)))
        farg
        c pr-string def-string)
    (if (null item)
        (error "Unknown command %s" cmd)
      (setq cmd (car item)
            item (cdr item)
            farg t)
      (while item
        (setq c (car item))
        (if (listp c)
            (setq pr-string (car c)
                  def-string (cdr c))
          (setq pr-string c
                def-string dfl))
        (setq cmd (concat cmd
                          (if farg " " ",")
                          (read-string (concat pr-string ": ")
                                       def-string 'lre-sql-hist)))
        (setq item (cdr item)
              farg nil))
      (setq cmd (concat (lre-replace-in-string cmd "[,]+$" "") "\ngo"))
      (goto-char (point-max))
      (insert cmd)
      (comint-send-input))))

(defun lre-sql-mode ()
  "Personal SQL definitions for sql-mode pre-21."
  (if sql-mode-syntax-tbl ()
    (setq sql-mode-syntax-tbl (copy-syntax-table text-mode-syntax-table))
    (modify-syntax-entry ?_ "_" sql-mode-syntax-tbl)
    (modify-syntax-entry ?= "." sql-mode-syntax-tbl)
    (modify-syntax-entry ?/ ". 14" sql-mode-syntax-tbl)
    (modify-syntax-entry ?* ". 23" sql-mode-syntax-tbl)
    (modify-syntax-entry ?\" "\"" sql-mode-syntax-tbl)
    (modify-syntax-entry ?\' "\"" sql-mode-syntax-tbl)
    (modify-syntax-entry ?@ "\'" sql-mode-syntax-tbl))
  (set-syntax-table sql-mode-syntax-tbl)
  (lre-set-local paragraph-start "\f\\|$\\|go$")
  (lre-set-local paragraph-separate paragraph-start)
  (lre--sql-mode)
  (define-key sql-mode-map [M-return]  'lre-sql-go)
  (when (lre-memb 'flock)
    (make-local-variable 'font-lock-keywords)
    (setq font-lock-keywords sql-mode-font-lock-keywords)
    (put 'sql-mode 'font-lock-keywords-case-fold-search t)))

(defun lre-sql-mode-21 ()
  "Personal SQL definitions."
  (setq paragraph-separate "\\([\f]\\|go\\)*$"
        paragraph-start "[\n\f]")
  (if (fboundp 'sql-add-product-keywords)
      (sql-add-product-keywords 'sybase lre-sql-mode-font-lock-keywords-21)
    (setq font-lock-defaults '(lre-sql-mode-font-lock-keywords-21
                             nil t (("_" . "w") ("." . "w")))))
  (setq imenu-generic-expression
        (list '("Create"
                "^\\s-*create\\s-+\\(\\sw\\(\\sw\\|\\s_\\)+\\s-+\\sw\\(\\sw\\|\\s_\\)+\\)" 1)
              '(nil "^--\\s-+\\[\\[\\s-+\\(Index:\\)?\\([^]\n]+\\)\\(\\]\\]\\)?$"
                    2))
        imenu-case-fold-search t)
  (lre--imenu-add)
  (lre--sql-mode))

(defun lre-sql-mode-21i ()
  "Personal SQL definitions for interactive sql-mode v21."
  (lre-sql-mode-21)
  (easy-menu-change '("SQL") "Utilities" (lre-sql-mode-menu-base))
  (lre-set-local sql-prompt-regexp "^[0-9]+> ")
  (lre-set-local comint-prompt-regexp "^[0-9]+> ")
  (lre-set-local comint-input-ignoredups t)
  (lre-set-local sql-prompt-length 3)
  (lre-set-local comint-buffer-maximum-size 4096)
  (lre-set-local comint-input-ring-size 128)
  (lre-set-local comint-input-history-ignore "^go\\s-*$")
  (lre--sql-keys sql-interactive-mode-map)
  (define-key sql-interactive-mode-map [M-return]  'lre-comint-go))

(defun lre-sql ()
  "Start interactive sql"
  (interactive)
  (if (lre-memb 'tvist)
      (if (lre-memb 'e21+)
          (progn
            (if (lre-memb-all 'tvist 'win32) (setenv "LANG" ""))
            (call-interactively 'sql-sybase))
        (call-interactively 'sql-get-going))
    (error "No TVIST")))

;;; ------------------------------------------------------------------
;;; /// Makefiles ///

(defun lre-makefile-mode ()
"Personal `makefile-mode' definitions."
  (lre-colors)
  (lre--imenu-add)
  (lre-brace-mode 1)
  (if (lre-memb 'keys)
    (define-key makefile-mode-map [kp-enter] 'lre-newline-and-indent))
  (tplsub-mode 1)
  (lre-add-x-hilit 'makefile-mode))


;;; ----------------------------------------------------------------------
;;; /// Shell scripts ///

(defun lre-shell-mode ()
  "Additions to various shell-modes"
  (lre-safe-require 'shell)
  (lre-colors)
  (lre--imenu-add)
  (lre-turn-on-font-lock)
  (when (lre-memb 'keys)
    (define-key (if (boundp 'shell-mode-map) shell-mode-map
                  sh-mode-map)
              (kbd "C-d") 'lre-comint-delchar-or-eof-or-kill-buffer)
    (local-set-key [backspace] 'backward-delete-char-untabify)
    (local-set-key [return] 'newline-and-indent))
  (and (lre-memb 'imenu)
       (featurep 'which-func)
       (which-func-mode t))
  (tplsub-mode 1)
  (lre-brace-mode 1)
  (lre-set-local paragraph-start "\\s-*[ \t]*#?\\s-*$\\|^\f")
  (lre-set-local dabbrev-abbrev-skip-leading-regexp "\\$")
  (lre-set-local dabbrev-case-fold-search           nil)
  (lre-set-local dabbrev-case-replace               nil))


(defun lre-ksh-mode ()
"Additions to `ksh-mode' setup."
  (setq ksh-case-indent       nil
        ksh-case-item-offset  3
        ksh-indent            lre-std-indent
        ksh-group-offset      (- ksh-indent)
        ksh-multiline-offset  4)
  (lre-set-local font-lock-defaults (list ksh-font-lock-keywords))
  (when (lre-memb 'hilit)
    (hilit-set-mode-patterns
     'ksh-mode
     '(
       ("['][^\']*[']" nil string)
       (hilit-string-find 92 string)
       ("\\(^\\|[^\\$(]\\)#.*$" nil comment)
       ("\\<function\\s-+\\([^(; \t]+\\)" nil defun)
       ("\\<\\(break\\|continue\\|exit\\|return\\)\\>" nil crossref)
       ("\\(^\\s-*[A-Za-z_][A-Za-z_0-9]*\\s-*()\\)" nil defun)
       )
     nil 'case-sensitive)
    (if (lre-add-x-hilit 'ksh-mode)
        (progn
          (hilit-add-pattern "typeset .*" "[^\\]$" 'type 'ksh-mode)
          (hilit-add-pattern
           (concat
            "\\<\\("
            (eval-and-compile
              (if (and (boundp 'ksh-keywords)
                       (listp 'ksh-keywords))
                  (lre-regexp-opt ksh-keywords)
                ""))
              ;;            (mapconcat 'identity ksh-keywords "\\>\\|\\<")
              "\\)\\>") "" 'keyword 'ksh-mode))))
  (lre-shell-mode))

(defun lre-csh-mode ()
  "Additions to `csh-mode' setup."
  (setq csh-indent-level lre-std-indent)
  (lre-colors)
  (lre-brace-mode 1)
  (lre-add-x-hilit 'csh-mode))

(defun lre-comint-delchar-or-eof-or-kill-buffer (arg)
  ;; http://whattheemacsd.com/
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

;;; --------------------------------------------------------------------------
;;; /// Compiling ///

(defvar lre-comp-alist
  '(
    (sgml-mode . "sgmlfmt -v")
    (indented-text-mode . "ispell")
    (text-mode . "ispell")
    (csh-mode . "csh -fVX")
;    (ksh-mode . "ksh -fvx")
    (fe-mode . "fesjekk -s")
    (perl-mode . "perl -w")
    (emacs-lisp-mode . lre-comp-lisp)
    (java-mode . jde-compile)
    (cf-mode   . lre-comp-cf)
    )
  "List of compile commands for different modes.
Contains cons cells \(mode-name . COMPILE\).  If COMPILE is a string,
it is executed via `lre-comp-simple'.
Otherwise, it is executed via `funcall'.
Modes not listed will be handled by make.")

(defvar lre-comp-ext-alist
  '(
    (ksh-mode . ".shx")
    (sql-mode . ".sql")
    (vsq-mode . ".sql")
    )
  "List of \"object file\" suffixes different than .o for different modes.
Contains cons cells \(mode-name . suffix \).")

(defun lre-comp-make (&optional psuff)
  (lre--compiler
   (read-from-minibuffer "Command: "
                         (cons
                          (concat (if (lre-memb 'win32)
                                      "nmake -f ?.mak "
                                    "make -k ")
                                  (nth 1 (lre-file-name-split
                                          (buffer-file-name)))
                                  (or psuff
                                      (cdr (assq major-mode
                                                 lre-comp-ext-alist))
                                      (if (lre-memb 'win32) ".obj" ".o")))
                          (if (lre-memb 'win32) 10 8))
                         nil nil
                         'compile-history)
     "no more errors"))

(defun lre-comp-simple (cmd)
  (lre--compiler
   (read-from-minibuffer "Command: "
                         (concat cmd " " (buffer-file-name)))
   "no more errors"))

(defun lre-comp-lisp ()
  (let ((byte-compile-generate-call-tree t))
    (byte-compile-file
     (expand-file-name
      (read-file-name "File name: "
                      "" (buffer-file-name) t (buffer-file-name))))))

(defun lre-comp-cf ()
  (lre--compiler
   (read-from-minibuffer "Command: "
                         (concat "greip autoSvar igjen "
                                 (lre-replace-in-string
                                  (file-name-nondirectory
                                   (file-name-sans-extension
                                    (buffer-file-name)))
                                  "_\\(def\\|proc\\)"))
                         )
   "no more errors"))

(defun lre-compile ()
  "Perform mode-dependent compile."
  (interactive)
  (if (not (buffer-file-name))
      (error "File has not been saved"))
  ;; Make sure compile-internal is available
  (require 'compile)
  ;; Offer to save changed files
  (save-some-buffers)
  ;; Try to find current mode in alist, otherwise, use default
  (let* ((comp-elt (or (cdr (assq major-mode lre-comp-alist))
                       'lre-comp-make)))
    (cond ((stringp comp-elt) (lre-comp-simple comp-elt))
          ((null    comp-elt) (error "Unknown command!"))
          (t                  (funcall comp-elt)))))


;;; --------------------------------------------------------------------------
;;; /// CVS ///

(defun lre-make-cvs-script (dir)
  "Lager script fra cvs update"
  (interactive "DKatalog: ")
  (let* (dodel
         (fnavn "adder.bat")
         (fulltnavn (concat dir "\\" fnavn))
         (komm "")
         (addfiles "")
         (spurt -1)
         (cvscmd "cvs -q -n update -d")
         pt
         file)
    (cd dir)
    (switch-to-buffer (get-buffer-create fnavn))
    (message "Kjører CVS...")
    (shell-command cvscmd (current-buffer))
    (message nil)
    (sort-lines nil (point-min) (point-max))
    (goto-char (point-min))
    (insert "@echo off\n")
    (while (not (eobp))
      (setq dodel t)
      (cond ((looking-at "[UP] ")
             (insert "rem    cvs remove "))
            ((looking-at "[ARM] ")
             (insert "rem    cvs commit "))
            ((looking-at "[C] ")
             (insert "ECHO KONFLIKT! ")
             (setq dodel nil))
            ((looking-at "[?] ")
             (insert "rem     ")
             (forward-char 2)
             (setq pt (point))
             (end-of-line)
             (setq file (buffer-substring pt (point)))
             (unless (string= file fnavn)
                 (setq addfiles (concat addfiles " " file))))
            (t
             (setq dodel nil)))
      (when dodel
        (if (< spurt 0)
            (if (y-or-n-p "Felles endringskommentar? ")
                (setq spurt 1
                      komm (read-string "Kommentar: "))
              (setq spurt 0)))
        (when (and (not (eolp))
                   (> spurt 0))
          (insert "-m'" komm "' ")
          (delete-char 2)))
      (forward-line 1))
    (insert "\n")
    (unless (string= addfiles "")
      (insert "\ncvs add "
              (if (> spurt 0) (concat "-m'" komm "' ") "")
              addfiles
              "\n"))
    (insert "\n" cvscmd "\n")
    (write-file fulltnavn)
    (if (y-or-n-p "Kjøre med det samme? ")
        (shell-command fulltnavn))))


;;; --------------------------------------------------------------------------
;;; /// Scala ///

(defun lre-scala-mode ()
  "Load scala mode"
  (unless (featurep 'scala-mode)
      (require 'scala-mode-auto)
      (setq scala-interpreter (concat (lre-fixed :scala) "/bin/scala.bat")))
  (scala-mode))


;;; --------------------------------------------------------------------------
;;; /// Clojure ///

(defun lre-run-clojure()
  "Run interactive Clojure"
  (interactive)
  (let ((inferior-lisp-program
         (concat "java -cp " (lre-fixed :clojure) " clojure.main")))
    (inferior-lisp inferior-lisp-program)))



(provide 'lre-prog)

;;; lre-prog.el ends here
