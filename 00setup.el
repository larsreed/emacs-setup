(setq inhibit-startup-echo-area-message (user-login-name))

;;; synes utenfra:
;; lre-lisp lre-cfg lre-ini lre-00

(setq LRE-this-cfg     'lreibm)
;; (setq LRE-this-cfg     'tadnt)
;; (setq LRE-this-cfg     'tadntp4)
;; (setq LRE-this-cfg     'gnunix)

(setq ;; LRE-jde-pkg "pkg/jde-2.0.7"
      ;; LRE-jde-pkg "pkg/jde-2.2.9b4/lisp"
      ;; LRE-jde-pkg "pkg/jde-2.2.9b12/lisp"
      ;; LRE-jde-pkg "pkg/jde-2.3.2/lisp"
      LRE-jde-pkg nil
      ;; LRE-jde-pkg "pkg/jde-2.3.5/lisp"
      LRE-cc-pkg  nil
      ;; LRE-nxml-pkg "pkg/nxml-mode-20040726"
      LRE-nxml-pkg (if (< emacs-major-version 23)
                       "pkg/nxml-mode-20041004"
                     nil)
      LRE-semant-pkg nil
      ;; LRE-semant-pkg "pkg/semantic-1.4b13/lisp"
      ;; LRE-semant-pkg "pkg/cedet-1.0beta3b/semantic"
      LRE-eieio-pkg nil
      ;; LRE-eieio-pkg "pkg/eieio-0.17b3/lisp"
      ;; LRE-eieio-pkg "pkg/cedet-1.0beta3b/eieio"
      LRE-elib-pkg "pkg/elib-1.0/lisp"
      LRE-xsltp-pkg "pkg/xslt-process-2.1/lisp"
      LRE-xslide-pkg "pkg/xslide-0.2"
      LRE-psgml-pkg (if (eq LRE-this-cfg 'lreibm) "pkg/psgml-1.2.0/lisp" nil)
      LRE-sb-pkg  (if (= emacs-major-version 21)
                      "pkg/speedbar-0.14b4/lisp"
                      ;; "pkg/cedet-1.0beta3b/speedbar"
                    (if (>= emacs-major-version 22)
                        nil
                      "pkg/oldsb"))
      LRE-cogre-pkg nil
      ;; LRE-cogre-pkg "pkg/cedet-1.0beta3b/cogre"
      LRE-ede-pkg nil
      ;; LRE-ede-pkg "pkg/cedet-1.0beta3b/ede"
      LRE-cedet-pkg nil
      ;; LRE-cedet-pkg "pkg/cedet-1.0beta3b/common"
      LRE-cedet-ico-pkg nil
      ;; LRE-cedet-ico-pkg "pkg/cedet-1.0beta3b/common/icons"
      LRE-cedet-x-pkg nil
      ;; LRE-cedet-x-pkg "pkg/cedet-1.0beta3b/contrib"
      ;; LRE-cedet-x-pkg "pkg/cedet-1.0beta3b/contrib"
      LRE-autocomplete-pkg nil
      ;; LRE-autocomplete-pkg "pkg/auto-complete-1.3.1"
      LRE-multiple-cursors-pkg "pkg/multiple-cursors"
      LRE-expand-region-pkg "pkg/expand-region"
      )
(setq LRE-pkg-list '(LRE-elib-pkg   LRE-jde-pkg   LRE-cc-pkg     LRE-psgml-pkg
                     LRE-sb-pkg     LRE-eieio-pkg LRE-semant-pkg LRE-xsltp-pkg
                     LRE-cedet-pkg  LRE-cedet-ico-pkg            LRE-cedet-x-pkg
                     LRE-ede-pkg    LRE-cogre-pkg LRE-xslide-pkg LRE-nxml-pkg
                     LRE-autocomplete-pkg         LRE-multiple-cursors-pkg
                     LRE-expand-region-pkg))

(setq lre-fixed-paths
      '((:printer-path . "//msprint02/h9505eh") ; 1
        (:printer1 . "p9505jh") ; 2
        (:printer2 . "h9505eh") ; 3
        (:text-print . "d:/000out/E-txt.prn")   ; 4
        (:ps-print . "d:/000out/E-ps.prn")    ; 5
        (:csdiff . "c:/progra~1/csdiff/csdiff.exe") ; 6
        (:pmd . "c:\\progra~2\\java\\pmd-1.2.1") ; 7
        (:winzip . "c:/program files/winzip") ; 8
        (:xmlspy . "C:/Program Files/Altova/XMLSPY/XMLspy.exe") ; 9
        (:wincvs . "c:/program files/GNU/WinCvs 1.1") ; 10
        (:def-xsl . "d:/usr/lib/xml/default.xsl") ; 11
        (:lre-start . "d:/usr/lre/desktop/toolbars/lre-start.html") ; 12
        (:site-lisp . "p:/GNUemacs/Site-lisp") ; 13
        (:recentf1 . "c:/_recentf") ; 14
        (:recentf2 . "~/.recentf") ; 15
        (:pr-exe . "pr.exe") ; 16
        (:temp . "d:\\temp") ; 17
        (:shell-ex . "shelex.exe") ; 18
        (:scala . "C:/tools/scala-2.10") ; 19
        (:clojure . "C:/tools/clojure-1.3.0/clojure-1.3.0.jar") ; 20
        ))

(setq lre-office-path (cond ((eq LRE-this-cfg 'lreibm)
                             "C:/Program Files/Microsoft Office/Office15")
                            ((eq LRE-this-cfg 'tadnt)
                             "C:/Programfiler/Microsoft Office/Office")
                            ((eq LRE-this-cfg 'tadntp4)
                             "C:/Programfiler/Microsoft Office/Office11")
                            (t nil)))

(setq lre-ie-path (cond ((eq LRE-this-cfg 'lreibm)
                         "C:/Program Files/Internet Explorer")
                        ((eq LRE-this-cfg 'tadnt)
                         "C:/Programfiler/Plus!/Microsoft Internet")
                        ((eq LRE-this-cfg 'tadntp4)
                         "C:/Programfiler/Internet Explorer")
                        (t nil)))

(if LRE-jde-pkg
    (cond ((eq LRE-this-cfg 'lreibm)
           (custom-set-variables
            '(jde-checkstyle-classpath
              "C:\\Program Files\\Java\\checkstyle-3.4\\checkstyle-all-3.4.jar")
            '(jde-checkstyle-option-config-file
              "d:\\usr\\lib\\java\\checkstyle_checks.xml")
            '(jde-jdk  '("JDK1.4.2"))
            '(jde-jdk-registry '(("JDK1.4.2" .
                                  "C:/Program Files/Java/j2sdk1.4.2_04")
                                 ("JDK1.3" . "C:/Program Files/Java/jdk131_03")
                                 ("JDK1.5" .
                                  "C:/Program Files/Java/j2sdk1.5.0")))
            ))
          ((eq LRE-this-cfg  'tadnt)
           (setq
            jde-jdk          "JDK1.1.8"
            jde-jdk-registry '(("JDK1.1.8" .
                                "C:/Programfiler/SYBASEOC/jdk1.1.8"))))
          ((eq LRE-this-cfg  'tadntp4)
           (setq
            jde-jdk          "JDK1.4.2"
            jde-jdk-registry '(("JDK1.4.2" .
                                "C:\j2sdk1.4.2_08"))))
          ((eq LRE-this-cfg 'gnunix)
           (setq
            jde-jdk          "JDK1.1.8"
            jde-jdk-registry '(("JDK1.1.8" . "/usr/jdk_base"))))))

(setq lre-scala-pkg (if (file-exists-p (cdr (assoc :scala lre-fixed-paths)))
                        (concat (cdr (assoc :scala lre-fixed-paths))
                                "/misc/scala-tool-support/emacs")))

(setq ccas-setup       nil
      LRE-load         load-path
      lre-lisp         nil
      lre-slisp        nil
      LRE-run-setup    t
      lre-00           nil)

(defun LRE-build-dir (lst &optional sub new-base)
  (mapcar (function (lambda (subsub)
                      (concat (or new-base LRE-base) (if sub (concat "/" sub))
                              (if (> (length subsub) 0) "/") subsub)))
          lst))

(defsubst LRE-std-ini ()
  (concat (car lre-lisp) "/lresetup.el"))

(defsubst LRE-std-00 ()
  (concat (car (cdr lre-lisp)) "/00setup.el"))


(cond
 ((string-match "^2[0-9][.]" emacs-version)
  (setq LRE-ver (if (string= window-system "w32") 'win2k
                   'gnunix)))
 ((>= emacs-major-version 22)
  ;; Fallback - tillater kjøring med advarsel
  (message "Unknown EMACS version!!!!")
  (beep) (beep)
  (setq LRE-ver (if (string= window-system "w32") 'win2k
                   'gnunix)))
 ((string-match "^21.1.*Canyonlands" emacs-version)
  (setq LRE-ver  'xemacs21))
 ((string= emacs-version "20.3.1 p} unix, jf Ronny...")
  (setq LRE-ver  'gnunix-x))
 ((string= emacs-version "19.19.0")
  (setq LRE-ver   'dos))
 ((string= emacs-version "19.28.1")
  (setq LRE-ver   'unix))
 ((string= emacs-version "19.31.1")
  (setq LRE-ver 'win95-31))
 ((string= emacs-version "19.33.1")
  (setq LRE-ver  'win95-33))
 ((string= emacs-version "19.34.1")
  (setq LRE-ver  'win95-341))
 ((string= emacs-version "19.34.6")
  (setq LRE-ver  'win95))
 ((string= emacs-version "19.13 XEmacs Lucid")
  (setq LRE-ver  'xemacs))
 (t
  (message "Unknown EMACS version!!!!")
  (beep) (beep)
  (setq LRE-run-setup nil)))

(cond
 ;;  STANDARD VERSJON
 ((or (eq LRE-ver 'win2k)
      (eq LRE-ver 'gnunix))
  (let ((v-string (concat "e" (substring emacs-version 0 4)))
        (g-string (format "e%d" emacs-major-version))
        (uxver (eq LRE-ver 'gnunix))
        )
    (setq LRE-base (if uxver "/emacs"
                     (if (or (eq LRE-this-cfg 'tadnt)
                             (eq LRE-this-cfg 'tadntp4))
                         "P:/GNUemacs"
                       (getenv "EMACS_DIR")))
          LRE-load (if uxver load-path
                     (LRE-build-dir
                      '("" "calc" "calendar" "cedet" "emacs-lisp" "emulation"
                        "erc" "eshell" "gnus" "international" "language" "mail"
                        "mh-e" "net" "nxml" "org" "play" "progmodes" "term"
                        "textmodes" "toolbar" "url" "vc")
                       "lisp" (if (or (eq LRE-this-cfg 'tadnt)
                                     (eq LRE-this-cfg 'tadntp4))
                                  (getenv "EMACS_DIR") nil)))
          lre-slisp (concat LRE-base "/site-lisp")
          lre-lisp (LRE-build-dir (list "lre" "" v-string g-string "override"
                                        (concat "override/" v-string)
                                        (concat "override/" g-string))
                                  "site-lisp")
          lre-ini  (LRE-std-ini)
          lre-00   (LRE-std-00)
          lre-cfg  '(stdsetup))
    (if (or (string= "lare9505" (user-login-name))
            (string= "larsr" (user-login-name))
            (string= "lare" (user-login-name))
            (string= "lars.reed" (user-login-name))
            (string= "lreed" (user-login-name))
            (string= "lre" (user-login-name)))
        (setq lre-cfg (cons 'personal lre-cfg)))
    (if (>= emacs-major-version 20) (setq lre-cfg (cons 'e20+ lre-cfg)))
    (if (>= emacs-major-version 21) (setq lre-cfg (cons 'e21+ lre-cfg)))
    (if (>= emacs-major-version 22) (setq lre-cfg (cons 'e22+ lre-cfg)))
    (if (>= emacs-major-version 23) (setq lre-cfg (cons 'e23+ lre-cfg)))
    (if (>= emacs-major-version 24) (setq lre-cfg (cons 'e24+ lre-cfg)))
    (if (not (eq LRE-this-cfg 'lreibm)) (setq lre-cfg (cons 'tvist lre-cfg)))
    (setq lre-cfg (cons (if uxver 'unix 'win32) lre-cfg))
  ))
 ((eq LRE-ver  'xemacs21)
  (setq lre-ini  (concat (substitute-in-file-name "$SPE_HOME/site-lisp")
                         "/tadxsetup.el")
        lre-cfg  '(tvist xem21+ xemacs))
  (if (string= (user-real-login-name) "lare")
      (setq lre-cfg (cons 'personal lre-cfg))))
 ((eq LRE-ver 'gnunix-x)
  (setq LRE-ver  'gnunix
        LRE-base "/emacs"
        LRE-load (LRE-build-dir
                  '("" "textmodes" "progmodes" "play" "mail"
                    "language" "international" "gnus" "emulation"
                    "emacs-lisp" "calendar")
                  "lisp")
        lre-lisp (LRE-build-dir '("lre" "" "e20" "override" "override/e20")
                                "site-lisp")
        lre-ini  (LRE-std-ini)
        lre-cfg  '(unix gnunix e20+)))
 ((eq LRE-ver 'dos)
  (setq lre-lisp  nil
        lre-cfg   '(dos personal)))
 ((eq LRE-ver 'unix)
  (setq LRE-load  '("~/bin/emacs/gnus/lisp"
                    "/progs/local/lib/emacs/19.28/lisp"
                    "/progs/local/lib/emacs/site-lisp/term")
        lre-lisp  '("~/bin/emacs")
        lre-ini   (LRE-std-ini)
        lre-cfg   '(unix personal ccas)))
 ((eq LRE-ver 'win95-31)
  (setq LRE-ver 'win95-old
        LRE-base "d:/emacs-19.31"
        LRE-load (LRE-build-dir '("lisp"))
        lre-lisp (LRE-build-dir '("site-lisp")))
  lre-ini  (LRE-std-ini)
  lre-cfg '(win32 e1930+ personal))
 ((eq LRE-ver 'win95-33)
  (setq LRE-ver  'win95
        LRE-base "d:/emacs-19.33"
        LRE-load (LRE-build-dir '("lisp"))
        lre-lisp (LRE-build-dir '("site-lisp"))
        lre-ini  (LRE-std-ini)
        lre-cfg  '(win32 e1930+ personal)))
 ((eq LRE-ver 'win95-341)
  (setq LRE-ver  'win95
        LRE-base "d:/emacs-19.34"
        LRE-load (LRE-build-dir '("lisp"))
        lre-lisp (LRE-build-dir '("" "e19") "site-lisp")
        lre-ini  (LRE-std-ini)
        lre-cfg  '(win32 e1930+ personal)))
 ((eq LRE-ver 'win95)
  (setq LRE-base "d:/emacs.old"
        LRE-load (LRE-build-dir '("lisp"))
        lre-lisp (LRE-build-dir '("" "e19" "override") "site-lisp")
        lre-ini  (LRE-std-ini)
        lre-cfg  '(win32 e1930+ personal)))
 ((eq LRE-ver 'xemacs)
  (setq lre-lisp (list "~/emacs"
                       (substitute-in-file-name "$SPE_HOME/site-lisp"))
        lre-ini  (LRE-std-ini)
        lre-cfg  '(tvist xemacs)))
 (t
  (message "Unknown EMACS version!!!!")
  (beep) (beep)
  (setq LRE-run-setup nil)))

(if lre-lisp
    (setq load-path
          (append lre-lisp
                  (LRE-build-dir
                   (mapcar 'symbol-value LRE-pkg-list) "site-lisp")
                  LRE-load
                  '("."))))
(if lre-scala-pkg (add-to-list 'load-path lre-scala-pkg))

(setq lre-cfg (cons LRE-this-cfg lre-cfg))

(if LRE-semant-pkg (setq lre-cfg (cons 'semantic lre-cfg)))
(if LRE-sb-pkg (setq lre-cfg (cons 'speedbar lre-cfg)))
(if LRE-eieio-pkg (setq lre-cfg (cons 'eieio lre-cfg)))
(if LRE-psgml-pkg (setq lre-cfg (cons 'psgml lre-cfg)))
(if LRE-jde-pkg (setq lre-cfg (cons 'jde lre-cfg)))
(if LRE-xsltp-pkg (setq lre-cfg (cons 'xsltp lre-cfg)))
(if LRE-xslide-pkg (setq lre-cfg (cons 'xslide lre-cfg)))
(if (or LRE-nxml-pkg
        (>= emacs-major-version 23))
    (setq lre-cfg (cons 'nxml lre-cfg)))
(setq lre-gnuserv-server (< emacs-major-version 22))

;; Here goes!
(if LRE-run-setup (load (file-name-sans-extension lre-ini)))
