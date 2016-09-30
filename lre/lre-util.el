;;; lre-util.el --- Lars Reed - Emacs init file
;; Some less frequently used utilities

;; Copyright (C) 2002 Lars Reed
;;   See lresetup.el
;; Author:              Lars Reed <Lars@kalars.net>
;; Version:             2.0
; Hi-lock: (("^;;; lre.*\\.el[^\n]*" (0 (quote hi-black-hb) t)))
; Hi-lock: (("Lars.*R[e]ed" (0 (quote hi-blue) t)))
; Hi-lock: (("^;;; [^\n]+" (0 (quote hi-green) t)))


;;; Code:

(require 'cl)

(defun sub-lre-kill-save-all (buf)
  (let ((name (buffer-name buf)))
    (cond ((lre-junk-buf-p name)     (kill-buffer buf))
          ((lre-internal-buf-p name) nil)
          ((y-or-n-p (concat "Kill buffer " name "? "))
           (set-buffer buf)
           (lre-kill-save-buf)))))

(defun lre-kill-save-all ()
  "Ask whether to run `lre-kill-save-buf' on all buffers.
Junk buffers (see `lre-junk-buf-list') are always killed,
internal buffers (see `lre-internal-buf-p') are never killed."
  (interactive)
  (mapcar 'sub-lre-kill-save-all (buffer-list)))

(defvar lre-kill-some-this nil)
(defvar lre-kill-some-list nil)

(defun lre-kill-some-sub (win)
  (let ((b (window-buffer win)))
    (if (or (not b)
            (and (not (eq b lre-kill-some-this))
                 (lre-special-buf-p (buffer-name b))))
        (setq lre-kill-some-list (append (list win) lre-kill-some-list)))))

(defun lre-kill-some-windows (&optional buffer)
  "Close all non-file windows except BUFFER \(default: this\)."
  (interactive)
  (let ((lre-kill-some-list nil)
        (lre-kill-some-this (if buffer buffer
                                (current-buffer))))
    (walk-windows 'lre-kill-some-sub nil nil)
    (mapcar 'delete-window lre-kill-some-list)))

(defun lre-goto-percent (pct)
  "Gå til angitt posisjon i bufferet."
  (interactive "NPercent location: ")
  (let
      ((pct-loc (cond ((< pct 0) 0)
                      ((> pct 100) 100) ( t pct))))
    (goto-char (/ (* pct-loc (point-max)) 100))))

(defun lre-kill-others (&optional buffer)
  "Kill all buffers except BUFFER \(default: this\)."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (mapcar '(lambda (b)
               (if (not (or (eq b buf)
                            (lre-special-buf-p (buffer-name b))))
                   (kill-buffer b)))
            (buffer-list))))

(defun lre-occ-at-point (evt)
  (interactive "e")
  (save-mark-and-excursion
    (mouse-set-point evt)
    (let ((name (word-at-point)))
      (setq name (read-string "Search for: " name))
      (occur name))))

(defun lre-resize-window (&optional arg)
  "Resize window interactively.
Hirose Yuuji and Bob Wiener"
  (interactive "p")
  (if (one-window-p) (error "(resize-window): Cannot resize sole window"))
  (or arg (setq arg 1))
  (let (c)
    (catch 'done
      (while t
        (message
         "h=heighten, s=shrink, w=widen, n=narrow (by %d);  1-9=unit, q=quit"
         arg)
        (setq c (read-char))
        (condition-case ()
            (cond
             ((= c ?h) (enlarge-window arg))
             ((= c ?s) (shrink-window arg))
             ((= c ?w) (enlarge-window-horizontally arg))
             ((= c ?n) (shrink-window-horizontally arg))
             ((= c ?\^G) (keyboard-quit))
             ((= c ?q) (throw 'done t))
             ((and (> c ?0) (<= c ?9)) (setq arg (- c ?0)))
             (t (beep)))
          (error (beep)))))
    (message "Done.")))

(defun lre-touch-file (fn)
  "Touch a file by the given name."
  (interactive "F")
  (shell-command (concat "touch " fn) nil))

(defun lre-how-many (regexp)
  "Return number of matches for REGEXP following point
- based on `how-many'.

If REGEXP contains upper case characters (excluding those preceded by
`\\'),
the matching is case-sensitive."
  (let ((count 0) opoint
        (case-fold-search  (and case-fold-search
                                (isearch-no-upper-case-p regexp t))))
    (save-mark-and-excursion
      (while (and (not (eobp))
                  (progn (setq opoint (point))
                         (re-search-forward regexp nil t)))
        (if (= opoint (point))
            (forward-char 1)
          (setq count (1+ count)))))
    count))


(defun lre-char-class (pt mk)
  "Convert `c' to `[cC]'."
  (interactive "*r")
  (let (lc
        uc
        (last-pos (1+ mk)))
    (while (< pt last-pos)
      (goto-char (setq last-pos (1- last-pos)))
      (setq lc (downcase (buffer-substring (point) (+ (point) 1)))
            uc (upcase lc))
      (delete-char 1)
      (if (string= uc lc)
          (insert (regexp-quote lc))
        (insert "[" lc uc "]")))))


(defun lre-split-string  (string regexp)
  "Split a string into words, using REGEXP as a delimiter.
By Peter Breton."
  (let ((result)
        (index 0)
        (match-end 0)
        )
    (while (string-match regexp string index)
      (setq match-end (match-end 0))
      (and (not (equal  match-end 1))
           (setq result (append (list (substring string index (- match-end 1)))
                                result)))
      (setq index match-end ))
    (setq result (append (list (substring string index)) result))
    (reverse result)))

(defun lre-kill-back-line (&optional count)
  "Kill to beginning of line."
  (interactive "*P")
  (kill-line
   (cond ((null count) 0)
         ((eq count '-) 1)
         ((numberp count) (- count))
         (t (- (car count))))))

(defun lre-kill-this-line ()
  "Kill the current line."
  (interactive "*")
  (let ((curr-pt (point))
        beg-pt)
    (next-line 1)
    (save-mark-and-excursion
      (goto-char curr-pt)
      (beginning-of-line)
      (setq beg-pt (point))
      (beginning-of-line 2)
      (kill-region beg-pt (point)))))

(defun lre-copy-line-other-window (p)
  "Copy current line(s) to current point in other-window."
  (interactive "p")
  (let (p1 s)
    (beginning-of-line)
    (setq p1 (point))
    (forward-line p)
    (setq s (buffer-substring p1 (point)))
    (other-window 1)
    (insert s)
    (other-window -1)))

(defun lre-inc-num-str (s incr)
  "Return a copy of S in which the first number has been incremented by INCR."
  (if (string-match "[0-9]+" s)
      (let ((num (string-to-number (match-string 0 s))))
        (setq num (+ num incr))
        (replace-match (number-to-string num) nil t s))))

;(defun lre-renumber-lines (min-p max-p regex str incr)
;  "Renumber a range of lines from BEGIN to END"
;  (interactive "*r\nsRegexp: \nsInitial string: \np")
;  (if (not incr)
;      (setq incr 1))
;  (save-mark-and-excursion
;    (let ((lines (count-lines begin end))
;         (delta (if (not incr) 1 incr))
;         (s-reg (if regex (if (> (length regex) 0) regex "^.*$") "^.*$")))
;      (goto-char begin)
;      (while (> lines 0)
;        (if (re-search-forward s-reg)
;           (save-mark-and-excursion (progn (end-of-line) (point))) t)
;       (progn
;              (goto-char (match-beginning 1))
;              (delete-region (match-end 1) (match-beginning 1))
;              (insert string)
;              (setq string (increment-string string amount))))
;        (beginning-of-line)
;        (forward-line)
;        (setq lines (1- lines))))))

(defun lre-number-lines (start-at &optional rbegin rend)
  "Insert line numbers at the beginning of each line in the region,
or the whole file, if the region is not active."
  (interactive "*p")
  (let ((cnt (or start-at 1))
        p1
        p2)
    (if mark-active
        (if (< (point) (mark))
            (setq p1 (point-marker)
                  p2 (mark-marker))
          (setq p2   (point-marker)
                p1   (mark-marker)))
      (setq p1       (point-min-marker)
            p2       (point-max-marker)))
    (save-mark-and-excursion
      (goto-char p1)
      (while (< (point) p2)
        (insert (format "%5d\t" cnt))
        (setq cnt (1+ cnt))
        (beginning-of-line 2)))))

(defun lre-next-long-line()
  "Goto next long line (see `lre-long-line')..."
  (interactive)
  (let ((found nil)
        (cont t)
        (adv 0)
        (curr (point))
        (message-log-max nil))
    (while (and cont (not found))
      (end-of-line 2)
      (setq cont (< (point) (1- (point-max))))
      (setq adv (1+ adv))
      (message "%d-%d" adv (current-column))
      (setq found (>= (current-column) lre-long-line)))
    (if (not found)
        (message "No long lines!")
      (backward-word 1)
      (forward-word 1)
      (when (and (<= (point) curr)
                 cont)
        (end-of-line 2)
        (lre-next-long-line))
      (message nil))))

(defun lre-adjust-separator()
  "Adjust final sequence of this line to line up..."
  (interactive)
  (end-of-line 1)
  (backward-word 1)
  (forward-word 1)
  (delete-horizontal-space)
  (indent-relative))

(defun lre-fix-ps ()
  "Fix HP prn/ps file."
  (interactive "*")
  (save-mark-and-excursion
    (goto-char (point-min))
    (save-mark-and-excursion
      (beginning-of-line 5)
      (kill-region (point-min) (point)))
    (while (re-search-forward "\r" nil t)
      (replace-match "" nil nil))
    (goto-char (point-max))
    (beginning-of-line 0)
    (kill-region (point) (point-max))))

(defun lre-temp-clip ()
  "Show contents of clipboard in temp buffer."
  (interactive)
  (erase-buffer)
  (let ((currbuf (current-buffer)))
    (pop-to-buffer (get-buffer-create "*Clipboard contents*"))
    (erase-buffer)
    (clipboard-yank)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (pop-to-buffer currbuf)))

(defun lre-ascii-chart ()
  "Displays a character table in the *ascii map * buffer
for all characters (0..255) in the current display mapping. Note that
the low characters (0..31) are displayed as the control characters
that they represent regardless of display mapping format."
  (interactive)
  (let ((i 0)
        j c)
    (switch-to-buffer (get-buffer-create "*ascii map*"))
    (erase-buffer)
    (insert "\n---   +0   +32   +64   +96    +128  +160  +192  +224\n\n")
    (setq i 0)
    (while (< i 32)
      (insert (format "%3d  " i))
      (setq j 0)
      (while (< j 8)
        (setq c (+ i (* j 32)))
        (if (eq j 0) (insert (format " ^%c" (+ c 64)))
          (insert (format " %c " c)))
        (insert "   ")
        (setq j (1+ j))
        )
      (insert "\n")
      (setq i (1+ i))
      )
    (goto-char (point-min))
    (set-buffer-modified-p nil))
    "")

(when (lre-memb-all 'e21+ 'welcome)
  (defun lre-welcome ()
    (interactive)
    (switch-to-buffer (get-buffer-create "***  VELKOMMEN!!   ***"))
    (erase-buffer)
    (sit-for 0)
    (setq indent-tabs-mode nil)
    (animate-string "Velkommen til" 3)
    (animate-string (concat "EMACS v" emacs-version "!") 4)
    (animate-string (if (lre-memb 'personal)
                        "- HEI, LARS! -"
                      "-LRE-") 5)
    (sit-for 0)
    (if (lre-memb 'tvist) (animate-string "TVIST" 6))
    (if (lre-memb 'win32) (animate-string "Windows" 7))
    (if (lre-memb 'server) (animate-string "server" 8))
    (sit-for 0)
    (animate-string "Du måtte trykke på den knappen..." 10)
    (sit-for 6)
    (set-buffer-modified-p nil)
    (kill-this-buffer)))

(defsubst lre--make-bar (maxlen part total &optional barchar)
  "Make a \"progress bar \"."
  (make-string
   (car
    (round* (* maxlen (/ part (float total)))))
   (or barchar ?*)))

(defun lre-disp-dir-info (directory)
  "Display number of files and file sizes in a directory.
Displays both total and per suffix."
  (interactive "DDirectory: ")
  (let ((suffix-count '())
        (suffix-size '())
        (total 0)
        (total-size 0)
        (max 0)
        (target-column 78)
        (total-label " = ")
        (empty-label " - ")
        (directory-label "Directory of: ")
        (buffer (get-buffer-create "*Directory Summary Information*"))
        (extension-description "Suffix / size / count")
        ini-pos
        ini-buf)
    (setq ini-buf (buffer-name))
    (set-buffer buffer)
    (goto-char (point-max))
    (insert "\n\n")
    (setq ini-pos (point))
    (loop for file in (remove-if-not #'file-regular-p
                                     (directory-files directory t))
          for suffix = (or (file-name-extension file)
                           empty-label)
          for fsize = (float (nth 7 (file-attributes file)))
          for assoc-elt  = (assoc suffix suffix-count)
          for assoc-size = (assoc suffix suffix-size)
          count file into suffix-total
          sum fsize into fsize-total
          maximize (length suffix) into max-length
          finally (setq total suffix-total
                        total-size fsize-total
                        max max-length)
          if assoc-elt
          do (incf (cdr assoc-elt))
          else
          do (push (cons suffix 1) suffix-count)

          if assoc-size
          do (incf (cdr assoc-size) fsize)
          else
          do (push (cons suffix fsize) suffix-size)
          )

    (insert directory-label directory "\n\n"
            extension-description "\n"
            (make-string (length extension-description) ?=)
            "\n")

    (if (zerop total)
        (insert "No files found")
      (if (= total-size 0) (setq total-size 0.00001)) ; Hack...
      (loop for elt in (sort* suffix-count #'string< :key #'car)
            for fsize = (float (cdr (assoc (car elt) suffix-size)))
            for fcount = (cdr elt)
            do (insert (format "%s" (car elt)))
            (insert (make-string (1+ (- max (current-column))) ? ))
            (insert (format "  %12.0f (%5.2f%%)"
                            fsize
                            (* 100 (/ fsize (float total-size))))
                    (format "  %6d (%5.2f%%)"
                            fcount
                            (* 100 (/ fcount (float total)))))
            (let* ((tot (1- (- target-column (current-column))))
                   (len (/ tot 2)))
              (insert (format (format "%%%ds|%%s" len)
                              (lre--make-bar len fsize total-size ?+)
                              (lre--make-bar len fcount total))))
            (insert "\n")
            )
      (insert total-label)
      (insert (make-string (1+ (- max (current-column))) ? )
              (format "  %12.0f         " total-size)
              (format "  %6d" total)
              "\n")
      )
    (or (string= ini-buf (buffer-name))
        (switch-to-buffer-other-window buffer))
    (goto-char ini-pos)
    (recenter)))



;;; ----------------------------------------------------------------------
;;; SPE/FE

(defun lre-spe-sign ()
  "Add footer to SPE-file."
  (interactive "*")
  (let ((pfx "# "))
    (goto-char (point-max))
    (insert pfx "SPE" "                Utviklingsmilj|"
                      "                Mesan  20"      lre-today-year "\n"
            pfx "@(#) $Id: lre-util.el,v 1.4 2005/07/05 07:14:54 larsr Exp $" "\n"
            pfx "%W" "% " "SPE" "\n")))

;; (if (lre-memb 'ccas)
;;     (defun lre-fe-init-log (uke)
;;       "Function to initialize some fields in the FE-file."
;;       (interactive "*sUke: ")
;;       (goto-char (point-min))
;;       (fe-set-field "Kort"
;;                  (if (string= uke "") ""
;;                    (format "Feil & advarsler fra kompilering u%s" uke))
;;                  nil nil t)
;;       (fe-set-field "Status" (if (string= uke "") "" "KLAR") nil nil t)
;;       (fe-set-field "Org" lre-fe-org nil nil t)
;;       (fe-set-field "Tlf" "22772595" nil nil t)
;;       (fe-set-field "Beskr" (if (string= uke "") "" "Se \"Kort\".") nil nil t)
;;       (fe-set-field "UNIXid" fe-sign nil nil t)
;;       (fe-set-field "CCASid" "X06LRE" nil nil t)
;;       (fe-set-field "PROGid" (if (string= uke "") "mccas" "*") nil nil t)
;;       (fe-set-field "Maskin" "alma" nil nil t)
;;       (fe-set-field "DBnavn" (if (string= uke "") "CCAS" "") nil nil t)
;;       (fe-set-field "Delsys" (if (string= uke "") "MAD" "*") nil nil t)
;;       (fe-set-field "Modul" (if (string= uke "") "" "Se Historie") nil nil t)
;;       (fe-set-field "Ansv" (if (string= uke "") "" fe-sign) nil nil t)
;;       (fe-set-field "Lev" "B R" nil nil t)
;;       (fe-set-field "Sign" (if (string= uke "") "" fe-sign) nil nil t)
;;       (fe-set-field "Impl" (if (string= uke "") "" fe-sign) nil nil t)
;;       (goto-char (point-min))
;;       (end-of-line 4)))

;; (defun lre-fe-mode ()
;; "Additions to `fe-mode' setup."
;;   (if (lre-memb-all 'ccas 'keys)
;;       (define-key fe-mode-map "\C-cl" 'lre-fe-init-log))
;;   (cond ((not window-system) nil)
;;         ((lre-memb 'flock)
;;          (fe-font-lock)
;;          (lre-colors))
;;         (t
;;          (fe-highlight)
;;          (lre-colors)
;;          (lre-add-x-hilit 'fe-mode))))


;;; --------------------------------------------------------------------------
;;; calendar

(defun lre-cal-mode ()
  "Additions to `cal-mode'."
  (setq calendar-time-display-form (quote
                                    (24-hours ":" minutes
                                              (if time-zone "(")
                                              time-zone
                                              (if time-zone ")")))
      calendar-week-start-day      1
      european-calendar-style      t))


(provide 'lre-util)

;;; lre-util.el ends here
