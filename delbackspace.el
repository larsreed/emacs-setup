;; Tastaturfiks for Xemacs og Emacs
;; Delete-knappen skal ha annen funksjonalitet enn Backspace

(if (string= (substring (emacs-version) 0 6) "XEmacs")
    (global-set-key [(delete)] 'delete-char)
  (global-set-key [delete] 'delete-char))
