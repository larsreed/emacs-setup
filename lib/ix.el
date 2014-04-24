(defun lre_gen_ix()
  (interactive)
  (beginning-of-line)
  (let (tbl
	p
	coname
	elist)
    (save-excursion
      (search-backward-regexp "create table \\([A-Za-z0-9_]+\\)")
      (setq tbl (match-string 1)))
    (forward-word 1)
    (forward-char 1)
    (setq p (point))
    (forward-word 1)
    (setq coname (buffer-substring-no-properties p (point)))
    (search-forward-regexp "\( *\\([^ \n]+\\) *\)")
    (setq elist (match-string 1))
    (message elist)
    (search-forward-regexp "^go")
    (end-of-line)
    (insert "\nalter table " tbl "\n   add constraint " coname
	    " primary key (" elist ")\ngo\nsp_primarykey " tbl "," elist "\ngo")
    ))
