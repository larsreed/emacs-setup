;;;; psgml-charent.el
;;; Last edited: 1999-10-06 07:53:23 lenst
;;; $Id: psgml-charent.el,v 1.5 1999/10/06 16:20:37 lenst Exp $

;; Copyright (C) 1994 Lennart Staflin

;; Author: Steinar Bang, Falch Hurtigtrykk as., Oslo, 940711
;;	Lennart Staflin <lenst@lysator.liu.se>
;;
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

;;  Functions to convert character entities into displayable characters
;;  and displayable characters back into character entities.


;;;; Code:

(provide 'psgml-charent)
(require 'psgml-parse)


;;;; Variable declarations

(defvar psgml-display-char-list-filename
  (expand-file-name "iso88591.map"
                    (file-name-directory (locate-library "psgml")))
  "*Name of file holding relations between character codes and character
names of displayable characters")

(defvar psgml-display-char-alist-cache nil)


;;;; Function declarations

(defun psgml-display-char-alist ()
  "Return the current display character alist.
Alist with entity name as key and display character as content."
  (unless (file-exists-p psgml-display-char-list-filename)
    (error "No display char file: %s"
	   psgml-display-char-list-filename))
  (psgml-cache-catalog psgml-display-char-list-filename 
		      'psgml-display-char-alist-cache
		      (function psgml-read-display-char-alist)))

(defun psgml-read-display-char-alist ()
  (let (key disp-char alist)
    (while (re-search-forward "^\\([0-9]+\\)[ \t]+\\(.+\\)$" nil t)
      (setq key (buffer-substring (match-beginning 2) (match-end 2)))
      (setq disp-char
	    (char-to-string
	     (string-to-number
	      (buffer-substring (match-beginning 1) (match-end 1)))))
      (push (cons key disp-char)
	    alist))
    alist))

(defun psgml-charent-to-dispchar-alist ()
  "Association list to hold relations of the type
     (CHARACTER-NAME . CHARACTER)
    where 
     CHARACTER-NAME is a string holding a character name
     CHARACTER      is a string holding a single displayable character"
  (psgml-need-dtd)
  (let ((display-chars (psgml-display-char-alist))
	(alist nil))
    (psgml-map-entities
     (function
      (lambda (entity)
	(let ((char (cdr (assoc (psgml-entity-text entity)
				display-chars))))
	  (when char
	    (push (cons (psgml-entity-name entity) char) alist)))))
     (psgml-dtd-entities psgml-dtd-info))
    
    alist))


(defun psgml-charent-to-display-char ()
  "Replace character entities with their display character equivalents"
  (interactive)
  (let ((charent-to-char
	 (psgml-charent-to-dispchar-alist))
	charent replacement)
    (save-excursion
      (goto-char (point-min))
      (psgml-with-parser-syntax
       (while (re-search-forward "&\\(\\w\\(\\w\\|\\s_\\)*\\);?" nil t)
	 (setq charent (buffer-substring (match-beginning 1) (match-end 1)))
	 (if (setq replacement (cdr (assoc charent charent-to-char)))
	     (replace-match replacement t t)))))))

(defun psgml-display-char-to-charent ()
  "Replace displayable characters with their character entity equivalents"
  (interactive)
  (let ((case-fold-search nil))
    (save-excursion
      (loop for pair in (psgml-charent-to-dispchar-alist)
	    do (goto-char (point-min))
	    (while (search-forward (cdr pair) nil t)
	      (replace-match (concat "&" (car pair) ";") t t))))))



;;; psgml-charent.el ends here
