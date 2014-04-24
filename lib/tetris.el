From: "Glynn Clements" <glynn@sensei.co.uk>
Subject: Re: tetris.el - Bugfix release
Newsgroups: gnu.emacs.sources
References: <33FCA204.322447A7@sensei.co.uk> <33FF8663.7ED19BEC@sensei.co.uk> <3400DE45.7BC1F086@sensei.dot.co.dot.uk>
Message-ID: <01bcb29f$e48e3380$50fb47c1@im2u>
X-Newsreader: Microsoft Internet News 4.70.1160


The following version of tetris.el fixes some bugs, most notably the
timer not being killed when it should be, or being killed when it
shouldn't be.

Due to the number of different versions I've posted recently, and
formatting issues, the source is again included in full.

Hopefully from now on I'll be able to generate suitable patches if
anything else needs fixing.


Glynn Clements <glynn@sensei.co.uk>

--8<------------------------------------------------------------------------
--

;;; tetris.el -- Implementation of Tetris for Emacs.

;; Copyright (C) 1997 Glynn Clements <glynn@sensei.co.uk>

;; Author: Glynn Clements <glynn@sensei.co.uk>
;; Version: 1.6
;; Created: 1997-08-13
;; Keywords: games

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not synched.

;;; Commentary:

;; Modified: 1997-08-17, added tetris-move-bottom
;; Modified: 1997-08-22, changed setting of display table for compatibility
;;	with XEmacs 19.15
;; Modified: 1997-08-23, changed setting of display table for TTY
compatibility
;; Modified: 1997-08-24, various changes for FSF Emacs compatibility
;; Modified: 1997-08-25
;;	modified existing docstrings, added new docstrings
;;	L now rotates the same way as T and mirror-L
;;	now adds tetris-end-game to buffer-local value of kill-buffer-hook
;; Modified: 1997-08-26, miscellaneous bugfixes
;; URL: ftp://sensei.co.uk/misc/tetris.el.gz
;; Tested with XEmacs 20.3-beta and Emacs 19.34
;; Reported to work with XEmacs 19.15 and 20.2

(eval-when-compile
  (require 'cl))

(defconst tetris-tty-block [?O]
  "Character used for a full square in text mode")

(defconst tetris-tty-blank [?.]
  "Character used for an empty square in text mode")

(defconst tetris-tty-space [?\040]
  "Character used for a space in text mode")

(defconst tetris-buffer-name "*Tetris*")

(defconst tetris-shapes
  [[[[1 1 0 0] [1 1 0 0] [1 1 0 0] [1 1 0 0]]
    [[1 1 0 0] [1 1 0 0] [1 1 0 0] [1 1 0 0]]
    [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]
    [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]]

   [[[2 2 2 0] [0 2 0 0] [2 0 0 0] [2 2 0 0]]
    [[0 0 2 0] [0 2 0 0] [2 2 2 0] [2 0 0 0]]
    [[0 0 0 0] [2 2 0 0] [0 0 0 0] [2 0 0 0]]
    [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]]

   [[[3 3 3 0] [3 3 0 0] [0 0 3 0] [3 0 0 0]]
    [[3 0 0 0] [0 3 0 0] [3 3 3 0] [3 0 0 0]]
    [[0 0 0 0] [0 3 0 0] [0 0 0 0] [3 3 0 0]]
    [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]]

   [[[4 4 0 0] [0 4 0 0] [4 4 0 0] [0 4 0 0]]
    [[0 4 4 0] [4 4 0 0] [0 4 4 0] [4 4 0 0]]
    [[0 0 0 0] [4 0 0 0] [0 0 0 0] [4 0 0 0]]
    [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]]

   [[[0 5 5 0] [5 0 0 0] [0 5 5 0] [5 0 0 0]]
    [[5 5 0 0] [5 5 0 0] [5 5 0 0] [5 5 0 0]]
    [[0 0 0 0] [0 5 0 0] [0 0 0 0] [0 5 0 0]]
    [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]]

   [[[0 6 0 0] [6 0 0 0] [6 6 6 0] [0 6 0 0]]
    [[6 6 6 0] [6 6 0 0] [0 6 0 0] [6 6 0 0]]
    [[0 0 0 0] [6 0 0 0] [0 0 0 0] [0 6 0 0]]
    [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]]

   [[[7 7 7 7] [7 0 0 0] [7 7 7 7] [7 0 0 0]]
    [[0 0 0 0] [7 0 0 0] [0 0 0 0] [7 0 0 0]]
    [[0 0 0 0] [7 0 0 0] [0 0 0 0] [7 0 0 0]]
    [[0 0 0 0] [7 0 0 0] [0 0 0 0] [7 0 0 0]]]])

(defconst tetris-shape-dimensions [[2 2] [3 2] [3 2] [3 2] [3 2] [3 2] [4
1]])

(defconst tetris-colors
  [[0 0 0] [0 0 1] [0.7 0 1] [1 1 0] [1 0 1] [0 1 1] [0 1 0] [1 0 0]]
  "Vector of colors of the various shapes
Element 0 is the background colour")

(defconst tetris-xpm "\
/* XPM */
static char *noname[] = {
/* width height ncolors chars_per_pixel */
\"16 16 3 1\",
/* colors */
\"+ s col1\",
\". s col2\",
\"- s col3\",
/* pixels */
\"---------------+\",
\"--------------++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"-+++++++++++++++\",
\"++++++++++++++++\"
};
"
  "XPM format image used for each square")

(defconst tetris-blank 0)
(defconst tetris-space ?.)

(defconst tetris-width 10
  "Width of playing area")

(defconst tetris-height 20
  "Height of playing area")

(defconst tetris-next-x 0
  "X position of next shape")
(defconst tetris-next-y (+ 1 tetris-height)
  "Y position of next shape")

(defconst tetris-score-x 0
  "X position of score")

(defconst tetris-score-y (+ 6 tetris-height)
  "Y position of score")

(defconst tetris-buffer-width 15
  "Width of used portion of buffer")

(defconst tetris-buffer-height 30
  "Height of used portion of buffer")

(defconst tetris-tick-period 0.3
  "The time taken for a shape to drop one row")

(defvar tetris-buffer-start 1)

(defvar tetris-shape 0)
(defvar tetris-rot 0)
(defvar tetris-next-shape 0)
(defvar tetris-n-shapes 0)
(defvar tetris-n-rows 0)
(defvar tetris-pos-x 0)
(defvar tetris-pos-y 0)

(defvar tetris-timer nil)

(defvar tetris-display-table nil)

(defvar tetris-mode-map
  (make-sparse-keymap 'tetris-mode-map))

(define-key tetris-mode-map "n"		'tetris-start-game)
(define-key tetris-mode-map "q"		'tetris-end-game)

(define-key tetris-mode-map " "		'tetris-move-bottom)
(define-key tetris-mode-map [left]	'tetris-move-left)
(define-key tetris-mode-map [right]	'tetris-move-right)
(define-key tetris-mode-map [up]	'tetris-rotate-prev)
(define-key tetris-mode-map [down]	'tetris-rotate-next)

(defvar tetris-null-map
  (make-sparse-keymap 'tetris-null-map))

(define-key tetris-null-map "n"		'tetris-start-game)

(defun tetris-start-timer ()
  (if (featurep 'itimer)
      (setq tetris-timer
	    (start-itimer
	     "Tetris"
	     'tetris-update-game
	     tetris-tick-period
	     tetris-tick-period
	     nil
	     t (current-buffer)))
    (setq tetris-timer
	  (run-with-timer
	   tetris-tick-period
	   tetris-tick-period
	   'tetris-update-game
	   (current-buffer)))))

(defun tetris-kill-timer ()
  (if tetris-timer
      (if (featurep 'itimer)
          (delete-itimer tetris-timer)
        (timer-set-time tetris-timer '(0 0 0) nil)))
  (setq tetris-timer nil))

(defun tetris-cell-offset (x y)
  (+ tetris-buffer-start
     (* (1+ tetris-buffer-width) y)
     x))

(defun tetris-get-cell (x y)
  (char-after (tetris-cell-offset x y)))

(defun tetris-set-cell (x y c)
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (tetris-cell-offset x y))
      (delete-char 1)
      (insert-char c 1))))

(defun tetris-get-shape-cell (x y)
  (aref
   (aref
    (aref
     (aref tetris-shapes tetris-shape)
     y)
    tetris-rot)
   x))

(defun tetris-shape-width ()
  (aref (aref tetris-shape-dimensions tetris-shape)
	(% tetris-rot 2)))

(defun tetris-shape-height ()
  (aref (aref tetris-shape-dimensions tetris-shape)
	(- 1 (% tetris-rot 2))))

(defun tetris-new-shape ()
  (setq tetris-shape tetris-next-shape)
  (setq tetris-rot 0)
  (setq tetris-next-shape (random 7))
  (setq tetris-pos-x (random (- tetris-width (tetris-shape-width))))
  (setq tetris-pos-y 0)
  (setq tetris-n-shapes (1+ tetris-n-shapes))
  (tetris-draw-next-shape)
  (tetris-draw-score))

(defun tetris-draw-score ()
  (let ((strings (vector
		  (format "Shapes: %05d" tetris-n-shapes)
		  (format "Rows:   %05d" tetris-n-rows))))
    (loop for y from 0 to 1 by 1 do
	  (let* ((string (aref strings y))
		 (len (length string)))
	    (loop for x from 0 to (1- len) by 1 do
		  (tetris-set-cell
		   (+ tetris-score-x x)
		   (+ tetris-score-y y)
		   (aref string x)))))))

(defun tetris-draw-next-shape ()
  (loop for y from 0 to 3 by 1 do
	(loop for x from 0 to 3 by 1 do
	      (tetris-set-cell
	       (+ tetris-next-x x)
	       (+ tetris-next-y y)
	       (let ((tetris-shape tetris-next-shape)
		     (tetris-rot 0))
		 (tetris-get-shape-cell x y))))))

(defun tetris-draw-shape ()
  (let (x y c)
    (setq y 0)
    (while (< y (tetris-shape-height))
      (setq x 0)
      (while (< x (tetris-shape-width))
	(setq c (tetris-get-shape-cell x y))
	(if (/= c tetris-blank)
	    (tetris-set-cell
	     (+ tetris-pos-x x)
	     (+ tetris-pos-y y)
	     c))
	(setq x (1+ x)))
      (setq y (1+ y)))))

(defun tetris-erase-shape ()
  (let (x y c)
    (setq y 0)
    (while (< y (tetris-shape-height))
      (setq x 0)
      (while (< x (tetris-shape-width))
	(setq c (tetris-get-shape-cell x y))
	(if (/= c tetris-blank)
	    (tetris-set-cell
	     (+ tetris-pos-x x)
	     (+ tetris-pos-y y)
	     tetris-blank))
	(setq x (1+ x)))
      (setq y (1+ y)))))

(defun tetris-test-shape ()
  (let ((hit nil)
	x y c)
    (setq y 0)
    (while (< y (tetris-shape-height))
      (setq x 0)
      (while (< x (tetris-shape-width))
	(unless hit
	  (setq hit
		(let ((c (tetris-get-shape-cell x y))
		      (xx (+ tetris-pos-x x))
		      (yy (+ tetris-pos-y y)))
		  (and (/= c tetris-blank)
		       (or (>= xx tetris-width)
			   (>= yy tetris-height)
			   (/= (tetris-get-cell xx yy)
			       tetris-blank))))))
	(setq x (1+ x)))
      (setq y (1+ y)))
    hit))

(defun tetris-full-row (y)
  (let ((full t)
	(x 0))
    (while (< x tetris-width)
      (if (= (tetris-get-cell x y) tetris-blank)
	  (setq full nil))
      (setq x (1+ x)))
    full))

(defun tetris-shift-row (y)
  (let ((x 0) c)
    (while (< x tetris-width)
      (setq c (tetris-get-cell x (1- y)))
      (tetris-set-cell x y c)
      (setq x (1+ x)))))

(defun tetris-shift-down ()
  (let (y0 y)
    (setq y0 (1- tetris-height))
    (while (>= y0 0)
      (if (tetris-full-row y0)
	  (progn
	    (setq tetris-n-rows (1+ tetris-n-rows))
	    (tetris-draw-score)
	    (setq y y0)
	    (while (> y 0)
	      (tetris-shift-row y)
	      (setq y (1- y))
	      ))
	(setq y0 (1- y0))))))

(defun tetris-init-buffer ()
  (let ((line (concat
	       (make-string tetris-buffer-width tetris-space)
	       "\n"))
	(buffer-read-only nil))
    (erase-buffer)
    (setq tetris-buffer-start (point))
    (dotimes (i tetris-buffer-height)
	  (insert-string line))
    (loop for y from 0 to (1- tetris-height) do
	  (loop for x from 0 to (1- tetris-width) do
		(tetris-set-cell x y tetris-blank))) ))

(defun tetris-reset-game ()
  (tetris-kill-timer)
  (tetris-init-buffer)
  (setq tetris-next-shape (random 7))
  (setq tetris-shape		0
	tetris-rot		0
	tetris-n-shapes		0
	tetris-n-rows		0
	tetris-pos-x		0
	tetris-pos-y		0)
  (tetris-new-shape)
  (tetris-draw-shape))

(defun tetris-shape-done ()
  (tetris-shift-down)
  (tetris-new-shape)
  (if (tetris-test-shape)
      (progn
	(tetris-end-game))
    (tetris-draw-shape)))

(defun tetris-update-game (tetris-buffer)
  "Called on each clock tick.
Drops the shape one square, testing for collision."
;  (interactive)
  (if (eq (current-buffer) tetris-buffer)
      (let (hit)
	(tetris-erase-shape)
	(setq tetris-pos-y (1+ tetris-pos-y))
	(setq hit (tetris-test-shape))
	(if hit
	    (setq tetris-pos-y (1- tetris-pos-y)))
	(tetris-draw-shape)
	(if hit
	  (tetris-shape-done)))))

(defun tetris-move-bottom ()
  "Drops the shape to the bottom of the playing area"
  (interactive)
  (let ((hit nil))
    (tetris-erase-shape)
    (while (not hit)
      (setq tetris-pos-y (1+ tetris-pos-y))
      (setq hit (tetris-test-shape)))
    (setq tetris-pos-y (1- tetris-pos-y))
    (tetris-draw-shape)
    (tetris-shape-done)))

(defun tetris-move-left ()
  "Moves the shape one square to the left"
  (interactive)
  (unless (= tetris-pos-x 0)
    (tetris-erase-shape)
    (setq tetris-pos-x (1- tetris-pos-x))
    (if (tetris-test-shape)
	(setq tetris-pos-x (1+ tetris-pos-x)))
    (tetris-draw-shape)))

(defun tetris-move-right ()
  "Moves the shape one square to the right"
  (interactive)
  (unless (= (+ tetris-pos-x (tetris-shape-width))
	     tetris-width)
    (tetris-erase-shape)
    (setq tetris-pos-x (1+ tetris-pos-x))
    (if (tetris-test-shape)
	(setq tetris-pos-x (1- tetris-pos-x)))
    (tetris-draw-shape)))

(defun tetris-rotate-prev ()
  "Rotates the shape clockwise"
  (interactive)
  (tetris-erase-shape)
  (setq tetris-rot (% (+ 1 tetris-rot) 4))
  (if (tetris-test-shape)
      (setq tetris-rot (% (+ 3 tetris-rot) 4)))
  (tetris-draw-shape))

(defun tetris-rotate-next ()
  "Rotates the shape anticlockwise"
  (interactive)
  (tetris-erase-shape)
  (setq tetris-rot (% (+ 3 tetris-rot) 4))
  (if (tetris-test-shape)
      (setq tetris-rot (% (+ 1 tetris-rot) 4)))
  (tetris-draw-shape))

(defun tetris-end-game ()
  "Terminates the current game"
  (interactive)
  (tetris-kill-timer)
  (use-local-map tetris-null-map))

(defun tetris-start-game ()
  "Starts a new game of Tetris"
  (interactive)
  (tetris-reset-game)
  (use-local-map tetris-mode-map)
  (tetris-start-timer))

(put 'tetris-mode 'mode-class 'special)

(defun tetris-mode ()
  "A mode for playing Tetris.

tetris-mode keybindings:
   \\{tetris-mode-map}
"
  (kill-all-local-variables)
  (use-local-map tetris-null-map)
  (if (fboundp 'specifierp)
      (add-spec-to-specifier current-display-table
			     tetris-display-table
			     (current-buffer)
			     nil 'remove-locale)
    (setq buffer-display-table tetris-display-table))
  (setq truncate-lines 't)
  (setq major-mode 'tetris-mode)
  (setq mode-name "Tetris"))

(defun tetris-color (col shade)
  (let* ((vec (aref tetris-colors col))
	 (v (floor (* shade 255)))
	 (r (* v (aref vec 0)))
	 (g (* v (aref vec 1)))
	 (b (* v (aref vec 2))))
    (format "#%02x%02x%02x" r g b)))

(defun tetris-make-glyph (index)
  (make-glyph
   (vector
    'xpm
    :data tetris-xpm
    :color-symbols (list
		    (cons "col1" (tetris-color index 0.6))
		    (cons "col2" (tetris-color index 0.8))
		    (cons "col3" (tetris-color index 1.0))) )))

(defun tetris-make-display-table ()
  (setq tetris-display-table (make-display-table))
  (cond ((and (eq window-system 'x) (featurep 'xpm))
	 (aset tetris-display-table tetris-space
	       (make-glyph (vector 'xpm :data tetris-xpm)))
	 (loop for i from 0 to 7 by 1 do
	       (aset tetris-display-table
		     (+ tetris-blank i)
		     (tetris-make-glyph i))))
	(t
	 (aset tetris-display-table tetris-space tetris-tty-space)
	 (aset tetris-display-table tetris-blank tetris-tty-blank)
	 (loop for i from 1 to 7 by 1 do
	       (aset tetris-display-table
		     (+ tetris-blank i)
		     tetris-tty-block)))))

(defun tetris ()
  "Tetris

Shapes drop from the top of the screen, and the user has to move and
rotate the shape to fit in with those at the bottom of the screen so
as to form complete rows.

tetris-mode keybindings:
   \\<tetris-mode-map>
\\[tetris-start-game]	Starts a new game of Tetris
\\[tetris-end-game]	Terminates the current game
\\[tetris-move-left]	Moves the shape one square to the left
\\[tetris-move-right]	Moves the shape one square to the right
\\[tetris-rotate-prev]	Rotates the shape clockwise
\\[tetris-rotate-next]	Rotates the shape anticlockwise
\\[tetris-move-bottom]	Drops the shape to the bottom of the playing area

"
  (interactive)

  (tetris-make-display-table)
  (switch-to-buffer tetris-buffer-name)
  (tetris-mode)
  (setq buffer-read-only t)
  (buffer-disable-undo (current-buffer))
  (if (fboundp 'specifierp)
      (set-specifier text-cursor-visible-p nil (current-buffer)))

  (make-local-hook 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'tetris-end-game nil t)

  (make-local-variable 'tetris-shape)
  (make-local-variable 'tetris-rot)
  (make-local-variable 'tetris-next-shape)
  (make-local-variable 'tetris-n-shapes)
  (make-local-variable 'tetris-n-rows)
  (make-local-variable 'tetris-pos-x)
  (make-local-variable 'tetris-pos-y)

  (tetris-start-game))

(provide 'tetris)

;;; tetris.el ends here

