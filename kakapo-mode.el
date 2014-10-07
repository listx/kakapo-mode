; kakapo-mode.el --- Only TABs for indentation (leading whitespace), and SPACES
; for alignment.

;; LICENSE

; Copyright (C) 2014 Linus Arver
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
; Version: 0.1
; Keywords: indentation
; URL: https://github.com/listx/kakapo-mode

;; ABOUT

; Have you ever fought long battles with the major modes out there (especially
; programming-language modes) that force its indentation style on you? Worse,
; have you discovered that some of these modes indent your code with a MIXTURE
; of TABS *AND* SPACES?
;
; Kakapo-mode is about giving back the control of the TAB character back to you,
; the user, with some conditions:
;
;   * The concept of "indentation" and "leading whitespace" are the same.
;   * Indentation is taken care of with TAB characters.
;   * If you press a TAB character *after* some text, we insert SPACES up to the
;     next tab-stop column; this is a simpler version of "Smart Tabs"
;     (http://www.emacswiki.org/emacs/SmartTabs).
;   * If at any point we detect a mixture of tabs and spaces in the indentation,
;     we display a warning message instead of modifying the buffer.
;
; `kakapo-mode' is very similar to "Smart Tabs", but with a key difference: the
; latter requires you to write helper functions for it to work properly;
; instead, kakapo-mode relies on the human user for aesthetics.
;
; `kakapo-mode' inserts a TAB when we are indenting (leading whitespace), or the
; right number of SPACE characters when we are aligning something inside or at
; the end of a line.

;; INSTALLATION

; Move this file to somewhere in the `load-path'.
; Then add the following lines to ~/.emacs:
;
;   (require 'kakapo-mode)
;   (add-hook 'text-mode-hook 'kakapo-mode)
;   (add-hook 'prog-mode-hook 'kakapo-mode)
;
; . You can of course change, e.g., `prog-mode-hook' to be some other more
; specific hook.
;
; When `kakapo-mode' is enabled, the TAB key will invoke `kakapo-tab' (inserting
; tabs or spaces as necessary) instead of being interpreted as usual by whatever
; mode is active.
;
; You should probably use the following keymappings to play well with Evil mode
; and any other mode that insists on using mixed TAB/SPACE characters for
; indentation (leading whitespace). Granted, these helper functions do not
; respect semantic indentation (as kakapo-mode doesn't care about the *number*
; of TABS in the indentation as long as they are all TABS) --- this kind of
; simplicity is exactly what kakapo-mode is all about!
;
;   (define-key evil-normal-state-map "o" (lambda () (interactive) (kakapo-open nil)))
;   (define-key evil-normal-state-map "O" (lambda () (interactive) (kakapo-open t)))
;   (define-key evil-insert-state-map (kbd "RET") 'kakapo-ret-and-indent)
;
;; BEGIN PROGRAM

; Internal variable used for development/testing purposes.
(defcustom kakapo-debug nil
	"Display debug messages instead of indenting; useful only for
	development.")

; Either print debug message, or execute `func'.
(defun kakapo-indent-debug (str func)
	(if kakapo-debug
		(message str)
		func)
)

(defun kakapo-lw ()
	"Retrieve the leading whitespace of the current line."
	(interactive)
	(let*
		(; bindings
			(point-column-0 (line-beginning-position))
			(point-column-till-text
				(save-excursion
					(back-to-indentation)
					(point)
				)
			)
		)
		(buffer-substring-no-properties
			point-column-0 point-column-till-text)
	)
)

(defun kakapo-lc ()
	"Retrieve the current line's contents."
	(interactive)
	(buffer-substring-no-properties
		(line-beginning-position) (line-end-position))
)

(defun kakapo-lw-search (above)
	"Search either above or below the current line for leading whitespace."
	(let*
		(; bindings
			(point-end nil)
			(lw-initial (kakapo-lw))
			(lw "")
			(lc "")
			(stop-loop nil)
		)
		(save-excursion
			(if (string= "" lw-initial)
				(progn
					; `point-end' ensures that we always terminte the `while'
					; loop. If we're searching up, we use `point-min', because
					; that is the ultimate `line-beginning-position' (which is
					; where `forward-line' goes to) when we move up one line
					; repeatedly to the start of the buffer. If we move down to
					; the end of the buffer, we use `point-max', and not the
					; last line's first column, because Emacs defines
					; `point-max' as the point that would be reached if we call
					; `forward-line' past the end of the buffer. In short, the
					; `point-end' variable always guarantees that we exit the
					; `while' loop if all the lines searched either above or
					; below are all blank lines.

					; The `stop-loop' variable is there to short-circuit the
					; loop if the line we're on is not a blank line.
					(setq point-end (if above (point-min) (point-max)))
					(while (and (not stop-loop) (not (eq (point) point-end)))
						(forward-line (if above -1 1))
						(beginning-of-line)
						(setq lw (kakapo-lw))
						(setq lc (kakapo-lc))
						; Only continue the search if the current line is a blank line.
						(if (not (string= "" lc))
							(setq stop-loop t)
						)
					)
					lw
				)
				lw-initial
			)
		)
	)
)

(defun kakapo-tab ()
	"If point is at the beginning of a line, or if all characters
preceding it on the current line are tab characters, insert a
literal tab character. Otherwise, insert space characters based
on tab-width, to simulate a real tab character; this is just like
'expandtab' in Vim"
	(interactive)
	(let*
		(; bindings
			(p (point))
			(point-column-0 (line-beginning-position))
			(line-contents
				(buffer-substring-no-properties
					point-column-0 (line-end-position)))
			(up-to-point
				(buffer-substring-no-properties
					point-column-0 p))
			(columns-tab-width
				(-
					(* tab-width (+ 1 (/ (current-column) tab-width)))
					(current-column)))
			(columns-til-next-tab-stop
				(if (eq 0 columns-tab-width
					)
					tab-width
					columns-tab-width
				))
		)
		(cond
			; If the line is blank, insert a TAB.
			((eq (line-beginning-position) (line-end-position))
				(kakapo-indent-debug
					"BLANK"
					(insert ?\t)
				)
			)
			; if line is all-whitespace, insert a TAB, unless we detect mixed
			; tabs/spaces.
			((string-match "^[ \t]+$" line-contents)
				(if (string-match "^[\t]+$" line-contents)
					(kakapo-indent-debug
						"TABS LINE"
						(insert ?\t)
					)
					(error
						(concat
							"<<< SPACE-BASED INDENTATION DETECTED ON "
							"WHITESPACE-ONLY LINE >>>"))
				)
			)

			; Since the all-whitespace above failed, this line has some text on
			; it; we consider the case where it already has leading whitespace.
			((string-match "^[ \t]+" line-contents)
				; Is the leading whitespace all TABS?
				(if (string-match "^[\t]+$" (kakapo-lw))
					; Since the leading whitespace is well-formed, we only need
					; consider where point is. If point is inside the
					; well-formed whitespace, we insert a TAB. Otherwise, we
					; insert saces because we are obviously NOT trying to indent
					; all the text.
					(if (string-match "^[\t]*$" up-to-point)
						(kakapo-indent-debug
							"LEADING TABS TO POINT"
							(insert ?\t)
						)
						(kakapo-indent-debug
							"LEADING TABS: POINT IS ELSEWHERE"
							(cl-loop
								repeat
								columns-til-next-tab-stop
								do (insert " "))
						)

					)
					(error
						(concat
							"<<< SPACE-BASED INDENTATION DETECTED ON "
							"NON-EMPTY LINE >>>"))
				)
			)
			; We do *not* have leading whitespace. There are two cases: point is
			; located at the beginning of the line, in which case we insert a
			; TAB; otherwise, we insert spaces.
			(t (if (eq point-column-0 p)
				(kakapo-indent-debug
					"NO LEADING WHITESPACE"
					(insert ?\t)
				)
				(kakapo-indent-debug
					"NO LEADING WHITESPACE: POINT IS ELSEWHERE"
					(cl-loop
						repeat
						columns-til-next-tab-stop
						do (insert " ")))
				)
			)
		)
	)
)

; Pressing RETURN/ENTER is such a closely-tied operation to inserting tabs and
; indentation, that we define a companion function to go along with
; kakapo-tab.
;
; You can use this function like so:
;
;   (define-key evil-insert-state-map (kbd "RET") 'kakapo-ret-and-indent)
(defun kakapo-ret-and-indent ()
	"Insert a newline at point, and indent relative to the current line."
	(interactive)
	(let*
		(
			(lw (kakapo-lw))
			(lc (kakapo-lc))
			(point-column-till-text
				(save-excursion
					(back-to-indentation)
					(point)
				)
			)
			(lw-below (kakapo-lw-search nil))
			(lw-above (kakapo-lw-search t))
		)
		(cond
			((string-match " " lw)
				(error "<< SPACE-BASED INDENTATION DETECTED ON CURRENT LINE >>")
			)
			; For an empty line, search downwards for indentation, and use that,
			; if any. If no indentation below at all (all empty lines), then
			; search for indentation above, and use that, if any. Otherwise
			; (e.g., there is 0-indented text above and below), do not use
			; insert any indentation.
			((string= "" lc)
				(cond
					((not (string= "" lw-below))
						(insert (concat "\n" lw-below))
					)
					((not (string= "" lw-above))
						(insert (concat "\n" lw-above))
					)
					(t (insert "\n"))
				)
			)
			; This is an all-tabs line --- chances are that the indentation was
			; created by this very same function (unless the file we're editing
			; has lots of meaningless all-tabs lines); assuming this is the
			; case, the indentation needs to be preserved as-is. So, we just add
			; the newline at the very begnning of the line, and then move to the
			; end of the line (leaving the indentation untouched).
			((and (string-match "^[\t]+$" lw) (string= lw lc))
				(progn
					(beginning-of-line)
					(insert "\n")
					(end-of-line)
				)
			)
			; We are here if there is some text on the line already, in which
			; case we simply preserve whatever indentation we found. We take
			; care to remove any whitespace we may be breaking up.
			(t
				(progn
					(delete-horizontal-space)
					(insert (concat "\n" lw))
				)
			)
		)
	)
)

; The `kakapo-open-above-or-below' function is meant to be used in conjunction
; with evil-mode, where the default "o" and "O" keys introduce mixed tab/space
; indentation.
(defun kakapo-open (above)
	"Insert a newline above if `above' is t, and indent relative to the current
line (not the line(s) above, as with Evil's default 'o'. If the
current line does not have any indentation, use the indentation
of the the closest line above.

	Otherwise, if `above' is nil, insert a newline below, and
indent relative to the current line. If the current line does not
have any indentation, use the indentation of the the closest line
above."
	(interactive)
	(let*
		(; bindings
			(pos-initial (point))
			(lw-initial (kakapo-lw))
			(lw "")
			(lc "")
			(lw-nearest (kakapo-lw-search above))
		)
		(cond
			; If we're on the first line, and we want to open above, add a
			; newline above at the first column, disregarding all issues about
			; indentation.
			((and above (eq (line-beginning-position) (point-min)))
				(progn
					(beginning-of-line)
					(insert "\n")
					(forward-line -1)
					(evil-append nil)
				)
			)
			((not (string-match " " lw-nearest))
				(progn
					(if above (forward-line -1))
					(end-of-line)
					(insert (concat "\n" lw-nearest))
					(evil-append nil)
				)
			)
			(t
				(error
					(concat
						"<< SPACE-BASED INDENTATION DETECTED ON "
						(cond
							((string-match " " lw-initial) "CURRENT LINE")
							(above  "NEAREST LINE ABOVE")
							(t      "NEAREST LINE BELOW")
						)
						" >>"
					)
				)
			)
		)
	)
)

;;;###autoload
(define-minor-mode kakapo-mode
	"Stupid TAB character."
	:init-value nil
	:lighter "/ki"
	:global nil
	:keymap (let ((map (make-sparse-keymap)))
			(define-key map (kbd "TAB") 'kakapo-tab)
			map))

(provide 'kakapo-mode)
