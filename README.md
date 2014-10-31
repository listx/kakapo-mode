#kakapo-mode

Use TABS or SPACES (but not both!) for indentation (leading whitespace), and SPACES for everything else.

##Install

See "Installation" in `kakapo-mode.el`.

##Example

As the "Installation" section in `kakapo-mode.el` suggests, you might want to put in the following in your `~/.emacs.d/init.el`:

    (require 'kakapo-mode)
	(add-hook 'text-mode-hook 'kakapo-mode)
	(add-hook 'prog-mode-hook 'kakapo-mode)
	; `kakapo-open` *requires* evil-mode!
	(define-key evil-normal-state-map "o" (lambda () (interactive) (kakapo-open nil)))
	(define-key evil-normal-state-map "O" (lambda () (interactive) (kakapo-open t)))
	(define-key evil-insert-state-map (kbd "RET") 'kakapo-ret-and-indent)
	(define-key evil-insert-state-map (kbd "DEL") 'kakapo-backspace)
	(define-key evil-insert-state-map (kbd "<S-backspace>") 'kakapo-upline)

. This is the simplest setup; it is important to note that the `kakapo-open` function calls `(evil-append nil)` in its body and therefore requires `evil-mode` to work properly.
If you do not use `evil-mode`, you probably don't need to use `kakapo-open` at all; still, change the calls to `define-key` to work with your non-`evil-mode` setup.

### Advanced Example

You can choose to unify all of your indentation settings (and also enable per-project settings) by creating a file `kakapo-project-settings.el` in your `~/.emacs.d/` folder or some such, and then load it into your `.emacs.d/init.el`.
Your `~/.emacs.d/init.el` will now look like this:

	(load "~/.emacs.d/kakapo-project-settings")
	(require 'kakapo-mode)
	(add-hook 'prog-mode-hook 'my-kakapo-indents)
	(add-hook 'text-mode-hook 'my-kakapo-indents)
	; `kakapo-open` *requires* evil-mode!
	(define-key evil-normal-state-map "o" (lambda () (interactive) (kakapo-open nil)))
	(define-key evil-normal-state-map "O" (lambda () (interactive) (kakapo-open t)))
	(define-key evil-insert-state-map (kbd "RET") 'kakapo-ret-and-indent)
	(define-key evil-insert-state-map (kbd "DEL") 'kakapo-backspace)
	(define-key evil-insert-state-map (kbd "<S-backspace>") 'kakapo-upline)

. Notice how the `add-hook` calls the function `my-kakapo-indents`, which is defined in `kakapo-project-settings.el`.
You can then set `kakapo-project-settings.el` as follows:

	(defun my-add-hook (hook tmode twidth &optional func)
		(lexical-let
			(
				(tmode tmode)
				(twidth twidth)
				(func func)
			)
			(add-hook hook
				(lambda ()
					(kakapo-mode)
					(setq indent-tabs-mode tmode)
					(setq tab-width twidth)

					; this is evil-specific; omit this line if you do not use evil
					(setq evil-shift-width twidth)

					func
				)
			)
		)
	)

	; Adapted from KimStorm's solution from http://www.emacswiki.org/ProjectSettings
	(defun my-kakapo-indents ()
		(let
			(
				(b (buffer-file-name))
			)
			(defun h (hook tmode twidth &optional func)
				(my-add-hook hook tmode twidth func)
			)
			(if b
			(cond
				; a project called 'depot'
				((string-match "webdev/depot/.+\\.rb$" b)
					(h 'ruby-mode-hook nil 2
						(progn
							(setq ruby-indent-level 2)
							(message
								"ruby-indent-level set to %d"
								ruby-indent-level
							)
						)
					)
				)
				; a project called 'fido'
				((string-match "webdev/fido/.+\\.rb$" b)
					(h 'ruby-mode-hook nil 2
						(progn
							(setq ruby-indent-level 2)
							(message
								"ruby-indent-level set to %d"
								ruby-indent-level
							)
						)
					)
				)

				; Default options by language

				; C
				((string-match "\\.[ch]$" b)
					(h 'c-mode-hook t 8
						(progn
							(setq default-tab-width 8)
							(message
								"default-tab-width set to %d"
								default-tab-width
							)
						)
					)
				)
				; C++
				((string-match "\\.[ch]pp$" b)
					(h 'c++-mode-hook t 8
						(progn
							(setq default-tab-width 8)
							(message
								"default-tab-width set to %d"
								default-tab-width
							)
						)
					)
				)
				; Emacs lisp
				((string-match "\\.el$" b)
					(h 'emacs-lisp-mode-hook t 4)
				)
				; Haskell
				((string-match "\\.[l]?hs$" b)
					(h 'haskell-mode-hook t 4)
				)
				; Hazelnut
				((string-match "\\.hzl$" b)
					(h 'text-mode-hook t 4)
				)
				; HTML
				((string-match "\\.htm[l]?$" b)
					(h 'html-mode-hook t 4)
				)
				; HTML + ERB
				((string-match "\\.html\\.erb?$" b)
					(h 'html-mode-hook nil 2)
				)
				; Latex
				((string-match "\\.tex$" b)
					(h 'latex-mode-hook t 4)
				)
				; Markdown
				((string-match "\\.md$" b)
					(h 'markdown-mode-hook t 4)
				)
				; Python
				((string-match "\\.py$" b)
					(h 'python-mode-hook t 4
						(progn
							(setq python-indent 4)
							(message "python-indent set to %d" python-indent)
						)
					)
				)
				; Ruby
				((string-match "\\.rb$" b)
					(h 'ruby-mode-hook t 4
						(progn
							(setq ruby-indent-level 4)
							(message
								"ruby-indent-level set to %d"
								ruby-indent-level
							)
						)
					)
				)
				; Shell
				((string-match "\\.sh$" b)
					(h 'sh-mode-hook t 4)
				)
			)
			)
		)
	)

Now, you have per-project indentation settings that are different than the default ones.
If you know emacs lisp, you can probably write a less repetitive block of code than the above.
