# kakapo-mode

[![MELPA](https://melpa.org/packages/kakapo-mode-badge.svg)](https://melpa.org/#/kakapo-mode)

Use TABS or SPACES (but not both!) for indentation (leading whitespace), and SPACES for everything else.

## Install

You can find this package in [MELPA](http://melpa.org/#/kakapo-mode), and just install with the `list-packages` command in Emacs.
For a guide on adding MELPA to your Emacs setup, see [this page](http://ergoemacs.org/emacs/emacs_package_system.html).

## Basic Configuration

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

## Advanced Configuration

You can choose to unify all of your indentation settings (and also enable per-project settings) by creating a file `kakapo-project-settings.el` in your `~/.emacs.d/` folder or some such, and then load it into your `.emacs.d/init.el`.
Your `~/.emacs.d/init.el` will now look like this:

	(load "~/.emacs.d/kakapo-project-settings")
	(require 'kakapo-mode)
	(add-hook 'prog-mode-hook 'l/kakapo-indents)
	(add-hook 'text-mode-hook 'l/kakapo-indents)
	(add-hook 'conf-mode-hook 'l/kakapo-indents)
	(add-hook 'css-mode-hook 'l/kakapo-indents)
	; `kakapo-open` *requires* evil-mode!
	(define-key evil-normal-state-map "o" (lambda () (interactive) (kakapo-open nil)))
	(define-key evil-normal-state-map "O" (lambda () (interactive) (kakapo-open t)))
	(define-key evil-insert-state-map (kbd "RET") 'kakapo-ret-and-indent)
	(define-key evil-insert-state-map (kbd "DEL") 'kakapo-backspace)
	(define-key evil-insert-state-map (kbd "<S-backspace>") 'kakapo-upline)

. Notice how the `add-hook` calls the function `l/kakapo-indents`, which is defined in `kakapo-project-settings.el`.
You can then set `kakapo-project-settings.el` as follows:

	(defun l/add-hook (hook tmode twidth &optional func)
	(add-hook hook `(lambda ()
		(when (not l/kakapo-settings-applied)
		(kakapo-mode)
		(electric-indent-mode -1)
		(setq indent-tabs-mode ,tmode)
		(setq tab-width ,twidth)
		(setq evil-shift-width ,twidth)
		(setq l/kakapo-settings-applied t)
		(eval ,func)
		(message (concat
			(if (boundp 'l/kakapo-project-id)
			(concat "kakapo project `" l/kakapo-project-id "'")
			"kakapo defaults") ": "
			"indent-tabs-mode=%s "
			"tab-width=%s "
			"evil-shift-width=%s ")
			indent-tabs-mode
			tab-width
			evil-shift-width)))))

	; Check if a buffer's name matches the a project's path (regex). If it does,
	; then also set the project-id.
	(defun l/kakapo-set-project-id (project-id project-regex buffer-name)
	(when (string-match project-regex buffer-name)
		(setq l/kakapo-project-id project-id)))

	; Adapted from KimStorm's solution from http://www.emacswiki.org/ProjectSettings.
	(defun l/kakapo-indents ()
	(let
		(
		(b (buffer-file-name))
		(h (getenv "HOME")))
		(defun h (hook tmode twidth &optional func)
		(l/add-hook hook tmode twidth func))
		(setq l/kakapo-settings-applied nil)
		; Default settings for unrecognized languages.
		(kakapo-mode)
		(setq indent-tabs-mode t)
		(setq tab-width 4)
		(setq evil-shift-width 4)
		; Language-specific settings.
		(when b
		; C
		(h 'c-mode-hook t 8
			'(progn
			(setq default-tab-width 8)))
		; C++
		(h 'c++-mode-hook t 8
			'(progn
			(setq default-tab-width 8)))
		; CSS
		(h 'css-mode-hook nil 2
			'(progn
			(setq css-indent-offset 2)))
		; Emacs lisp
		(h 'emacs-lisp-mode-hook nil 2)
		; Haskell
		(h 'haskell-mode-hook nil 2)
		; Literate Haskell.
		(if (string-match "\\.lhs$" b)
			(progn
			(h 'latex-mode-hook nil 2)
			(setq mmm-global-mode 't)
			(setq mmm-submode-decoration-level 1)
			(mmm-ify-by-class 'literate-haskell-latex)
			(column-enforce-mode)))
		; HTML
		(h 'html-mode-hook t 4)
		; Idris
		(h 'idris-mode-hook nil 2)
		; Latex
		(h 'latex-mode-hook nil 2)
		; Markdown
		(h 'markdown-mode-hook t 4
			'(progn
			(define-key markdown-mode-map [backspace] nil)
			(define-key markdown-mode-map [tab] nil)))
		; Nix expression language
		(h 'nix-mode-hook nil 2)
		; Org-mode
		(h 'org-mode-hook nil 2)
		; Python
		(h 'python-mode-hook nil 4
			'(progn
			(setq python-indent 4)))
		; Ruby
		(h 'ruby-mode-hook nil 2
			'(progn
			(setq ruby-indent-level 2)))
		; Shell
		(h 'sh-mode-hook t 4)
		; Shen
		(h 'shen-mode-hook nil 2)

		; Additional project-specific settings.
		(if (or
			(l/kakapo-set-project-id "elementary-haskell" (concat h "/prog/elementary-haskell/") b)
			(l/kakapo-set-project-id "codex" "prog/codex/" b)
			(l/kakapo-set-project-id "new-keyboard (esrille nisse)" (concat h "/prog/nisse/new-keyboard/") b))
			(progn
			(h 'haskell-mode-hook nil 4)
			(h 'c-mode-hook nil 4
				'(progn
				(setq default-tab-width 4)))
			(h 'python-mode-hook nil 4
				'(progn
				(setq python-indent 4)))))
		(if (or
			(l/kakapo-set-project-id "miro" "prog/miro/" b)
			(l/kakapo-set-project-id "lovelace" "prog/lovelace/" b))
			(progn
			(add-hook 'haskell-mode-hook 'l/haskell-intero-setup)
			(h 'haskell-mode-hook nil 2)
			(h 'c-mode-hook t 8
				'(progn
				(setq default-tab-width 8)))
			(h 'python-mode-hook nil 4
				'(progn
				(setq python-indent 4)))
			(h 'sh-mode-hook t 4)))

			(if (l/kakapo-set-project-id "gv" "prog/gv/" b)
			(h 'sh-mode-hook nil 2)))))

Now, you have per-project indentation settings that are different than the default ones.
If you know emacs lisp, you can probably write a less repetitive block of code than the above.

## Quirks

### Haskell

The `haskell-cabal-mode` major mode that comes with `haskell-mode`, as of 2015-01-13 seems to remap keys and somehow ignore Kakapo mode.
You can still make Kakapo work by doing the following:

```
(evil-define-key 'insert haskell-cabal-mode-map (kbd "<tab>") 'kakapo-tab)
(evil-define-key 'insert haskell-cabal-mode-map (kbd "DEL") 'kakapo-backspace)
(add-hook 'haskell-cabal-mode-hook
	(lambda ()
		(kakapo-mode)
		(setq indent-tabs-mode nil)
		(setq tab-width 2)
		(setq evil-shift-width 2)
	)
)

```

## How to contribute

1. Fork it to your Github account.
2. Create a new branch named after your feature/bugfix/intention.
3. Push your branch to your forked repo.
4. Create a pull request!

Ideally, I'd love to use Git's own policy of having a read-only official repo, with the development/discussion taking place in a mailing list.
But as this project is so small, the above way is probably best.

## Bugs? Comments? Issues? Concerns? Opinions?

Please don't hesitate to write comments, suggestions, and/or observations to the issue tracker.
Click on the green "New issue" button to voice your concerns.
You might write, "`kakapo-mode` totally sucks."
And, maybe it does!
But please tell us *why* it sucks, so that we can hopefully make it better.
