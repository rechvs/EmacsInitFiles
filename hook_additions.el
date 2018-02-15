(add-hook 'after-init-hook 'my-customize-interface)
(add-hook 'after-init-hook 'my-load-bookmarks-on-startup)

(add-hook 'before-save-hook 'time-stamp)

(add-hook 'bookmark-bmenu-mode-hook 'hl-line-mode)

(add-hook 'Buffer-menu-mode-hook 'hl-line-mode)

(add-hook 'diff-mode-hook
	(lambda ()
	  (setq my-back-to-indentation-skip-chars "	 +-")
	  (local-set-key "\M-m" 'my-back-to-indentation)))

(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook 'my-customize-interface)

(add-hook 'emacs-lisp-mode-hook 'my-Emacs-Lisp-mode-bindings)

(add-hook 'ess-mode-hook 'my-ESS_S_-mode-bindings)

(add-hook 'gnus-group-mode-hook 'hl-line-mode)
(add-hook 'gnus-summary-mode-hook 'hl-line-mode)

(add-hook 'LaTeX-mode-hook '(lambda () (setq TeX-command-default "LatexMake")))	; The string must correspond to a handle in variable "TeX-command-list".
(add-hook 'LaTeX-mode-hook 'my-LaTeX/P-mode-bindings)
(add-hook 'LaTeX-mode-hook 'my-LaTeX/P-mode-symbol-additions)
(add-hook 'LaTeX-mode-hook 'my-visual-line-mode-on)
(add-hook 'LaTeX-mode-hook 'my-abbrev-mode-on)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(add-hook 'lisp-interaction-mode-hook 'my-Lisp-Interaction-mode-bindings)

(add-hook 'octave-mode-hook 'my-Octave-mode-bindings)

(add-hook 'org-mode-hook 'my-Org-mode-bindings)

(add-hook 'sh-mode-hook 'my-Shell-script-mode-bindings)

(add-hook 'shell-mode-hook '(lambda ()
			(setq comint-scroll-show-maximum-output nil) ; Deactivate fancy scrolling due to interpreter output.
			(setq scroll-conservatively 101) ; Deactivate automatic scrolling if point moves outside the window.
			))

(add-hook 'vc-dir-mode-hook 'hl-line-mode)

(add-hook 'view-mode-hook 'my-customize-interface)
