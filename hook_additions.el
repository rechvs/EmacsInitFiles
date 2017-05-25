(add-hook 'after-init-hook 'my-customize-interface)
(add-hook 'after-init-hook 'my-load-bookmarks-on-startup)

(add-hook 'before-save-hook 'time-stamp)

(add-hook 'bookmark-bmenu-mode-hook 'hl-line-mode)

(add-hook 'Buffer-menu-mode-hook 'hl-line-mode)

(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook 'my-customize-interface)

(add-hook 'emacs-lisp-mode-hook 'my-Emacs-Lisp-mode-bindings)

(add-hook 'ess-mode-hook 'my-ESS_S_-mode-bindings)

(add-hook 'LaTeX-mode-hook 'my-LaTeX/P-mode-bindings)
(add-hook 'LaTeX-mode-hook 'my-LaTeX/P-mode-symbol-additions)
(add-hook 'LaTeX-mode-hook 'my-visual-line-mode-on)
(add-hook 'LaTeX-mode-hook 'my-abbrev-mode-on)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(add-hook 'lisp-interaction-mode-hook 'my-Lisp-Interaction-mode-bindings)

(add-hook 'octave-mode-hook 'my-Octave-mode-bindings)

(add-hook 'org-mode-hook 'my-Org-mode-bindings)

(add-hook 'view-mode-hook 'my-customize-interface)
