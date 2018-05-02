(add-hook 'after-init-hook 'my-customize-interface)
(add-hook 'after-init-hook 'my-load-bookmarks-on-startup)

(add-hook 'before-save-hook 'time-stamp)

(add-hook 'bookmark-bmenu-mode-hook 'hl-line-mode)

(add-hook 'Buffer-menu-mode-hook 'hl-line-mode)

(add-hook 'diff-mode-hook '(lambda () (setq my-move-beginning-of-line-skip-regexp "^[ +-]?[ 	]*")))

(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook 'my-customize-interface)

(add-hook 'elpy-mode-hook 'my-Elpy-mode-bindings)

(add-hook 'emacs-lisp-mode-hook 'my-Emacs-Lisp-mode-bindings)
(add-hook 'emacs-lisp-mode-hook '(lambda () (setq my-move-end-of-line-skip-regexp "\\([ 	]+;.*\\)\\|\\([ 	]+\\)")))

(add-hook 'ess-mode-hook 'my-ESS_S_-mode-bindings)

(add-hook 'gnus-group-mode-hook 'hl-line-mode)
(add-hook 'gnus-summary-mode-hook 'hl-line-mode)
(add-hook 'gnus-summary-mode-hook '(lambda () (setq my-move-beginning-of-line-skip-regexp "[A ]+([MDFSoira, 0-9:-]*)")))
(add-hook 'gnus-summary-mode-hook 'my-gnus-summary-mode-bindings)
(add-hook 'gnus-summary-mode-hook 'my-set-message-signature)

(add-hook 'inferior-python-mode-hook 'anaconda-eldoc-mode)
(add-hook 'inferior-python-mode-hook 'anaconda-mode)
(add-hook 'inferior-python-mode-hook 'my-Inferior-Python-mode-bindings)

(add-hook 'LaTeX-mode-hook '(lambda () (setq my-move-end-of-line-skip-regexp "\\([ 	]+%+.*\\)\\|\\([ 	]+\\)")))
(add-hook 'LaTeX-mode-hook '(lambda () (setq TeX-command-default "LatexMake"))) ; The string must correspond to a handle in variable "TeX-command-list".
(add-hook 'LaTeX-mode-hook 'my-LaTeX/P-mode-bindings)
(add-hook 'LaTeX-mode-hook 'my-LaTeX/P-mode-symbol-additions)
(add-hook 'LaTeX-mode-hook 'my-visual-line-mode-on)
(add-hook 'LaTeX-mode-hook 'my-abbrev-mode-on)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(add-hook 'lisp-interaction-mode-hook 'my-Lisp-Interaction-mode-bindings)

(add-hook 'octave-mode-hook 'my-Octave-mode-bindings)

(add-hook 'plain-tex-mode-hook 'my-LaTeX/P-mode-bindings)

(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'my-Python-mode-bindings)

(add-hook 'org-mode-hook 'my-Org-mode-bindings)
(add-hook 'org-mode-hook '(lambda () (setq my-move-beginning-of-line-skip-regexp "^\\(\\*+ +\\| +[+-] \\(\\[[ X-]\\] \\)?\\| +[0-9]+\\. \\(\\[[ X-]\\] \\)?\\| +\\)")))

(add-hook 'sh-mode-hook 'my-Shell-script-mode-bindings)

(add-hook 'shell-mode-hook '(lambda ()
                              (setq comint-scroll-show-maximum-output nil) ; Deactivate fancy scrolling due to interpreter output.
                              (setq scroll-conservatively 101) ; Deactivate automatic scrolling if point moves outside the window.
                              ))

(add-hook 'vc-dir-mode-hook 'hl-line-mode)

(add-hook 'view-mode-hook 'my-customize-interface)
