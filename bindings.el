(global-set-key (kbd "C-x C-ö") 'my-find-or-browse-region-or-at-point)

(global-set-key (kbd "M-ö") 'my-immediately-switch-to-buffer)

(global-set-key (kbd "C-c C-m") 'my-man)

(global-set-key (kbd "C-a") 'my-move-beginning-of-line)

(global-set-key (kbd "C-e") 'my-move-end-of-line)

(global-set-key (kbd "C-<") 'my-other-window)

(global-set-key (kbd "C-x C-y") 'my-recenter)

(global-set-key (kbd "<f5>") 'my-revert-buffer-without-confirmation)

(global-set-key (kbd "M-ä") 'my-switch-windows)

;; Org mode key bindings.
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
