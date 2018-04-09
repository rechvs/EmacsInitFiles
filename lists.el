(add-to-list 'auto-mode-alist '("\\.dtd\\'" . fundamental-mode))

(add-to-list 'auto-mode-alist '("\\.oct\\'" . octave-mode))
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(add-to-list 'auto-mode-alist '("\\.bbx\\'" . latex-mode))
(add-to-list 'auto-mode-alist '("\\.cbx\\'" . latex-mode))

;; Prevent "openwith" from interfering with email attachments (see https://www.emacswiki.org/emacs/OpenWith).
(add-to-list  'mm-inhibit-file-name-handlers 'openwith-file-handler)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/"))
