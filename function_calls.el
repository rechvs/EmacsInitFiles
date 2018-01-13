;; Visit all files mentioned in "my-files-to-visit-at-startup", then visit the file mentioned in "initial-buffer-choice".
(my-visit-multiple-files my-files-to-visit-at-startup)
(find-file initial-buffer-choice)

;; Activate Openwith mode.
(openwith-mode)
