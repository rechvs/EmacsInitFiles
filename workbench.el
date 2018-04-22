(defun my-find-region-or-at-point (&optional arg)
  "If region is active, use text in region as the filename to visit. Otherwise use the text at point as the filename to visit. The characters delimiting the filename from surrounding text are set via  `my-find-region-or-at-point-delim-chars'. If it exists, the file is visited via `find-file'. An empty filename is
ignored."
  (interactive "P")
  (let (FILENAME) 
    ;; If the region is active, obtain the filename from it.
    (if (region-active-p)
        (setq FILENAME (buffer-substring-no-properties (region-beginning) (region-end)))
      ;; If the region is not active, obtain the filename by scanning for text at point enclosed in the delimiting characters.
      (let* ((DELIMCHARS (concat "^" my-find-region-or-at-point-delim-chars))
             (P1 (save-excursion (skip-chars-backward DELIMCHARS (line-beginning-position)) (point)))
             (P2 (save-excursion (skip-chars-forward DELIMCHARS (line-end-position)) (point))))
        (setq FILENAME (buffer-substring-no-properties P1 P2))))
    ;; If FILENAME is empty, message the user about it, but do not visit it.
    (if (string= "" FILENAME)
        (message "Empty filename ignored.")
      ;; If prefix argument ARG is non-nil, deactivate region (if applicable) and visit the file.
      (if arg
          (progn
            (if (region-active-p) (deactivate-mark t))
            (find-file FILENAME))
        ;; Else, if FILENAME exists, deactivate region (if applicable) and visit the file.
        (if (file-exists-p FILENAME)
            (progn
              (if (region-active-p) (deactivate-mark t))
              ;; ...and visit FILENAME.
              (find-file FILENAME))
          ;; If FILENAME does not exist, message the user about it.
          ;; In order to distinguish FILENAME from the rest of the message, we can use either quotation marks...
          ;; (message "File \"%s\" does not exist." FILENAME)))))
          ;; ...or text color.
          (add-face-text-property 0 (length FILENAME) '(:foreground "blue") t FILENAME)
          (message "File %s does not exist." FILENAME))))))
