(defun my-find-region-or-at-point (&optional arg)
  "If region is active, use text in region as the filename to visit. Otherwise use the text at point as the filename to visit. The characters delimiting the filename from surrounding text are set via  `my-find-region-or-at-point-delim-chars'.
Without prefix argument ARG, a non-existent filename results in an error.
An empty filename results in an error."
  (interactive "P")
  (let ((flnm (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (buffer-substring-no-properties
                 (save-excursion (skip-chars-backward (concat "^" my-find-region-or-at-point-delim-chars) (line-beginning-position)) (point))
                 (save-excursion (skip-chars-forward (concat "^" my-find-region-or-at-point-delim-chars) (line-end-position)) (point))))))
    ;; If flnm is empty, message the user about it, but do not visit it.
    (if (string= "" flnm)
        (error "Empty filename")
      ;; If arg is non-nil, deactivate region (if applicable) and visit the file.
      (if arg
          (progn
            (if (region-active-p) (deactivate-mark t))
            (find-file flnm))
        ;; Else, if flnm exists, deactivate region (if applicable) and visit the file.
        (if (file-exists-p flnm)
            (progn
              (if (region-active-p) (deactivate-mark t))
              (find-file flnm))
          ;; If flnm does not exist, signal an error.
          (error "File %s does not exist" flnm))))))
