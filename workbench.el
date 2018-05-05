(defun my-view-python-object-help ()
  "Prompt for a Python object name, using either the region or the text around point as default. Display the output of \"help(\"OBJECT\")\" in a dedicated buffer."
  (interactive)
  (let* ((cur-buf (current-buffer))
         (proc-name "py3-doc-gen")
         (skippable-chars "a-zA-Z0-9")
         obj
         (obj-def-input "")
         buf-name
         doc-command
         (match-data-old (match-data)))
    (unwind-protect
        (progn
          ;; If the region is active, obtain object name from it.
          (if (region-active-p)
              (setq obj-def-input (buffer-substring-no-properties (region-beginning) (region-end)))
            ;; If the region is not active, obtain the object name by scanning for text at point.
            (let (p1 p2)
              (save-excursion (skip-chars-backward skippable-chars (point-min))
                              (setq p1 (point)))
              (save-excursion (skip-chars-forward skippable-chars (point-max))
                              (setq p2 (point)))
              (setq obj-def-input (buffer-substring-no-properties p1 p2))))
          ;; Prompt for object name.
          (setq obj (read-string (if (string= "" obj-def-input) "Object: " (concat "Object (default " obj-def-input "): ")) nil nil obj-def-input))
          ;; Generate and display help buffer if necessary.
          (setq buf-name (concat "*help[Python3](" obj ")*"))
          (if (member buf-name (mapcar (lambda (window)
                                         (buffer-name (window-buffer window)))
                                       (window-list)))
              (display-message-or-buffer (concat "Python documentation for " obj " already displayed."))
            (setq doc-command (concat "help(\"" obj "\")"))
            (unless (get-buffer buf-name)
              (start-process proc-name buf-name "/usr/bin/python3" "-c" doc-command)
              (while (get-process proc-name) (sleep-for 0.01))
              (set-buffer buf-name)
              (goto-char (point-min))
              (if (looking-at (concat "No Python documentation found for '" obj "'"))
                  (let ((error-message (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
                    (kill-buffer buf-name)
                    (error "%s" error-message))
                (while (re-search-forward (concat "\nProcess " proc-name " finished\n") (point-max) t)
                  (replace-match ""))
                (goto-char (point-max))
                (delete-blank-lines))
              (set-buffer-modified-p nil)
              (read-only-mode)
              (help-mode)                 ; Temporary fix until I have written a proper major mode for viewing (and quitting) Python help buffers.
              (goto-char (point-min)))
            (switch-to-buffer-other-window buf-name)
            (other-window 1))
          (if (region-active-p) (deactivate-mark)))
      (set-match-data match-data-old)
      (set-buffer cur-buf))))
