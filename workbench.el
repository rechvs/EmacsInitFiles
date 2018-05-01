(defun my-view-python-function-doc ()
  "If not already running, start an asynchronous Python3 subprocess named \"py3-doc-gen\" and assosciate it with buffer \"*Python3 Documentation Generator*\".
Prompt for function name FUNC and use the subprocess to generate the documentation for it;
this is the output of either \"print(FUNC.__doc__)\", or, if that is empty, \"help(\"FUNC\")\".
  The documentation is then shown in buffer \"*help[Python3](FUNC)*\"."
  (interactive)
  (let* ((cur-buf (current-buffer))
         (proc-name "py3-doc-gen")
         (skippable-chars "a-zA-Z0-9")
         func
         (func-initial "")
         buf-name
         doc-command
         (match-data-old (match-data))
         )
    (unwind-protect
        (progn
          (if (and func (not (stringp func))) (error "Function name must be a string"))
          ;; If the region is active, obtain function name from it.
          (if (region-active-p)
              (setq func-initial (buffer-substring-no-properties (region-beginning) (region-end)))
            ;; If the region is not active, obtain the function name by scanning for text at point.
            (let (p1 p2)
              (save-excursion (skip-chars-backward skippable-chars (point-min))
                              (setq p1 (point)))
              (save-excursion (skip-chars-forward skippable-chars (point-max))
                              (setq p2 (point)))
              (setq func-initial (buffer-substring-no-properties p1 p2))))
          ;; Prompt for function name
          (setq func (completing-read "Function: " nil nil nil func-initial))
          ;; Generate and display help buffer if necessary.
          (setq buf-name (concat "*help[Python3](" func ")*"))
          (setq doc-command (concat "help(\"" func "\")"))
          (unless (get-buffer buf-name)
            (start-process proc-name buf-name "/usr/bin/python3" "-c" doc-command)
            (while (get-process proc-name) (sleep-for 0.01)))
          (set-buffer buf-name)
          (goto-char (point-min))
          (if (looking-at (concat "No Python documentation found for '" func "'"))
              (let ((error-message (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
                (kill-buffer buf-name)
                (error "%s" error-message))
            (while (re-search-forward (concat "\nProcess " proc-name " finished\n") (point-max) t)
              (replace-match ""))
            (goto-char (point-max))
            (delete-blank-lines))
          (set-buffer-modified-p nil)
          (read-only-mode)
          (if (member buf-name (mapcar (lambda (window)
                                         (buffer-name (window-buffer window)))
                                       (window-list)))
              (display-message-or-buffer (concat "Python documentation for " func " already displayed."))
            (switch-to-buffer-other-window buf-name)
            (goto-char (point-min))
            (other-window 1)))
      (set-match-data match-data-old)
      (set-buffer cur-buf))))
