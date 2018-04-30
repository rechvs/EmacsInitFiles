(defun my-view-python-function-doc ()
  "If not already running, start an asynchronous Python3 subprocess named \"py3-doc-gen\" and assosciate it with buffer \"*Python3 Documentation Generator*\".
Prompt for function name FUNC and use the subprocess to generate the documentation for it;
this is the output of either \"print(FUNC.__doc__)\", or, if that is empty, \"help(\"FUNC\")\".
  The documentation is then shown in buffer \"*help[Python3](FUNC)*\"."
  (interactive)
  (let* ((proc-name "py3-doc-gen")
         ;; (assoc-buf-name "*Python3 Documentation Generator*")
         ;; (proc (get-process proc-name))
         (skippable-chars "a-z0-9")
         func
         (func-initial "")
         buf-name
         doc-command
         )

    ;; (unless proc (setq proc (start-process proc-name assoc-buf-name "/usr/bin/python3")))
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
    ;; Create help buffer if necessary. Display help buffer.
    (setq buf-name (concat "*help[Python3](" func ")*"))
    (setq doc-command (concat "print(" func ".__doc__)"))
    (unless (get-buffer buf-name) (start-process proc-name buf-name "/usr/bin/python3" "-c" doc-command))
    (unless (member buf-name (mapcar (lambda (window)
                                       (buffer-name (window-buffer window)))
                                     (window-list)))
      (display-buffer buf-name))
    
    ))
