(defun my-visit-file-add-to-git-whitelist (&optional arg)
  "If called with prefix argument ARG, prompt for git repository directory, otherwise use \"~/\" as default. Prompt for filename. If the file is accessible and not already mentioned in gitignore file, ask for comment and add comment and negated filename to it. Visit the file."
  (interactive "P")
  (let (cmmnt
        curbuflst
        dir
        flnm-for-visit
        flnm
        flnmexp
        gitigbuf
        gitigflnm
        gitigflnmexp
        git-repo-dir
        (home-dir-path "~/"))
    (catch 'outer
      (catch 'inner
        ;; Store current buffer list in "curbuflst".
        (setq curbuflst (buffer-list))
        ;; If called with (prefix) argument, prompt for git repository directory, otherwise set it to "/~".
        (setq git-repo-dir (if arg (read-directory-name (concat "Git repository directory: "))
			  home-dir-path))
        ;; Store path to gitignore file in "gitigflnm".
        (setq gitigflnm (concat git-repo-dir ".gitignore"))
        ;; Store expansion of "gitigflnm" in "gitigflnmexp".
        (setq gitigflnmexp (expand-file-name gitigflnm))
        ;; Ask for filename, store it in "flnm" and "flnm-for-visit".
        (setq flnm (read-file-name "Filename: "))
        (setq flnm-for-visit flnm)
        ;; Store expansion of "flnm" in "flnmexp".
        (setq flnmexp (expand-file-name flnm))
        ;; Ensure that the file is actually trackable by the git repository.
        (if (not (string-match-p (regexp-quote (expand-file-name git-repo-dir)) flnmexp))
	  (error (concat "File " flnm " not trackable by Git repository " git-repo-dir)))
        ;; Store directory component of "flnm" in "dir".
        (setq dir (file-name-directory flnm))
        ;; Add text properties to strings for pretty printing.
        (add-face-text-property 0 (length dir) '(:foreground "blue") t dir)
        (add-face-text-property 0 (length flnm) '(:foreground "blue") t flnm)
        (add-face-text-property 0 (length flnmexp) '(:foreground "blue") t flnmexp)
        (add-face-text-property 0 (length gitigflnm) '(:foreground "blue") t gitigflnm)
        ;; Check whether "flnm" and "dir" are accessible/readable/writable, stop if not.
        (if (not (file-accessible-directory-p (file-name-directory flnm)))
	  (throw 'outer (message "Directory %s is not accessible." dir)))
        (if (and (file-exists-p flnm) (not (file-readable-p flnm)))
	  (throw 'outer (message "File %s is not readable." flnm)))
        (if (and (file-exists-p flnm) (not (file-writable-p flnm)))
	  (throw 'outer (message "File %s is not writable." flnm)))
        ;; Check whether file "gitigflnmexp" is readable/writable, stop if not.
        (if (and (file-exists-p gitigflnmexp) (not (file-readable-p gitigflnmexp)))
	  (throw 'outer (message "File %s is not readable." gitigflnm)))
        (if (and (file-exists-p gitigflnmexp) (not (file-writable-p gitigflnmexp)))
	  (throw 'outer (message "File %s is not writable." gitigflnm)))
        ;; Remove either "~/" or the expansion of "~/" from the beginning of "flnm" and "flnmexp".
        (setq flnm (replace-regexp-in-string (concat "\\(^" git-repo-dir "\\)\\|\\(^" (expand-file-name git-repo-dir) "\\)") "" flnm))
        (setq flnmexp (replace-regexp-in-string (concat "\\(^" git-repo-dir "\\)\\|\\(^" (expand-file-name git-repo-dir) "\\)") "" flnmexp))
        ;; Visit "gitigflnmexp" and store the resulting buffer in "gitigbuf".
        (save-excursion
	(setq gitigbuf (find-file-noselect gitigflnmexp))
	;; Make "gitigbuf" the current buffer.
	(set-buffer gitigbuf)
	(goto-char (point-min))
	;; If the supplied filename (or its expanded equivalent) is already present in "gitigbuf" (either in its blacklist or in its whitelist), inform about it and skip ahead to visiting the specified file.
	(if (search-forward-regexp (concat "^" flnm "$") nil t)
	    (throw 'inner (message "Filename %s is already present in %s's blacklist." flnm gitigflnm)))
	(if (search-forward-regexp (concat "^" flnmexp "$") nil t)
	    (throw 'inner (message "Filename %s is already present in %s's blacklist." flnmexp gitigflnm)))
	(if (search-forward-regexp (concat "^!" flnm "$") nil t)
	    (throw 'inner (message "Filename %s is already present in %s's whitelist." flnm gitigflnm)))
	(if (search-forward-regexp (concat "^!" flnmexp "$") nil t)
	    (throw 'inner (message "Filename %s is already present in %s's whitelist." flnmexp gitigflnm)))
	;; Ask for a comment.
	(setq cmmnt (read-string "Comment (comment character may be omitted): "))
	;; Trim comment of leading and trailing whitespace.
	(setq cmmnt (replace-regexp-in-string "^[ ]+" "" cmmnt))
	(setq cmmnt (replace-regexp-in-string "[ ]+$" "" cmmnt))
	;; If necessary, prepend "# " to the comment.
	(if (not (string-equal "#" (substring cmmnt 0 1)))
	    (setq cmmnt (concat "# " cmmnt)))
	;; If necessary, append a newline to "gitigbuf" (before appending the comment).
	(goto-char (point-max))
	(beginning-of-line)
	(if (= (point) (point-max))
	    (insert "\n")
	  (end-of-line)
	  (insert "\n\n"))
	;; Remove text properties from "flnm".
	(set-text-properties 0 (length flnm) nil flnm)
	;; Append comment, negated "flnm" and trailing newline to "gitigbuf".
	(insert cmmnt "\n" "!" flnm "\n")
	;; Save "gitigbuf".
	(save-buffer)
	;; If "gitigbuf" is not a member of "curbuflst", kill "gitigbuf".
	(if (not (member gitigbuf curbuflst))
	    (kill-buffer gitigbuf))))
      ;; Visit the specified file.
      (find-file flnm-for-visit))))
