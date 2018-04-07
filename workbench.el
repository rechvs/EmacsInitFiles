(defun my-visit-file-add-to-git-whitelist (&optional arg)
  "If called with prefix argument ARG, prompt for git repository directory, otherwise use \"~/\" as default. Prompt for filename. If the file is accessible and not already mentioned in gitignore file, add filename to gitignore whitelist. Visit the file."
  (interactive "P")
  (let (bl
        bl-start
        bl-end
        curbuflst
        dir
        dirs-list
        (dir-tmp "")
        flnm
        flnm-for-visit
        flnmexp
        gitigbuf
        gitigflnm
        gitigflnmexp
        git-repo-dir
        (home-dir-path "~/")
        (match-data-old (match-data))
        wl
        wl-start
        wl-end)
    (unwind-protect
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
	(setq flnm (read-file-name "Filename: " nil nil 'confirm ""))
	(setq flnm-for-visit flnm)
	;; Store expansion of "flnm" in "flnmexp".
	(setq flnmexp (expand-file-name flnm (file-name-directory flnm)))
	;; Ensure that the file is actually trackable by the git repository.
	(if (not (string-match-p (regexp-quote (expand-file-name git-repo-dir)) flnmexp))
	    (error "File %s cannot be tracked by Git repository %s" flnm git-repo-dir))
	;; If "flnm" is a directory, ensure that it and "flnmexp" are correctly formatted for usage in gitignore file.
	(if (file-directory-p flnm)
	    (progn
	      ;; Remove leading "/".
	      (if (string= "/" (substring flnm 0 1)) (setq flnm (substring flnm 1)))
	      (if (string= "/" (substring flnmexp 0 1)) (setq flnmexp (substring flnmexp 1)))
	      ;; Append "/**".
	      (setq flnm (concat (file-name-as-directory flnm) "**"))
	      (setq flnmexp (concat (file-name-as-directory flnmexp) "**"))))
	;; Store directory component of "flnm" in "dir", while ensuring that both end with "/".
	(setq dir (file-name-directory flnm))
	(if (not (string= "/" (substring dir -1)))
	    (setq dir (concat dir "/")))
	;; Add text properties to strings for pretty printing.
	(add-face-text-property 0 (length dir) '(:foreground "blue") t dir)
	(add-face-text-property 0 (length flnm) '(:foreground "blue") t flnm)
	(add-face-text-property 0 (length flnmexp) '(:foreground "blue") t flnmexp)
	(add-face-text-property 0 (length gitigflnm) '(:foreground "blue") t gitigflnm)
	;; Check whether "flnm" and "dir" are accessible/readable/writable, stop if not.
	(if (not (file-accessible-directory-p (file-name-directory flnm)))
	    (error "Directory %s is not accessible." dir))
	(if (and (file-exists-p flnm) (not (file-readable-p flnm)))
	    (error "File %s is not readable." flnm))
	(if (and (file-exists-p flnm) (not (file-writable-p flnm)))
	    (error "File %s is not writable." flnm))
	;; Check whether file "gitigflnmexp" is readable/writable, stop if not.
	(if (and (file-exists-p gitigflnmexp) (not (file-readable-p gitigflnmexp)))
	    (error "File %s is not readable." gitigflnm))
	(if (and (file-exists-p gitigflnmexp) (not (file-writable-p gitigflnmexp)))
	    (error "File %s is not writable." gitigflnm))
	;; Remove the provided Git repo directory or its expansion from the beginning of "dir", "flnm", and "flnmexp".
	(setq dir (replace-regexp-in-string (concat "\\(^" git-repo-dir "\\)\\|\\(^" (expand-file-name git-repo-dir) "\\)") "" dir))
	(setq flnm (replace-regexp-in-string (concat "\\(^" git-repo-dir "\\)\\|\\(^" (expand-file-name git-repo-dir) "\\)") "" flnm))
	(setq flnmexp (replace-regexp-in-string (concat "\\(^" git-repo-dir "\\)\\|\\(^" (expand-file-name git-repo-dir) "\\)") "" flnmexp))
	(save-excursion
	  ;; Visit "gitigflnmexp" and store the resulting buffer in "gitigbuf".
	  (setq gitigbuf (find-file-noselect gitigflnmexp))
	  ;; Make "gitigbuf" the current buffer.
	  (set-buffer gitigbuf)
	  ;; Check whether "gitigbuf" is formatted correctly to be processed by this function.
	  (goto-char (point-min))
	  (if (or (not (progn (goto-char (point-min)) (search-forward-regexp "^# BLACKLIST START.$" (point-max) t)))
		(not (progn (goto-char (point-min)) (search-forward-regexp "^# BLACKLIST END.$" (point-max))))
		(not (progn (goto-char (point-min)) (search-forward-regexp "^# WHITELIST START.$" (point-max) t)))
		(not (progn (goto-char (point-min)) (search-forward-regexp "^# WHITELIST END.$" (point-max) t))))
	      (error "File %s not formatted correctly" gitigflnm))
	  ;; Create Elisp list from blacklist.
	  (goto-char (point-min))
	  (if (search-forward-regexp "^# BLACKLIST START" (point-max) t)
	      (progn
	        (move-beginning-of-line 2)
	        (setq bl-start (point))))
	  (if (and bl-start (search-forward-regexp "^# BLACKLIST END" (point-max)))
	      (progn
	        (move-end-of-line 0)
	        (setq bl-end (point))))
	  (if (> bl-start bl-end) (setq bl-start bl-end))
	  (setq bl (delete "" (split-string (buffer-substring-no-properties bl-start bl-end) "\n")))
	  ;; Check blacklist for consistency.
	  (if (memq t (mapcar (lambda (elt) (string= "!" (substring elt 0 1))) bl))
	      (error "Blacklist in %s contains negated element" gitigflnm))
	  ;; Create Elisp list from whitelist.
	  (goto-char (point-min))
	  (if (search-forward-regexp "^# WHITELIST START" (point-max) t)
	      (progn
	        (move-beginning-of-line 2)
	        (setq wl-start (point))))
	  (if (and wl-start (search-forward-regexp "^# WHITELIST END" (point-max) t))
	      (progn
	        (move-end-of-line 0)
	        (setq wl-end (point))))
	  (if (> wl-start wl-end) (setq wl-start wl-end))
	  (setq wl (delete "" (split-string (buffer-substring-no-properties wl-start wl-end) "\n")))
	  ;; Check whitelist for consistency.
	  (if (memq nil (mapcar (lambda (elt) (string= "!" (substring elt 0 1))) wl))
	      (error "Whitelist in %s contains non-negated element" gitigflnm))
	  ;; Check whether the supplied filename (or its expanded equivalent) is already present in either the black- or the whitelist. If yes, inform about it and skip ahead to visiting the file.
	  (if (memq nil (list (not (member flnm bl)) (not (member flnmexp bl)) (not (member (concat "!" flnm) wl)) (not (member (concat "!" flnmexp) wl))))
	      (throw 'inner (message "Filename %s or its expansion is already present in black- or whitelist in %s." flnm gitigflnm)))
	  ;; Remove text properties from "flnm".
	  (set-text-properties 0 (length flnm) nil flnm)
	  ;; If applicable, create a descending list of directories from the path of "flnm".
	  (if (file-name-directory flnm)
	      (progn
	        (setq dirs-list (delete "" (split-string (file-name-directory flnm) "/")))
	        (setq dirs-list (mapcar (lambda (elt)
				    (setq dir-tmp (concat dir-tmp elt "/")))
				  dirs-list))))
	  ;; Create new whitelist.
	  (setq wl (sort (delete-dups (cl-union (cl-union (list (concat "!" flnm)) (mapcar (lambda (elt) (concat "!" elt)) dirs-list)) wl)) 'string<))
	  ;; Replace whitelist in "gitigbuf" with new one.
	  (setq format-string (apply 'concat (make-list (length wl) "%s\n")))
	  (setq format-string (concat (if (= wl-start wl-end) "\n") (substring format-string 0 -1)))
	  (delete-region wl-start wl-end)
	  (goto-char wl-start)
	  (insert (apply 'format format-string wl))
	  ;; Save "gitigbuf".
	  (save-buffer)))
      ;; If "gitigbuf" is non-nil and not a member of "curbuflst", kill "gitigbuf".
      (if (and gitigbuf (not (member gitigbuf curbuflst)))
	(kill-buffer gitigbuf))
      (set-match-data match-data-old))
    ;; Visit the specified file.
    (find-file flnm-for-visit)))
