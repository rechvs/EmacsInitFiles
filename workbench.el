(defun my-visit-file-add-to-git-whitelist (&optional arg)
  "If called with prefix argument ARG, prompt for git repository directory, otherwise use \"~/\" as default. Prompt for filename. If the file is accessible and not already mentioned in gitignore file, ask for comment and add comment and negated filename to it. Visit the file."
  (interactive "P")
  (let (bl-start
        bl-end
        cmmnt
        curbuflst
        dir
        direxp
        file-is-dir
        flnm-for-visit
        flnm
        flnmexp
        gitigbuf
        gitigflnm
        gitigflnmexp
        git-repo-dir
        (home-dir-path "~/")
        wl
        wl-start
        wl-end)
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
        ;; Check whether the specified file is a directory.
        (if (file-directory-p flnm) (setq file-is-dir t))
        ;; Store expansion of "flnm" in "flnmexp".
        (setq flnmexp (expand-file-name flnm))
        ;; Ensure that the file is actually trackable by the git repository.
        (if (not (string-match-p (regexp-quote (expand-file-name git-repo-dir)) flnmexp))
	  (error (concat "File " flnm " not trackable by Git repository " git-repo-dir)))
        ;; Store directory component of "flnm" and "flnmexp" in "dir" and "direxp", respectively, while ensuring that both end with "/".
        (setq dir (file-name-directory flnm))
        (if (not (string= "/" (substring dir -1)))
	  (setq dir (concat dir "/")))
        (setq direxp (file-name-directory flnmexp))
        (if (not (string= "/" (substring direxp -1)))
	  (setq direxp (concat direxp "/")))
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
        ;; Remove the provided Git repo directory or its expansion from the beginning of "dir", "direxp", "flnm", and "flnmexp".
        (setq dir (replace-regexp-in-string (concat "\\(^" git-repo-dir "\\)\\|\\(^" (expand-file-name git-repo-dir) "\\)") "" dir))
        (setq direxp (replace-regexp-in-string (concat "\\(^" git-repo-dir "\\)\\|\\(^" (expand-file-name git-repo-dir) "\\)") "" direxp))
        (setq flnm (replace-regexp-in-string (concat "\\(^" git-repo-dir "\\)\\|\\(^" (expand-file-name git-repo-dir) "\\)") "" flnm))
        (setq flnmexp (replace-regexp-in-string (concat "\\(^" git-repo-dir "\\)\\|\\(^" (expand-file-name git-repo-dir) "\\)") "" flnmexp))
        ;; Visit "gitigflnmexp" and store the resulting buffer in "gitigbuf".
        (save-excursion
	(setq gitigbuf (find-file-noselect gitigflnmexp))
	;; Make "gitigbuf" the current buffer.
	(set-buffer gitigbuf)
	(goto-char (point-min))
	;; Create Elisp list from whitelist.
	(goto-char (point-min))
	(if (search-forward-regexp "^# WHITELIST START" (point-max) t)
	    (progn
	      (move-beginning-of-line 2)
	      (setq wl-start (point))))
	(if (and wl-start (search-forward-regexp "^# WHITELIST END" (point-max)))
	    (progn
	      (move-end-of-line 0)
	      (setq wl-end (point))))
	(if (and wl-start wl-end)
	    (setq wl (split-string (buffer-substring-no-properties wl-start wl-end) "\n")))
	;; TODO: add mechanism for whitelisting "dir" (i.e., the path component of "flnm") without whitelisting the same directory twice (solution: create an Elisp list from the whitelist, then apply "(delete-dups ...)" to it
	;; BEGIN TESTING
	;; (setq test (mapcar (lambda (elt)
			 ;; (set-text-properties 0 (length elt) nil elt)
			 ;; elt)
		         ;; (list dir direxp flnm flnmexp)))
	;; (display-message-or-buffer (prin1-to-string test))
	(display-message-or-buffer (prin1-to-string wl))
	)))))
;; END TESTING
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

	;; TODO: check whether the specified file is a directory (because (recursively) whitelisting a directory requires different syntax than whitelisting a single file) 
	(if file-is-dir
	    (progn
	      ;; If "flnm" starts with a "/", remove it.
	      (if (string= "/" (substring flnm 0 1))
		(setq flnm (substring flnm 1)))
	      ;; Ensure that "flnm" ends with "/**".
	      (setq flnm (concat (if (string= "/" (substring flnm -1))
			         "**"
			       "/**")))
			         
			         ;; CONTINUE HERE
			         )))))

	;; TODO: add mechanism for ensuring the whitelist to be lexicographically ordered 
	;; Append comment, negated "flnm" and trailing newline to "gitigbuf".
	(insert cmmnt "\n" "!" flnm "\n")
	;; Save "gitigbuf".
	(save-buffer)
	;; If "gitigbuf" is not a member of "curbuflst", kill "gitigbuf".
	(if (not (member gitigbuf curbuflst))
	    (kill-buffer gitigbuf))))
      ;; Visit the specified file.
      (find-file flnm-for-visit))))


(let (dir
      dirs-list
      (dirs-negated "!")
      (flnm "~/Mail/Privat/Eingang/1")
      (format-string "")
      (git-project-dir "~/"))
  (setq dirs-list (delq "" (split-string (file-name-directory (replace-regexp-in-string git-project-dir "" flnm)) "/")))
  (setq dirs-negated (mapcar (lambda (elt)
			      (setq dirs-negated (concat dirs-negated elt "/")))
			    dirs-list))
  (setq format-string (apply 'concat (make-list (length dirs-negated) "%s\n")))
  (setq format-string (substring format-string 0 -1))
  ;; (setq format-string (concat (substring format-string 0 -1) "**"))
  (setq dirs-negated (apply 'format format-string dirs-negated))
  ;; TESTING
  ;; dirs-negated
  ;; TESTING
  )
