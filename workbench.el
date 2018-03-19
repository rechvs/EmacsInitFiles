(defvar my-man-known-programs-only
  nil
  "List of all programs (without sections) known to man.")

(defvar my-man-known-programs-plus-sections
  nil
  "List of all programs (with sections) known to man.")

(defvar my-man-known-sections-only
  nil
  "List of all sections (without programs) known to man.")

(defun my-man ()
  "If region is active, use text in region as the program name for which to display the man page. \"Program name\" here means the actual program name including the man page section number if present. Otherwise use the text at point as the program name. If it exists, visit the man page via `man'. An empty program name is ignored."
  (interactive)
  (let ((cntr 1)
        (format-string "")
        list1
        list2
        list3
        list4
        (match-data-old (match-data)) 
        prog-name
        prog-name-and-sec-num
        sec-num
        sec-num-after
        sec-num-before
        sec-nums-regexp
        string1)
    ;; If not already available, create a list of all combinations of program names and section numbers known to "man".
    (if (or (not my-man-known-programs-plus-sections) (not my-man-known-programs-only) (not my-man-known-sections-only))
        (progn
	;; Store the list of all program names and corresponding section numbers known to man in "list1".
	(setq string1 (shell-command-to-string "apropos -l ."))
	(setq string1 (replace-regexp-in-string " (" "(" (replace-regexp-in-string ",$" "" (replace-regexp-in-string " +- .*\n" "," string1))))
	(setq list1 (split-string string1 ","))
	;; Store all section numbers known to man in "list2".
	(setq list2 (delete-dups (mapcar (lambda
				     (prog-name-plus-sec-num)
				     (if (string-match "(\\(.*\\))$" prog-name-plus-sec-num)
				         (substring prog-name-plus-sec-num (match-beginning 1) (match-end 1))
				       ))
				   list1)))
	;; Create a proper regexp to cover all section numbers based on "list2".
	(while (<= cntr (length list2))
	  (setq format-string (concat format-string "\\(%s\\)\\|"))
	  (setq cntr (1+ cntr)))
	(setq format-string (concat "\\(" (substring format-string 0 (- (length format-string) 2)) "\\)"))
	(setq sec-nums-regexp (apply 'format format-string list2))
	;; Store program names only in "list3".
	(setq list3 (mapcar (lambda
			  (prog-name-plus-sec-num)
			  (substring prog-name-plus-sec-num 0 (string-match (concat "(" sec-nums-regexp ")$") prog-name-plus-sec-num)))
			list1))
	;; Store names of programs with man pages in multiple sections in "list4".
	(setq list4 (delq nil (delete-dups (mapcar (lambda
					     (prog-name-only)
					     (if (> (count prog-name-only list3 :test 'equal) 1)
					         prog-name-only))
					   list3))))
	;; Add "list4" to "list1".
	(nconc list1 list4)
	;; Set Emacs variables.
	(setq my-man-known-programs-only (delete-dups list3))
	(setq my-man-known-programs-plus-sections list1)
	(setq my-man-known-sections-only list2)))
    ;; If the region is active, obtain program name and, if present, section number from it.
    (if (region-active-p)
        (progn
	(setq prog-name-and-sec-num (buffer-substring-no-properties (region-beginning) (region-end)))
	;; Check whether the section number is mentioned at the before the program name. Extract the program name if applicable.
	(setq sec-num-before (string-match (concat "^" sec-nums-regexp " ") prog-name-and-sec-num))
	(if sec-num-before
	    (progn
	      (setq sec-num-before (substring prog-name-and-sec-num (match-beginning 1) (match-end 1)))
	      (setq prog-name (substring prog-name-and-sec-num (match-end 0)))
	      ))
	;; Check whether the section number is mentioned after the program name. Extract the program name if applicable.
	(setq sec-num-after (string-match (concat " ?[.(]?" sec-nums-regexp ")?$") prog-name-and-sec-num))
	(if (and sec-num-after (not sec-num-before)) ;We also check that "sec-num-before" is unset because that should be preferred over "sec-num-after".
	    (progn
	      (setq sec-num-after (substring prog-name-and-sec-num (match-beginning 1) (match-end 1)))
	      (setq prog-name (substring prog-name-and-sec-num 0 (match-beginning 0)))
	      ))
	;; Extract the actual section number from the check results.
	(setq sec-num (catch 'here
		      (mapc (lambda
			    (element)
			    (if element
			        (throw 'here element)
			      ;; (throw 'here nil)
			      ))
			  (list sec-num-before sec-num-after))))
	(setq sec-num (delq nil sec-num))	;This is required in order to allow using "sec-num" as a boolean value further down.
	;; If the "prog-name" hasn't been set by now, use the original region content.
	(if (not prog-name)
	    (setq prog-name prog-name-and-sec-num)))
      ;; If the region is not active, obtain the program name and, if present, the section number by scanning for text at point enclosed in the delimiting characters.
      (let (p1 p2 (delim-chars "[a-z0-9:._-]"))
        (save-excursion (skip-chars-backward delim-chars (point-min))
		    (setq p1 (point))
		    ;; (if (looking-back "[\n[:space:]]+\\([1-8ln]\\|\\(3am\\)\\|\\(3perl\\)\\|\\(3pm\\)\\|\\(3posix\\)\\) " (- p1 8))
		    (if (looking-back "[\n[:space:]]+\\([1-8ln]\\|\\(3am\\)\\|\\(3perl\\)\\|\\(3pm\\)\\|\\(3posix\\)\\) " (- p1 8))
		        (setq sec-num (match-string 1))))
        (save-excursion (skip-chars-forward delim-chars (point-max))
		    (setq p2 (point))
		    ;; (if (looking-at "[.(]?\\([1-8ln]\\|\\(3am\\)\\|\\(3perl\\)\\|\\(3pm\\)\\|\\(3posix\\)\\)[\n[:space:][:punct:])]+")
		    (if (looking-at "[.(]?\\([1-8ln]\\|\\(3am\\)\\|\\(3perl\\)\\|\\(3pm\\)\\|\\(3posix\\)\\)[\n[:space:][:punct:])]+")
		        (setq sec-num (match-string 1))))
        (set-match-data match-data-old)
        (setq prog-name (buffer-substring-no-properties p1 p2))))
    ;; If prog-name is empty, message the user about it, but do not visit it.
    (if (string= "" prog-name)
        (error "Empty program name ignored")
      ;; If a man page for "prog-name" exists...
      (if (member prog-name my-man-known-programs-only)
	(progn
	  ;; ...and if the region is active, deactivate the mark...
	  (if (region-active-p)
	      (deactivate-mark t))
	  ;; ...and open the man page for "prog-name" and, if present, "sec-num".
	  ;; TODO: concatenate "prog-name" and "sec-num" so that they may serve as input for a "(man ...)" call.
	  (man (concat prog-name sec-num)))
        ;; If prog-name does not exist, message the user about it.
        ;; In order to distinguish prog-name from the rest of the message, we can use either quotation marks...
        ;; (message "File \"%s\" does not exist." prog-name)))))
        ;; ...or text color.
        (add-face-text-property 0 (length prog-name) '(:foreground "blue") t prog-name)
        (error "Program name %s unknown to man" prog-name)))))

;; (let (string1 list1 list2 list3 list4)
(progn
  (setq string1 (shell-command-to-string "apropos -l ."))
  (setq string1 (replace-regexp-in-string " (" "(" (replace-regexp-in-string ",$" "" (replace-regexp-in-string " +- .*\n" "," string1))))
  (setq list1 (split-string string1 ","))
  (setq list2 (delete-dups (mapcar (lambda
			       (prog-name-plus-sec-num)
			       (if (string-match "(\\(.*\\))$" prog-name-plus-sec-num)
				 (substring prog-name-plus-sec-num (match-beginning 1) (match-end 1))
				 ))
			     list1)))
  ;; Create a proper regexp to cover all section numbers known to man.
  (while (< cntr (length list2))
    (setq format-string (concat format-string "\\(%s\\)\\|"))
    (setq cntr (1+ cntr)))
  (setq format-string (concat "\\(" (substring format-string 0 (- (length format-string) 2)) "\\)"))
  (setq sec-nums-regexp (apply 'format format-string list2))
  
  (setq list3 (mapcar (lambda
		    (prog-name-plus-sec-num)
		    (substring prog-name-plus-sec-num 0 (string-match "([1-8ln]" prog-name-plus-sec-num)))
		  list1))
  (setq list4 (delq nil (delete-dups (mapcar (lambda
				       (prog-name-only)
				       (if (> (count prog-name-only list3 :test 'equal) 1)
					 prog-name-only))
				     list3))))
  (nconc list1 list4)
  nil
  )

(let (
      prog-name
      (prog-name-and-sec-num "intro")
      ;; (prog-name-and-sec-num "6 intro")
      ;; (prog-name-and-sec-num "intro.6")
      ;; (prog-name-and-sec-num "intro(6)")
      ;; (prog-name-and-sec-num "intro (6)")
      ;; (prog-name-and-sec-num "2to3-2.7 (1)")
      ;; (prog-name-and-sec-num "1 2to3-2.7")
      sec-num-before
      sec-num-after
      (cntr 1)
      (format-string "")
      sec-nums-regexp
      )
  ;; Check whether the section number is mentioned at the before the program name.
  (setq sec-num-before (string-match (concat "^" sec-nums-regexp " ") prog-name-and-sec-num))
  (if sec-num-before
      (progn
        (setq sec-num-before (substring prog-name-and-sec-num (match-beginning 1) (match-end 1)))
        (setq prog-name (substring prog-name-and-sec-num (match-end 0)))
        ))
  ;; Check whether the section number is mentioned after the program name.
  (setq sec-num-after (string-match (concat " ?[.(]?" sec-nums-regexp ")?$") prog-name-and-sec-num))
  (if (and sec-num-after (not sec-num-before))
      (progn
        (setq sec-num-after (substring prog-name-and-sec-num (match-beginning 1) (match-end 1)))
        (setq prog-name (substring prog-name-and-sec-num 0 (match-beginning 0)))
        ))
  ;; Extract the actual section number from the check results.
  (setq sec-num (catch 'here
	        (mapc (lambda
		      (element)
		      (if element
			(throw 'here element)
		        ;; (throw 'here nil)
		        ))
		    (list sec-num-before sec-num-after))))
  (setq sec-num (delq nil sec-num))	;This is required in order to allow using "sec-num" as a boolean value further down.
  (if (not prog-name)
      (setq prog-name prog-name-and-sec-num))
  (display-message-or-buffer (concat prog-name "." (if sec-num sec-num "NA"))) ;TESTING
  )

(completing-read "Prompt: " list1)

(let (list1 list2)
  (with-temp-buffer
    (shell-command "man -k ." (current-buffer) "*Messages*")
    (goto-char (point-min))
    (replace-regexp " +- .*\n" "," nil (point-min) (point-max))
    (goto-char (point-min))
    (replace-regexp ",$" "" nil (point-min) (point-max))
    (goto-char (point-min))
    (replace-string " (" "(" nil (point-min) (point-max))
    (goto-char (point-min))
    (setq list1 (split-string (buffer-substring-no-properties (point-min) (point-max)) ","))
    (goto-char (point-min))
    (setq list2 (delq nil (delete-dups (mapcar (lambda
				         (prog-name-plus-sec-num)
				         (let (prog-name-only)
					 (setq prog-name-only (regexp-quote (substring prog-name-plus-sec-num 0 (string-match "([1-8ln]" prog-name-plus-sec-num))))
					 (if (> (count-matches (concat "," prog-name-only "(") (point-min) (point-max)) 1)
					     prog-name-only
					   )))
				       list1))))
    (nconc list1 list2)
    ))
