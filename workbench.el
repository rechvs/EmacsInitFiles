(progn
  (defvar my-man-input-hist
    nil
    "List of man pages invoked by the user.")

  (defvar my-man-known-programs-only
    nil
    "List of all programs (without sections) known to man.")

  (defvar my-man-known-programs-plus-sections
    nil
    "List of all programs (with and without sections) known to man.")

  (defvar my-man-known-programs-skip-chars-string
    nil
    "String covering all characters of all program names known to man.") 

  (defvar my-man-known-sections-only
    nil
    "List of all sections (without programs) known to man.")

  (defvar my-man-known-sections-regexp
    nil
    "Regexp covering all sections known to man.")
  )

(defun my-man (&optional prefix-arg)
  "Use text in region or around point as the program name (and section number 
if present) for which to display the man page(s). Supported formats for
guessing the section number are: SECNUM PROGNAME, PROGNAME(SECNUM), PROGNAME
(SECNUM). An empty program name is ignored. If called with a prefix argument,
rebuild the lists of all program names and section numbers known to man. This
function requires variables `my-man-input-hist', `my-man-known-programs-only',
`my-man-known-programs-plus-sections',
`my-man-known-programs-skip-chars-string', `my-man-known-sections-only', and
`my-man-known-sections-regexp'."
  (interactive "P")
  (let ((format-string "")
        input-initial
        input-user
        list1
        list2
        list3
        list4
        man-buffer
        (match-data-old (match-data)) 
        prog-name
        prog-name-and-sec-num
        prog-names-skip-chars-string
        sec-num
        sec-num-after
        sec-num-before
        sec-nums-regexp
        string1)
    (unwind-protect
;; Check whether all necessary variables are set.
(mapcar 'symbol-value (list 'my-man-input-hist
		        'my-man-known-programs-only
		        'my-man-known-programs-plus-sections
		        'my-man-known-programs-skip-chars-string
		        'my-man-known-sections-only
		        'my-man-known-sections-regexp))
;; If not already available, create a list of all combinations of program names and section numbers known to "man".
(if (or (not my-man-known-programs-plus-sections) (not my-man-known-programs-only) (not my-man-known-sections-only) (not my-man-known-sections-regexp) (not my-man-known-programs-skip-chars-string) prefix-arg)
    (progn
      (display-message-or-buffer "Rebuilding the list of all program names plus section numbers known to man...")
      ;; Store the list of all program names and corresponding section numbers known to man in "list1".
      (setq string1 (shell-command-to-string "apropos -l ."))
      (setq string1 (replace-regexp-in-string " (" "(" (replace-regexp-in-string ",$" "" (replace-regexp-in-string " +- .*\n" "," string1))))
      (setq list1 (split-string string1 ","))
      ;; Store all section numbers known to man in "list2".
      (setq list2 (delete-dups (mapcar (lambda
				 (prog-name-plus-sec-num)
				 (if (string-match "(\\(.*\\))$" prog-name-plus-sec-num)
				     (match-string 1 prog-name-plus-sec-num)))
			         list1)))
      ;; Create a proper regexp to cover all section numbers based on "list2".
      (setq format-string (apply 'concat (make-list (length list2) "\\(%s\\)\\|")))
      (setq format-string (concat "\\(" (substring format-string 0 (- (length format-string) 2)) "\\)"))
      (setq list2 (sort list2 'string<))
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
      ;; Create a "(skip-chars-* ...)" string covering all characters contained in "list3".
      (setq prog-names-skip-chars-string (apply 'concat (sort (delete-dups (split-string (apply 'concat list3) "")) 'string<)))
      (setq prog-names-skip-chars-string (replace-regexp-in-string "\\\\" "\\\\\\\\" prog-names-skip-chars-string))
      (setq prog-names-skip-chars-string (replace-regexp-in-string "\\^" "\\\\^" prog-names-skip-chars-string))
      (setq prog-names-skip-chars-string (replace-regexp-in-string "-" "\\\\-" prog-names-skip-chars-string))
      ;; Add "list4" to "list1".
      (nconc list1 list4)
      ;; Set Emacs variables.
      (setq my-man-known-programs-only (delete-dups list3))
      (setq my-man-known-programs-plus-sections (sort list1 'string<))
      (setq my-man-known-programs-skip-chars-string prog-names-skip-chars-string)
      (setq my-man-known-sections-only list2)
      (setq my-man-known-sections-regexp sec-nums-regexp)))
;; If the region is active, obtain program name and, if present, section number from it.
(if (region-active-p)
    (progn
      (setq prog-name-and-sec-num (buffer-substring-no-properties (region-beginning) (region-end)))
      ;; Check whether the section number is mentioned before the program name. If so, extract it and the program name.
      (setq sec-num-before (string-match (concat "^" my-man-known-sections-regexp " ") prog-name-and-sec-num))
      (if sec-num-before
	(progn
	  (setq sec-num-before (match-string 1 prog-name-and-sec-num))
	  (setq prog-name (substring prog-name-and-sec-num (match-end 0)))))
      ;; Check whether the section number is mentioned after the program name. If so, extract it and the program name.
      (setq sec-num-after (string-match (concat " ?[(]?" my-man-known-sections-regexp ")?$") prog-name-and-sec-num))
      (if (and sec-num-after (not sec-num-before)) ;We also check that "sec-num-before" is unset because that should be preferred over "sec-num-after".
	(progn
	  (setq sec-num-after (match-string 1 prog-name-and-sec-num))
	  (setq prog-name (substring prog-name-and-sec-num 0 (match-beginning 0)))))
      ;; Extract the actual section number from the check results.
      (setq sec-num (catch 'here
		  (mapc (lambda
			(element)
			(if element
			    (throw 'here element)))
		        (list sec-num-before sec-num-after))))
      (setq sec-num (delq nil sec-num))	;This is required in order to allow using "sec-num" as a boolean value further down.
      ;; If the "prog-name" hasn't been set by now, use the original region content.
      (if (not prog-name)
	(setq prog-name prog-name-and-sec-num)))
  ;; If the region is not active, obtain the program name and, if present, the section number by scanning for text at point enclosed in the delimiting characters.
  (let (p1 p2)
    (save-excursion (skip-chars-backward my-man-known-programs-skip-chars-string (point-min))
		(setq p1 (point))
		(if (looking-back (concat "[\n[:space:]]+" my-man-known-sections-regexp " ") (- p1 (+ 2 (apply 'max (mapcar 'length my-man-known-sections-only)))))
		    (setq sec-num (match-string 1))))
    (save-excursion (skip-chars-forward my-man-known-programs-skip-chars-string (point-max))
		(setq p2 (point))
		(if (looking-at (concat " ?[(]?" my-man-known-sections-regexp "[\n[:space:][:punct:])]+"))
		    (setq sec-num (match-string 1))))
    (setq prog-name (buffer-substring p1 p2))))
;; Prompt for user input.
(setq input-initial (if (and (not (null prog-name)) (not (string= "" prog-name)) (not (null sec-num)) (not (string= "" sec-num)))
		    (concat prog-name "(" sec-num ")")
		  prog-name))
(set-text-properties 0 (length input-initial) nil input-initial)
(setq input-user (completing-read "Program name[(section)]: " my-man-known-programs-plus-sections nil 'confirm input-initial 'my-man-input-hist))
(setq my-man-input-hist (nconc (delq nil my-man-input-hist) (list input-user)))
;; If active, deactivate mark.
(if (region-active-p)
    (deactivate-mark))
;; If "input-user" is empty, signal an error.
(if (string= "" input-user)
    (error "Missing program name")
  ;; Else, extract program name and section number from "input-user" and concatenate them appropriately.
  (setq sec-num (if (string-match "(\\(.*\\))$" input-user)
		(match-string 1 input-user)))
  (setq prog-name (if sec-num
		  (progn
		    (string-match "^\\(.*\\)(" input-user)
		    (match-string 1 input-user))
		input-user))
  (setq prog-name-and-sec-num (concat prog-name (if sec-num (concat "." sec-num))))
  ;; Create the format string needed to create a comma separated list of section names based on "my-man-known-sections-only" which would be suited as an argument for "man -S ...".
  (setq format-string (apply 'concat (make-list (length my-man-known-sections-only) "%s,")))
  (setq format-string (substring format-string 0 (1- (length format-string))))
  ;; Invoke man.
  (setq man-buffer (man prog-name-and-sec-num))
  ;; Wait for the man subprocess to finish.
  (while (process-status "man")
    (sleep-for 0.01))
  ;; Switch to the man page buffer in another window.
  (switch-to-buffer-other-window man-buffer)
  ;; If the sections list in "Man-switches" is outdated, inform about it.
  (if (not (string-match-p (apply 'format format-string my-man-known-sections-only) Man-switches))
      (display-message-or-buffer (concat "The sections list in Man-switches seems to be outdated (updated value: " (apply 'format format-string my-man-known-sections-only) ")."))))
(set-match-data match-data-old))))
