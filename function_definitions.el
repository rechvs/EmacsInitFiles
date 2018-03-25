;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous functions ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-abbrev-mode-on ()
  "Turn abbrev mode ON."
  (abbrev-mode 1))

(defun my-back-to-indentation ()
  "Move point to the first character on current line, which is not mentioned in `my-back-to-indentation-skip-chars'."
  (interactive "^")
  (beginning-of-line 1)
  (skip-chars-forward my-back-to-indentation-skip-chars (line-end-position))
  (backward-prefix-chars))

(defun my-comment-or-uncomment ()
  "When region is not active, comment the current line if uncommented, otherwise uncomment it. When region is active, comment uncommented lines and uncomment commented lines that are at least partly contained in region. Lines consisting only of a newline character or of whitespace and a newline character are skipped."
  (interactive)
  ;; Define a function for commenting or uncommenting the text between 2 positions.
  (cl-flet ((my-comment-or-uncomment-current-line
	   (current-line-beginning current-line-end)
	   (if (and (not (= current-line-beginning current-line-end)) (not (= current-line-end (point))))
	       (if (equal (buffer-substring (point) (+ (point) (length comment-start))) comment-start)
		 (uncomment-region current-line-beginning current-line-end)
	         (comment-region current-line-beginning current-line-end)))))
    (let (line-beginning line-end)
      ;; If the mark is active,...
      (if mark-active
	(let ((current-line-number 1) first-line-beginning number-of-lines)
	  ;; ...count the lines in the region,...
	  (setq number-of-lines (count-lines (region-beginning) (region-end)))
	  (save-excursion
	    (goto-char (region-beginning))
	    (setq first-line-beginning (line-beginning-position))
	    ;; ...and successively comment or uncomment all lines in region using the previously defined function.
	    (while (not (> current-line-number number-of-lines))
	      (goto-char first-line-beginning)
	      (forward-line (- current-line-number 1))
	      (setq line-end (line-end-position))
	      (setq line-beginning (line-beginning-position))
	      (goto-char line-beginning)
	      (skip-syntax-forward " " line-end)
	      (my-comment-or-uncomment-current-line line-beginning line-end)
	      (setq current-line-number (+ current-line-number 1)))))
        ;; If the mark is not active, comment or uncomment the current line using the previously defined function.
        (save-excursion
	(setq line-end (line-end-position))
	(setq line-beginning (line-beginning-position))
	(goto-char line-beginning)
	(skip-syntax-forward " " line-end)
	(my-comment-or-uncomment-current-line line-beginning line-end))))))

(defun my-customize-interface ()
  "Change interface features in the following way:

blink cursor mode OFF
tool bar mode OFF
NO scroll bar
column number mode ON
menu bar mode OFF"
  (interactive)
  (blink-cursor-mode -1)
  (tool-bar-mode -1)
  (menu-bar-no-scroll-bar)
  (column-number-mode 1)
  (menu-bar-mode -1))

(defun my-delete-and-insert-blank-lines (ARG)
  "If point is on a blank line, delete all blank lines around point and
subsequently create ARG (default: 3) newlines, leaving point on the middle
newline (in case of an odd-numbered ARG) or on the line directly below the
middle of the created newlines (in case of an even-numbered ARG).
If point is not on a blank line do nothing."
  (interactive "P")
  (let (current-line-blank line-above-blank line-below-blank)
    (save-excursion
      (move-end-of-line nil)
      (back-to-indentation)
      (if (looking-at "[ *\n]")
	(setq current-line-blank 't)
        ()))
    (save-excursion
      (forward-line -1)
      (move-end-of-line nil)
      (back-to-indentation)
      (if (looking-at "[ *\n]")
	(setq line-above-blank 't)
        ()))
    (save-excursion
      (forward-line 1)
      (move-end-of-line nil)
      (back-to-indentation)
      (if (looking-at "[ *\n]")
	(setq line-below-blank 't)
        ()))
    (if (equal current-line-blank t)
        (list
         (or ARG (setq ARG 3))
         (if (equal line-above-blank t)
	   (list
	    (if (= 1 (% ARG 2))
	        (list
	         (delete-blank-lines)
	         (delete-blank-lines)
	         (open-line ARG)
	         (forward-line (/ ARG 2)))
	      (delete-blank-lines)
	      (delete-blank-lines)
	      (open-line ARG)
	      (forward-line (- (/ ARG 2) 1))))
	 (if (equal line-below-blank t)
	     (list
	      (if (= 1 (% ARG 2))
		(list
		 (delete-blank-lines)
		 (delete-blank-lines)
		 (open-line ARG)
		 (forward-line (/ ARG 2)))
	        (delete-blank-lines)
	        (open-line ARG)
	        (forward-line (- (/ ARG 2) 1))))
	   (if (= 1 (% ARG 2))
	       (list
	        (delete-blank-lines)
	        (open-line ARG)
	        (forward-line (/ ARG 2)))
	     (delete-blank-lines)
	     (open-line ARG)
 (forward-line (- (/ ARG 2) 1))))))
      (message "Current line is not blank\."))))

(defun my-find-region-or-at-point ()
  "If region is active, use text in region as the filename to visit.
Otherwise use the text at point as the filename to visit. The
characters delimiting the filename from surrounding text are set via
 `my-find-region-or-at-point-delim-chars'. 
If it exists, the file is visited via `find-file'. An empty filename is
ignored."
  (interactive)
  (let (FILENAME)
    ;; If the region is active, obtain the filename from it.
    (if (region-active-p)
        (progn
	(setq FILENAME (buffer-substring-no-properties (region-beginning) (region-end)))
	)
      ;; If the region is not active, obtain the filename by scanning for text at point enclosed in the delimiting characters.
      (let (P1 P2 DELIMCHARS)
        (setq DELIMCHARS (concat "^" my-find-region-or-at-point-delim-chars))
        (save-excursion (skip-chars-backward DELIMCHARS (line-beginning-position))
		    (setq P1 (point)))
        (save-excursion (skip-chars-forward DELIMCHARS (line-end-position))
		    (setq P2 (point)))
        (setq FILENAME (buffer-substring-no-properties P1 P2))))
    ;; If FILENAME is empty, message the user about it, but do not visit it.
    (if (string= "" FILENAME)
        (message "Empty filename ignored.")
      ;; If FILENAME exists...
      (if (file-exists-p FILENAME)
	(progn
	  ;; ...and if the region is active, deactivate the mark...
	  (if (region-active-p)
	      (deactivate-mark t))
	  ;; ...and visit FILENAME.
	  (find-file FILENAME))
        ;; If FILENAME does not exist, message the user about it.
        ;; In order to distinguish FILENAME from the rest of the message, we can use either quotation marks...
        ;; (message "File \"%s\" does not exist." FILENAME)))))
        ;; ...or text color.
        (add-face-text-property 0 (length FILENAME) '(:foreground "blue") t FILENAME)
        (message "File %s does not exist." FILENAME)))))

(defun my-follow-bookmark (BKMK)
  "Call (`delete-other-windows'), open bookmark BKMK in two vertically separated windows and activate follow-mode."
  (interactive)
   (list
    (let (BKMKS)
      (bookmark-load "~/.emacs.d/bookmarks" t)
      (setq BKMKS (bookmark-all-names))
      (setq BKMK (completing-read "Bookmark: " BKMKS))
      (bookmark-jump BKMK)
   (delete-other-windows)
   (split-window-horizontally)
   (follow-mode t))))

(defun gnus-user-format-function-a (hdr)
  "If HDR is not a vector, return the string \"void\". Otherwise, if one of my addresses is contained in either the To: or the Cc: header or if both these headers are empty, return the From: header; else return the To: header or, if that is empty, the Cc: header. In all cases, prefer the full name contained in the header if present, otherwise use only the email address. See `gnus-summary-line-format' on how this function interfaces with Gnus."
  (if (not (vectorp hdr))
      "void"
    (let ((from (mail-extract-address-components (mail-header-from hdr)))
	(to (cdr (assoc 'To (mail-header-extra hdr))))
	(cc (cdr (assoc 'Cc (mail-header-extra hdr))))
	(ingorable-adresses-regexp "\\(\\(magic_willebinski\\)\\|\\(members\\)\\|\\(renke.vonseggern\\)\\)@gmx\\.\\(\\(de\\)\\|\\(net\\)\\)"))
      (if (or (and (or (string= "" to) (null to))
	         (or (string= "" cc) (null cc)))
	    (string-match-p ingorable-adresses-regexp (if (null to) "" to))
	    (string-match-p ingorable-adresses-regexp (if (null cc) "" cc)))
	(progn
	  (setq from (if (null (nth 0 from))
		       (nth 1 from)
		     (nth 0 from)))
	  from)
        (setq to (mail-extract-address-components to))
        (setq to (if (null (nth 0 to))
		 (nth 1 to)
	         (nth 0 to)))
        (if (null to)
	  (progn
	    (setq cc (mail-extract-address-components cc))
	    (setq cc (if (null (nth 0 cc))
		       (nth 1 cc)
		     (nth 0 cc)))
	    cc)
	to)))))

(defun my-hiwi-ssh-bayeos ()
  "Visit directory `/home/aknohl´ on host `134.76.19.50 ´ as user `aknohl´ via `ssh´."
  (interactive)
  (find-file "/ssh:aknohl@134.76.19.50:/home/aknohl"))

(defun my-hiwi-ssh-tss ()
  "Visit directory `/home/lukas´ on host `134.76.19.175 ´ as user `lukas´ via `ssh´."
  (interactive)
  (find-file "/ssh:lukas@134.76.19.175:/home/lukas"))

(defun my-immediately-switch-to-buffer ()
  "Immediately switch to the most recently selected buffer other than the current buffer, disregarding buffers already visible. If called in succession, cycle through the list returned by `buffer-list'."
  (interactive)
  (let (WINDOW-LIST (WINDOW_NR 0) (BUFFER-NAME-LIST (list "")) (BUFFER_NR 0) (BUFFER_NEXT_IN_ROW ""))
    (catch 'CATCH
    ;; If we are in the minibuffer, inform about this and do nothing else.
    (if (string-match "\ \*Minibuf-+[0-9]*[0-9]\*" (buffer-name (current-buffer)))
        (display-message-or-buffer "Currently in minibuffer.")
      ;; Else, do the following.
      (progn
        ;; Store the window list in "WINDOW-LIST".
        (setq WINDOW-LIST (window-list))
        ;; While "WINDOW_NR" (default: 0) is not the same as the length of "WINDOW-LIST" minus 1, ...
        (while (<= WINDOW_NR (- (length WINDOW-LIST) 1))
	;; ...set element nr. "WINDOW_NR" of list "BUFFER-NAME-LIST" to the name of the buffer displayed in window nr. "WINDOW_NR" as given in "WINDOW-LIST"... (See Elisp manual at "elisp.info::Window Type" on the hash notation of windows in Elisp on which this approach is based.)
	(setf (nth WINDOW_NR BUFFER-NAME-LIST) (nth 1 (split-string (prin1-to-string (nth WINDOW_NR WINDOW-LIST) nil) "#<window [0-9]+ on " nil ">")))
	;; If "WINDOW_NR" is less than the length of "WINDOW-LIST" minus 1,...
	(if (< WINDOW_NR (- (length WINDOW-LIST) 1))
	    ;; ...append an empty string to "BUFFER-NAME-LIST".
	    (setq BUFFER-NAME-LIST (append BUFFER-NAME-LIST (list ""))))
	;; ...and step "WINDOR_NR".
	(setq WINDOW_NR (+ WINDOW_NR 1)))
        ;; If the previous command was "my-immediately-switch-to-buffer",...
        (if (eq last-command 'my-immediately-switch-to-buffer)
	  ;; ...step variable "my-immediately-switch-to-buffer-counter".
	  (setq my-immediately-switch-to-buffer-counter (+ 1 my-immediately-switch-to-buffer-counter))
	;; Else, set variable "my-immediately-switch-to-buffer-counter" to 1.
	(setq my-immediately-switch-to-buffer-counter 1))
        ;; Set "BUFFER_NEXT_IN_ROW" to the buffer name at position "my-immediately-switch-to-buffer-counter" in the list given by "buffer-list".
        (setq BUFFER_NEXT_IN_ROW (buffer-name (nth my-immediately-switch-to-buffer-counter (buffer-list))))
        ;; While "BUFFER_NEXT_IN_ROW" is a member of "BUFFER-NAME-LIST" or is mentioned in variable "my-immediately-switch-to-buffer-excluded-buffers",...
        (while (or (member BUFFER_NEXT_IN_ROW BUFFER-NAME-LIST)
	         (my-string-match-list my-immediately-switch-to-buffer-excluded-buffers BUFFER_NEXT_IN_ROW))
	;; ...step variable "my-immediately-switch-to-buffer-counter"...
	(setq my-immediately-switch-to-buffer-counter (+ 1 my-immediately-switch-to-buffer-counter))
	;; If "my-immediately-switch-to-buffer-counter" is a larger numer than the length of the list returned by "buffer-list", inform about it and exit.
	(if (> my-immediately-switch-to-buffer-counter (length (buffer-list)))
	    (progn
	      (error "No more buffers left to switch to")
	      (throw 'CATCH t)))
	;; ...and set "BUFFER_NEXT_IN_ROW" to the buffer name at position "my-immediately-switch-to-buffer-counter" in the list given by "buffer-list".
	(setq BUFFER_NEXT_IN_ROW (buffer-name (nth my-immediately-switch-to-buffer-counter (buffer-list)))))
        ;; Switch to buffer "BUFFER_NEXT_IN_ROW".
        (switch-to-buffer BUFFER_NEXT_IN_ROW))))))

(defun my-load-bookmarks-on-startup ()
  "Uses the function bookmark-load."
  (bookmark-load "~/.emacs.d/bookmarks" "t"))

(defun my-log-edit-insert-message-template ()
  "Custom version of `log-edit-insert-message-template'. Insert template with Author but without Summary."
  (interactive)
  (when (or (called-interactively-p 'interactive)
            (log-edit-empty-buffer-p))
    ;; (insert "Summary: ")
    (when log-edit-setup-add-author
      ;; (insert "\nAuthor: "))
      (insert "Author: "))
    ;; (insert "\n\n")
    (message-position-point)))

(defun my-man (&optional prefix-arg)
  "Use text in region or around point as the program name (and section number
if present) for which to display the man page(s). Supported formats for
guessing the section number are: SECNUM PROGNAME, PROGNAME(SECNUM), PROGNAME
(SECNUM). If called with a prefix argument, reset the following variables
based on the output of \"apropos -l .\": `my-man-known-programs-only',
`my-man-known-programs-plus-sections',
`my-man-known-programs-skip-chars-string', `my-man-known-sections-only', and
`my-man-known-sections-regexp'."
  (interactive "P")
  (let ((cntr 0)
(format-string "")
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
  ;; If not already available, create several lists related to the program names and section numbers known to man.
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
        ;; Process lists if necessary in order to allow comparison between temporary and permanent variables.
        (setq list1 (sort list1 'string<))
        (setq list3 (sort (delete-dups list3) 'string<))
        ;; Check whether the values of the temporary variables match those of the corresponding permanent variables. If that is not the case, warn about this and set the permanent variable to the correct value for the remainder of this Emacs session.
        (let ((permanent-vars-symbols-list (list 'my-man-known-programs-only
				         'my-man-known-programs-plus-sections
				         'my-man-known-programs-skip-chars-string
				         'my-man-known-sections-only
				         'my-man-known-sections-regexp))
	    (temporary-vars-symbols-list (list 'list3
				         'list1
				         'prog-names-skip-chars-string
				         'list2
				         'sec-nums-regexp)))
	(while (< cntr (length permanent-vars-symbols-list))
	  (let ((cur-permanent-var-symbol (nth cntr permanent-vars-symbols-list))
	        (cur-temporary-var-symbol (nth cntr temporary-vars-symbols-list)))
	    (if (not (equal (symbol-value cur-permanent-var-symbol) (symbol-value (nth cntr temporary-vars-symbols-list))))
	        (progn (warn "The previous value of `%s' seems to be outdated." cur-permanent-var-symbol)
		     (set cur-permanent-var-symbol (symbol-value cur-temporary-var-symbol)))))
	  (setq cntr (1+ cntr))))))
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

(defun my-move-end-of-line ()
  "If point is not at the end of the current line, move point to the end of the current visual line. If point is at the end of the current visual line, move point to the beginning of the last whitespace character sequence on the current visual line."
  (interactive)
  (if visual-line-mode
      (progn
        (setq LINEEND (save-excursion (end-of-visual-line nil) (point)))
        (setq LINEBEGINNING (save-excursion (beginning-of-visual-line nil) (point)))
        (setq POSAFTERSKIP (save-excursion (goto-char LINEBEGINNING) (skip-chars-forward "[:space:]" LINEEND) (point)))
        (if (= POSAFTERSKIP LINEEND)
	  (setq ONLYWHITESPACE t)
	(setq ONLYWHITESPACE nil))
        (if (not (eq (point) LINEEND))
	  (end-of-visual-line nil)
	(if ONLYWHITESPACE
	    (goto-char LINEBEGINNING)
	  (end-of-visual-line nil)
	  (search-backward-regexp "[^ ]" LINEBEGINNING)
	  (forward-char))))
    (progn
      (setq LINEEND (save-excursion (move-end-of-line nil) (point)))
      (setq LINEBEGINNING (save-excursion (move-beginning-of-line nil) (point)))
      (setq POSAFTERSKIP (save-excursion (goto-char LINEBEGINNING) (skip-chars-forward "[:space:]" LINEEND) (point)))
      (if (= POSAFTERSKIP LINEEND)
	(setq ONLYWHITESPACE t)
        (setq ONLYWHITESPACE nil))
      (if (not (eq (point) LINEEND))
	(move-end-of-line nil)
        (if ONLYWHITESPACE
	  (goto-char LINEBEGINNING)
	(move-end-of-line nil)
	(search-backward-regexp "[^ ]" LINEBEGINNING)
	(forward-char))))))

(defun my-newline-at-80 ()
  "Insert newline character after column 80 or, if there is a nonwhitespace character in column 80, at the beginning of the last whitespace character sequence before column 80."
  (interactive)
  (save-excursion (move-end-of-line nil) (setq COLNR (current-column)))
  (if (<= COLNR 80)
      (next-line)
    (progn
      (move-to-column 80)
      (setq CHARBEFORE (char-before)))
    (if (eq CHARBEFORE ? )
        (newline-and-indent)
      (progn
        (save-excursion (move-beginning-of-line nil) (setq LINEBEGINNING (point)))
        (search-backward-regexp "[ ]" LINEBEGINNING)
        (search-backward-regexp "[^ ]" LINEBEGINNING)
        (forward-char)
        (newline-and-indent)))))

(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "R")))

(defun my-other-window ()
  "Call (`other-window' 1). Intended for use in keybinding."
  (interactive)
  (other-window 1))

(defun my-recenter (ARG)
  "When called without a prefix argument, redisplay window with point on 10th line of the window. When called with a prefix argument, redisplay window with 1st text line of the current paragraph on 1st line of the window. If current paragraph is too large for this, do the same as if called without a prefix argument."
  (interactive "P")
  (save-excursion (backward-paragraph) (forward-line 1) (recenter 0)
	        (setq LASTPOSPAR (save-excursion (forward-paragraph) (forward-line -1) (point)))
	        (setq LASTPOSWIN (save-excursion (move-to-window-line -1) (point))))
  (if (and (or (= LASTPOSWIN LASTPOSPAR) (> LASTPOSWIN LASTPOSPAR)) (not (eq ARG nil)))
      (save-excursion
        (backward-paragraph)
        (forward-line 1)
        (recenter 0))
    (recenter 9)))
    ;; (recenter 0)))

(defun my-recenter-tenth-line ()
  "Redisplay window with point on 10th line of the window."
  (interactive)
  (recenter 9))

(defun my-recenter-paragrpah ()
  "Redisplay window with 1st line of the paragraph on 1st line of the window."
  (interactive)
  (save-excursion
    (backward-paragraph)
    (forward-line 0)
    (recenter 0)))

(defun my-revert-buffer-without-confirmation ()
  "Call (`revert-buffer' nil t nil)."
  (interactive)
  (revert-buffer nil t nil))

(defun my-select-gnus-message-archive-group (sgrp)
  "Function for returning the name of the Gnus group in which to archive outgoing messages, based on the name of the group from which the message was sent. See `gnus-message-archive-group' for details."
  (if (string-match-p "^Bewerbung.*" sgrp)
      "Bewerbung.Ausgang"
    "Privat.Ausgang"))

(defun my-set-message-signature ()
  "Set variable `message-signature', depending on the name of the current buffer. This function is meant to be listed in `gnus-summary-mode-hook'."
  ()
  (let ((match-data-old (match-data)))
    (setq message-signature
	(if (string-match "^\*Summary Bewerbung\..*$\*" (buffer-name))
	    "Renke Christian von Seggern
Magdeburger Weg 1, 37085 Göttingen
mobil: +49176 953 053 79 fest: +49(551) 401 573 56
E-Mail: renke.vonseggern@gmx.de"))
    (set-match-data match-data-old)
    ))

(defun my-sh-smie-sh-rules (origfun kind token)
  "Custom function to correct indentation in shell scripts after `&&´. Taken from `http://superuser.com/questions/1037043/emacs-suppress-indent-after-in-shell-script-mode´."
  (pcase (cons kind token)
    (`(:before . "&&") nil)
    (_ (funcall origfun kind token))))
(advice-add 'sh-smie-sh-rules :around #'my-sh-smie-sh-rules)

(defun my-start-or-switch-to-R-process ()
  "If no R process is running, call (R). If exactly one R process is running, switch to the corresponding process buffer. Otherwise, offer a selection of R process buffers and switch to the selected buffer."
  (interactive)
  (require 'ido)
  (require 'ess)
  (let (process-names-list R-buffer-names-list (index 0) cur-process-name)
    ;; If a process named "R" does NOT exist,...
    (if (not (get-process "R"))
        ;; ...call "R",...
        (R)
      ;; ...else, do the following.
      ;; Convert list of currently running sub-processes to a list of process names as strings and store it in "process-names-list.
      (setq process-names-list (split-string (prin1-to-string (process-list)) "[ ]" t "(\\{,1\\}#<process\\|>)\\{,1\\}"))
      ;; Loop over all elements in "process-names-list".
      (while (<= index (- (length process-names-list) 1))
        ;; Store current element of "process-names-list" in "cur-process-name".
        (setq cur-process-name (nth index process-names-list))
        ;; If "cur-process-name" matches the regexp "R[:]?[0-9]*",...
        (if (string-match "^R[:]?[0-9]*$" cur-process-name)
	  ;; ...append the name of the corresponding buffer to "R-buffer-names-list".
	  (setq R-buffer-names-list (cons (buffer-name (process-buffer (get-process cur-process-name))) R-buffer-names-list)))
        ;; Increment "index".
        (setq index (+ index 1)))
      ;; If "R-buffer-names-list" has exactly 1 element,...
      (if (= 1 (length R-buffer-names-list))
	;; ...immediatly switch to the buffer listed in it.
	(switch-to-buffer (nth 0 R-buffer-names-list))
        ;; Else, offer a selection of buffer names based on "R-buffer-names-list" and switch to selected buffer.
        (switch-to-buffer (ido-completing-read "Select R process buffer: " R-buffer-names-list nil t)))
      ;; Clear echo area.
      (message nil)
      )))

(defun my-string-match-list (LIST STRING)
  "Match string STRING against the elements of list LIST (using `string-match'). Return t upon finding the first match, otherwise return nil."
  (if (not (listp LIST))
      (error "LIST must be a list object"))
  (if (not (stringp STRING))
      (error "STRING must be a string"))
  (let ((ELTS (length LIST)) (ELT 0))
        (catch 'CATCH
	(while (< ELT ELTS)
	  (if (string-match (nth ELT LIST) STRING)
	      (throw 'CATCH t))
	  (setq ELT (+ 1 ELT))))))

(defun my-switch-windows (ARG)
  "If exactly two vertically separated windows are displayed, switch the buffers in them. When called without a prefix argument, select the previously selected buffer. When called with a prefix argument, select the previously unselected buffer."
  (interactive "P")
  (let (BUFFER_CURRENT)
    (if (= (length (window-list)) 2)
        (progn
	(require 'windmove)
	(setq WINDOW_ABOVE (windmove-find-other-window 'up))
	(setq WINDOW_BELOW (windmove-find-other-window 'down))
	(if (or WINDOW_ABOVE (and WINDOW_BELOW (not (string-match "*Minibuf" (pp-to-string WINDOW_BELOW)))))
	    (message "The windows are not separated vertically.")
	  (progn
	    (setq BUFFER_CURRENT (buffer-name))
	    (setq WINDOW_LEFT (windmove-find-other-window 'left))
	    (if WINDOW_LEFT
	        (progn
		(delete-window)
		(split-window-right)
		(switch-to-buffer BUFFER_CURRENT)
		)
	      (progn
	        (windmove-right)
	        (delete-other-windows)
	        (switch-to-buffer-other-window BUFFER_CURRENT)))
	    (if (not (eq ARG nil))
	        (other-window 1)))))
      (message "There are more or less than two windows displayed."))))

(defun my-visit-file-add-to-git-whitelist ()
  "Ask for a filename. If the file is accessible and not already mentioned in \"~/.gitignore\", ask for a comment, and add comment and negated filename to \"~/.gitignore\". If accessible, visit the file."
  (interactive)
  (let (cmmnt curbuflst dir flnm flnmexp gitigbuf gitigflnm gitigflnmexp)
    (catch 'outer
      (catch 'inner
        ;; Store current buffer list in "curbuflst".
        (setq curbuflst (buffer-list))
        ;; Store path to "~/.gitignore" in "gitigflnm".
        (setq gitigflnm "~/.gitignore")
        ;; Store expansion of "gitigflnm" in "gitigflnmexp".
        (setq gitigflnmexp (expand-file-name gitigflnm))
        ;; Ask for filename, store it in "flnm".
        (setq flnm (read-file-name "Filename: "))
        ;; Store expansion of "flnm" in "flnmexp".
        (setq flnmexp (expand-file-name flnm))
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
        (setq flnm (replace-regexp-in-string (concat "\\(^~/\\)\\|\\(^" (expand-file-name "~/") "\\)") "" flnm))
        (setq flnmexp (replace-regexp-in-string (concat "\\(^~/\\)\\|\\(^" (expand-file-name "~/") "\\)") "" flnmexp))
        ;; Visit "gitigflnmexp" and store the resulting buffer in "gitigbuf".
        (save-excursion
	(setq gitigbuf (find-file-noselect gitigflnmexp))
	;; Make "gitigbuf" the current buffer.
	(set-buffer gitigbuf)
	(goto-char (point-min))
	;; If the supplied filename (or its expanded equivalent) is already present in "gitigbuf" (either in its blacklist or in its whitelist), inform about it and skip ahead to visiting the specified file.
	(if (search-forward-regexp (concat "^" flnm "$") nil t)
	    (throw 'inner (message "Filename %s is already present in %s’s blacklist." flnm gitigflnm)))
	(if (search-forward-regexp (concat "^" flnmexp "$") nil t)
	    (throw 'inner (message "Filename %s is already present in %s’s blacklist." flnmexp gitigflnm)))
	(if (search-forward-regexp (concat "^!" flnm "$") nil t)
	    (throw 'inner (message "Filename %s is already present in %s’s whitelist." flnm gitigflnm)))
	(if (search-forward-regexp (concat "^!" flnmexp "$") nil t)
	    (throw 'inner (message "Filename %s is already present in %s’s whitelist." flnmexp gitigflnm)))
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
      (find-file flnm))))

(defun my-visit-multiple-files (LIST)
  "Visit the files mentioned in LIST in the given order."
  ()
  (dolist (FILE LIST)
    (find-file FILE)))

(defun my-visual-line-mode-on ()
  "Turn visual line mode ON."
  (visual-line-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for mode dependent key bindings. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-Emacs-Lisp-mode-bindings ()
  "This function contains custom key bindings intended for use in Emacs-Lisp mode.
The bindings are:
C-c C-u    my-comment-or-uncomment"
  (local-set-key "\C-c\C-u" 'my-comment-or-uncomment))

(defun my-ESS_S_-mode-bindings ()
  "This function contains custom key bindings intended for use in ESS[S] mode.
The bindings are:
C-c C-u    my-comment-or-uncomment
C-c C-p    ess-eval-paragraph"
  (local-set-key "\C-c\C-u" 'my-comment-or-uncomment)
  (local-set-key "\C-c\C-p" 'ess-eval-paragraph))

(defun my-LaTeX/P-mode-bindings ()
  "This function contains custom key bindings intended for use in LaTeX/P mode.
The bindings are:
C-c C-u    my-comment-or-uncomment
RET        reindent-then-newline-and-indent
C-c C-y    my-delete-and-insert-blank-lines"
  (local-set-key "\C-c\C-u" 'my-comment-or-uncomment)
  (local-set-key "\r" 'reindent-then-newline-and-indent)
  (local-set-key "\C-c\C-y" 'my-delete-and-insert-blank-lines))

(defun my-LaTeX/P-mode-symbol-additions ()
  "This function uses `TeX-add-symbols´ to add symbols to the list of symbols known by AUCTeX in LaTeX/P mode.

The symbols and their descriptions are:
textcite  TeX-arg-cite
parencite TeX-arg-cite"
  (TeX-add-symbols
   '("textcite" TeX-arg-cite)
   '("parencite" TeX-arg-cite)))

(defun my-Lisp-Interaction-mode-bindings ()
  "This function contains custom key bindings intended for use in Lisp Interaction mode.
The bindings are:
C-c C-u    my-comment-or-uncomment"
  (local-set-key "\C-c\C-u" 'my-comment-or-uncomment))

(defun my-Octave-mode-bindings ()
  "This function contains custom key bindings intended for use in Octave mode.
The bindings are:
C-c C-u    my-comment-or-uncomment"
  (local-set-key "\C-c\C-u" 'my-comment-or-uncomment))

(defun my-Org-mode-bindings ()
  "This function contains custom key bindings intended for use in Org mode.
The bindings are:
RET        org-return-indent
C-j        org-return"
  (local-set-key "\r" 'org-return-indent)
  (local-set-key "\C-j" 'org-return))

(defun my-Python-mode-bindings ()
  "This function contains custom key bindings intended for use in Python mode.
The bindings are:
C-c C-u    my-comment-or-uncomment"
  (local-set-key "\C-c\C-u" 'my-comment-or-uncomment))

(defun my-Shell-script-mode-bindings ()
  "This function contains custom key bindings intended for use in Shell-script mode.
The bindings are:
C-c C-u    my-comment-or-uncomment"
  (local-set-key "\C-c\C-u" 'my-comment-or-uncomment))
