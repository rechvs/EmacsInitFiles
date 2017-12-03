;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous functions ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-abbrev-mode-on ()
  "Turn abbrev mode ON."
  (abbrev-mode 1))

(defun my-comment-or-uncomment (ARG1 ARG2)
  "When region is not active, comment the current line if uncommented, otherwise uncomment it. When region is active, comment uncommented lines and uncomment commented lines that are at least partly contained in region. Lines consisting only of a newline character or of whitespace and a newline character are skipped."
  (interactive "r")
  (let (ARG_DUMMY (CURRENT_LINE_NUMBER 1) LINE_BEG_FIRST_LINE LINE_BEG LINE_END)
    (if mark-active
        (progn
	(if (> ARG1 ARG2)
	    (setq ARG_DUMMY ARG2 ARG2 ARG1 ARG1 ARG_DUMMY))
	(setq NUMBER_OF_LINES (count-lines ARG1 ARG2))
	(save-excursion
	  (goto-char ARG1)
	  (move-beginning-of-line nil)
	  (setq LINE_BEG_FIRST_LINE (point))
	  (while (not (> CURRENT_LINE_NUMBER NUMBER_OF_LINES))
	    (goto-char LINE_BEG_FIRST_LINE)
	    (forward-line (- CURRENT_LINE_NUMBER 1))
	    (move-end-of-line nil)
	    (setq LINE_END (point))
	    (move-beginning-of-line nil)
	    (setq LINE_BEG (point))
	    (skip-syntax-forward " " LINE_END)
	    (if (and (not (= LINE_BEG LINE_END)) (not (= LINE_END (point))))
	        (if (equal (string (char-after)) comment-start)
		  (uncomment-region LINE_BEG LINE_END)
		(comment-region LINE_BEG LINE_END)))
	    (setq CURRENT_LINE_NUMBER (+ CURRENT_LINE_NUMBER 1)))))
      (progn
        (save-excursion
	(move-end-of-line nil)
	(setq LINE_END (point))
	(move-beginning-of-line nil)
	(setq LINE_BEG (point))
	(skip-syntax-forward " " LINE_END)
	(if (and (not (= LINE_BEG LINE_END)) (not (= LINE_END (point))))
	    (if (equal (string (char-after)) comment-start)
	        (uncomment-region LINE_BEG LINE_END)
	      (comment-region LINE_BEG LINE_END))))))))

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

;(defun my-glossary-entry (name)
;  "Insert \paragraph and \label using the input string."
;  (interactive "sName des Eintrags: ")
;  (insert (format "\\paragraph{%s}
;\\label{%s}\n
;" name name)
;          )
;  )
;
;(defun my-bind-glossary-entry ()
;  "Bind my-glossary-entry to C-c C-a."
;  (local-set-key "\C-c\C-a" 'my-glossary-entry))

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
	;; (deactivate-mark t)
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
      ;; If it exists, visit FILENAME.
      (if (file-exists-p FILENAME)
	(find-file FILENAME)
        ;; If FILENAME does not exist, message the user about it.
        ;; In order to distinguish FILENAME from the rest of the message, we can use either quotation marks...
        ;; (message "File \"%s\" does not exist." FILENAME)))))
        ;; ...or text color.
        (add-face-text-property 0 (length FILENAME) '(:foreground "blue") t FILENAME)
        (message "File %s does not exist." FILENAME)))))

(defun my-follow-bookmark (BKMK)
  "Call (`delete-other-windows'), open bookmark BKMK in two vertically separated windows and activate follow-mode."
  (interactive
   (list
    (let (BKMKS)
      (bookmark-load "~/.emacs.d/bookmarks" t)
      (setq BKMKS (bookmark-all-names))
      (setq BKMK (completing-read "Bookmark: " BKMKS))
      (bookmark-jump BKMK)
   (delete-other-windows)
   (split-window-horizontally)
   (follow-mode t)))))

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
	;; ...and set "BUFFER_NEXT_IN_ROW" to the buffer name at position "my-immediately-switch-to-buffer-counter" in the list given by "buffer-list".
	(setq BUFFER_NEXT_IN_ROW (buffer-name (nth my-immediately-switch-to-buffer-counter (buffer-list)))))
        ;; Switch to buffer "BUFFER_NEXT_IN_ROW".
        (switch-to-buffer BUFFER_NEXT_IN_ROW)))))

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

(defun my-sh-smie-sh-rules (origfun kind token)
  "Custom function to correct indentation in shell scripts after `&&´. Taken from `http://superuser.com/questions/1037043/emacs-suppress-indent-after-in-shell-script-mode´."
  (pcase (cons kind token)
    (`(:before . "&&") nil)
    (_ (funcall origfun kind token))))
(advice-add 'sh-smie-sh-rules :around #'my-sh-smie-sh-rules)

(defun my-start-or-switch-to-r-process ()
  "If an `R´ process is running, switch to the corresponding buffer. If not, switch to buffer `*R*´ and start an `R´ process using (`ess-request-a-process' nil t nil)."
  (interactive)
  (if (get-process "R")
      (switch-to-buffer "*R*")
    (progn
      (switch-to-buffer "*R*")
      (R))))

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
        ;; Visit "gitigflnmexp" and store the resulting buffer in "gitigbuf".
        (save-excursion
	(setq gitigbuf (find-file-noselect gitigflnmexp))
	;; Make "gitigbuf" the current buffer.
	(set-buffer gitigbuf)
	(beginning-of-buffer)
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
	(end-of-buffer)
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
C-c C-u    my-comment-or-uncomment"
  (local-set-key "\C-c\C-u" 'my-comment-or-uncomment))

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
