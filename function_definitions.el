;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous functions ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-abbrev-mode-on ()
  "Turn abbrev mode ON."
  (abbrev-mode 1))

(defun my-auctex-man ()
  "Go to bookmark 'auctex-man', split frame into two windows and enable follow-mode."
  (interactive)
;  (bookmark-bmenu-save "~/.emacs.d/bookmarks")
  (bookmark-load "~/.emacs.d/bookmarks" "t")
  (bookmark-jump "auctex-man")
  (delete-other-windows)
  (split-window-horizontally)
  (follow-mode "t"))

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

(defun my-elisp-int ()
  "Go to bookmark 'elisp-int', split frame into two windows and enable follow-mode."
  (interactive)
;  (bookmark-bmenu-save "~/.emacs.d/bookmarks")
  (bookmark-load "~/.emacs.d/bookmarks" "t")
  (bookmark-jump "elisp-int")
  (delete-other-windows)
  (split-window-horizontally)
  (follow-mode "t"))

(defun my-elisp-ref ()
  "Go to bookmark 'elisp-ref', split frame into two windows and enable follow-mode."
  (interactive)
;  (bookmark-bmenu-save "~/.emacs.d/bookmarks")
  (bookmark-load "~/.emacs.d/bookmarks" "t")
  (bookmark-jump "elisp-ref")
  (delete-other-windows)
  (split-window-horizontally)
  (follow-mode "t"))

(defun my-emacs-man ()
  "Go to bookmark 'emacs-man', split frame into two windows and enable follow-mode."
  (interactive)
;  (bookmark-bmenu-save "~/.emacs.d/bookmarks")
  (bookmark-load "~/.emacs.d/bookmarks" "t")
  (bookmark-jump "emacs-man")
  (delete-other-windows)
  (split-window-horizontally)
  (follow-mode "t"))

(defun my-ess-man ()
  "Go to bookmark 'ess-man', split frame into two windows and enable follow-mode."
  (interactive)
;  (bookmark-bmenu-save "~/.emacs.d/bookmarks")
  (bookmark-load "~/.emacs.d/bookmarks" "t")
  (bookmark-jump "ess-man")
  (delete-other-windows)
  (split-window-horizontally)
  (follow-mode "t"))

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

(defun my-gnus-man ()
  "Go to bookmark 'gnus-man', split frame into two windows and enable follow-mode."
  (interactive)
;  (bookmark-bmenu-save "~/.emacs.d/bookmarks")
  (bookmark-load "~/.emacs.d/bookmarks" "t")
  (bookmark-jump "gnus-man")
  (delete-other-windows)
  (split-window-horizontally)
  (follow-mode "t"))

(defun my-hiwi-ssh-login-server ()
  "Visit directory `/home/aknohl´ on host `134.76.19.50 ´ as user `aknohl´ via `ssh´."
  (interactive)
  (find-file "/ssh:aknohl@134.76.19.50:/home/aknohl"))

(defun my-immediately-switch-to-buffer ()
  "Immediately switch to the most recently selected buffer other than the current buffer, disregarding buffers already visible. If called in succession, cycle through the list returned by `buffer-list'"
  (interactive)
  (let (WINDOW-LIST (WINDOW_NR 0) (BUFFER-NAME-LIST (list "")) (BUFFER_NR 0) (BUFFER_NEXT_IN_ROW ""))
    (if (string-match "\ \*Minibuf-+[0-9]*[0-9]\*" (buffer-name (current-buffer)))
        (display-message-or-buffer "Currently in minibuffer.")
      (progn
        (setq WINDOW-LIST (window-list))
        (while (<= WINDOW_NR (- (length WINDOW-LIST) 1))
	(setf (nth WINDOW_NR BUFFER-NAME-LIST) (substring (nth 3 (split-string (pp-to-string (nth WINDOW_NR WINDOW-LIST)) " ")) 0 -1))
	(if (< WINDOW_NR (- (length WINDOW-LIST) 1))
	    (setq BUFFER-NAME-LIST (append BUFFER-NAME-LIST (list ""))))
	(setq WINDOW_NR (+ WINDOW_NR 1)))
        (if
	  (eq last-command 'my-immediately-switch-to-buffer)
	  (setq my-immediately-switch-to-buffer-counter (+ 1 my-immediately-switch-to-buffer-counter))
	(setq my-immediately-switch-to-buffer-counter 1))
        (setq BUFFER_NEXT_IN_ROW (buffer-name (nth my-immediately-switch-to-buffer-counter (buffer-list))))
        (while (or (member BUFFER_NEXT_IN_ROW BUFFER-NAME-LIST)
	         (my-string-match-list my-immediately-switch-to-buffer-excluded-buffers BUFFER_NEXT_IN_ROW))
	(setq my-immediately-switch-to-buffer-counter (+ 1 my-immediately-switch-to-buffer-counter))
	(setq BUFFER_NEXT_IN_ROW (buffer-name (nth my-immediately-switch-to-buffer-counter (buffer-list)))))
        (switch-to-buffer BUFFER_NEXT_IN_ROW)))))

(defun my-load-bookmarks-on-startup ()
  "Uses the function bookmark-load."
  (bookmark-load "~/.emacs.d/bookmarks" "t"))

(defun my-mail-man ()
  "Go to bookmark 'mail-man', split frame into two windows and enable follow-mode."
  (interactive)
;  (bookmark-bmenu-save "~/.emacs.d/bookmarks")
  (bookmark-load "~/.emacs.d/bookmarks" "t")
  (bookmark-jump "mail-man")
  (delete-other-windows)
  (split-window-horizontally)
  (follow-mode "t"))

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

(defun my-octave-man ()
  "Go to bookmark 'octave-man', split frame into two windows and enable follow-mode."
  (interactive)
;  (bookmark-bmenu-save "~/.emacs.d/bookmarks")
  (bookmark-load "~/.emacs.d/bookmarks" "t")
  (bookmark-jump "octave-man")
  (delete-other-windows)
  (split-window-horizontally)
  (follow-mode "t"))

(defun my-org-man ()
  "Go to bookmark 'org-man', split frame into two windows and enable follow-mode."
  (interactive)
;  (bookmark-bmenu-save "~/.emacs.d/bookmarks")
  (bookmark-load "~/.emacs.d/bookmarks" "t")
  (bookmark-jump "org-man")
  (delete-other-windows)
  (split-window-horizontally)
  (follow-mode "t"))

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
      (ess-request-a-process nil t nil))))

(defun my-string-match-list (LIST STRING)
  "Match string STRING against the elements of list LIST (using `string-match'). Return t upon finding the first match, otherwise retunr nil."
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
  "If exactly two vertically separated windows are displayed, switch the buffers in them. When called without a prefix argument select the previously selected buffer. When called with a prefix argument select the previously unselected buffer."
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