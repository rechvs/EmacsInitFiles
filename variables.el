;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings NOT involving "defcustom" or "defvar". ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable certain commands which are disabled by default. See Emacs info "Disabling".
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Disable certain commands which are enabled by default.
(put 'TeX-command-buffer 'disabled t)
(put 'save-buffers-kill-terminal 'disabled t)

;; Add "/usr/local/texlive/2017/bin/i386-linux" to "PATH" so AUCTeX finds it (http://tex.stackexchange.com/questions/24510/pdflatex-fails-within-emacs-app-but-works-in-terminal).
(setenv "PATH"
        (concat
         "/usr/local/texlive/2017/bin/i386-linux"
         ":"

         (getenv "PATH")))

;; Add "parencite" and "textcite" to list of symbols known in style "article" and dialect "LaTeX-dialect" (s. AUCTeX info 5.6.2) such that AUCTeX can ask for an optional argument.
(eval-after-load "latex"
  '(progn
     (TeX-add-style-hook
      "article"
      (lambda ()
        (TeX-add-symbols
         '("parencite"
	 (TeX-arg-conditional TeX-arg-cite-note-p ([ "Note" ]) ())
	 TeX-arg-cite)
         '("textcite"
	 (TeX-arg-conditional TeX-arg-cite-note-p ([ "Note" ]) ())
	 TeX-arg-cite)))
      LaTeX-dialect)))

;; Load certain variables whose standard value are too long to include directly in this file.
(load "~/.emacs.d/init-files/variables/my-man-known-programs-only.el" t t)
(load "~/.emacs.d/init-files/variables/my-man-known-programs-plus-sections.el" t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings involving "defcustom" or "defvar". ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom my-back-to-indentation-skip-chars
  " "
  "Regexp-like string containing the characters at the beginning of a line to be skipped by `my-back-to-indentation'. See `skip-chars-forward' for the string syntax."
  :type '(string))
(make-variable-buffer-local 'my-back-to-indentation-skip-chars)

(defvar my-Emacs-issue-index
  200
  "The index of the next private Emacs issue.")

(defcustom my-files-to-visit-at-startup
  '("/home/renke/Privat/Privat.org")
  "A list of paths to files which ought to be visited at startup."
  :type '(repeat file))

(defcustom my-find-region-or-at-point-delim-chars
  '"[][{}():\"'„“”][:space:]"
  "A regexp-like string of characters used as delimiting characters by `my-find-region-or-at-point' (see argument STRING of `skip-chars-forward')."
  :type '(string))

(defvar my-immediately-switch-to-buffer-counter
  0
  "Number of buffers in `buffer-list' to skip by `my-immediately-switch-to-buffer'.") 

(defcustom my-immediately-switch-to-buffer-excluded-buffers
  '("\\*Minibuf-+[0-9]*[0-9]\\*")
  "A list containing buffer names as regular expressions which should not be switched to by `my-immediately-switch-to-buffer'."
  :type '(repeat regexp))

(defvar my-man-input-hist
  nil
  "List of man pages invoked by the user.")

(defvar my-man-known-programs-skip-chars-string
  "+\\-.0123456789:@ABCDEFGHIJKLMNOPQRSTUVWXYZ[_abcdefghijklmnopqrstuvwxyz"
  "String covering all characters of all program names known to man.") 

(defvar my-man-known-sections-only
  (list "1" "1p" "1ssl" "2" "3" "3am" "3caca" "3perl" "3pm" "3readline" "3ssl" "3tiff" "4" "5" "5ssl" "6" "6x" "7" "7ssl" "8")
  "List of all sections (without programs) known to man.")

(defvar my-man-known-sections-regexp
  "\\(\\(1\\)\\|\\(1p\\)\\|\\(1ssl\\)\\|\\(2\\)\\|\\(3\\)\\|\\(3am\\)\\|\\(3caca\\)\\|\\(3perl\\)\\|\\(3pm\\)\\|\\(3readline\\)\\|\\(3ssl\\)\\|\\(3tiff\\)\\|\\(4\\)\\|\\(5\\)\\|\\(5ssl\\)\\|\\(6\\)\\|\\(6x\\)\\|\\(7\\)\\|\\(7ssl\\)\\|\\(8\\)\\)"
  "Regexp covering all sections known to man.")

(defcustom my-move-beginning-of-line-skip-regexp
    "[[:space:]]+"
    "See `my-move-beginning-of-line'."
    :type '(regexp))
  (make-variable-buffer-local 'my-move-beginning-of-line-skip-regexp)
