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

;; add `/usr/local/texlive/2017/bin/i386-linux´ to `PATH´ so AUCTeX finds it (http://tex.stackexchange.com/questions/24510/pdflatex-fails-within-emacs-app-but-works-in-terminal):
(setenv "PATH"
        (concat
         "/usr/local/texlive/2017/bin/i386-linux"
         ":"

         (getenv "PATH")))

;; define a customizable variable `my-files-to-visit-at-startup´:
(defcustom my-files-to-visit-at-startup
  '("/home/renke/Privat/Privat.org")
  "A variable containing the paths to files which ought to be visited at startup."
  :type '(repeat file))

;; define a customizable variable `my-immediately-switch-to-buffer-excluded-buffers´:
(defcustom my-immediately-switch-to-buffer-excluded-buffers
  '("\\*Minibuf-+[0-9]*[0-9]\\*")
  "A list containing buffer names as regular expressions which should not be switched to by `my-immediately-switch-to-buffer'."
  :type '(repeat regexp))

;; Define a customizable variable "my-find-region-or-at-point-delim-chars" which is used as the regexp of delimiting characters by "my-find-region-or-at-point".
(defcustom my-find-region-or-at-point-delim-chars
  '"[][{}():\"'„“”][:space:]"
  "A regexp-like string of characters used as delimiting characters by `my-find-region-or-at-point' (see argument STRING of `skip-chars-forward')."
  :type '(string))

;; add `parencite´ and `textcite´ to list of symbols known in style `article´ and dialect `LaTeX-dialect´ (s. AUCTeX info 5.6.2) such that AUCTeX can ask for an optional argument:
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

;; define a non-customizable variable `my-immediately-switch-to-buffer-counter´:
(defvar my-immediately-switch-to-buffer-counter
  0
  "Number of buffers in `buffer-list' to skip by `my-immediately-switch-to-buffer'.") 
