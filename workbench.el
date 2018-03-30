(progn
  (defcustom my-move-beginning-of-line-skip-regexp
    "[[:space:]]+"
    "See `my-move-beginning-of-line'."
    :type '(regexp))
  (make-variable-buffer-local 'my-move-beginning-of-line-skip-regexp)
  (setq my-move-beginning-of-line-skip-regexp "[[:space:]]+")
  )

(defun my-move-beginning-of-line ()
  "If point is not at the end of text matching `my-move-beginning-of-line-skip-regexp' (anchored at beginning of current visual line), move point there. Otherwise, move point to beginning of current visual line."
  (interactive)
  (let* ((match-data-old (match-data))
         (bol (save-excursion (beginning-of-visual-line) (point)))
         (eor (save-excursion (goto-char bol) (if (looking-at my-move-beginning-of-line-skip-regexp) (match-end 0) bol))))
    (unwind-protect
        (progn
	(if (not (= (point) eor))
	    (goto-char eor)
	  (goto-char bol)))
      (set-match-data match-data-old))))

(global-set-key (kbd "C-a") 'my-move-beginning-of-line)
