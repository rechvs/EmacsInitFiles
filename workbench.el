(progn
  (defvar my-newline-at-col
    78
    "Target column for `my-newline-at'.")
  )

(defun my-newline-at (pref-arg-raw pref-arg-num)
  "Insert newline character after the last whitespace (space or tab)
character before or in the column specified by the prefix argument or, if
that is nil, by `my-newline-at-col'. Move point to next line. If prefix
argument is non-nil, set `my-new-line-at-col' to its value."
  (interactive "P\np")
  (let* ((cur-col (current-column))
         (cur-line-length (save-excursion (move-end-of-line nil) (current-column)))
         display-message
         (match-data-old (match-data))
         (my-newline-at-col-old my-newline-at-col)
         (target-col (if pref-arg-raw pref-arg-num my-newline-at-col))
         (display-message (concat "Target column " (number-to-string target-col))))
    (unwind-protect
        (progn
	(if (< pref-arg-num 0)
	    (error "Negative prefix argument"))
	(if (<= cur-line-length target-col)
	    (next-line)
	  (progn
	    (move-to-column target-col)
	    (if (not (looking-back "[ 	]" (1- (point))))
	        (skip-chars-backward "^ 	" (point-at-bol)))
	    (newline-and-indent)
	    (move-to-column cur-col)
	    (if pref-arg-raw
	        (progn (setq my-newline-at-col pref-arg-num)
		     (setq display-message (concat display-message " (old was " (number-to-string my-newline-at-col-old) ")"))))
	    (display-message-or-buffer (concat display-message ".")))))
      (set-match-data match-data-old))))
