(progn
  (defvar my-newline-at-col
    78
    "Target column for `my-newline-at'.")
  )

(defun my-newline-at (&optional col)
  "Insert newline character after the last whitespace (space or tab) character
before or in column COL or, if that is nil, column
`my-newline-at-col'. Move point to next line. When called interactively,
prefix argument is taken as COL. If prefix argument is non-nil, set
`my-new-line-at-col' to its value."
  (interactive "p")
  (let* ((cur-col (current-column))
         (cur-line-length (save-excursion (move-end-of-line nil) (current-column)))
         display-message
         (match-data-old (match-data))
         (my-newline-at-col-old my-newline-at-col)
         (target-col (if (and (numberp col) (or (and (not col) (not current-prefix-arg)) (and (= col 1) (not current-prefix-arg)))) my-newline-at-col col))
         (display-message (concat "Target column " (prin1-to-string target-col))))
    (unwind-protect
        (progn
	(if (and col (not (numberp col)))
	    (error "Non-numeric column number"))
	(if (and (numberp col) (< col 0))
	    (error "Negative column number"))
	(if (<= cur-line-length target-col)
	    (next-line)
	  (progn
	    (move-to-column target-col)
	    (if (not (looking-back "[ 	]" (1- (point))))
	        (skip-chars-backward "^ 	" (point-at-bol)))
	    (if (= (point) (point-at-bol))
	        (error (concat "Target column number (" (number-to-string target-col) ") too low")))
	    (newline)))
	(if current-prefix-arg
	    (progn (setq my-newline-at-col col)
		 (setq display-message (concat display-message " (old was " (number-to-string my-newline-at-col-old) ")"))))
	(display-message-or-buffer (concat display-message ".")))
      (move-to-column cur-col)
      (set-match-data match-data-old))))
