(defun my-move-beginning-of-line ()
    "Move point to beginning of current line as displayed. If called twice in succession, skip over text matching `my-move-beginning-of-line-skip-regexp'."
    (interactive)
    (let ((match-data-old (match-data))
	)
      (unwind-protect
	(progn
	  )
        (set-match-data match-data-old))))
