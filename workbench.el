(defun my-delete-parens-content ()
  "If a matching pair of opening and closing parentheses before and after 
point is found, delete all text between it. \"Parentheses\" here means the 
following characters: \"()\", \"[]\", and \"{}\"."
  (interactive)
  (let ((error-message-1 "No opening parenthesis found")
        (error-message-2 "Not inside balanced parentheses")
        parens-beg
        opening-paren
        closing-paren
        (opening-parens-regexp "(\\|\\[\\|{")
        (match-data-old (match-data)))
    (unwind-protect
        (save-excursion
          (skip-chars-backward "^([{)]}" (point-min))
          (setq parens-beg (point))
          (goto-char (1- (point)))
          (if (looking-at opening-parens-regexp)
              (setq opening-paren (buffer-substring-no-properties (match-beginning 0) (match-end 0)))
            (error error-message-1))
          (setq closing-paren (cond ((string= "(" opening-paren) ")")
                                    ((string= "[" opening-paren) "]")
                                    ((string= "{" opening-paren) "}")))
          (skip-chars-forward (concat "^" closing-paren) (point-max))
          (or (looking-at (regexp-quote closing-paren)) (error error-message-2))
          (delete-region parens-beg (point)))
      (set-match-data match-data-old))))
