(defun my-delete-parens-content ()
  "Delete text between the opening and closing parentheses before and after 
point. While looking for the opening parenthesis before point, do not travel 
across closing parentheses. While looking for the matching closing 
parenthesis, do travel across balanced parentheses. \"Parentheses\" here 
means the following characters: \"()\", \"[]\", and \"{}\". "
  (interactive)
  (let ((error-message-1 "No opening parenthesis found")
        (error-message-2 "Not inside balanced parentheses")
        parens-beg
        opening-paren
        closing-paren
        (opening-parens-regexp "(\\|\\[\\|{")
        (parens-level 0)
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
          (goto-char (1+ (point)))
          (while (or (> parens-level 0) (null (looking-at (regexp-quote closing-paren))))
            (if (= (point) (point-max)) (error error-message-2))
            (if (looking-at (regexp-quote opening-paren))
                (progn
                  (setq parens-level (1+ parens-level))
                  (goto-char (1+ (point))))
              (if (looking-at (regexp-quote closing-paren))
                  (progn
                    (setq parens-level (1- parens-level))
                    (goto-char (1+ (point))))))
            (skip-chars-forward (concat "^" opening-paren closing-paren) (point-max)))
          (delete-region parens-beg (point)))
      (set-match-data match-data-old))))
