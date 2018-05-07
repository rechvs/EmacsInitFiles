(defun my-clear-parens-content ()
  "When inside balanced parentheses, clear all text between them. Otherwise, signal an error. \"Parentheses\" here means the following characters: \"()\", \"[]\", and \"{}\"."
  (interactive)
  (let ((error-message "Not inside balanced parentheses")
        parens-beg
        opening-paren
        closing-paren)
    (condition-case nil
        (save-excursion
          (skip-syntax-backward "^()" (point-min))
          (setq parens-beg (point))
          (goto-char (1- (point)))
          (forward-list)
          (goto-char (1- (point)))
          (delete-region parens-beg (point)))
      ('error (error error-message)))))

(ass asdfasdf=asdfasdfasdf)
[asdfsadf]
{asdfasdf}
