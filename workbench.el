(defun my-insert-enclosing-parens (arg &optional reg-beg reg-end)
  "In Transient Mark mode, if the mark is active, insert opening and closing parentheses around the region and deactivate the mark, leaving point after the closing parenthesis. If prefix argument ARG is non-nil, insert opening and closing parentheses at point, leaving point inside them. Otherwise, insert an opening parenthesis at point."
  (interactive "P\nr")
  (let ((reg-beg (and (use-region-p) reg-beg))
        (reg-end (and (use-region-p) reg-end))
        (cur-point (point)))
    (if (and reg-beg reg-end)
        (progn
          (goto-char reg-beg)
          (insert "(")
          (goto-char (1+ reg-end))
          (insert ")"))
      (if arg
          (progn (insert "(") (insert ")") (goto-char (1+ cur-point)))
        (insert "(")))))

(local-set-key (kbd "(") 'my-insert-enclosing-parens)
         
  ;; BEGIN TESTING
  (display-message-or-buffer "test"))
  ;; END TESTING

((()abc)()
