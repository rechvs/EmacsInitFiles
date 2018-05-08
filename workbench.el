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
          (skip-chars-forward (concat "^" opening-paren closing-paren) (point-max))
          (catch 'here
            (while t
              (skip-chars-forward (concat "^" opening-paren closing-paren) (point-max))
              (if (and (looking-at (regexp-quote closing-paren)) (= parens-level 0)) (throw 'here nil))
              (if (looking-at (regexp-quote opening-paren))
                  (progn
                    (setq parens-level (1+ parens-level))
                    (goto-char (1+ (point)))))
              (if (looking-at (regexp-quote closing-paren))
                  (progn
                    (setq parens-level (1- parens-level))
                    (goto-char (1+ (point))))) 
              (if (= (point) (point-max)) (error error-message-2))))
          (delete-region parens-beg (point)))
      (set-match-data match-data-old))))



(local-set-key (kbd "C-c SPC") 'my-delete-parens-content)

\input{../LebenslaufDeutschModerncv01Praeambel.tex}

% Color settings.
\definecolor{MyUrlColor}{HTML}{008000} % Green

% Signature.
\newcommand{\MySignature}
{\hspace{0.1em}\includegraphics{../../../../Bilder/Unterschrift01.png}}

\input{../LebenslaufDeutschModerncv01Text.tex}
