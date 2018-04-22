(defcustom my-regexp-url-identifier
  "^file://\\|^mailto:\\|^\\(ftp://\\|https?://\\|www\\.\\).*\\.org\\|\\.\\(com\\|de\\)"
  "Regexp with which to identify a URL."
  :type '(regexp))

(defun my-find-or-browse-region-or-at-point (&optional arg)
  "Use a string as a local filename/a URL which to visit/browse. If region is 
active, use text in region as the string; otherwise use text around point. 
The characters delimiting the string from surrounding text are set via 
`my-find-region-or-at-point-delim-chars'. `my-regexp-url-identifier' 
determines what constitutes a URL; every string not matching it is considered 
a local filename. Without prefix argument ARG, a non-existent local filename 
results in an error. An empty string results in an error."
  (interactive "P")
  (let* ((flnm (if (region-active-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (buffer-substring-no-properties
                  (save-excursion (skip-chars-backward (concat "^" my-find-region-or-at-point-delim-chars) (line-beginning-position)) (point))
                  (save-excursion (skip-chars-forward (concat "^" my-find-region-or-at-point-delim-chars) (line-end-position)) (point)))))
         (flnm-is-url (string-match my-regexp-url-identifier flnm))
         (match-data-old (match-data)))
    (unwind-protect
        (if (string= "" flnm)
            (error "Empty string")
          (if (region-active-p) (deactivate-mark t))
          (if arg
              (if flnm-is-url (browse-url flnm) (find-file flnm))
            (if (file-exists-p flnm) (if flnm-is-url (browse-url flnm) (find-file flnm))
              (if flnm-is-url (browse-url flnm) (error "File %s does not exist" flnm)))))
      (set-match-data match-data-old))))
