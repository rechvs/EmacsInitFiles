(defun gnus-user-format-function-a (hdr)
  "Return either the To: and Cc: header, or the From: header contained in
vector HDR; do the former if the From: header either matches
`my-email-addresses-regexp' or `my-email-addressees-regexp' or is empty; do
the latter otherwise. In all cases, prefer the full name over the raw email
address. Empty headers and To: and Cc: headers matching
`my-email-undisclosed-recipients-regexp' are replaced with \"NA\". If HDR is
not a vector, return \"void\". See `gnus-summary-line-format' on how this
function interfaces with Gnus."
  (if (null (vectorp hdr))
      "void"
    (let* ((from (nth 0 (mail-extract-address-components (mail-header-from hdr) t)))
           (from-string (or (nth 0 from) (nth 1 from) "NA"))
           (to-all (mail-extract-address-components (or (cdr (assoc 'To (mail-header-extra hdr))) "") t))
           (cc-all (mail-extract-address-components (or (cdr (assoc 'Cc (mail-header-extra hdr))) "") t))
           (to-cc-all (union to-all cc-all :test 'equal))
           to-cc-all-format-string
           (to-cc-all-string "NA"))
      (if (>= (length to-cc-all) 1)
          (progn
            (setq to-cc-all-format-string (apply 'concat (make-list (length to-cc-all) "%s, ")))
            (setq to-cc-all-format-string (substring to-cc-all-format-string 0 (- (length to-cc-all-format-string) 2)))
            (setq to-cc-all-string (apply 'format to-cc-all-format-string (mapcar (lambda (sublist)
                                                                                    (or (nth 0 sublist) (nth 1 sublist) "NA"))
                                                                                  to-cc-all)))))
      (or (null (string-match-p my-email-undisclosed-recipients-regexp to-cc-all-string)) (setq to-cc-all-string "NA"))
      (if (or (string-match-p my-email-addresses-regexp from-string)
              (string-match-p my-email-addressees-regexp from-string)
              (string= "NA" from-string))
          to-cc-all-string
        from-string))))
