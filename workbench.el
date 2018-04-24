(defun gnus-user-format-function-a (hdr)
  "If HDR is not a vector, return the string \"void\". Otherwise,
if one of my addresses is contained in either the To: or the
Cc: header or if both these headers are empty, return the From:
header; else return the To: header or, if that is empty, the
Cc: header. In all cases, prefer the full name contained in the
header if present, otherwise use only the email address. See
`gnus-summary-line-format' on how this function interfaces with
Gnus."
  (if (not (vectorp hdr))
      "void"
    (let ((from (mail-extract-address-components (mail-header-from hdr)))
          (to (cdr (assoc 'To (mail-header-extra hdr))))
          (to-all (mail-extract-address-components (or (cdr (assoc 'To (mail-header-extra hdr))) "") t))
          (cc (cdr (assoc 'Cc (mail-header-extra hdr))))
          (cc-all (mail-extract-address-components (or (cdr (assoc 'Cc (mail-header-extra hdr))) "") t))
          to-cc-all
          (to-cc-all-string "")
          to-all-format-string
          cc-all-format-string
          to-cc-all-format-string
          ;; (ingorable-adresses-regexp "\\(\\(magic_willebinski\\)\\|\\(members\\)\\|\\(renke.vonseggern\\)\\)@gmx\\.\\(\\(de\\)\\|\\(net\\)\\)")
          (ingorable-adresses-regexp "\\(magic_willebinski\\|members\\|renke\\.vonseggern\\)@gmx\\.\\(de\\|net\\)")
          )
      ;; BEGIN TESTING
      ;; (if
      ;;           (or (string-match ".*woidich.*" (prin1-to-string (cdr (assoc 'To (mail-header-extra hdr)))))
      ;;               (string-match ".*brink.*" (prin1-to-string (cdr (assoc 'To (mail-header-extra hdr))))))
      ;;           ;; t
      ;;           (display-message-or-buffer (concat
      ;;                                       ;; "To: "
      ;;                                       ;; (prin1-to-string
      ;;                                        ;; (elt (elt (elt (cdr eight) 0) (1- (length (elt (cdr eight) 0)))) 0))
      ;;                                       "
      ;; Address Components Cdr Assoc To: "
      ;;                                       (prin1-to-string
      ;;                                        ;; (mail-extract-address-components (cdr (assoc 'To (mail-header-extra hdr))))
      ;;                                        (mail-extract-address-components (cdr (assoc 'To (mail-header-extra hdr))) t))
      ;;                                        ;; (if (or (string= "" (nth 0 (nth 0 (mail-extract-address-components (or (cdr (assoc 'To (mail-header-extra hdr))) "") t))))
      ;;                                                ;; (null (nth 0 (nth 0 (mail-extract-address-components (or (cdr (assoc 'To (mail-header-extra hdr))) "") t)))))
      ;;                                            ;; (nth 1 (nth 0 (mail-extract-address-components (or (cdr (assoc 'To (mail-header-extra hdr))) "") t)))
      ;;                                          ;; (nth 0 (nth 0 (mail-extract-address-components (or (cdr (assoc 'To (mail-header-extra hdr))) "") t)))
      ;;                                        ;; ))
      ;; ;;                                       "
      ;; ;; Cc: "
      ;; ;;                                       (prin1-to-string
      ;; ;;                                        (elt (elt (elt (cdr eight) 0) (1- (length (elt (cdr eight) 0)))) 1))
      ;; ;;                                       "
      ;; ;; Address Components Cdr Assoc Cc: "
      ;; ;;                                       (prin1-to-string
      ;; ;;                                        ;; (mail-extract-address-components (cdr (assoc 'Cc (mail-header-extra hdr))))
      ;; ;;                                        (mail-extract-address-components (cdr (assoc 'Cc (mail-header-extra hdr))) t)
      ;; ;;                                        )
      ;;                                       ;; "
      ;;                                       ;; Recipients: "
      ;;                                       ;; (prin1-to-string
      ;;                                       ;; (elt (elt (cdr eight) 0) (1- (length (elt (cdr eight) 0)))))
      ;;                                       ;; "
      ;;                                       ;; Cdr Assoc To: "
      ;;                                       ;; (prin1-to-string
      ;;                                       ;; (cdr (assoc 'To (mail-header-extra hdr))))
      ;;                                       ;; "
      ;;                                       ;; Type Cdr Assoc To: "
      ;;                                       ;; (prin1-to-string
      ;;                                       ;; (type-of (cdr (assoc 'To (mail-header-extra hdr)))))
      ;;                                       ;; "
      ;;                                       ;; Cdr Assoc Cc: "
      ;;                                       ;; (prin1-to-string
      ;;                                       ;; (cdr (assoc 'Cc (mail-header-extra hdr))))
      ;;                                       )))
      ;; (concat
       ;; (prin1-to-string (or (nth 0 (nth 0 to-all))
      ;; (nth 1 (nth 0 to-all)))))
      ;; (setq to-all-format-string (apply 'concat (make-list (length to-all) "%s, ")))
      ;; (setq to-all-format-string (substring to-all-format-string 0 (1- (length to-all-format-string))))
      ;; (apply 'format to-all-format-string (mapcar (lambda (sublist)
                                                     ;; (or
                                                      ;; (nth 0 sublist)
                                                      ;; (nth 1 sublist)
                                                     ;; ))
                                                  ;; to-all))
      (setq to-cc-all (union to-all cc-all :test 'equal))
      (if (>= (length to-cc-all) 1)
          (progn
            (setq to-cc-all-format-string (apply 'concat (make-list (length to-cc-all) "%s, ")))
            (setq to-cc-all-format-string (substring to-cc-all-format-string 0 (- (length to-cc-all-format-string) 2)))
            (setq to-cc-all-string (apply 'format to-cc-all-format-string (mapcar (lambda (sublist)
                                                                                    (or
                                                                                     (nth 0 sublist)
                                                                                     (nth 1 sublist)
                                                                                     ))
                                                                                  to-cc-all)))))
      (if (or (string-match-p ingorable-adresses-regexp to-cc-all-string)
              (string-match-p "[rR]enke\\( Christian\\)?\\( von [sS]eggern\\)?" to-cc-all-string))
          "bla"
        to-cc-all-string
        ;; (prin1-to-string (length to-cc-all))
        )
      )))
;; END TESTING
      (if (or (string-match-p ingorable-adresses-regexp (prin1-to-string (nth 0 from)))
              (string-match-p ingorable-adresses-regexp (prin1-to-string (nth 1 from))))
          (if (or (string-match-p ingorable-adresses-regexp (prin1-to-string (nth 0 to)))
                  (string-match-p ingorable-adresses-regexp (prin1-to-string (nth 1 to))))
              (if (or (string-match-p ingorable-adresses-regexp (prin1-to-string (nth 0 cc)))
                      (string-match-p ingorable-adresses-regexp (prin1-to-string (nth 1 cc))))
                  "NA"
                (or (nth 0 cc) (nth 1 cc)))
            (or (nth 0 to) (nth 1 to)))
        (or (nth 0 from) (nth 1 from))))))



