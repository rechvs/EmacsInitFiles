(defun gnus-user-format-function-a (hdr)
  "Return either the concatenated To: and Cc: headers, or the From: header contained in
vector HDR; do the former if the From: header either matches
`my-email-addresses-regexp' or `my-email-addressees-regexp' or is empty; do
the latter otherwise. In all cases, prefer the full name over the raw email
address. Empty headers and To: and Cc: headers matching
`my-email-undisclosed-recipients-regexp' are replaced with \"NA\". If HDR is
not a vector, return \"void\". See `gnus-summary-line-format' on how this
function interfaces with Gnus."
  (if (null (vectorp hdr))
      "void"
    (let* (
           (match-data-old (match-data))
           (from (mail-header-from hdr))
           from-format-string
           (from-string "NA")
           (to-cdr-assoc (cdr (assoc 'To (mail-header-extra hdr))))
           (cc-cdr-assoc (cdr (assoc 'Cc (mail-header-extra hdr))))
           (to-cc-all (concat to-cdr-assoc "," cc-cdr-assoc))
           to-cc-all-format-string
           (to-cc-all-string "NA")
           (email-address-regexp1 "<\\([^<]*@[^>]*\\)>")
           (email-address-regexp2 "\\(.*@.*\\)"))
      (unwind-protect
          (progn
            ;; Process From: header.
            (setq from (replace-regexp-in-string "[ 	]+" " " from))
            (while (or (string-match "\\(.*\"\\)\\([^,\"]+\\), \\([^,\"]+\\)\\(\".*\\)" from)
                       (string-match "\\(.*'\\)\\([^,']+\\), \\([^,']+\\)\\('.*\\)" from))
              (setq from (concat (match-string 1 from) (match-string 3 from) " " (match-string 2 from) (match-string 4 from))))
            (setq from (mapcar (lambda (arg)
                                 (setq arg (replace-regexp-in-string "^ +" "" arg))
                                 (if (string-match "^\\(.+\\)[ 	]*<" arg)
                                     (replace-regexp-in-string "^[\"'\\ 	]*\\|[\"'\\ 	]*$" "" (rfc2047-decode-string (substring arg (match-beginning 1) (match-end 1)) t))
                                   (if (or (string-match email-address-regexp1 arg)
                                           (string-match email-address-regexp2 arg))
                                       (substring arg (match-beginning 1) (match-end 1))
                                     nil)))
                               (split-string from "," t)))
            (setq from (delete nil from))
            (if (>= (length from) 1)
                (progn
                  (setq from-format-string (apply 'concat (make-list (length from) "%s, ")))
                  (setq from-format-string (substring from-format-string 0 (- (length from-format-string) 2)))
                  (setq from-string (apply 'format from-format-string from))))
            ;; Process concatenated To: and Cc: headers.
            (setq to-cc-all (replace-regexp-in-string "[ 	]+" " " to-cc-all))
            (while (or (string-match "\\(.*\"\\)\\([^,\"]+\\), \\([^,\"]+\\)\\(\".*\\)" to-cc-all)
                       (string-match "\\(.*'\\)\\([^,']+\\), \\([^,']+\\)\\('.*\\)" to-cc-all))
              (setq to-cc-all (concat (match-string 1 to-cc-all) (match-string 3 to-cc-all) " " (match-string 2 to-cc-all) (match-string 4 to-cc-all))))
            (setq to-cc-all (mapcar (lambda (arg)
                                      (setq arg (replace-regexp-in-string "^ +" "" arg))
                                      (if (string-match "^\\(.+\\)[ 	]*<" arg)
                                          (replace-regexp-in-string "^[\"'\\ 	]*\\|[\"'\\ 	]*$" "" (rfc2047-decode-string (substring arg (match-beginning 1) (match-end 1)) t))
                                        (if (or (string-match email-address-regexp1 arg)
                                                (string-match email-address-regexp2 arg))
                                            (substring arg (match-beginning 1) (match-end 1))
                                          nil)))
                                    (split-string to-cc-all "," t)))
            (setq to-cc-all (delete nil to-cc-all))
            (if (>= (length to-cc-all) 1)
                (progn
                  (setq to-cc-all-format-string (apply 'concat (make-list (length to-cc-all) "%s, ")))
                  (setq to-cc-all-format-string (substring to-cc-all-format-string 0 (- (length to-cc-all-format-string) 2)))
                  (setq to-cc-all-string (apply 'format to-cc-all-format-string to-cc-all))))
            (or (null (string-match-p my-email-undisclosed-recipients-regexp to-cc-all-string)) (setq to-cc-all-string "NA"))
            ;; Return the appropriate string.
            (if (or (string-match-p my-email-addresses-regexp from-string)
                    (string-match-p my-email-addressees-regexp from-string)
                    (string= "NA" from-string))
                to-cc-all-string
              from-string))
        (set-match-data match-data-old)))))
  
(let ((to-cc-all
       (concat
        "nora.brinkschulte@gmx.de,niklas_nauroth@web.de,sebastianboeck87@gmail.com,Malteundmax@gmail.com,sonja.kunstmann@gmx.de,swantje.nawratil@gmx.de,bender_thomas@gmx.de,lifedying@163.com,markus.hoffmann.37441@freenet.de,Samira.Mehralian@yahoo.com,rerich.roman1987@gmail.com,kellermann.edgar@gmail.com,sarah.brockmann@posteo.de,hertzdoreen1@googlemail.com,tstolper@kabelmail.de"
        ;; ", "
        ;; "Renke Christian von Seggern <magic_willebinski@gmx.de>, Someone Else <r.vonseggern@stud.uni-goettingen.de>"
        ;; ", "
        ;; "\"Andreas Woidich\" <a.woidich@best-profile.eu>,nora.brinkschulte@gmx.de,\"Reinders, Maraike\" <Maraike.Reinders@thw.de>"
        ;; ", "
        ;; "\"Reinders, Maraike\" <Maraike.Reinders@thw.de>"
        ;; ", "
        ;; "\"Name, Bad' <very@bad.com>"
        ;; ", "
        ;; "\"Reinders, Maraike\" <Maraike.Reinders@thw.de>, 'Shit, Holy' <name@domain.de>"
        ;; ", "
        ;; "Carlo Rüdebusch <carlo.ruedebusch@gmail.com>, \"Fabian-ridder@web.de\" <Fabian-ridder@web.de>, \"christian.drieling@googlemail.com\" <christian.drieling@googlemail.com>"
        ;; ", "
        ;; ", "
        ;; "\"l.sielaff@studentec.de\" <l.sielaff@studentec.de>,  \"carlo.ruedebusch@gmail.com\" <carlo.ruedebusch@gmail.com>,  \"Fabian-ridder@web.de\" <Fabian-ridder@web.de>,  Christian Drieling <christian.drieling@googlemail.com>"
        ;; ", "
        ;; "\"A B\" <x@y.z>,  A B <x@y.z>,  \"A B\" <x@y.z>, x@y.z,  A B <x@y.z>,  \"D, C\" <x@y.z>,  A B <x@y.z>, \"A B\" <x@y.z>, \"A B\" <x@y.z>,  A B <x@y.z>, \"G H, E F\" <x@y.z>,  A B <x@y.z>, \"A B\" <x@y.z>, \"A B\" <x@y.z>"
        ;; ", "
        ;; "\"von Seggern, Renke Christian <renke@domain.de>"
        ;; ", "
        ;; "\"Andreas Woidich\" <a.woidich@best-profile.eu>, \"'Renke Christian von Seggern'\" <renke.vonseggern@gmx.de>,  <s.pommer@best-profile.eu>"
        )))
  ;; (
  ;; )
  (setq to-cc-all (replace-regexp-in-string "[ 	]+" " " to-cc-all))
  (while (or (string-match "\\(.*\"\\)\\([^,\"]+\\), \\([^,\"]+\\)\\(\".*\\)" to-cc-all)
             (string-match "\\(.*'\\)\\([^,']+\\), \\([^,']+\\)\\('.*\\)" to-cc-all))
    (setq to-cc-all (concat (match-string 1 to-cc-all) (match-string 3 to-cc-all) " " (match-string 2 to-cc-all) (match-string 4 to-cc-all))))
  to-cc-all)
