(defun kagi-search (query)
  "Search Kagi for QUERY.
If region is active, use it as the query, otherwise prompt user."
  (interactive
   (list
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (read-string "Kagi search: "))))
  (let* ((query (url-hexify-string query))
         (url (format "https://kagi.com/search?q=%s" query)))
    (browse-url url)))

(defun kagi-feeling-lucky (target)
  (interactive "sKagi feeling lucky: ")
  (kagi-search (format "! %s" target)))

(defun kagi-summarize (target)
  (interactive "sKagi summarize: ")
  (kagi-search (format "!sum %s" target)))

(defun kagi-translate (target)
  (interactive "sKagi translate: ")
  (kagi-search (format "!tr %s" target)))

(provide 'kagi)
