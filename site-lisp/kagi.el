(defun kagi-search (query)
  "Search QUERY using Kagi Search.
If region is active, use it as the query, otherwise prompt user."
  (interactive
   (list
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (read-string "Kagi search: "))))
  (let* ((query (url-hexify-string query))
         (url (format "https://kagi.com/search?q=%s" query)))
    (browse-url url)))

(defun kagi-summarize (query)
  "Summarize QUERY using Kagi Universal Summarizer."
  (interactive "sKagi summarize: ")
  (kagi-search (format "!sum %s" query)))

(defun kagi-translate (query)
  "Translate QUERY using Kagi Translator."
  (interactive "sKagi translate: ")
  (kagi-search (format "!tr %s" query)))

(provide 'kagi)
