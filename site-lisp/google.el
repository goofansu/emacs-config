(defun google-search (query)
  "Search QUERY using Google.
If region is active, use it as the query, otherwise prompt user."
  (interactive
   (list
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (read-string "Google search: "))))
  (let ((url (format "https://google.com/search?q=%s" (url-hexify-string query))))
    (browse-url url)))

(provide 'google)
