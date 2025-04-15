(defun kagi-search (query)
  "Search QUERY using Kagi Search.
If region is active, use it as the query, otherwise prompt user."
  (interactive
   (list
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (read-string "Kagi search: "))))
  (let ((url (format "https://kagi.com/search?q=%s" (url-hexify-string query))))
    (browse-url url)))

(defun kagi-summarize (query)
  "Summarize QUERY using Kagi Universal Summarizer."
  (interactive "sKagi summarize: ")
  (kagi-search (format "!sum %s" query)))

(defun kagi-translate (query)
  "Translate QUERY using Kagi Translator."
  (interactive "sKagi translate: ")
  (kagi-search (format "!tr %s" query)))

(defun kagi-assistant (query &optional profile)
  "Discuss QUERY with Kagi Assistant.
If region is active, use it as the query, otherwise prompt user.
Optionally, a PROFILE can be specified to determine the assistant's model.
Default profile is 'code'."
  (interactive
   (list
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (read-string "Kagi assistant: "))))
  (let* ((profile (or profile "code"))
         (url (format "https://kagi.com/assistant?profile=%s&q=%s"
                      profile
                      (url-hexify-string query))))
    (browse-url url)))

(provide 'kagi)
