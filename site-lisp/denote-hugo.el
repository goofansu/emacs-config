(require 'denote)
(require 'ox-hugo)

(defun denote-hugo-files ()
  "Return a list of Hugo-compatible files in `denote-directory'."
  (let ((default-directory (denote-directory)))
    (process-lines "rg" "-i" "-l" "^#\\+hugo_base_dir" "--glob" "*.org")))

(defun denote-hugo-find-file ()
  "Search Hugo-compatible files in `denote-directory' and visit the result."
  (interactive)
  (let* ((default-directory (denote-directory))
         (selected-file (consult--read
                         (denote-hugo-files)
                         :prompt (format "Select FILE in %s: "  default-directory)
                         :sort nil
                         :require-match t
                         :category 'file
                         :state (consult--file-preview)
                         :history 'denote-file-history)))
    (find-file selected-file)))

(defun denote-hugo-export-all ()
  "Export all Hugo-compatible files in `denote-directory'."
  (interactive)
  (let ((org-export-use-babel nil))
    (dolist (file (denote-hugo-files))
      (with-current-buffer (find-file-noselect file)
        (org-hugo-export-to-md)))
    (message "All notes have been exported to Hugo content")))

(provide 'denote-hugo)
