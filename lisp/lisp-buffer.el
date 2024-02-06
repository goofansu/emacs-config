(defun +buffer/search (&optional thing)
  (interactive)
  (consult-line (when thing (thing-at-point thing))))

(defun +buffer/search-for-symbol-at-point ()
  (interactive)
  (+buffer/search 'symbol))

(defun +buffer/yank-path (&optional buffer dir)
  "Save the buffer path into the kill-ring.
If BUFFER is not nil, find filename of BUFFER, otherwise, find
filename of `current-buffer'. If DIR is not nil, get a relative
file path, otherwise, get a full file path with
`abbreviate-file-name'."
  (interactive)
  (if-let* ((filename (if buffer
                          (buffer-filename buffer)
                        (buffer-file-name)))
            (path (if dir
                      (file-relative-name filename dir)
                    (abbreviate-file-name filename))))
      (progn
        (kill-new path)
        (message "Copied path: %s" path))
    (user-error "Buffer is not visiting any file")))

(defun +buffer/yank-path-relative-to-project ()
  "Save the relative buffer path into the kill-ring.
The path is relative to `project-current'."
  (interactive)
  (+buffer/yank-path nil (+project/root-dir)))

(provide 'lisp-buffer)
