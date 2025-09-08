(defcustom brew-executable (executable-find "brew")
  "Homebrew executable location."
  :group 'macos
  :type 'string)

(defcustom zed-executable (executable-find "zed")
  "Zed executable location."
  :group 'macos
  :type 'string)

(defun macos-reveal-in-finder ()
  (interactive)
  (if-let ((path (or (buffer-file-name)
                     (and (derived-mode-p 'dired-mode)
                          (expand-file-name default-directory)))))
      (start-process "finder" nil "open" "-R" path)
    (user-error "Buffer is not visiting a file or directory")))

(defun macos-reveal-project-in-finder ()
  (interactive)
  (when-let* ((project-root-dir
               (condition-case nil
                   (project-root (project-current))
                 (error nil)))
              (filename (expand-file-name project-root-dir)))
    (start-process "finder" nil "open" "-R" filename)))

(defun macos-notify (title body &optional sound-name)
  (let* ((sound (or sound-name "Default"))
         (script (format "display notification \"%s\" with title \"%s\" sound name \"%s\""
                         body title sound)))
    (do-applescript script)))

(defun macos-dark-mode-p ()
  (let ((script "tell application \"System Events\"
                     tell appearance preferences
                         if (dark mode) then
                             return \"true\"
                         else
                             return \"false\"
                         end if
                     end tell
                 end tell"))
    (string-equal "true" (do-applescript script))))

(defun zed-goto-file-at-point ()
  "Open the file at point in Zed editor."
  (interactive)
  (when-let* ((filename (buffer-file-name))
              (line (line-number-at-pos))
              (column (1+ (current-column))))
    (let ((path-with-position (format "%s:%d:%d" filename line column)))
      (start-process "zed" nil zed-executable path-with-position))))

(provide 'macos)
