(require 'denote)
(require 'ox)

(defun denote-agent-skills-export (&optional custom-dir)
  "Export denote note to Claude skill with frontmatter, using denote slug as skill name.
With prefix argument, prompt for custom skills directory."
  (interactive "P")
  (unless (denote-file-is-note-p (buffer-file-name))
    (user-error "Not in a denote note"))
  (let* ((file (buffer-file-name))
         (slug (denote-retrieve-filename-identifier file))
         (skill-name (denote-retrieve-title-or-filename file slug))
         (base-dir (if custom-dir
                       (read-directory-name "Skills directory: " "~/")
                     (expand-file-name "skills" my-sync-directory)))
         (skill-dir (expand-file-name skill-name base-dir))
         (output-file (expand-file-name "SKILL.md" skill-dir))
         (description (cadar (org-collect-keywords '("DESCRIPTION")))))
    (unless description
      (user-error "No #+description: found in the org file"))
    (unless (file-exists-p skill-dir)
      (make-directory skill-dir t))
    ;; Export with no TOC
    (let ((org-export-with-toc nil))
      (org-export-to-file 'md output-file))
    ;; Add frontmatter
    (with-temp-buffer
      (insert (format "---\nname: %s\ndescription: %s\n---\n"
                      skill-name description))
      (insert-file-contents output-file)
      (write-file output-file))))

(provide 'denote-agent-skills)
