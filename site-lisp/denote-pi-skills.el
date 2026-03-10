(require 'denote)
(require 'ox)

(defun denote-pi-skills-export (&optional custom-dir)
  "Export denote note to pi-stuff/skills with frontmatter, using denote slug as skill name."
  (interactive)
  (unless (denote-file-is-note-p (buffer-file-name))
    (user-error "Not in a denote note"))
  (let* ((file (buffer-file-name))
         (slug (denote-retrieve-filename-identifier file))
         (skill-name (denote-retrieve-title-or-filename file slug))
         (base-dir (expand-file-name "pi-stuff/skills" my-code-directory))
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
