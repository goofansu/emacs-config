(defvar my-notes-directory (expand-file-name "notes/" my-code-directory))
(defvar my-notes-attachments-directory (expand-file-name "attachments/" my-notes-directory))

(use-package org
  :ensure nil
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link)
   :map org-mode-map
   ([remap mark-defun] . org-babel-mark-block)
   ("M-g o" . consult-org-heading)
   ("C-c C-M-l" . org-toggle-link-display))

  :custom
  (org-export-dispatch-use-expert-ui t)
  (org-export-with-sub-superscripts '{})
  (org-use-fast-todo-selection 'expert)
  (org-use-sub-superscripts '{})

  ;; Editing
  (org-catch-invisible-edits 'show-and-error)
  (org-insert-heading-respect-content t)
  (org-special-ctrl-a/e t)

  ;; Styling
  (org-ellipsis "…")
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)

  ;; Agenda
  (org-agenda-files (list org-directory))
  (org-agenda-tags-column 0)

  ;; Capture
  (org-capture-templates
   `(("c" "Note" entry
      (file "notes.org")
      ,(concat "* %^{Title} %^g\n"
               ":PROPERTIES:\n"
               ":CAPTURED: %U\n"
               ":CUSTOM_ID: h:%(format-time-string \"%Y%m%dT%H%M%S\")\n"
               ":END:\n\n"
               "%a\n%?")
      :empty-lines-before 1)
     ("j" "Journal" entry
      (file+olp+datetree "journal.org")
      ,(concat "* %^{Title} %^g\n"
               ":PROPERTIES:\n"
               ":CAPTURED: %U\n"
               ":CUSTOM_ID: h:%(format-time-string \"%Y%m%dT%H%M%S\")\n"
               ":END:")
      :immediate-finish t
      :empty-lines-before 1)
     ("t" "Task" entry
      (file "tasks.org")
      ,(concat "* TODO %^{Title} %^g\n"
               ":PROPERTIES:\n"
               ":CAPTURED: %U\n"
               ":CUSTOM_ID: h:%(format-time-string \"%Y%m%dT%H%M%S\")\n"
               ":END:\n\n"
               "%a\n%?")
      :empty-lines-after 1
      :prepend t)
     ("e" "Email" entry ; Also see `org-capture-templates-contexts'
      (file "tasks.org")
      ,(concat "* TODO %:subject :mail:\n"
               ":PROPERTIES:\n"
               ":CAPTURED: %U\n"
               ":CUSTOM_ID: h:%(format-time-string \"%Y%m%dT%H%M%S\")\n"
               ":END:\n\n"
               "%a")
      :immediate-finish t
      :empty-lines-after 1
      :prepend t)
     ("r" "Reference note" plain
      (file denote-last-path)
      (function
       (lambda ()
         (let ((denote-use-title (alfred-browser-title)))
           (denote-org-capture-with-prompts :title :keywords))))
      :no-save t
      :immediate-finish nil
      :kill-buffer t
      :jump-to-captured t)
     ))

  (org-capture-templates-contexts
   '(("e" ((in-mode . "notmuch-search-mode")
           (in-mode . "notmuch-show-mode")
           (in-mode . "notmuch-tree-mode")))))

  ;; Code block
  (org-edit-src-content-indentation 0)
  (org-edit-src-persistent-message nil)
  (org-src-preserve-indentation t)
  (org-src-window-setup 'current-window)

  ;; Log
  (org-log-into-drawer t)
  (org-log-done 'time)
  (org-log-redeadline 'note)
  (org-log-reschedule 'note)

  ;; Tag
  (org-auto-align-tags nil)
  (org-tags-column 0)

  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (ruby . t)
     (python . t))))

(use-package org-modern
  :after org
  :custom
  (org-modern-star nil)
  (org-modern-table nil)
  :config
  (global-org-modern-mode))

(use-package ox-hugo
  :pin melpa
  :after org
  :custom
  (org-hugo-default-static-subdirectory-for-externals "attachments")
  (org-hugo-front-matter-format "yaml"))

(use-package denote-hugo
  :ensure nil
  :load-path "site-lisp/"
  :bind ("C-c n h" . denote-hugo-find-file))

(use-package ox-gfm
  :pin melpa
  :after org)

(use-package denote
  :init
  (setq denote-directory my-notes-directory)
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n t" . denote-type)
   ("C-c n o" . denote-sort-dired)
   ("C-c n r" . denote-rename-file)
   :map text-mode-map
   ("C-c n i" . denote-link-or-create)
   ("C-c n I" . denote-add-links)
   ("C-c n b" . denote-backlinks)
   ("C-c n R" . denote-rename-file-using-front-matter)
   :map dired-mode-map
   ("C-c C-d C-i" . denote-link-dired-marked-notes)
   ("C-c C-d C-r" . denote-dired-rename-marked-files)
   ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
   ("C-c C-d C-f" . denote-dired-rename-marked-files-using-front-matter))
  :custom
  (denote-save-buffers t)
  (denote-known-keywords nil)
  :config
  (denote-rename-buffer-mode 1)
  (advice-add 'denote-link-ol-export :around
              (lambda (orig-fun link description format)
                "Advice for `denote-link-ol-export' that customizes Org link export to Hugo Markdown.
If exporting to markdown ('md') with Hugo backend (`org-export-current-backend' is 'hugo'),
replace export with a relref shortcode referencing either the EXPORT_FILE_NAME or the file basename.
Otherwise, call the original function."
                (if (and (eq format 'md)
                         (eq org-export-current-backend 'hugo))
                    (let* ((path (denote-get-path-by-id link))
                           (export-file-name
                            (or
                             (when (file-exists-p path)
                               (with-temp-buffer
                                 (insert-file-contents path)
                                 (goto-char (point-min))
                                 (when (re-search-forward "^#\\+export_file_name: \\(.+\\)" nil t)
                                   (match-string 1))))
                             (file-name-nondirectory path))))
                      (format "[%s]({{< relref \"%s\" >}})"
                              description
                              export-file-name))
                  (funcall orig-fun link description format)))))

(use-package denote-org
  :after denote
  :custom
  (denote-org-capture-specifiers "%i\n%?")
  :config
  (keymap-set org-mode-map "C-c n a" 'my/denote-org-insert-attachment)
  (keymap-set org-mode-map "C-c n d l" 'denote-org-dblock-insert-links)
  (keymap-set org-mode-map "C-c n d b" 'denote-org-dblock-insert-backlinks)
  (defun my/denote-org-insert-attachment (file)
    "Process FILE to use as an attachment in the current buffer.

If FILE is already in the attachments directory, simply insert a link to it.
Otherwise, rename it using `denote-rename-file' with a title derived from
the filename, move it to the attachments directory, and insert a link.

The link format used is '[[file:attachments/filename]]', following Org syntax.
This function is ideal for managing referenced files in note-taking workflows."
    (interactive (list (read-file-name "File: " my-notes-attachments-directory)))
    (let* ((orig-buffer (current-buffer))
           (attachments-dir my-notes-attachments-directory))

      ;; Check if the file is already in the attachments directory
      (if (string-prefix-p
           (file-name-as-directory attachments-dir)
           (expand-file-name file))

          ;; If already in attachments, just insert the link
          (with-current-buffer orig-buffer
            (insert (format "[[file:attachments/%s]]" (file-name-nondirectory file))))

        ;; Otherwise, rename and move the file
        (let ((title (denote-sluggify-title (file-name-base file))))
          (when-let* ((renamed-file (denote-rename-file file title))
                      (renamed-name (file-name-nondirectory renamed-file))
                      (final-path (expand-file-name renamed-name attachments-dir)))
            (rename-file renamed-file final-path t)
            (with-current-buffer orig-buffer
              (insert (format "[[file:attachments/%s]]" renamed-name)))))))))

(use-package denote-journal
  :bind ("C-c n j" . denote-journal-new-entry))

(use-package denote-sequence
  :bind
  (("C-c n s s" . denote-sequence)
   ("C-c n s f" . denote-sequence-find)
   ("C-c n s l" . denote-sequence-link)
   ("C-c n s d" . denote-sequence-dired)
   ("C-c n s r" . denote-sequence-reparent)
   ("C-c n s c" . denote-sequence-convert))
  :custom
  (denote-sequence-scheme 'alphanumeric))

(use-package consult-denote
  :init
  (with-eval-after-load 'denote
    (consult-denote-mode 1))
  :bind
  (("C-c n f" . consult-denote-find)
   ("C-c n g" . consult-denote-grep))
  :custom
  (consult-denote-find-command #'consult-fd)
  (consult-denote-grep-command #'consult-ripgrep))

(use-package denote-explore
  :pin melpa
  :bind ("C-c n x" . denote-explore-sync-metadata))

(use-package citar-denote
  :pin melpa
  :init
  (with-eval-after-load 'citar
    (citar-denote-mode 1))
  :bind
  (("C-c w n" . citar-create-note)
   ("C-c w o" . citar-denote-open-note)
   ("C-c w d" . citar-denote-dwim)
   ("C-c w e" . citar-denote-open-reference-entry)
   ("C-c w a" . citar-denote-add-citekey)
   ("C-c w k" . citar-denote-remove-citekey)
   ("C-c w r" . citar-denote-find-reference)
   ("C-c w l" . citar-denote-link-reference)
   ("C-c w f" . citar-denote-find-citation)
   ("C-c w x" . citar-denote-nocite)
   ("C-c w y" . citar-denote-cite-nocite)
   ("C-c w z" . citar-denote-nobib)))

(provide 'init-writing)
