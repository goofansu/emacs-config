(defvar my-notes-directory (expand-file-name "notes/" my-src-directory))
(defvar my-notes-reference-file (expand-file-name "reference.bib" my-notes-directory))
(defvar my-notes-attachments-directory (expand-file-name "attachments/" my-notes-directory))

(use-package org
  :ensure nil
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link)
   :map org-mode-map
   ([remap mark-defun] . org-babel-mark-block)
   ("M-g o" . consult-org-heading))

  :custom
  (org-use-sub-superqscripts '{})
  (org-M-RET-may-split-line '((default . nil)))
  (org-insert-heading-respect-content t)

  ;; Agenda
  (org-agenda-files (list org-directory))
  (org-use-fast-todo-selection 'expert)

  ;; Capture
  (org-capture-templates
   `(("t" "Task" entry
      (file "tasks.org")
      ,(concat "* TODO %?\n"
               ":PROPERTIES:\n"
               ":CAPTURED: %U\n"
               ":END:"))
     ("f" "Fleeting note" plain
      (file denote-last-path)
      #'denote-org-capture
      :no-save nil
      :immediate-finish nil
      :kill-buffer t
      :jump-to-captured t)
     ("r" "Reference note" plain
      (file denote-last-path)
      (function
       (lambda ()
         (let ((denote-use-title (alfred-browser-title)))
           (denote-org-capture-with-prompts :title :keywords))))
      :no-save nil
      :immediate-finish nil
      :kill-buffer t
      :jump-to-captured t)
     ("c" "Interstitial journaling" entry
      (file denote-journal-capture-entry-today)
      ,(concat "* %^{Thought} %^g\n"
               ":PROPERTIES:\n"
               ":CAPTURED: %U\n"
               ":END:")
      :no-save t
      :immediate-finish t
      :kill-buffer t
      :jump-to-captured nil)
     ))

  ;; Code block
  (org-edit-src-content-indentation 0)
  (org-edit-src-persistent-message nil)
  (org-src-preserve-indentation t)
  (org-src-window-setup 'current-window)

  ;; Export
  (org-export-with-sub-superscripts '{})
  (org-export-dispatch-use-expert-ui t)

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
     (racket . t)
     (shell . t)
     (ruby . t)
     (python . t))))

(use-package org-anki
  :pin melpa
  :after org
  :custom
  (org-anki-default-deck "Default")
  (org-anki-default-match "@anki&todo<>\"TODO\"")
  (org-anki-inherit-tags nil))

(use-package ox-hugo
  :pin melpa
  :after org
  :bind ("C-c n h" . my/org-hugo-denote-files-find-file)
  :custom
  (org-hugo-default-static-subdirectory-for-externals "attachments")
  (org-hugo-front-matter-format "yaml")
  :config
  (defun my/org-hugo-denote-files ()
    "Return a list of Hugo-compatible files in `denote-directory'."
    (let ((default-directory (denote-directory)))
      (process-lines "rg" "-l" "^#\\+hugo_base_dir" "--glob" "*.org")))

  (defun my/org-hugo-denote-files-find-file ()
    "Search Hugo-compatible files in `denote-directory' and visit the result."
    (interactive)
    (let* ((default-directory (denote-directory))
           (selected-file (consult--read
                           (my/org-hugo-denote-files)
                           :prompt (format "Select FILE in %s: "  default-directory)
                           :sort nil
                           :require-match t
                           :category 'file
                           :state (consult--file-preview)
                           :history 'denote-file-history)))
      (find-file selected-file)))

  (defun my/org-hugo-export-all-denote-files ()
    "Export all Hugo-compatible files in `denote-directory'."
    (interactive)
    (let ((org-export-use-babel nil))
      (dolist (file (my/org-hugo-denote-files))
        (with-current-buffer (find-file-noselect file)
          (org-hugo-export-to-md))))))

(use-package ox-gfm
  :pin melpa
  :after org)

(use-package citar
  :pin melpa
  :init
  (setq org-cite-global-bibliography `(,my-notes-reference-file))
  (setq org-cite-insert-processor 'citar)
  (setq org-cite-follow-processor 'citar)
  (setq org-cite-activate-processor 'citar)
  :custom
  (citar-bibliography org-cite-global-bibliography)
  (citar-at-point-function #'embark-act))

(use-package citar-embark
  :pin melpa
  :after (citar embark)
  :config
  (citar-embark-mode 1))

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

(use-package denote
  :init
  (setq denote-directory my-notes-directory)
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n o" . denote-sort-dired)
   ("C-c n r" . denote-rename-file)
   ("C-c n j" . denote-journal-extras-new-or-existing-entry)
   ("C-c n J" . denote-journal-extras-new-entry)
   :map text-mode-map
   ("C-c n i" . denote-link-or-create)
   ("C-c n I" . denote-add-links)
   ("C-c n b" . denote-backlinks)
   ("C-c n R" . denote-rename-file-using-front-matter)
   ("M-g l" . denote-find-link)
   ("M-g L" . denote-find-backlink)
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
                (if (and (eq format 'md)
                         (eq org-export-current-backend 'hugo))
                    (let* ((path (denote-get-path-by-id link))
                           (export-file-name
                            (or
                             ;; Use export_file_name if it exists
                             (when (file-exists-p path)
                               (with-temp-buffer
                                 (insert-file-contents path)
                                 (goto-char (point-min))
                                 (when (re-search-forward "^#\\+export_file_name: \\(.+\\)" nil t)
                                   (match-string 1))))
                             ;; Otherwise, use the original file's base name
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
  :after denote
  :config
  (defun denote-journal-capture-entry-today ()
    "Capture to Denote Journal entry for today."
    (let ((date (format-time-string "%Y-%m-%d %H:%M:%S")))
      (denote-journal-path-to-new-or-existing-entry date))))

(use-package consult-denote
  :init
  (with-eval-after-load 'denote
    (consult-denote-mode 1))
  :bind
  (("C-c n f" . consult-denote-find)
   ("C-c n F" . my/consult-denote-find-today)
   ("C-c n g" . consult-denote-grep))
  :custom
  (consult-denote-find-command #'consult-fd)
  (consult-denote-grep-command #'consult-ripgrep)
  :config
  (defun my/consult-denote-find-today ()
    "Call `consult-denote-find-command' in `denote-directory' using the current date."
    (declare (interactive-only t))
    (interactive)
    (let ((initial (format-time-string "%Y%m%d")))
      (funcall-interactively consult-denote-find-command
                             (denote-directory)
                             initial))))

(use-package denote-explore
  :pin melpa
  :hook (org-capture-after-finalize . my/denote-explore-sync-metadata)
  :bind ("C-c n s" . denote-explore-sync-metadata)
  :config
  (defun my/denote-explore-sync-metadata ()
    "Sync denote filenames when org-capture's target is a denote file.
It means the target is (file denote-last-path)."
    (let ((target-file (org-capture-get :target)))
      (when (and (listp target-file)
                 (eq (car target-file) 'file)
                 (eq (cadr target-file) 'denote-last-path))
        (denote-explore-sync-metadata)))))

(provide 'init-writing)
