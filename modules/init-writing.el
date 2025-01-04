(setq my-notes-directory (expand-file-name "notes/" my-src-directory))
(setq my-notes-reference-file (expand-file-name "reference.bib" my-notes-directory))
(setq my-notes-attachments-directory (expand-file-name "attachments/" my-notes-directory))

(use-package org
  :ensure nil
  :bind
  (("C-c c" . org-capture)
   ("C-c l" . org-store-link)
   :map org-mode-map
   ([remap mark-defun] . org-babel-mark-block)
   ("M-g o" . consult-org-heading))

  :custom
  (org-ellipsis "…")
  (org-use-sub-superscripts '{})
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  ;; Capture
  (org-capture-templates
   `(("c" "Fleeting note" plain
      (file denote-last-path)
      (function
       (lambda ()
         (let ((denote-use-keywords '("fleeting")))
           (denote-org-capture-with-prompts nil))))
      :no-save nil
      :immediate-finish nil
      :kill-buffer t
      :jump-to-captured nil)
     ("o" "OA ticket" plain
      (file denote-last-path)
      (function
       (lambda ()
         (let ((denote-use-title (alfred-browser-title))
               (denote-use-keywords '("jira" "openapply")))
           (denote-org-capture))))
      :no-save nil
      :immediate-finish nil
      :kill-buffer t
      :jump-to-captured t)))

  ;; Code block
  (org-edit-src-content-indentation 0)
  (org-edit-src-persistent-message nil)
  (org-src-preserve-indentation t)
  (org-src-window-setup 'current-window)

  ;; Export
  (org-export-with-sub-superscripts '{})

  ;; Log
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-log-redeadline 'time)
  (setq org-log-reschedule 'time)

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

(use-package org-download
  :pin melpa
  :after org
  :custom
  (org-download-image-dir my-notes-attachments-directory))

(use-package ox-hugo
  :pin melpa
  :after org)

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
  (("C-c n c" . citar-create-note)
   ("C-c n o" . citar-denote-open-note)
   ("C-c n ." . citar-denote-dwim))
  :custom
  (citar-denote-subdir "reference"))

(use-package denote
  :init
  (setq denote-directory my-notes-directory)
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote-open-or-create-with-command)
   ("C-c n d" . denote-sort-dired)
   ("C-c n r" . denote-rename-file)
   ("C-c n z" . denote-rename-file-signature)
   ("C-c n j" . denote-journal-extras-new-entry)
   :map org-mode-map
   ("C-c n i" . denote-link-or-create)
   ("C-c n I" . denote-org-extras-link-to-heading)
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
  (denote-org-capture-specifiers "%i\n%?")
  :config
  (require 'denote-org-extras)
  (require 'denote-journal-extras)
  (denote-rename-buffer-mode 1))

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
  :bind ("C-c n s" . denote-explore-sync-metadata))

(provide 'init-writing)
