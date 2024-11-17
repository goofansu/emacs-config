(use-package denote
  :defer 1
  :init
  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
                 '("n" "Note" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t)))
  :bind
  (("C-c n n" . denote)
   ("C-c n N" . denote-type)
   ("C-c n d" . denote-date)
   ("C-c n z" . denote-signature) ; "zettelkasten" mnemonic
   ("C-c n s" . denote-subdirectory)
   ("C-c n o" . denote-sort-dired) ; "order" mnemonic
   ("C-c n r r" . denote-rename-file-using-front-matter)
   ("C-c n r t" . denote-rename-file-title)
   ("C-c n r k" . denote-rename-file-keywords)
   ("C-c n r z" . denote-rename-file-signature)
   :map search-map
   ("n" . denote-open-or-create)
   ("j" . denote-journal-extras-new-or-existing-entry)
   :map text-mode-map
   ("C-c n i" . denote-link-or-create)
   ("C-c n I" . denote-org-extras-link-to-heading)
   ("C-c n b" . denote-backlinks)
   ("C-c n B" . denote-org-extras-backlinks-for-heading)
   ("C-c n l" . denote-add-links)
   ("C-c n f f" . denote-find-link)
   ("C-c n f b" . denote-find-backlink)
   :map dired-mode-map
   ("C-c C-d C-i" . denote-link-dired-marked-notes)
   ("C-c C-d C-r" . denote-dired-rename-marked-files)
   ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
   ("C-c C-d C-f" . denote-dired-rename-marked-files-using-front-matter))
  :custom
  (denote-directory (expand-file-name "notes/" my-src-directory))
  (denote-journal-extras-title-format 'day-date-month-year)
  (denote-known-keywords nil)
  :config
  (require 'denote-journal-extras)
  (require 'denote-org-extras)
  (denote-rename-buffer-mode 1))

(use-package consult-denote
  :init
  (with-eval-after-load 'denote
    (consult-denote-mode 1))
  :bind (:map search-map ("g" . consult-denote-grep))
  :custom
  (consult-denote-grep-command #'consult-ripgrep))

(provide 'init-denote)
