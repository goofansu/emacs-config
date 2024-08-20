(use-package denote
  :vc (denote :url "https://github.com/protesilaos/denote.git")
  :bind
  (("C-c n n" . denote)
   ("C-c n N" . denote-type)
   ("C-c n d" . denote-date)
   ("C-c n z" . denote-signature) ; "zettelkasten" mnemonic
   ("C-c n s" . denote-subdirectory)
   ("C-c n o" . denote-sort-dired) ; "order" mnemonic
   ("C-c n j" . denote-journal-extras-new-entry)
   ("C-c n J" . denote-journal-extras-new-or-existing-entry)
   :map search-map
   ("n" . denote-open-or-create)
   :map text-mode-map
   ("C-c n i" . denote-link)
   ("C-c n I" . denote-add-links)
   ("C-c n b" . denote-backlinks)
   ("C-c n f f" . denote-find-link)
   ("C-c n f b" . denote-find-backlink)
   :map dired-mode-map
   ("C-c C-d C-i" . denote-link-dired-marked-notes)
   ("C-c C-d C-r" . denote-dired-rename-marked-files)
   ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
   ("C-c C-d C-f" . denote-dired-rename-marked-files-using-front-matter))
  :custom
  (denote-directory "~/src/notes/")
  (denote-known-keywords '("emacs" "programming" "education"))
  :config
  (require 'denote-journal-extras)
  (require 'denote-org-extras)
  (denote-rename-buffer-mode 1))

(use-package consult-denote
  :bind (:map search-map ("/" . consult-denote-grep))
  :custom
  (consult-denote-grep-command #'consult-ripgrep)
  :config
  (consult-denote-mode 1))

(provide 'init-denote)
