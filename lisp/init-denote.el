(use-package denote
  :defer t
  :init
  (setq denote-directory "~/src/notes")
  :config
  (message "denote is loaded")

  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
                 '("n" "Note" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t)))

  :custom
  (denote-date-prompt-use-org-read-date t)
  (denote-known-keywords '("emacs" "nix" "ruby" "elixir" "webdev")))

(defun yejun/browse-notes ()
  (interactive)
  (yejun/find-file-in-project denote-directory))

(defun yejun/search-notes ()
  (interactive)
  (yejun/search-project denote-directory))

(defun yejun/search-notes-for-symbol-at-point ()
  (interactive)
  (yejun/search-project denote-directory 'symbol))

(provide 'init-denote)
