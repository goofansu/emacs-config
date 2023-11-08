(use-package denote
  :defer t
  :init
  (setq denote-directory "~/src/notes")
  :config
  (message "denote is loaded")
  :custom
  (denote-date-prompt-use-org-read-date t)
  (denote-known-keywords '("emacs" "nix" "ruby" "elixir" "webdev")))

(defun yejun/browse-notes ()
  (interactive)
  (yejun/browse-project denote-directory))

(defun yejun/search-notes ()
  (interactive)
  (consult-ripgrep denote-directory))

(defun yejun/search-notes-for-symbol-at-point ()
  (interactive)
  (consult-ripgrep denote-directory (thing-at-point 'symbol)))

(provide 'init-denote)
