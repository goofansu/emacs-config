(use-package magit
  :pin nongnu
  :config
  (message "magit is loaded")
  :custom
  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  :hook
  (git-commit-mode . evil-insert-state))

(provide 'init-git)
