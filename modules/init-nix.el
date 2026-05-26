(use-package nix-mode
  :pin nongnu
  :hook (before-save . nix-format-before-save)
  :bind (:map my-insert-map ("n n" . my/homebrew-casks-insert))
  :config
  (defun my/homebrew-casks-insert ()
    "Insert currently installed Homebrew Casks at point."
    (interactive)
    (let* ((casks-output (string-trim
                          (shell-command-to-string "brew list --cask")))
           (casks-list (when (not (string-empty-p casks-output))
                         (split-string casks-output "\n")))
           (quoted-casks (mapcar (lambda (cask) (format "\"%s\"" cask))
                                 casks-list))
           (result (format "[ %s ];" (string-join quoted-casks " "))))
      (insert result))))

(provide 'init-nix)
