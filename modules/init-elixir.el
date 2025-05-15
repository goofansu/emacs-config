(use-package elixir-ts-mode
  :ensure nil
  :config
  (defun elixir-format-before-save ()
    (add-hook 'before-save-hook #'eglot-format-buffer nil t))
  (add-hook 'elixir-ts-mode-hook #'elixir-format-before-save)

  (defun my/elixir-package-insert (package)
    "Insert Hex PACKAGE config at point."
    (interactive "sPackage: ")
    (let* ((command (format "mix hex.info %s | grep 'Config:' | sed 's/Config: //g'" package))
           (result (shell-command-to-string command)))
      (insert result)))
  (keymap-set my-insert-map "e e" 'my/elixir-package-insert))

(use-package heex-ts-mode
  :ensure nil
  :defer t)

(use-package exunit
  :pin melpa
  :hook elixir-ts-mode)

(provide 'init-elixir)
