(use-package heex-ts-mode
  :pin melpa
  :config
  (message "heex-ts-mode is loaded")
  :hook (heex-ts-mode . eglot-ensure))

(use-package elixir-ts-mode
  :pin melpa
  :after heex-ts-mode
  :config
  (message "elixir-ts-mode is loaded")

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `((elixir-ts-mode heex-ts-mode) .
                   ("nextls" "--stdio=true"))))

  (defun elixir-format-before-save ()
    (when (derived-mode-p 'elixir-ts-mode)
      (eglot-format-buffer)))

  :hook
  (elixir-ts-mode . eglot-ensure)
  (before-save . elixir-format-before-save))

(provide 'init-elixir)
