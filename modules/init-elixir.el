(use-package elixir-ts-mode
  :ensure nil
  :mode ("\\.elixir\\'" "\\.ex\\'" "\\.exs\\'" "mix\\.lock"))

(use-package heex-ts-mode
  :ensure nil
  :defer t)

(provide 'init-elixir)
