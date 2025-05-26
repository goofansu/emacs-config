(use-package eglot
  :ensure nil
  :hook
  ((ruby-mode ruby-ts-mode) . eglot-ensure)
  ((python-mode python-ts-mode) . eglot-ensure)
  ((elixir-ts-mode heex-ts-mode) . eglot-ensure)
  (nix-mode . eglot-ensure)
  (c-ts-mode . eglot-ensure)
  :custom
  (eglot-ignored-server-capabilities '(:completionProvider))
  :config
  (dolist (mode '(((ruby-mode ruby-ts-mode) . ("ruby-lsp"))
                  ((python-mode python-ts-mode) . ("ruff" "server"))
                  ((elixir-ts-mode heex-ts-mode) . ("nextls" "--stdio=true"))
                  (nix-mode . ("nixd"))
                  (c-ts-mode . ("clangd"
                                ;; feature options
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                ;; misc options
                                "-j=8"
                                "--pch-storage=memory"
                                ;; logging options
                                "--log=error"
                                ))))
    (add-to-list 'eglot-server-programs mode)))

(provide 'init-eglot)
