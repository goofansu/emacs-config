(use-package ruby-ts-mode
  :ensure nil
  :defer t)

(use-package inf-ruby
  :pin nongnu
  :hook (ruby-ts-mode . inf-ruby-minor-mode)
  :bind (:map inf-ruby-minor-mode-map ("C-c C-s" . inf-ruby-console-auto))
  :custom
  (inf-ruby-console-environment "development")
  :config
  ;; Reserve "C-c C-r" for `rubocop-mode-map'.
  (unbind-key "C-c C-r" inf-ruby-minor-mode-map))

(use-package bundler
  :pin melpa
  :defer t)

(use-package rspec-mode
  :pin melpa
  :hook (ruby-ts-mode rspec-compilation-mode))

(use-package rubocop
  :pin nongnu
  :hook ruby-ts-mode)

(provide 'init-ruby)
