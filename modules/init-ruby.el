(use-package ruby-ts-mode
  :ensure nil
  :defer t
  :config
  (message "ruby-ts-mode is loaded"))

(use-package inf-ruby
  :pin nongnu
  :defer t
  :config
  (message "inf-ruby is loaded")
  :custom
  (inf-ruby-console-environment "development")
  :hook (ruby-ts-mode . inf-ruby-minor-mode)
  :bind ( :map inf-ruby-minor-mode-map
          ("C-c C-s" . inf-ruby-console-auto)))

(use-package bundler
  :pin melpa
  :defer t
  :config
  (message "bundler is loaded"))

(use-package rake
  :pin melpa
  :defer t
  :config
  (message "rake is loaded")
  :custom
  (rake-completion-system 'default))

(use-package rspec-mode
  :pin melpa
  :defer t
  :config
  (message "rspec-mode is loaded")
  :bind ( :map rspec-mode-map
          ("C-c C-t a" . rspec-verify-all)
          ("C-c C-t s" . rspec-verify-single)
          ("C-c C-t v" . rspec-verify)
          ("C-c C-t r" . rspec-rerun)
          ("C-c C-t l" . rspec-run-last-failed)
          ("C-c C-t e" . rspec-toggle-example-pendingness)
          ("C-c C-t t" . rspec-toggle-spec-and-target)))

(provide 'init-ruby)
