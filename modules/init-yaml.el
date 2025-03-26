(use-package yaml-mode
  :pin nongnu
  :defer t
  :mode "Procfile\\'"
  :mode "Procfile\\.dev\\'")

(use-package yaml-pro
  :pin melpa
  :hook (yaml-ts-mode . yaml-pro-ts-mode)
  :bind (:map yaml-ts-mode-map ("C-c M-y" . yaml-pro-copy-node-path-at-point)))

(provide 'init-yaml)
