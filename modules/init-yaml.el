(use-package yaml-mode
  :pin nongnu
  :mode ("Procfile\\'" "Procfile\\.dev\\'"))

(use-package yaml-pro
  :pin melpa
  :hook (yaml-ts-mode . yaml-pro-ts-mode)
  :bind (:map yaml-ts-mode-map ("C-c M-y" . yaml-pro-copy-node-path-at-point)))

(provide 'init-yaml)
