(use-package csv-mode
  :defer t
  :hook ((csv-mode . csv-header-line)
         (csv-mode . csv-align-mode)))

(use-package json-mode
  :defer t)

(use-package sqlite-mode
  :ensure nil
  :defer t)

(provide 'init-data)
