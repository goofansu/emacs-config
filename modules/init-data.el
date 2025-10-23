(use-package csv-mode
  :hook ((csv-mode . read-only-mode)
         (csv-mode . csv-align-mode)
         (csv-mode . csv-header-line)))

(use-package json-mode
  :defer t)

(use-package sqlite-mode
  :ensure nil
  :defer t)

(provide 'init-data)
