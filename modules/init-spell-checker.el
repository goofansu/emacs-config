(use-package jinx
  :ensure nil
  :hook org-mode
  :bind
  (([remap ispell-word] . jinx-correct)
   ("C-c J" . jinx-languages)
   :map my-toggle-map
   ("j" . jinx-mode))
  :custom
  (jinx-languages "en_US en_GB"))

(provide 'init-spell-checker)
