(use-package dictionary
  :ensure nil
  :bind
  (:map search-map ("d" . dictionary-lookup-definition))
  :custom
  (dictionary-server "dict.org")
  (dictionary-default-popup-strategy "lev")
  (dictionary-create-buttons nil)
  (dictionary-use-single-buffer t))

(use-package osx-dictionary
  :pin melpa
  :if (eq system-type 'darwin)
  :bind (:map search-map ("t" . osx-dictionary-search-word-at-point)))

(provide 'init-search)
