(use-package devdocs
  :bind (:map search-map ("k" . devdocs-search)))

(use-package dictionary
  :ensure nil
  :bind
  (:map search-map
        ("d" . dictionary-lookup-definition)
        ("D" . dictionary-search))
  :custom
  (dictionary-server "dict.org")
  (dictionary-default-popup-strategy "lev")
  (dictionary-create-buttons nil)
  (dictionary-use-single-buffer t))

(use-package osx-dictionary
  :pin melpa
  :if (eq system-type 'darwin)
  :bind (:map search-map ("t" . osx-dictionary-search-word-at-point)))

(use-package google
  :ensure nil
  :load-path "site-lisp/"
  :bind (:map search-map ("M-s" . google-search)))

(provide 'init-search)
