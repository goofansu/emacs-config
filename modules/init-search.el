(use-package osx-dictionary
  :pin melpa
  :if (eq system-type 'darwin)
  :bind (:map search-map
              ("d" . osx-dictionary-search-word-at-point)
              ("D" . osx-dictionary-search-input)))

(provide 'init-search)
