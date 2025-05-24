(use-package 0x0
  :pin melpa
  :bind (:map embark-region-map ("U" . 0x0-upload-text)))

(use-package osm
  :bind ("C-c o" . osm-prefix-map)
  :custom (osm-home '(31.231271 121.470015 8)))

(provide 'init-misc)
