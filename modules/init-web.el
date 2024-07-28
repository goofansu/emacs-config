(use-package web-mode
  :pin nongnu
  :defer t
  :mode "\\.erb\\'"
  :mode "\\.vue\\'"
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

(use-package css-mode
  :ensure nil
  :defer t
  :custom
  (css-indent-offset 2))

(use-package js-mode
  :ensure nil
  :defer t
  :custom
  (js-indent-level 2))

(use-package haml-mode
  :pin melpa
  :defer t)

(use-package coffee-mode
  :pin melpa
  :defer t
  :custom
  (coffee-tab-width 2))

(provide 'init-web)
