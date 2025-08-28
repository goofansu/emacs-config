(use-package racket-mode
  :pin nongnu
  :defer t)

(use-package ob-racket
  :vc (ob-racket :url "https://github.com/hasu/emacs-ob-racket.git")
  :after org
  :config
  (setq ob-racket-default-lang "sicp"))

(use-package sicp
  :pin melpa
  :defer t)

(provide 'init-racket)
