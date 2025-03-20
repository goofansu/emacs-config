(use-package whitespace
  :ensure nil
  :bind
  (("C-c z" . whitespace-cleanup)
   :map my-toggle-map
   ("w" . whitespace-mode)))

(use-package display-line-numbers
  :ensure nil
  :bind (:map my-toggle-map ("l" . display-line-numbers-mode))
  :custom
  (display-line-numbers-widen t))

(use-package visual-line-mode
  :ensure nil
  :hook text-mode
  :bind (:map my-toggle-map ("v" . visual-line-mode)))

(use-package logos
  :init
  (setq-default logos-hide-cursor nil
                logos-hide-mode-line t
                logos-hide-buffer-boundaries t
                logos-hide-fringe t
                logos-variable-pitch nil
                logos-buffer-read-only nil
                logos-scroll-lock nil
                logos-olivetti t)
  :bind
  (([remap narrow-to-region] . logos-narrow-dwim)
   ([remap forward-page]     . logos-forward-page-dwim)
   ([remap backward-page]    . logos-backward-page-dwim)
   ("M-]" . logos-forward-page-dwim)
   ("M-[" . logos-backward-page-dwim)
   :map my-toggle-map
   ("f" . logos-focus-mode))
  :custom
  (logos-outlines-are-pages t))

(use-package olivetti
  :pin melpa
  :custom
  (olivetti-minimum-body-width 120)
  (olivetti-recall-visual-line-mode-entry-state t))

(use-package header-line
  :ensure nil
  :load-path "site-lisp/"
  :hook (prog-mode text-mode conf-mode))

(use-package lin
  :config
  (lin-global-mode 1))

(use-package pulsar
  :custom
  (pulsar-pulse-region-functions pulsar-pulse-region-common-functions)
  :config
  (pulsar-global-mode 1))

(provide 'init-ux)
