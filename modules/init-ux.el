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
  (setq-default logos-hide-mode-line t
                logos-hide-header-line t
                logos-hide-buffer-boundaries t
                logos-hide-fringe t
                logos-variable-pitch t
                logos-olivetti t)
  :bind
  (("C-x n n" . logos-narrow-dwim)
   ("C-x ]" . logos-forward-page-dwim)
   ("C-x [" . logos-backward-page-dwim)
   ("M-]" . logos-forward-page-dwim)
   ("M-[" . logos-backward-page-dwim)
   :map my-toggle-map
   ("f" . logos-focus-mode))
  :custom
  (logos-outlines-are-pages t)
  :config
  (add-hook 'enable-theme-functions #'logos-update-fringe-in-buffers))

(use-package olivetti
  :pin melpa
  :defer t
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
