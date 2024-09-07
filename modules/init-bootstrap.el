;; Making custom-file disposable
(setq custom-file (make-temp-file "emacs-custom-"))

;; Initializing package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq use-package-always-pin "gnu")

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

;; Enabling `envrc-global-mode'
(use-package envrc
  :pin melpa
  :hook (after-init . envrc-global-mode))

;; Enabling Emacs server
(use-package server
  :ensure nil
  :if (display-graphic-p)
  :defer 20
  :custom
  (server-name "gui")
  :config
  (unless (server-running-p)
    (server-start)))

(provide 'init-bootstrap)
