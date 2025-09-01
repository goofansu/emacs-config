;; Initializing package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq use-package-always-pin "gnu")

;; Benchmark Emacs startup time
(use-package benchmark-init
  :pin melpa
  :disabled t
  :config
  (benchmark-init/activate)
  (add-hook 'emacs-startup-hook 'benchmark-init/deactivate))

;; Set $PATH correctly
(use-package exec-path-from-shell
  :pin nongnu
  :if (memq window-system '(mac ns x))
  :custom
  (exec-path-from-shell-shell-name "/run/current-system/sw/bin/fish")
  :config
  (exec-path-from-shell-initialize))

;; Enabling `envrc-global-mode'
(use-package envrc
  :pin melpa
  :hook (after-init . envrc-global-mode))

;; Enabling `desktop-save-mode'
(use-package desktop
  :ensure nil
  :disabled t
  :init
  (setq desktop-dirname user-emacs-directory)
  :hook (after-init . desktop-save-mode))

;; Enabling Emacs server
(use-package server
  :ensure nil
  :if (display-graphic-p)
  :defer 1
  :custom
  (server-name "gui")
  :config
  (unless (server-running-p)
    (server-start)))

(provide 'init-bootstrap)
