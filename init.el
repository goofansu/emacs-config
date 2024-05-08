;; Making custom-file disposable
(setq custom-file (make-temp-file "emacs-custom-"))

;; Loading modules
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Bootstrap
(require 'init-lisp)
(require 'init-elpa)
(require 'init-env)

;; Interaction
(require 'init-minibuffer)
(require 'init-completion)
(require 'init-window)

;; Visualization
(require 'init-ui)
(require 'init-ux)
(require 'init-pulse)
(require 'init-mode-line)

;; Common
(require 'init-better-defaults)
(require 'init-editing-utils)
(require 'init-gui-frames)

;; Evil
(require 'init-evil)
(require 'init-evil-keybindings)

;; Note-taking
(require 'init-denote)
(require 'init-org)

;; Programming
(require 'init-tree-sitter)
(require 'init-eglot)
(require 'init-data)
(require 'init-elixir)
(require 'init-markdown)
(require 'init-nix)
(require 'init-racket)
(require 'init-ruby)
(require 'init-web)
(require 'init-yaml)

;; Tools
(require 'init-git)
(require 'init-pass)
(require 'init-mail)
(require 'init-irc)
(require 'init-gpt)
(require 'init-timer)
(require 'init-lookup)
(require 'init-reading)
(require 'init-restclient)
(require 'init-network-tools)
(require 'init-spell-checker)
