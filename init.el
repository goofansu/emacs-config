(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Custom variables
(require 'init-custom)

;; Bootstrap
(require 'init-bootstrap)
(require 'init-macos)
(require 'init-emacs)

;; Features
(require 'init-git)
(require 'init-gpt)
(require 'init-mail)
(require 'init-pass)
(require 'init-search)

;; Contexts
(require 'init-programming)
(require 'init-reading)
(require 'init-writing)
