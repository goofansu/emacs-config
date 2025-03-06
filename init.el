(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(defvar my-src-directory "~/src/"
  "Directory for source code repos.")

;; Bootstrap
(require 'init-bootstrap)
(require 'init-emacs)
(require 'init-macos)

;; Features
(require 'init-git)
(require 'init-gpt)
(require 'init-mail)
(require 'init-pass)
(require 'init-search)
(require 'init-shell-command)
(require 'init-spell-checker)
(require 'init-syntax-checker)

;; Contexts
(require 'init-programming)
(require 'init-writing)

;; Services
(require 'init-github)
