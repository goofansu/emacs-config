(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(defvar my-src-directory "~/src/"
  "Directory for source code repos.")

;; Bootstrap
(require 'init-bootstrap)
(require 'init-emacs)
(require 'init-macos)

;; Features
(require 'init-alfred)
(require 'init-elfeed)
(require 'init-git)
(require 'init-gpt)
(require 'init-irc)
(require 'init-mail)
(require 'init-org)
(require 'init-pass)
(require 'init-search)
(require 'init-shell-command)
(require 'init-spell-checker)

;; Contexts
(require 'init-programming)
(require 'init-writing)

;; Services
(require 'init-github)
(require 'init-0x0)
