(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(defvar my-src-directory "~/src/"
  "Directory for source code repos.")

(defvar my-sync-directory "~/Library/CloudStorage/Dropbox/"
  "Directory for syncing files.")

;; Bootstrap
(require 'init-bootstrap)
(require 'init-emacs)
(require 'init-macos)

;; Features
(require 'init-git)
(require 'init-gpt)
(require 'init-irc)
(require 'init-mail)
(require 'init-pass)
(require 'init-search)
(require 'init-shell-command)
(require 'init-spell-checker)
(require 'init-syntax-checker)

;; Contexts
(require 'init-programming)
(require 'init-reading)
(require 'init-writing)

;; Miscellaneous
(require 'init-misc)
