(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'lisp-buffer)
(require 'lisp-file)
(require 'lisp-git)
(require 'lisp-macos)
(require 'lisp-lookup)
(require 'lisp-mail)
(require 'lisp-project)
(require 'lisp-exercism)
(require 'lisp-org)
(require 'lisp-embark)

(provide 'init-lisp)
