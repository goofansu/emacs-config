(use-package gh
  :ensure nil
  :load-path "site-lisp/"
  :bind
  (("C-c g c" . gh-pr-create)
   ("C-c g v" . gh-pr-view)
   :map embark-region-map
   ("G" . gh-gist-create)))

(use-package consult-gh
  :pin melpa
  :after consult
  :bind ("C-c g g" . consult-gh)
  :custom
  (consult-gh-default-clone-directory "~/src/")
  (consult-gh-repo-action #'consult-gh--repo-browse-files-action)
  (consult-gh-issue-action #'consult-gh--issue-view-action)
  (consult-gh-pr-action #'consult-gh--pr-view-action)
  (consult-gh-code-action #'consult-gh--code-view-action)
  (consult-gh-file-action #'consult-gh--files-view-action)
  (consult-gh-large-file-warning-threshold 2500000)
  :config
  (add-to-list 'savehist-additional-variables 'consult-gh--known-orgs-list)
  (add-to-list 'savehist-additional-variables 'consult-gh--known-repos-list)
  (consult-gh-enable-default-keybindings))

(use-package consult-gh-embark
  :pin melpa
  :after consult-gh
  :config
  (consult-gh-embark-mode 1))

(provide 'init-github)
