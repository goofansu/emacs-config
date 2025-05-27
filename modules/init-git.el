(use-package magit
  :pin melpa
  :defer 1
  :bind
  (("C-c g b" . magit-checkout)
   ("C-c g B" . magit-blame-addition)
   ("C-c g f" . magit-fetch)
   ("C-c g F" . magit-pull)
   ("C-c g l" . magit-log-current)
   ("C-c g L" . magit-log-buffer-file))

  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  :config
  (require 'magit-extras) ;; Bind magit-project-status
  (with-eval-after-load 'transient
    ;; git push with skip-ci option
    (transient-append-suffix 'magit-push "-n"
      '("-s" "Skip CI" "--push-option=skip-ci"))

    ;; git push to all remotes
    (defun my/magit-push-all (&optional args)
      (interactive (list (magit-push-arguments)))
      (dolist (remote (magit-list-remotes))
        (magit-push-to-remote remote args)))
    (transient-append-suffix 'magit-push "e"
      '("E" "everywhere" my/magit-push-all))))

(use-package browse-at-remote
  :pin melpa
  :bind
  (("C-c g o" . browse-at-remote)
   ("C-c g y" . browse-at-remote-kill)))

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
  :init
  (add-to-list 'savehist-additional-variables 'consult-gh--known-orgs-list)
  (add-to-list 'savehist-additional-variables 'consult-gh--known-repos-list)
  :bind ("C-c g p" . consult-gh-search-prs)
  :custom
  (consult-gh-pr-action #'consult-gh--pr-browse-url-action)
  :config
  (consult-gh-enable-default-keybindings))

(use-package consult-gh-embark
  :pin melpa
  :after consult-gh
  :config
  (consult-gh-embark-mode 1))

(provide 'init-git)
