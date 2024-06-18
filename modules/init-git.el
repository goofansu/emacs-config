(use-package magit
  :pin melpa
  :demand t
  :init
  (setq magit-repository-directories
        '(("~/src" . 1)
          ("~/work" . 1)))
  :config
  (message "magit is loaded")
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-bury-buffer-function #'quit-window)
  (magit-show-long-lines-warning nil))

(use-package transient
  :ensure nil
  :after magit
  :config
  (message "transient is loaded")

  ;; git push with skip-ci option
  (transient-append-suffix 'magit-push "-n"
    '("-s" "Skip CI" "--push-option=skip-ci"))

  ;; git push to all remotes
  (defun +magit/push-all (&optional args)
    (interactive (list (magit-push-arguments)))
    (dolist (remote (magit-list-remotes))
      (magit-push-to-remote remote args)))
  (transient-append-suffix 'magit-push "e"
    '("E" "everywhere" +magit/push-all)))

(use-package browse-at-remote
  :pin melpa
  :defer t
  :config
  (message "browse-at-remote is loaded")
  :custom
  (browse-at-remote-add-line-number-if-no-region-selected nil))

(use-package git-gutter
  :pin melpa
  :config
  (message "git-gutter is loaded")
  (with-eval-after-load 'evil
    (evil-define-key 'motion 'global
      "]d" #'git-gutter:next-hunk
      "[d" #'git-gutter:previous-hunk))
  :custom
  (git-gutter:added-sign " ")
  (git-gutter:deleted-sign " ")
  (git-gutter:modified-sign " ")
  :hook prog-mode)

(use-package git-timemachine
  :pin melpa
  :defer t
  :config
  (message "git-timemachine is loaded"))

(use-package git-link
  :pin melpa
  :defer t
  :config
  (message "git-link is loaded"))

(provide 'init-git)
