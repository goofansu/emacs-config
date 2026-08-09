(use-package nix-mode
  :pin nongnu
  :hook (before-save . apheleia-mode)
  :bind (:map my-insert-map
              ("n n" . my/homebrew-casks-insert)
              ("n f" . my/homebrew-formulae-insert))
  :config
  (defun my/homebrew-packages-insert (kind)
    "Insert currently installed Homebrew packages of KIND at point."
    (let* ((output (string-trim
                    (shell-command-to-string (format "brew list --%s" kind))))
           (packages (unless (string-empty-p output)
                       (split-string output "\n")))
           (quoted-packages
            (mapcar (lambda (package) (format "\"%s\"" package)) packages)))
      (insert (format "[ %s ];" (string-join quoted-packages " ")))))
  (defun my/homebrew-casks-insert ()
    "Insert currently installed Homebrew Casks at point."
    (interactive)
    (my/homebrew-packages-insert "cask"))
  (defun my/homebrew-formulae-insert ()
    "Insert currently installed Homebrew Formulae at point."
    (interactive)
    (my/homebrew-packages-insert "formulae")))

(provide 'init-nix)
