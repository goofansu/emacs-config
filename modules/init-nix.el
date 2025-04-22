(use-package nix-mode
  :pin nongnu
  :hook (before-save . nix-format-before-save)
  :bind (:map my-insert-map ("n n" . my/homebrew-casks-insert))
  :config
  (defun org-babel-execute:nix (body params)
    (setq strict-option (if (assoc :strict params) "--strict" ""))
    (with-temp-buffer
      (insert body)
      (shell-command-on-region
       (point-min) (point-max)
       (concat "nix-instantiate --eval " strict-option " - <<EOF\n$(cat)\nEOF")
       (current-buffer)
       t)
      (buffer-string)))

  (defun my/homebrew-casks-insert ()
    "Insert currently installed Homebrew Casks at position."
    (interactive)
    (let* ((casks (shell-command-to-string "brew list --cask | awk '{print \"\\\"\" $0 \"\\\"\"}'"))
           (result (concat "[" casks "];")))
      (insert result))))

(use-package org-nix-shell
  :pin melpa
  :hook org-mode)

(provide 'init-nix)
