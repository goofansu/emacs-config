(use-package go-mode
  :pin nongnu
  :hook (go-mode . go-format-before-save)
  :config
  (defun go-format-before-save ()
    (add-hook 'before-save-hook (lambda () (call-interactively 'eglot-code-action-organize-imports)) nil t)
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t)))

(provide 'init-go)
