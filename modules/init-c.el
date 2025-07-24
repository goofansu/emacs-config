(use-package c-ts-mode
  :ensure nil
  :hook (c-ts-mode . c-format-before-save)
  :config
  (defun c-format-before-save ()
    (add-hook 'before-save-hook #'eglot-format-buffer nil t)))

(provide 'init-c)
