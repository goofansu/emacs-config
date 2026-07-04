(use-package markdown-mode
  :pin nongnu
  :bind
  (:map markdown-mode-map
        ("C-c C-v" . my/markdown-toggle-view)
   :map markdown-view-mode-map
        ("C-c C-v" . my/markdown-toggle-view))
  :config
  (defun my/markdown-toggle-view ()
    "Toggle between `markdown-mode' and `markdown-view-mode'."
    (interactive)
    (cond
     ((eq major-mode 'markdown-view-mode)
      (markdown-mode))
     ((derived-mode-p 'markdown-mode)
      (markdown-view-mode))
     (t
      (user-error "Current buffer is not a Markdown buffer")))))

(provide 'init-markdown)
