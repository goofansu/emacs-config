(use-package macos
  :ensure nil
  :load-path "site-lisp/"
  :if (eq system-type 'darwin)
  :demand t
  :init
  (keymap-global-unset "C-z")
  :bind
  (("s-o" . find-file)
   ("s-s" . save-buffer)
   ("s-S" . write-file)
   ("s-a" . mark-whole-buffer)
   ("s-c" . kill-ring-save)
   ("s-v" . yank)
   ("s-x" . kill-region)
   ("s-z" . undo)
   ("s-Z" . undo-redo)
   ("C-z z" . zap-to-char)
   ("C-z C-z" . zap-up-to-char))
  :custom
  (vscode-program "cursor"))

(use-package alfred
  :ensure nil
  :load-path "site-lisp/"
  :if (eq system-type 'darwin)
  :bind
  (:map my-insert-map
        ("b m" . alfred-browser-md-link)
        ("b o" . alfred-browser-org-link)
        ("b t" . alfred-browser-title)
        ("b u" . alfred-browser-url)))

(provide 'init-macos)
