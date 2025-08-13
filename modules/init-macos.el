(use-package macos
  :ensure nil
  :load-path "site-lisp/"
  :if (eq system-type 'darwin)
  :custom
  (ns-use-native-fullscreen nil)
  :config
  (keymap-global-set "s-o" #'find-file)
  (keymap-global-set "s-s" #'save-buffer)
  (keymap-global-set "s-S" #'write-file)
  (keymap-global-set "s-a" #'mark-whole-buffer)
  (keymap-global-set "s-c" #'kill-ring-save)
  (keymap-global-set "s-v" #'yank)
  (keymap-global-set "s-x" #'kill-region)
  (keymap-global-set "s-z" #'undo)
  (keymap-global-set "s-Z" #'undo-redo)
  ;; zap to char
  (keymap-global-unset "C-z")
  (keymap-global-set "C-z z" #'zap-to-char)
  (keymap-global-set "C-z C-z" #'zap-up-to-char))

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
