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
  (keymap-global-set "C-z C-z" #'zap-up-to-char)
  ;; Open file in Zed
  (keymap-global-set "M-g z" #'zed-goto-file-at-point))

(provide 'init-macos)
