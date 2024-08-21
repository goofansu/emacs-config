(when (eq system-type 'darwin)
  (bind-key "s-o" #'find-file)          ; File -> Open
  (bind-key "s-s" #'save-buffer)        ; File -> Save
  (bind-key "s-S" #'write-file)         ; File -> Save As
  (bind-key "s-a" #'mark-whole-buffer)  ; File -> Select All
  (bind-key "s-c" #'kill-ring-save)     ; Edit -> Copy
  (bind-key "s-v" #'yank)               ; Edit -> Paste
  (bind-key "s-x" #'kill-region)        ; Edit -> Cut
  (bind-key "s-z" #'undo)               ; Edit -> Undo
  (bind-key "s-Z" #'undo-redo))         ; Edit -> Redo

(use-package macos
  :ensure nil
  :load-path "site-lisp/"
  :if (eq system-type 'darwin))

(use-package alfred-browser
  :ensure nil
  :load-path "site-lisp/"
  :if (eq system-type 'darwin)
  :bind
  (("C-c i b m" . alfred-browser-link-in-markdown-format)
   ("C-c i b o" . alfred-browser-link-in-org-format)
   ("C-c i b t" . alfred-browser-link-title)
   ("C-c i b u" . alfred-browser-link-url)))

(provide 'init-macos)
