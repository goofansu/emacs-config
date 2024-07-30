(use-package pass
  :pin melpa
  :init
  (auth-source-pass-enable)
  :bind ("C-c p" . password-store-copy)
  :config
  (defvar-keymap embark-password-store-map
    "c" #'password-store-copy
    "C" #'password-store-otp-token-copy
    "f" #'password-store-copy-field
    "i" #'password-store-insert
    "I" #'password-store-generate
    "r" #'password-store-rename
    "e" #'password-store-edit
    "d" #'password-store-remove
    "U" #'password-store-url)
  (with-eval-after-load 'embark
    (add-to-list 'embark-keymap-alist '(password-store . embark-password-store-map)))
  (with-eval-after-load 'marginalia
    (add-to-list 'marginalia-prompt-categories '("Password entry" . password-store))))

(provide 'init-pass)
