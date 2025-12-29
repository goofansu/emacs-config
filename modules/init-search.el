(use-package devdocs
  :init
  (setq-default devdocs-current-docs '("html" "css" "javascript"))
  :hook
  (ruby-ts-mode . (lambda () (setq-local devdocs-current-docs '("ruby~3.4" "rails~8.0"))))
  :bind
  ( :map search-map
    ("k" . devdocs-lookup)
    ("K" . devdocs-search))
  :config
  (defun my/devdocs-install ()
    "Download and install selected DevDocs documentations."
    (interactive)
    (let* ((available-docs (mapcar (lambda (doc) (alist-get 'slug doc))
                                   (devdocs--available-docs)))
           (selected-docs (completing-read-multiple "Install documentations: " available-docs)))
      (dolist (doc selected-docs)
        (devdocs-install doc)))))

(use-package dictionary
  :ensure nil
  :bind
  (:map search-map ("d" . dictionary-lookup-definition))
  :custom
  (dictionary-server "dict.org")
  (dictionary-default-popup-strategy "lev")
  (dictionary-create-buttons nil)
  (dictionary-use-single-buffer t))

(use-package osx-dictionary
  :pin melpa
  :if (eq system-type 'darwin)
  :bind (:map search-map ("t" . osx-dictionary-search-word-at-point)))

(use-package google
  :ensure nil
  :load-path "site-lisp/"
  :bind (:map search-map ("M-s" . google-search)))

(provide 'init-search)
