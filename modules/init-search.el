(use-package browser-hist
  :pin melpa
  :bind (:map search-map ("U" . browser-hist-search)))

(use-package devdocs
  :hook
  (ruby-ts-mode . (lambda () (setq-local devdocs-current-docs '("ruby~3.3" "rails~7.0"))))
  (elixir-ts-mode . (lambda () (setq-local devdocs-current-docs '("elixir~1.17" "phoenix"))))
  ((web-mode haml-mode heex-ts-mode) . (lambda () (setq-local devdocs-current-docs '("html" "css" "javascript"))))
  :bind
  (:map search-map
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
  (:map search-map
        ("d" . dictionary-lookup-definition)
        ("D" . dictionary-search))
  :custom
  (dictionary-server "dict.org")
  (dictionary-default-popup-strategy "lev")
  (dictionary-create-buttons nil)
  (dictionary-use-single-buffer t))

(use-package osx-dictionary
  :pin melpa
  :if (eq system-type 'darwin)
  :bind
  (:map search-map
        ("t" . osx-dictionary-search-word-at-point)
        ("T" . osx-dictionary-search-input)))

(use-package kagi
  :ensure nil
  :demand t
  :load-path "site-lisp/"
  :bind
  ( :map search-map
    ("s" . kagi-search)
    ("M-s" . kagi-assistant)
    :map embark-general-map
    ("g S" . kagi-summarize)
    ("g T" . kagi-translate)))

(provide 'init-search)
