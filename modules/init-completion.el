(use-package corfu
  :init
  ;; Enable indentation+completion using the TAB key.
  (setq tab-always-indent 'complete)
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.5)
  :config
  (global-corfu-mode 1))

(use-package corfu-quick
  :ensure nil
  :after corfu
  :bind (:map corfu-map ("'" . corfu-quick-complete)))

(use-package corfu-history
  :ensure nil
  :after corfu
  :config
  (corfu-history-mode 1))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  :bind-keymap ("C-c p" . cape-prefix-map))

(use-package tempel
  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  :hook ((prog-mode text-mode conf-mode) . tempel-setup-capf)
  :bind
  (("M-+" . tempel-complete)
   ("M-*" . tempel-insert)
   ("C-c T" . my/tempel-find-template-file))
  :custom
  (tempel-path (list (expand-file-name "templates/*.eld" user-emacs-directory)
                     (expand-file-name "tempel-templates/*.eld" my-src-directory)))
  :config
  (defun my/tempel-find-template-file ()
    "List template files and open the selected one."
    (interactive)
    (when-let* ((template-files
                 (delete-dups
                  (mapcan (lambda (path)
                            (if (file-directory-p path)
                                (directory-files path t "\\.eld\\'")
                              (file-expand-wildcards path t)))
                          tempel-path)))
                (selected-file (completing-read "Select file: " template-files nil t)))
      (find-file selected-file))))

(use-package org-block-capf
  :ensure nil
  :load-path "vendor/"
  :hook (org-mode . org-block-capf-add-to-completion-at-point-functions))

(provide 'init-completion)
