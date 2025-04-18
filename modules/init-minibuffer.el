(use-package vertico
  :init
  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt.
  (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  :custom
  (vertico-cycle t)
  :config
  (vertico-mode 1))

(use-package ffap
  :bind ("M-m" . ffap-menu)
  :config
  ;; Disable `ffap-menu's *Completions* buffer* because it's uncessary with vertico.
  (advice-add 'ffap-menu-ask :around
              (lambda (&rest args)
                (cl-letf (((symbol-function #'minibuffer-completion-help)
                           #'ignore))
                  (apply args)))))

(use-package vertico-repeat
  :ensure nil
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind ("C-x ." . vertico-repeat))

(use-package vertico-suspend
  :ensure nil
  :bind ("M-z" . vertico-suspend))

(use-package vertico-directory
  :ensure nil
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word)))

(use-package vertico-quick
  :ensure nil
  :bind
  (:map vertico-map
        ("M-q" . vertico-quick-insert)
        ("C-q" . vertico-quick-exit)))

(use-package vertico-multiform
  :ensure nil
  :custom
  (vertico-multiform-categories
   '((embark-keybinding grid)
     (jinx grid (vertico-grid-annotate . 20))))
  :config
  (vertico-multiform-mode 1))

(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :bind
  (([remap goto-line] . consult-goto-line)
   ([remap bookmark-jump] . consult-bookmark)
   ([remap switch-to-buffer] . consult-buffer)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
   ([remap switch-to-buffer-other-tab] . consult-buffer-other-tab)
   ([remap project-switch-to-buffer] . consult-project-buffer)
   ("M-X" . consult-mode-command)
   ("M-y" . consult-yank-pop)
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store)
   :map goto-map
   ("i" . consult-imenu)
   ("I" . consult-imenu-multi)
   ("m" . consult-mark)
   ("k" . consult-global-mark)
   ("o" . consult-outline)
   ("r" . consult-register)
   ("f" . consult-flycheck)
   ("a" . consult-org-agenda)
   :map search-map
   ("l" . consult-line)
   ("L" . consult-line-multi)
   ("r" . consult-ripgrep)
   ("e" . consult-isearch-history)
   :map minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history))

  :custom
  (consult-fd-args "fd --ignore-case --full-path --color=never"))

(use-package consult-dir
  :pin melpa
  :bind
  (("C-x C-d" . consult-dir)
   :map minibuffer-local-completion-map
   ("C-x C-d" . consult-dir)
   ("C-x C-j" . consult-dir-jump-file)))

(use-package embark
  :demand t
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (unbind-key "C-h C-h")
  :bind
  (([remap describe-bindings] . embark-bindings)
   ("C-;" . embark-act)
   ("M-." . embark-dwim)
   :map minibuffer-local-map
   ("C-;" . embark-act)
   ("C-c C-;" . embark-export)
   ("C-c C-l" . embark-collect)
   :map embark-file-map
   ("t" . find-file-other-tab)
   ("T" . find-file-other-frame)
   :map embark-buffer-map
   ("t" . switch-to-buffer-other-tab)
   ("T" . switch-to-buffer-other-frame))
  :custom
  (embark-indicators
   '(embark-minimal-indicator
     embark-highlight-indicator
     embark-isearch-highlight-indicator)))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package embark-jira
  :ensure nil
  :load-path "site-lisp/"
  :custom
  (embark-jira-host "https://fariaedu.atlassian.net")
  :config
  (with-eval-after-load 'embark
    (add-to-list 'embark-target-finders (lambda () (embark-jira-target-link "OA")))))

(provide 'init-minibuffer)
