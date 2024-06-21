(setq inhibit-startup-screen t        ; Skip the startup screen
      initial-scratch-message nil     ; Blank the *scratch* buffer
      use-dialog-box nil              ; Disable dialog windows
      confirm-kill-emacs 'y-or-n-p    ; Confirm when leaving Emacs
      ring-bell-function 'ignore      ; Disable ring bell
      create-lockfiles nil            ; Avoid creating ".#filename"
      make-backup-files nil)          ; Avoid creating "filename~"

(setq native-comp-async-report-warnings-errors 'silent)
(setq initial-major-mode 'fundamental-mode)
(setq ns-use-native-fullscreen nil)

(use-package recentf
  :ensure nil
  :config
  (message "recentf is loaded")
  (recentf-mode 1))

(use-package savehist
  :ensure nil
  :config
  (message "savehist is loaded")
  (add-to-list 'savehist-additional-variables 'log-edit-comment-ring)
  (savehist-mode 1)
  :custom
  (history-length 500)
  (history-delete-duplicates t))

(use-package ibuffer
  :ensure nil
  :config
  (message "ibuffer is loaded")
  :custom
  (ibuffer-expert t)
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :bind ([remap list-buffers] . ibuffer))

(use-package ediff
  :ensure nil
  :defer t
  :config
  (message "ediff is loaded")
  :custom
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-split-window-function #'split-window-horizontally))

(use-package helpful
  :pin melpa
  :config
  (message "helpful is loaded")
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)))

(use-package undo-fu
  :pin melpa
  :init
  (setq undo-limit 67108864          ; 64mb
        undo-strong-limit 100663296  ; 96mb
        undo-outer-limit 1006632960) ; 960mb
  :config
  (message "undo-fu is loaded"))

(use-package undo-fu-session
  :pin melpa
  :after undo-fu
  :config
  (message "undo-fu-session is loaded")
  (undo-fu-session-global-mode 1)
  :custom
  (undo-fu-session-compression 'zst)
  (undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(use-package xclip
  :unless (display-graphic-p)
  :config
  (message "xclip is loaded")
  (xclip-mode 1))

(use-package compile
  :ensure nil
  :config
  (message "compile is loaded")
  (defun compilation-filter-colorize ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook (compilation-filter . compilation-filter-colorize))

(use-package project
  :ensure nil
  :defer t
  :config
  (message "project is loaded")
  :custom
  (project-switch-commands
   '((project-find-file "Find file")
     (project-find-dir "Find directory")
     (project-dired "Root dired")
     (project-eshell "Eshell")
     (magit-project-status "Git" "g")
     (+project/search "Search project" "s")
     (+project/search-for-symbol-at-point "Search project with symbol" "S"))))

(use-package time
  :ensure nil
  :custom
  (world-clock-time-format "%F %T %z")
  (zoneinfo-style-world-list
   '(("America/Vancouver" "Vancouver")
     ("America/Chicago" "Portland")
     ("UTC" "UTC")
     ("Europe/London" "London")
     ("Europe/Kyiv" "Kyiv")
     ("Asia/Shanghai" "Shanghai"))))

(use-package visual-line-mode
  :ensure nil
  :config
  (message "visual-line-mode is enabled")
  :hook (org-mode markdown-mode gptel-mode))

(use-package window
  :ensure nil
  :config
  (message "window is loaded")
  :bind ("C-x !" . delete-other-windows-vertically))

(use-package winner
  :ensure nil
  :config
  (message "winner is loaded")
  (winner-mode 1))

(use-package dictionary
  :ensure nil
  :custom
  (dictionary-server "dict.org")
  (dictionary-default-popup-strategy "lev")
  (dictionary-create-buttons nil)
  (dictionary-use-single-buffer t)
  :bind ("C-c d" . dictionary-search))

(use-package browse-url
  :ensure nil
  :init
  (setq browse-url-firefox-program "/opt/homebrew/bin/firefox")
  :custom
  (browse-url-browser-function #'browse-url-firefox))

;; https://tecosaur.github.io/emacs-config/config.html#better-defaults
(setq-default delete-by-moving-to-trash t         ; Delete files to trash
              window-combination-resize t         ; take new window space from all other windows (not just current)
              x-stretch-cursor t)                 ; Stretch cursor to the glyph width

(setq auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"               ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil                   ; I can trust my computers ... can't I?
      scroll-margin 2                             ; It's nice to maintain a little margin
      display-time-default-load-average nil       ; I don't think I've ever found this useful
      use-short-answers t                         ; Prefer short anwsers
      require-final-newline t)                    ; Save file with a trailing newline

(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))

;; Advices
(defun move-beginning-of-line-advice (orig-fun &rest args)
  "Advice to toggle point movement between first non-whitespace
character and beginning of line."
  (let ((orig-point (point)))
    (beginning-of-line-text)
    (when (= orig-point (point))
      (apply orig-fun args))))

(advice-add 'move-beginning-of-line :around #'move-beginning-of-line-advice)

(provide 'init-better-defaults)
