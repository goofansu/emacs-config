(setq inhibit-startup-screen t        ; Skip the startup screen
      initial-scratch-message nil     ; Blank the *scratch* buffer
      confirm-kill-emacs 'y-or-n-p    ; Confirm when leaving Emacs
      ring-bell-function 'ignore      ; Disable ring bell
      create-lockfiles nil            ; Avoid creating ".#filename"
      make-backup-files nil)          ; Avoid creating "filename~"

(setq native-comp-async-report-warnings-errors 'silent)
(setq initial-major-mode 'fundamental-mode)

(when (display-graphic-p)
  (tool-bar-mode -1)
  (set-scroll-bar-mode nil))

(unless (display-graphic-p)
  (menu-bar-mode -1))

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

(use-package helpful
  :pin melpa
  :config
  (message "helpful is loaded")
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)))

(use-package ibuffer
  :ensure nil
  :config
  (message "ibuffer is loaded")
  :custom
  (ibuffer-expert t)
  :bind ([remap list-buffers] . ibuffer)
  :hook (ibuffer-mode . ibuffer-auto-mode))

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
  (undo-fu-session-global-mode)
  :custom
  (undo-fu-session-compression 'zst)
  (undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

;; https://tecosaur.github.io/emacs-config/config.html#better-defaults
(setq-default delete-by-moving-to-trash t         ; Delete files to trash
              window-combination-resize t         ; take new window space from all other windows (not just current)
              x-stretch-cursor t)                 ; Stretch cursor to the glyph width

(setq auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"                ; Unicode ellispis are nicer than "...", and also save /precious/ space
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
