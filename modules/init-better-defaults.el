(use-package emacs
  :ensure nil
  :init
  (setq-default indent-tabs-mode nil
                tab-width 4
                fill-column 80)
  :custom
  (scroll-margin 2)
  (use-dialog-box nil)
  (use-short-answers t)
  (ring-bell-function 'ignore)
  (confirm-kill-emacs 'y-or-n-p)
  (truncate-string-ellipsis "…")
  (select-enable-clipboard t)

  ;; Editing
  (require-final-newline t)

  ;; File
  (delete-by-moving-to-trash t)
  (create-lockfiles nil)
  (make-backup-files nil)
  (auto-save-interval 2400)
  (auto-save-timeout 300)
  (auto-save-file-name-transforms
   `((".*" ,temporary-file-directory t)))

  ;; Input method
  (default-input-method "chinese-py")

  ;; Keyboard
  (echo-keystrokes 0.25)

  ;; Password
  (password-cache-expiry nil)

  ;; Startup
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (initial-major-mode 'fundamental-mode)

  ;; Version control
  (vc-follow-symlinks t)

  ;; Window
  (window-combination-resize t)

  ;; Xref
  (xref-search-program 'ripgrep))

(use-package emacs
  :ensure nil
  :bind-keymap ("M-r" . ctl-x-r-map)
  :bind
  (("C-c y" . my/yank-file-path)
   ("C-c Y" . my/yank-file-path-relative-to-project)
   ("C-c D" . my/delete-this-file)
   ("C-c R" . my/rename-this-file)
   :map goto-map
   ("M-g" . my/project-find-file-and-goto-line))
  :config
  (defun my/yank-file-path (&optional buffer dir)
    "Save the buffer path into the kill-ring.
If BUFFER is not nil, find filename of BUFFER, otherwise, find filename of `current-buffer'.
If DIR is not nil, get a relative file path, otherwise, get a full file path.
When region is active, includes line numbers (e.g., file.rb#L1 or file.rb#L1-L10)."
    (interactive)
    (let* ((target-buffer (or buffer (current-buffer)))
           (filename (buffer-file-name target-buffer))
           (file-path (if filename
                          (if dir
                              (file-relative-name filename dir)
                            filename)
                        (buffer-name target-buffer)))
           (reference (if (use-region-p)
                          (let ((start-line (line-number-at-pos (region-beginning)))
                                (end-line (line-number-at-pos (region-end))))
                            (if (= start-line end-line)
                                (format "%s#L%d" file-path start-line)
                              (format "%s#L%d-L%d" file-path start-line end-line)))
                        file-path)))
      (kill-new reference)
      (message "Copied path: %s" reference)))

  (defun my/yank-file-path-relative-to-project (&optional buffer)
    "Save the buffer path into the kill-ring.
If BUFFER is not nil, use that buffer, otherwise use current buffer.
If in a project, the path is relative to project root.
If not in a project, uses the full file path.
When region is active, includes line numbers (e.g., file.rb#L1 or file.rb#L1-L10)."
    (interactive)
    (let ((project-root-dir
           (condition-case nil
               (project-root (project-current))
             (error nil))))
      (my/yank-file-path buffer project-root-dir)))

  (defun my/delete-this-file ()
    "Kill the current buffer and deletes the file it is visiting."
    (interactive)
    (unless (and buffer-file-name (file-exists-p buffer-file-name))
      (user-error "Buffer is not visiting any file"))
    (when-let* ((buffer (current-buffer))
                (filename (buffer-file-name buffer))
                (path (abbreviate-file-name filename)))
      (when (y-or-n-p (format "Really delete %s? " path))
        (move-file-to-trash path)
        (kill-buffer buffer)
        (message "Deleted %s" path))))

  (defun my/rename-this-file (new-path)
    "Rename the current file to NEW-PATH."
    (interactive (list (read-file-name "Rename file to: ")))
    (unless (and buffer-file-name (file-exists-p buffer-file-name))
      (user-error "Buffer is not visiting any file"))
    (let ((old-path (buffer-file-name (buffer-base-buffer)))
          (new-path (expand-file-name new-path)))
      (when (directory-name-p new-path)
        (setq new-path (concat new-path (file-name-nondirectory old-path))))
      (make-directory (file-name-directory new-path) 't)
      (rename-file old-path new-path)
      (set-visited-file-name new-path t t)
      (message "Renamed to %s" (abbreviate-file-name new-path))))

  (defun my/project-find-file-and-goto-line (file-line)
    "Open a file within the current project and go to a specific line number.
FILE-LINE should be in the format 'file:line' relative to the project root."
    (interactive "sGo to line: ")
    (if (string-match "\\(.*\\):\\([0-9]+\\)$" file-line)
        (let ((file (match-string 1 file-line))
              (line (string-to-number (match-string 2 file-line))))
          (let* ((project-root-dir (project-root (project-current t)))
                 (full-path (expand-file-name file project-root-dir)))
            (if (file-exists-p full-path)
                (progn
                  (find-file full-path)
                  (goto-char (point-min))
                  (forward-line (1- line)))
              (user-error "File does not exist: %s" full-path))))
      (user-error "FILE-LINE must be in the format: 'file:line'"))))

(use-package recentf
  :ensure nil
  :hook after-init
  :custom
  (recentf-max-saved-items 200))

(use-package savehist
  :ensure nil
  :hook after-init
  :custom
  (history-length 500)
  (history-delete-duplicates t)
  :config
  (add-to-list 'savehist-additional-variables 'log-edit-comment-ring))

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package isearch
  :ensure nil
  :custom
  (isearch-lazy-count t)
  (isearch-repeat-on-direction-change t))

(use-package ibuffer
  :ensure nil
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :bind ([remap list-buffers] . ibuffer))

(use-package ediff
  :ensure nil
  :defer t
  :custom
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-split-window-function #'split-window-horizontally))

(use-package undo-fu-session
  :pin nongnu
  :init
  (setq undo-limit (* 64 1024 1024)
        undo-strong-limit (* 96 1024 1024)
        undo-outer-limit (* 960 1024 1024))
  :hook (prog-mode text-mode conf-mode)
  :custom
  (undo-fu-session-compression 'zst))

(use-package helpful
  :pin melpa
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h x" . helpful-command)))

(use-package crux
  :pin nongnu
  :bind
  (([remap move-beginning-of-line] . crux-move-beginning-of-line)
   ([remap kill-whole-line] . crux-kill-whole-line)
   ("C-c d" . crux-duplicate-current-line-or-region)
   ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
   ("s-k" . crux-smart-kill-line)
   ("s-j" . crux-top-join-line)
   ("C-^" . crux-switch-to-previous-buffer)
   ("s-n" . crux-create-scratch-buffer)))

(use-package doc-view
  :ensure nil
  :defer t
  :custom
  (doc-view-mupdf-use-svg t)
  (doc-view-resolution 300))

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package dired
  :ensure nil
  :defer t
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-dwim-target t))

(use-package dired-preview
  :hook dired-mode
  :custom
  (dired-preview-trigger-on-start nil))

(use-package ffap
  :bind ("M-m" . ffap-menu)
  :config
  ;; Disable `ffap-menu's *Completions* buffer* because it's uncessary with vertico.
  (advice-add 'ffap-menu-ask :around
              (lambda (&rest args)
                (cl-letf (((symbol-function #'minibuffer-completion-help)
                           #'ignore))
                  (apply args)))))

(use-package winner
  :ensure nil
  :hook after-init)

(use-package desktop
  :ensure nil
  :disabled t
  :init
  (setq desktop-dirname user-emacs-directory)
  :hook (after-init . desktop-save-mode))

(provide 'init-better-defaults)
