(use-package gptel
  :pin melpa
  :init
  (defvar gptel--openai nil
    "Override the variable to hide OpenAI models")

  :bind
  (("C-c <return>" . gptel-send)
   ("C-c C-<return>" . gptel-menu)
   :map search-map
   ("=" . my/gptel-summarize))

  :custom
  (gptel-include-reasoning nil)

  :config
  (defface my/gptel-user-prefix
    '((t :inherit font-lock-keyword-face :weight bold))
    "Face for the gptel user prefix.")

  (defface my/gptel-assistant-prefix
    '((t :inherit font-lock-function-name-face :weight bold))
    "Face for the gptel assistant prefix.")

  (setf (alist-get 'markdown-mode gptel-prompt-prefix-alist)
        (propertize (format "%s\n" user-full-name)
                    'font-lock-face 'my/gptel-user-prefix))
  (setf (alist-get 'markdown-mode gptel-response-prefix-alist)
        (propertize "Assistant\n" 'font-lock-face 'my/gptel-assistant-prefix))

  (setq gptel-model 'qwen/qwen3.5-9b
        gptel-backend (gptel-make-openai "LM Studio"
                        :protocol "http"
                        :host "127.0.0.1:1234"
                        :endpoint "/v1/chat/completions"
                        :stream t
                        :models '(qwen/qwen3.5-9b)))

  (defun my/gptel-buffer-names ()
    "Return the names of buffers where `gptel-mode' is active."
    (mapcar #'buffer-name (match-buffers
                           (lambda (buf)
                             (with-current-buffer buf
                               (bound-and-true-p gptel-mode))))))

  (defvar consult--source-gptel
    `( :name     "gptel buffer"
       :narrow   ?g
       :category buffer
       :face     consult-buffer
       :history  beframe-history
       :items    ,#'my/gptel-buffer-names
       :action   ,#'switch-to-buffer
       :state    ,#'consult--buffer-state
       :hidden   t)
    "gptel buffer source for `consult-buffer'.")

  (add-to-list 'consult-buffer-sources 'consult--source-gptel)

  (defun my/gptel-remap-header-line-button-underline ()
    "Make gptel header-line buttons align with spacious-padding underline."
    (face-remap-add-relative
     'button
     `(:underline (:color ,(or (face-foreground 'header-line nil t)
                               (face-foreground 'default nil t))
                          :position t))))

  (add-hook 'gptel-mode-hook #'my/gptel-remap-header-line-button-underline)

  (defconst my/gptel-summary-prompt
    "Summarize the main message and key points in plain, concise language. No extra interpretation or detail."
    "Prompt for summary requests.")

  (defun my/gptel-send-current-buffer (buffer-name default-prompt)
    "Send current buffer context to BUFFER-NAME with a minibuffer prompt.
PROMPT-NAME labels the minibuffer prompt. DEFAULT-PROMPT is used as the
minibuffer default."
    (let* ((source-buffer (current-buffer))
           (minibuffer-setup-hook
            (cons (lambda ()
                    (goto-char (minibuffer-prompt-end))
                    (push-mark (point-max) nil t))
                  minibuffer-setup-hook))
           (prompt (read-string "Prompt: " default-prompt nil default-prompt)))
      (with-current-buffer (gptel buffer-name nil nil t)
        (setq-local gptel-context (list source-buffer))
        (setq-local gptel-use-context 'system)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (or (gptel-prompt-prefix-string) "") prompt))
        (gptel-send))))

  (defun my/gptel-summarize ()
    "Summarize the current buffer in a gptel buffer."
    (interactive)
    (my/gptel-send-current-buffer "*gptel-summary*" my/gptel-summary-prompt)))

(provide 'init-gpt)
