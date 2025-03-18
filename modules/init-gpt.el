(use-package gptel
  :pin melpa
  :init
  (defvar gptel--openai nil
    "Override the variable to hide OpenAI models")

  (gptel-make-gemini "Google"
    :key (lambda () (auth-source-pass-get 'secret "api-key/gemini"))
    :stream t)

  (defvar gptel--openrouter
    (gptel-make-openai "OpenRouter"
      :host "openrouter.ai"
      :endpoint "/api/v1/chat/completions"
      :stream t
      :key (lambda () (auth-source-pass-get 'secret "api-key/openrouter"))
      :models '(anthropic/claude-3.7-sonnet
                openai/o3-mini-high
                openai/gpt-4o-mini
                openai/gpt-4o
                openai/gpt-4o-mini-search-preview
                openai/gpt-4o-search-preview
                )))

  :bind
  (("C-c <return>" . gptel-send)
   ("C-c C-<return>" . gptel-menu)
   ("C-c M-<return>" . my/gptel-send-all-buffers)
   :map embark-region-map
   ("g t" . my/gptel-translate)
   ("g u" . my/gptel-summarize-text)
   :map embark-prose-map
   ("g t" . my/gptel-translate)
   ("g u" . my/gptel-summarize-text))

  :custom
  (gptel-default-mode 'org-mode)

  :config
  (setq gptel-backend gptel--openrouter
        gptel-model 'openai/gpt-4o)

  (defun my/gptel-send-all-buffers (prompt)
    "Send PROMPT in all buffers where gptel-mode is active."
    (interactive "sPrompt: ")
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (bound-and-true-p gptel-mode)
          (save-excursion
            (goto-char (point-max))
            (insert prompt)
            (gptel-send))))))

  (defun my/gptel--callback-display-bottom (response info)
    (if (not (stringp response))
        (message "Response failed with error: %S" response)
      (pcase-let ((`(,pattern) (plist-get info :context)))
        (with-current-buffer (generate-new-buffer (format "*gptel-%s*" pattern))
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert response)
            (display-buffer
             (current-buffer)
             `((display-buffer-in-side-window)
               (side . bottom)
               (window-height . ,#'fit-window-to-buffer))))
          (special-mode)))))

  (defun my/gptel-translate (text)
    "Translate TEXT into English using LLM.
If region is active, use it as TEXT; otherwise prompt for input.
Display the result in a side window with the content selected."
    (interactive "sText: ")
    (let ((gptel-backend gptel--openrouter)
          (gptel-model 'openai/gpt-4o-mini))
      (gptel-request text
        :system "You translate text between English and Chinese (Mandarin),
preserving both the original formatting and intended
meaning. Whether translating from English to Chinese or Chinese
to English, you maintain elements like paragraph breaks,
emphasis, and tone while ensuring the translation captures the
nuances of the source text."
        :context (list "translate")
        :callback #'my/gptel--callback-display-bottom)))

  (defun my/gptel-summarize-text (text)
    "Summarize TEXT into takeaways."
    (interactive "sText: ")
    (let ((gptel-backend gptel--openrouter)
          (gptel-model 'openai/gpt-4o-mini))
      (gptel-request text
        :system "You help users distill key points from
text passages. When given a piece of text, you will identify and
explain at least 3 important takeaways that capture the main
ideas, themes, and insights from the content."
        :context (list "summarize-text")
        :callback #'my/gptel--callback-display-bottom))))

(use-package gptel-quick
  :vc (gptel-quick :url "https://github.com/karthink/gptel-quick.git")
  :bind (:map embark-general-map ("?" . gptel-quick))
  :config
  (setq gptel-quick-backend gptel--openrouter
        gptel-quick-model 'openai/gpt-4o-mini))

(provide 'init-gpt)
