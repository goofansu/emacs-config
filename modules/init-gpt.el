(use-package gptel
  :pin melpa
  :init
  (defvar gptel--openai nil
    "Override the variable to hide OpenAI models")

  (defvar gptel--openrouter
    (gptel-make-openai "OpenRouter"
      :host "openrouter.ai"
      :endpoint "/api/v1/chat/completions"
      :stream t
      :key (lambda () (auth-source-pass-get 'secret "api-key/openrouter"))
      :models '(anthropic/claude-3.7-sonnet
                anthropic/claude-3.7-sonnet:thinking
                google/gemini-2.5-flash-preview
                google/gemini-2.5-pro-preview-03-25
                openai/gpt-4.1
                openai/gpt-4.1-mini
                openai/gpt-4.1-nano
                openai/o4-mini
                openai/o4-mini-high)))

  :bind
  (("C-c <return>" . gptel-send)
   ("C-c C-<return>" . gptel-menu)
   ("C-c M-<return>" . my/gptel-send-all-buffers)
   :map search-map
   ("T" . my/gptel-translate)
   :map embark-general-map
   ("g t" . my/gptel-translate)
   ("g s" . my/gptel-summarize))

  :custom
  (gptel-default-mode 'org-mode)

  :config
  (setq gptel-backend gptel--openrouter
        gptel-model 'openai/gpt-4.1)

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
    (interactive "sTranslate text: ")
    (let ((gptel-backend gptel--openrouter)
          (gptel-model 'google/gemini-2.5-flash-preview))
      (gptel-request text
        :system "Translate the provided text between English and
Chinese (Mandarin). Return ONLY the completed translation without
explanations, notes, or commentary. Maintain all original formatting
including paragraphs, bullet points, and emphasis while ensuring the
translation reads naturally to native speakers."
        :context (list "translate")
        :callback #'my/gptel--callback-display-bottom)))

    (defun my/gptel-summarize (text)
    "Translate TEXT into English using LLM.
If region is active, use it as TEXT; otherwise prompt for input.
Display the result in a side window with the content selected."
    (interactive "sSummarize text: ")
    (let ((gptel-backend gptel--openrouter)
          (gptel-model 'openai/gpt-4.1-mini))
      (gptel-request text
        :system "Summarize the given text."
        :context (list "summary")
        :callback #'my/gptel--callback-display-bottom))))

(use-package gptel-quick
  :vc (gptel-quick :url "https://github.com/karthink/gptel-quick.git")
  :bind (:map embark-general-map ("?" . gptel-quick))
  :config
  (setq gptel-quick-backend gptel--openrouter
        gptel-quick-model 'openai/gpt-4.1-mini))

(provide 'init-gpt)
