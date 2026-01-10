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
      :models '((anthropic/claude-opus-4.5 :input-cost 5 :output-cost 25)
                (anthropic/claude-sonnet-4.5 :input-cost 3 :output-cost 15)
                (anthropic/claude-haiku-4.5 :input-cost 1 :output-cost 5))))

  :bind
  (("C-c <return>" . gptel-send)
   ("C-c C-<return>" . gptel-menu)
   :map search-map
   ("T" . my/gptel-translate)
   :map embark-region-map
   ("g t" . my/gptel-translate))

  :config
  (setq gptel-model 'anthropic/claude-haiku-4.5
        gptel-backend gptel--openrouter)

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
    (let ((gptel-include-reasoning nil))
      (gptel-request text
                     :system "Translate the provided text between English and
Chinese (Mandarin). Return ONLY the completed translation without
explanations, notes, or commentary. Maintain all original formatting
including paragraphs, bullet points, and emphasis while ensuring the
translation reads naturally to native speakers."
                     :context (list "translate")
                     :callback #'my/gptel--callback-display-bottom))))

(use-package gptel-quick
  :vc (gptel-quick :url "https://github.com/karthink/gptel-quick.git" :branch "master")
  :bind (:map embark-general-map ("?" . gptel-quick))
  :custom
  (gptel-quick-display nil))

(provide 'init-gpt)
