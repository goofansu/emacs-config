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
      :models '((anthropic/claude-sonnet-4
                 :description "High-performance model with exceptional reasoning and efficiency"
                 :capabilities (media tool-use cache)
                 :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
                 :context-window 200
                 :input-cost 3
                 :output-cost 15
                 :cutoff-date "2025-03")
                (openai/gpt-4.1
                 :description "Flagship model for complex tasks"
                 :capabilities (media tool-use json url)
                 :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
                 :context-window 1024
                 :input-cost 2.0
                 :output-cost 8.0
                 :cutoff-date "2024-05")
                (openai/gpt-4.1-mini
                 :description "Balance between intelligence, speed and cost"
                 :capabilities (media tool-use json url)
                 :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
                 :context-window 1024
                 :input-cost 0.4
                 :output-cost 1.6
                 :cutoff-date "2024-05")
                (openai/gpt-4.1-nano
                 :description "Fastest, most cost-effective GPT-4.1 model"
                 :capabilities (media tool-use json url)
                 :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
                 :context-window 1024
                 :input-cost 0.10
                 :output-cost 0.40
                 :cutoff-date "2024-05")
                (openai/o4-mini
                 :description "Fast, effective reasoning with efficient performance in coding and visual tasks"
                 :capabilities (reasoning media tool-use json url)
                 :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
                 :context-window 200
                 :input-cost 1.10
                 :output-cost 4.40
                 :cutoff-date "2024-05"))))

  :bind
  (("C-c <return>" . gptel-send)
   ("C-c C-<return>" . gptel-menu)
   ("s-N" . my/gptel-chat)
   :map search-map
   ("T" . my/gptel-translate)
   :map embark-region-map
   ("g t" . my/gptel-translate))

  :custom
  (gptel-default-mode 'org-mode)

  :config
  (setq gptel-backend gptel--openrouter
        gptel-model 'openai/gpt-4.1)

  (gptel-make-gemini "Google"
    :key (lambda () (auth-source-pass-get 'secret "api-key/gemini"))
    :stream t
    :models
    (cl-remove-if-not
     (lambda (model)
       (string-prefix-p "gemini-2.5" (symbol-name (car model))))
     gptel--gemini-models))

  (defun my/gptel-chat ()
    (interactive)
    (let* ((name (generate-new-buffer-name "*gptel-chat*"))
           (buffer (gptel name)))
      (pop-to-buffer buffer)))

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
    (let ((gptel-backend gptel--openrouter)
          (gptel-model 'openai/gpt-4.1))
      (gptel-request text
        :system "Translate the provided text between English and
Chinese (Mandarin). Return ONLY the completed translation without
explanations, notes, or commentary. Maintain all original formatting
including paragraphs, bullet points, and emphasis while ensuring the
translation reads naturally to native speakers."
        :context (list "translate")
        :callback #'my/gptel--callback-display-bottom)))

  (defun my/gptel-read-documentation (symbol)
    "Read the documentation for SYMBOL, which can be a function or variable."
    (let ((sym (intern symbol)))
      (cond
       ((fboundp sym)
        (documentation sym))
       ((boundp sym)
        (documentation-property sym 'variable-documentation))
       (t
        (format "No documentation found for %s" symbol)))))

  (defun my/brave-search-query (query)
    "Perform a web search using the Brave Search API with the given QUERY."
    (let ((url-request-method "GET")
          (url-request-extra-headers `(("X-Subscription-Token" . ,(auth-source-pass-get 'secret "api-key/brave-search"))))
          (url (format "https://api.search.brave.com/res/v1/web/search?q=%s" (url-encode-url query))))
      (with-current-buffer (url-retrieve-synchronously url)
        (goto-char (point-min))
        (when (re-search-forward "^$" nil 'move)
          (let ((json-object-type 'hash-table)) ; Use hash-table for JSON parsing
            (json-parse-string (buffer-substring-no-properties (point) (point-max))))))))

  (setq gptel-tools
        (list (gptel-make-tool
               :name "read_documentation"
               :function #'my/gptel-read-documentation
               :description "Read the documentation for a given function or variable"
               :args (list '( :name "name"
                              :type string
                              :description "The name of the function or variable whose documentation is to be retrieved"))
               :category "emacs")

              (gptel-make-tool
               :function (lambda (buffer)
                           (unless (buffer-live-p (get-buffer buffer))
                             (error "Error: buffer %s is not live." buffer))
                           (with-current-buffer buffer
                             (buffer-substring-no-properties (point-min) (point-max))))
               :name "read_buffer"
               :description "Return the contents of an Emacs buffer"
               :args (list '( :name "buffer"
                              :type string
                              :description "The name of the buffer whose contents are to be retrieved"))
               :category "emacs")

              (gptel-make-tool
               :function (lambda (command)
                           (with-temp-message (format "Running command: %s" command)
                             (shell-command-to-string command)))
               :name "run_command"
               :description "Run a command."
               :args (list '( :name "command"
                              :type string
                              :description "Command to run."))
               :confirm t
               :category "command")

              (gptel-make-tool
               :function (lambda (filepath)
                           (with-temp-buffer
                             (insert-file-contents (expand-file-name filepath))
                             (buffer-string)))
               :name "read_file"
               :description "Read and display the contents of a file"
               :args (list '( :name "filepath"
                              :type string
                              :description "Path to the file to read. Supports relative paths and ~."))
               :category "filesystem")

              (gptel-make-tool
               :function (lambda (url)
                           (with-current-buffer (url-retrieve-synchronously url)
                             (goto-char (point-min))
                             (forward-paragraph)
                             (let ((dom (libxml-parse-html-region (point) (point-max))))
                               (run-at-time 0 nil #'kill-buffer (current-buffer))
                               (with-temp-buffer
                                 (shr-insert-document dom)
                                 (buffer-substring-no-properties (point-min) (point-max))))))
               :name "read_url"
               :description "Fetch and read the contents of a URL"
               :args (list '( :name "url"
                              :type string
                              :description "The URL to read"))
               :confirm t
               :category "web")

              (gptel-make-tool
               :function #'my/brave-search-query
               :name "brave_search"
               :description "Perform a web search using the Brave Search API"
               :args (list '( :name "query"
                              :type string
                              :description "The search query string"))
               :category "web"))))

(use-package gptel-quick
  :vc (gptel-quick :url "https://github.com/karthink/gptel-quick.git")
  :bind (:map embark-general-map ("?" . gptel-quick))
  :config
  (setq gptel-quick-backend gptel--openrouter
        gptel-quick-model 'openai/gpt-4.1))

(use-package mcp-hub
  :ensure nil
  :load-path "vendor/"
  :after gptel
  :custom
  (mcp-hub-servers
   `(("time" . (:command "uvx" :args ("mcp-server-time" "--local-timezone" "Asia/Shanghai")))
     )))

(use-package gptel-mcp-tools
  :ensure nil
  :load-path "site-lisp/"
  :after (gptel mcp-hub)
  :config
  (mcp-hub-start-all-server #'gptel-mcp-tools-register)
  (keymap-set my-toggle-map "t" #'gptel-mcp-tools-mode))

(provide 'init-gpt)
