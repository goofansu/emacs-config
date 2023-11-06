(use-package chatgpt-shell
  :pin melpa
  :config
  (message "chatgpt-shell is loaded")
  :custom
  (chatgpt-shell-welcome-function nil)
  (chatgpt-shell-model-version (cl-position "gpt-4" chatgpt-shell-model-versions :test 'string=))
  (chatgpt-shell-openai-key (lambda () (auth-source-pick-first-password :host "api.openai.com")))
  :bind (("C-c z z" . chatgpt-shell)
         ("C-c z b" . chatgpt-shell-prompt)
         ("C-c z c" . chatgpt-shell-prompt-compose)
         ("C-c z s" . chatgpt-shell-send-region)
         ("C-c z S" . chatgpt-shell-send-and-review-region)
         ("C-c z e" . chatgpt-shell-explain-code)
         ("C-c z r" . chatgpt-shell-refactor-code)))

(push '("*chatgpt*"
        (display-buffer-in-direction)
        (direction . below)
        (window-height . 0.5))
      display-buffer-alist)

(provide 'init-chatgpt)