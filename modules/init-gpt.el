(use-package gptel
  :pin melpa
  :init
  (defvar gptel--openai nil
    "Override the variable to hide OpenAI models")

  :bind
  (("C-c <return>" . gptel-send)
   ("C-c C-<return>" . gptel-menu)
   :map search-map
   ("P" . my/gptel-fabric-pattern)
   ("S" . my/gptel-summarize)
   ("T" . my/gptel-translate))

  :custom
  (gptel-include-reasoning nil)

  :config
  (require 'seq)
  (require 'subr-x)

  (setf (alist-get 'markdown-mode gptel-prompt-prefix-alist) "**User**\n")
  (setf (alist-get 'markdown-mode gptel-response-prefix-alist) "**Assistant**\n")

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

  (defconst my/fabric-patterns-directory
    (expand-file-name "~/.config/fabric/patterns/")
    "Directory containing Fabric patterns.")

  (defun my/fabric-pattern-names ()
    "Return Fabric pattern names that contain a system prompt."
    (seq-filter
     (lambda (name)
       (let ((pattern-dir (expand-file-name name my/fabric-patterns-directory)))
         (and (file-directory-p pattern-dir)
              (file-exists-p (expand-file-name "system.md" pattern-dir)))))
     (directory-files my/fabric-patterns-directory nil "^[^.]")))

  (defun my/fabric-read-pattern-prompt (pattern)
    "Return the system prompt for Fabric PATTERN."
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "system.md"
                         (expand-file-name pattern my/fabric-patterns-directory)))
      (string-trim (buffer-string))))

  (defvar my/gptel-default-user-prompt
    "Please process the provided context according to the system instructions."
    "Default user prompt for `my/gptel-send-current-buffer'.")

  (defun my/gptel-send-current-buffer (buffer-name system-prompt user-prompt &optional temperature)
    "Send current buffer context to BUFFER-NAME with SYSTEM-PROMPT.
Use USER-PROMPT as the prompt text, or `my/gptel-default-user-prompt' when nil.
Set `gptel-temperature' buffer-locally to TEMPERATURE, or 0.7 when omitted."
    (let ((source-buffer (current-buffer)))
      (with-current-buffer (gptel buffer-name nil nil t)
        (setq-local gptel-context (list source-buffer))
        (setq-local gptel-use-context 'system)
        (setq-local gptel-system-prompt system-prompt)
        (setq-local gptel-temperature (or temperature 0.7))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (gptel-prompt-prefix-string)
                  (or user-prompt my/gptel-default-user-prompt)))
        (gptel-send))))

  (defun my/gptel-fabric-pattern (pattern)
    "Send the current buffer using a selected Fabric PATTERN."
    (interactive (list (completing-read "Fabric pattern: "
                                        (my/fabric-pattern-names) nil t)))
    (my/gptel-send-current-buffer
     (format "*gptel-%s*" pattern)
     (my/fabric-read-pattern-prompt pattern)
     nil))

  (defun my/gptel-summarize ()
    "Summarize the current buffer using the Fabric summarize pattern."
    (interactive)
    (my/gptel-send-current-buffer
     "*gptel-summarize*"
     (my/fabric-read-pattern-prompt "summarize")
     nil
     0.2))

  (defun my/gptel-translate ()
    "Translate the current buffer using the Fabric translate pattern."
    (interactive)
    (my/gptel-send-current-buffer
     "*gptel-translate*"
     (my/fabric-read-pattern-prompt "translate")
     "Translate into Chinese."
     0.3)))

(provide 'init-gpt)
