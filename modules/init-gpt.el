(use-package gptel
  :pin melpa
  :bind
  (("C-c <return>" . gptel-send)
   ("C-c C-<return>" . gptel-menu))

  :custom
  (gptel-include-reasoning nil)

  :config
  (setf (alist-get 'markdown-mode gptel-prompt-prefix-alist) "**User**\n")
  (setf (alist-get 'markdown-mode gptel-response-prefix-alist) "**Assistant**\n")

  (setq gptel-model 'gpt-5.6-luna
        gptel-backend (gptel-make-openai-oauth "OpenAI-sub"))

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

  (add-hook 'gptel-mode-hook #'my/gptel-remap-header-line-button-underline))

(provide 'init-gpt)
