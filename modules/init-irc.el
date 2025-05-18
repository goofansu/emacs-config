(use-package circe
  :pin melpa
  :init
  (setq circe-network-defaults
        '(("Libera.Chat"
           :host "irc.libera.chat"
           :port 6697
           :use-tls t
           :nick "goofansu"
           :realname "Yejun Su"
           :sasl-username "goofansu"
           :sasl-password (lambda (&rest _) (auth-source-pass-get 'secret "irc.libera.chat"))
           :channels ("#emacs" "#ruby" "#elixir"))))

  :bind (:map goto-map ("K" . my/circe-jump-to-channel))
  :config
  (circe-set-display-handler "353" 'circe-display-ignore)
  (circe-set-display-handler "366" 'circe-display-ignore)

  (defun my/circe-channel-names ()
    (mapcar #'buffer-name (match-buffers '(major-mode . circe-channel-mode))))

  (defun my/circe-jump-to-channel ()
    (interactive)
    (let* ((channel-buffers (my/circe-channel-names))
           (target (completing-read "Select channel: " channel-buffers nil t)))
      (when target
        (switch-to-buffer target))))

  (defvar consult--source-circe
    `(:name     "Circe Channels"
                :narrow   ?C
                :category buffer
                :face     consult-buffer
                :items    ,#'my/circe-channel-names
                :action   ,#'switch-to-buffer
                :state    ,#'consult--buffer-state
                :hidden   t)
    "Circe buffer source for `consult-buffer'.")
  (add-to-list 'consult-buffer-sources 'consult--source-circe))

(provide 'init-irc)
