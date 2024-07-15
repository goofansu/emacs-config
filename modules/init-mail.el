(setq user-full-name "Yejun Su"
      user-mail-address "goofan.su@gmail.com")

(use-package message
  :ensure nil
  :defer t
  :config
  (message "message is loaded")
  :custom
  (message-sendmail-f-is-evil t)
  (message-sendmail-extra-arguments '("--read-envelope-from"))
  (message-send-mail-function #'message-send-mail-with-sendmail))

(use-package sendmail
  :ensure nil
  :after message
  :config
  (message "sendmail is loaded")
  :custom
  (sendmail-program (executable-find "msmtp"))
  (send-mail-function #'smtpmail-send-it))

(use-package notmuch
  :pin melpa
  :config
  (message "notmuch is loaded")
  :custom
  ;; General UI
  (notmuch-show-logo nil)
  (notmuch-column-control 1.0)
  (notmuch-hello-auto-refresh t)
  (notmuch-hello-recent-searches-max 20)
  (notmuch-hello-thousands-separator "")
  (notmuch-show-all-tags-list t)
  (notmuch-hello-sections
   '(notmuch-hello-insert-saved-searches
     notmuch-hello-insert-recent-searches
     notmuch-hello-insert-tags-section))
  ;; Search
  (notmuch-search-oldest-first nil)
  (notmuch-search-result-format
   '(("date" . "%12s  ")
     ("count" . "%-7s  ")
     ("authors" . "%-20.20s  ")
     ("subject" . "%-80.80s  ")
     ("tags" . "(%s)")))
  (notmuch-tree-result-format
   '(("date" . "%12s  ")
     ("authors" . "%-20.20s  ")
     ((("tree" . "%s")
       ("subject" . "%s"))
      . " %-80.80s  ")
     ("tags" . "(%s)")))
  (notmuch-saved-searches
   `(( :name "inbox"
       :query "tag:inbox"
       :sort-order newest-first
       :key ,(kbd "i"))
     ( :name "unread (inbox)"
       :query "tag:unread and tag:inbox"
       :sort-order newest-first
       :key ,(kbd "u"))
     ( :name "home"
       :query "to:goofan.su@gmail.com"
       :sort-order newest-first
       :key ,(kbd "h"))
     ( :name "work"
       :query "to:james.su@managebac.com"
       :sort-order newest-first
       :key ,(kbd "w"))
     ))
  ;; Compose
  (notmuch-always-prompt-for-sender t)
  (notmuch-fcc-dirs "sent +sent -unread")
  ;; Reading
  (notmuch-show-indent-messages-width 0)
  (notmuch-show-part-button-default-action 'notmuch-show-view-part)
  (notmuch-wash-wrap-lines-length 120)
  (notmuch-unthreaded-show-out nil)
  :hook (notmuch-mua-send . notmuch-mua-attachment-check)
  :bind (("C-c m" . notmuch)
         ("C-x m" . notmuch-mua-new-mail)))

(use-package ol-notmuch
  :pin melpa
  :after notmuch
  :config
  (message "ol-notmuch is loaded"))

(use-package gnus-alias
  :pin melpa
  :config
  (message "gnus-alias is loaded")
  :custom
  (gnus-alias-default-identity "home")
  (gnus-alias-identity-alist
   '(("home" nil "Yejun Su <goofan.su@gmail.com>" nil nil nil nil)
     ("work" nil "James Su <james.su@managebac.com>" nil nil nil nil)))
  (gnus-alias-identity-rules
   '(("work account" ("any" "managebac.com" both) "work")))
  :hook (message-setup . gnus-alias-determine-identity))

(provide 'init-mail)
