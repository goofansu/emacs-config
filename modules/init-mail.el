(setq user-full-name "Yejun Su"
      user-mail-address "goofan.su@gmail.com")

(use-package message
  :ensure nil
  :defer t
  :custom
  (message-sendmail-f-is-evil t)
  (message-sendmail-extra-arguments '("--read-envelope-from"))
  (message-send-mail-function #'message-send-mail-with-sendmail))

(use-package sendmail
  :ensure nil
  :defer t
  :custom
  (sendmail-program (executable-find "msmtp"))
  (send-mail-function #'smtpmail-send-it))

(use-package notmuch
  :pin melpa
  :hook (notmuch-mua-send . notmuch-mua-attachment-check)
  :bind
  (("C-c m" . notmuch)
   ("C-x m" . notmuch-mua-new-mail))
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
     ("count" . "%-10s  ")
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
   `(( :name "home"
       :query "tag:inbox and path:Home/**"
       :count-query "tag:inbox and tag:unread and path:Home/**"
       :sort-order newest-first
       :key ,(kbd "h"))
     ( :name "work"
       :query "tag:inbox and path:Work/**"
       :count-query "tag:inbox and tag:unread and path:Work/**"
       :sort-order newest-first
       :key ,(kbd "w"))
     ( :name "github"
       :query "tag:inbox and from:github.com"
       :count-query "tag:inbox and tag:unread and from:github.com"
       :sort-order 'newest-first
       :key ,(kbd "g"))
     ( :name "jira"
       :query "tag:inbox and from:atlassian.net"
       :count-query "tag:inbox and tag:unread and from:atlassian.net"
       :sort-order 'newest-first
       :key ,(kbd "j"))
     ( :name "bamboohr"
       :query "tag:inbox and from:bamboohr.com"
       :count-query "tag:inbox and tag:unread and from:bamboohr.com"
       :sort-order 'newest-first
       :key ,(kbd "b"))
     ( :name "expensify"
       :query "tag:inbox and from:expensify.com"
       :count-query "tag:inbox and tag:unread and from:expensify.com"
       :sort-order 'newest-first
       :key ,(kbd "e"))
     ))
  ;; Compose
  (notmuch-always-prompt-for-sender nil)
  (notmuch-fcc-dirs "sent +sent -unread")
  ;; Reading
  (notmuch-show-indent-messages-width 0)
  (notmuch-show-part-button-default-action 'notmuch-show-view-part)
  (notmuch-wash-wrap-lines-length 120)
  (notmuch-unthreaded-show-out nil))

(use-package notmuch-indicator
  :init
  (setq notmuch-indicator-notmuch-config-file
        (expand-file-name "~/.config/notmuch/default/config"))
  :custom
  (notmuch-indicator-hide-empty-counters t)
  (notmuch-indicator-add-to-mode-line-misc-info nil)
  (notmuch-indicator-args
   '(( :terms "tag:unread and tag:inbox and path:Home/**"
       :label "H"
       :label-face prot-modeline-indicator-blue
       :counter-face prot-modeline-indicator-blue)
     ( :terms "tag:unread and tag:inbox and path:Work/**"
       :label "W"
       :label-face prot-modeline-indicator-cyan
       :counter-face prot-modeline-indicator-cyan)))
  :config
  (notmuch-indicator-mode 1))

(use-package ol-notmuch
  :pin melpa
  :after notmuch)

(provide 'init-mail)
