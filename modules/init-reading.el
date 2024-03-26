(use-package elfeed
  :pin melpa
  :defer t
  :init
  (defvar elfeed-browse-url-handlers
    '(("https:\\/\\/www\\.youtu\\.*be." . mpv-browse-url)
      ("." . eww-browse-url)))

  (defun mpv-browse-url (url &rest args)
    (message "Streaming in mpv: %s" url)
    (start-process "mpv" nil "mpv" url))

  :config
  (message "elfeed is loaded")

  (defun +elfeed/post-to-wombag (entries)
    (interactive (list (pcase major-mode
                         ('elfeed-search-mode
                          (elfeed-search-selected))
                         ('elfeed-show-mode
                          (list elfeed-show-entry)))))
    (dolist (entry (ensure-list entries))
      (+wombag/url (elfeed-entry-link entry))))

  (defun +elfeed/switch-to-wombag ()
    (interactive)
    (if-let ((buf (get-buffer "*wallabag-search*")))
        (switch-to-buffer buf)
      (wombag)))

  (defun +elfeed/browse ()
    (interactive)
    (let ((browse-url-handlers elfeed-browse-url-handlers))
      (pcase major-mode
        ('elfeed-search-mode (elfeed-search-browse-url))
        ('elfeed-show-mode (elfeed-show-visit)))))

  (when (featurep 'evil)
    (evil-define-key 'normal elfeed-search-mode-map
      "B" #'+elfeed/browse
      "R" #'+elfeed/post-to-wombag
      "W" #'+elfeed/switch-to-wombag)

    (evil-define-key 'normal elfeed-show-mode-map
      "B" #'+elfeed/browse
      "R" #'+elfeed/post-to-wombag))

  :custom
  (elfeed-search-remain-on-entry t))

(use-package elfeed-org
  :pin melpa
  :after elfeed
  :config
  (message "elfeed-org is loaded")
  (elfeed-org))

(use-package wombag
  :load-path "vendor/wombag"
  :commands (wombag wombag-add-entry)
  :defer t
  :init
  (defsubst +wombag/url (url)
    "Add URL to Wombag."
    (message "Sending to Wombag: %s" url)
    (wombag-add-entry url ""))

  :config
  (message "wombag is loaded")

  (defun +wombag/switch-to-elfeed ()
    (interactive)
    (if-let ((buf (get-buffer "*elfeed-search*")))
        (switch-to-buffer buf)
      (elfeed)))

  (defun +wombag/show-eww-open ()
    "Open Wombag entry in EWW."
    (interactive)
    (when-let ((url (alist-get 'url wombag-show-entry)))
      (eww-browse-url url)))

  (defun +wombag/show-browse-url ()
    "Open Wombag entry in original URL."
    (interactive)
    (when-let ((url (alist-get 'url wombag-show-entry)))
      (browse-url url)))

  (defun +wombag/show-browse-host ()
    "Open Wombag entry on `wombag-host'."
    (interactive)
    (when-let* ((id (alist-get 'id wombag-show-entry))
                (url (format "%s/view/%s" wombag-host id)))
      (browse-url url)))

  (defun +wombag/search-browse-host ()
    "Open Wombag entry on `wombag-host'."
    (interactive)
    (wombag-search--with-entry
     (when-let* ((id (map-elt entry 'id))
                 (url (format "%s/view/%s" wombag-host id)))
       (browse-url url))))

  (when (and
         (featurep 'evil)
         (featurep 'evil-collection))
    (evil-collection-set-readonly-bindings 'wombag-search-mode-map)
    (evil-define-key 'normal wombag-search-mode-map
      (kbd "<return>") #'wombag-search-show-entry
      (kbd "S-<return>") 'wombag-search-browse-url
      "A"  #'wombag-search-archive-entry
      "B"  #'wombag-search-eww-open
      "D"  #'wombag-search-delete-entry
      "E"  #'+wombag/switch-to-elfeed
      "F"  #'wombag-search-starred-entry
      "s"  #'wombag-search-live-filter
      "y"  #'wombag-search-copy
      "go" #'wombag-search-browse-url
      "gO" #'+wombag/search-browse-host
      "gr" #'wombag-search-update--force
      "gR" #'wombag-sync)

    (evil-collection-set-readonly-bindings 'wombag-show-mode-map)
    (evil-define-key 'normal wombag-show-mode-map
      "B"  #'+wombag/show-eww-open
      "go" #'+wombag/show-browse-url
      "gO" #'+wombag/show-browse-host))

  :custom
  (wombag-host "https://app.wallabag.it")
  (wombag-username "goofansu")
  (wombag-password (auth-source-pass-get 'secret "app.wallabag.it"))
  (wombag-client-id "23745_3qjblkrgo0qo4w4cwscg0g88wk4408wckw0gc8oskwg0cgkocw")
  (wombag-client-secret (auth-source-pass-get "client_secret" "app.wallabag.it"))

  :bind (:map embark-url-map
              ("R" . +wombag/url)))

(provide 'init-reading)
