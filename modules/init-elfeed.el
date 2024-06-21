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

  (defun +elfeed--selected-entry ()
    (pcase major-mode
      ('elfeed-search-mode
       (elfeed-search-selected :single))
      ('elfeed-show-mode
       elfeed-show-entry)))

  (defun +elfeed/delete (entry)
    (interactive (list (+elfeed--selected-entry)))
    (elfeed-untag entry 'inbox)
    (if (eq major-mode 'elfeed-search-mode)
        (elfeed-search-update--force)
      (elfeed-show-refresh)))

  (defun +elfeed/send-to-wombag (entry)
    (interactive (list (+elfeed--selected-entry)))
    (+wombag/url (elfeed-entry-link entry))
    (elfeed-tag entry 'sent)
    (if (eq major-mode 'elfeed-search-mode)
        (elfeed-search-update--force)
      (elfeed-show-refresh)))

  (defun +elfeed/summarize (entry)
    (interactive (list (+elfeed--selected-entry)))
    (+gptel/kagi-summarize-url (elfeed-entry-link entry))
    (elfeed-tag entry 'summarized)
    (if (eq major-mode 'elfeed-search-mode)
        (elfeed-search-update--force)
      (elfeed-show-refresh)))

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

  (defun +elfeed/set-filter ()
    (interactive)
    (let ((categories
           '(("all" . "@6-months-ago")
             ("sent" . "@6-months-ago +sent")
             ("inbox" . "@6-months-ago +inbox")
             ("trash" . "@6-months-ago -inbox")
             ("unread" . "@6-months-ago +inbox +unread")
             ("read" . "@6-months-ago +inbox -unread")
             ("summarized" . "@6-months-ago +summarized")
             ("news" . "@6-months-ago +inbox +unread +news")
             ("videos" . "@6-months-ago +inbox +unread +video")
             ("engineering" . "@6-months-ago +inbox +unread +engineering")
             ("web" . "@6-months-ago +inbox +unread +web"))))
      (if-let* ((category (completing-read "Select category: " categories))
                (filter (cdr (assoc category categories))))
          (elfeed-search-set-filter filter)
        (elfeed-search-set-filter category))))

  (when (featurep 'evil)
    (evil-define-key 'normal elfeed-search-mode-map
      "=" #'+elfeed/summarize
      "B" #'+elfeed/browse
      "D" #'+elfeed/delete
      "R" #'+elfeed/send-to-wombag
      "S" #'+elfeed/set-filter
      "W" #'+elfeed/switch-to-wombag)

    (evil-define-key 'normal elfeed-show-mode-map
      "=" #'+elfeed/summarize
      "B" #'+elfeed/browse
      "D" #'+elfeed/delete
      "R" #'+elfeed/send-to-wombag))

  :custom
  (elfeed-initial-tags '(unread inbox))
  (elfeed-search-remain-on-entry t)
  (elfeed-search-filter "@6-months-ago +inbox +unread"))

(use-package elfeed-org
  :pin melpa
  :after elfeed
  :init
  (setq rmh-elfeed-org-files (list "elfeed.org"))
  :config
  (message "elfeed-org is loaded")
  (elfeed-org))

(provide 'init-elfeed)
