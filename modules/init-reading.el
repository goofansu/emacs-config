(use-package elfeed
  :pin melpa
  :init
  (setq elfeed-feeds
        '("https://sachachua.com/blog/category/emacs-news/feed/"
          "https://world.hey.com/this.week.in.rails/feed.atom"))

  :bind
  (("C-c e" . elfeed)
   :map elfeed-search-mode-map
   ("B" . my/elfeed-eww)
   :map elfeed-show-mode-map
   ("B" . my/elfeed-eww))

  :custom
  (elfeed-initial-tags '(unread inbox))
  (elfeed-search-filter "#50 +unread ")

  :config
  (defun my/elfeed-eww ()
    (interactive)
    (let ((browse-url-browser-function #'eww))
      (pcase major-mode
        ('elfeed-search-mode (elfeed-search-browse-url))
        ('elfeed-show-mode (elfeed-show-visit)))))

  ;; Org export uses Elfeed entry's original link
  ;; https://takeonrules.com/2024/08/11/exporting-org-mode-elfeed-links/
  (with-eval-after-load 'org
    (org-link-set-parameters "elfeed"
                             :follow #'elfeed-link-open
                             :store #'elfeed-link-store-link
                             :export #'elfeed-link-export-link))

  (defun elfeed-link-export-link (link desc format _protocol)
    "Export `org-mode' `elfeed' LINK with DESC for FORMAT."
    (if (string-match "\\([^#]+\\)#\\(.+\\)" link)
        (if-let* ((entry
                   (elfeed-db-get-entry
                    (cons (match-string 1 link)
                          (match-string 2 link))))
                  (url
                   (elfeed-entry-link entry))
                  (title
                   (elfeed-entry-title entry)))
            (pcase format
              ('html (format "<a href=\"%s\">%s</a>" url desc))
              ('md (format "[%s](%s)" desc url))
              ('latex (format "\\href{%s}{%s}" url desc))
              ('texinfo (format "@uref{%s,%s}" url desc))
              (_ (format "%s (%s)" desc url)))
          (format "%s (%s)" desc url))
      (format "%s (%s)" desc link))))

(use-package calibredb
  :pin melpa
  :init
  (setq calibredb-root-dir (expand-file-name "Calibre/" my-sync-directory))
  :bind ("C-c E" . calibredb))

(use-package nov
  :pin melpa
  :mode ("\\.epub\\'" . nov-mode))

(provide 'init-reading)
