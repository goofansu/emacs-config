(use-package elfeed
  :pin melpa
  :init
  (setq elfeed-db-directory (expand-file-name ".elfeed/" my-sync-directory))
  (setq elfeed-feeds
        '("https://addyosmani.com/rss.xml"
          "https://andrealeopardi.com/feed.xml"
          "https://antirez.com/rss"
          "https://dev.37signals.com/feed/posts.xml"
          "https://ferd.ca/feed.rss"
          "https://herman.bearblog.dev/feed/"
          "https://huyenchip.com/feed.xml"
          "https://irreal.org/blog/?feed=rss2"
          "https://jvns.ca/atom.xml"
          "https://karpathy.bearblog.dev/feed/"
          "https://karthinks.com/tags/emacs/index.xml"
          "https://lucumr.pocoo.org/feed.atom"
          "https://martinfowler.com/feed.atom"
          "https://mitchellh.com/feed.xml"
          "https://overreacted.io/atom.xml"
          "https://paolino.me/feed.xml"
          "https://protesilaos.com/codelog.xml"
          "https://railsatscale.com/feed.xml"
          "https://simonwillison.net/atom/everything/"
          "https://sive.rs/en.atom"
          "https://world.hey.com/bb/feed.atom"
          "https://world.hey.com/dhh/feed.atom"
          "https://world.hey.com/jason/feed.atom"
          "https://world.hey.com/jorge/feed.atom"
          "https://www.mikeperham.com/index.xml"
          "https://www.mitchellhanberg.com/feed.xml"
          "https://www.ryansinger.co/rss/"))

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
