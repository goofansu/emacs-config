(defvar my-bibliography-file (expand-file-name "reference.bib" my-sync-directory))

(use-package elfeed
  :pin melpa
  :init
  (setq elfeed-db-directory (expand-file-name ".elfeed/" my-sync-directory))
  (setq elfeed-feeds
        '(("https://addyo.substack.com/feed" newsletter ai webdev)
          ("https://addyosmani.com/rss.xml" webdev)
          ("https://ampcode.com/news.rss" news agent)
          ("https://andrealeopardi.com/feed.xml" elixir)
          "https://antirez.com/rss"
          ("https://blog.appsignal.com/category/elixir-feed.xml" elixir)
          ("https://blog.appsignal.com/category/ruby-feed.xml" ruby)
          ("https://blog.cloud-mes.com/atom.xml" friend)
          ("https://dashbit.co/feed" elixir)
          ("https://dev.37signals.com/feed/posts.xml" rails)
          ("https://developer.chrome.com/static/blog/feed.xml" webdev)
          ("https://developer.mozilla.org/en-US/blog/rss.xml" webdev)
          ("https://ferd.ca/feed.rss" erlang)
          ("https://fly.io/phoenix-files/feed.xml" elixir)
          ("https://fly.io/ruby-dispatch/feed.xml" ruby)
          ("https://huyenchip.com/feed.xml" ai)
          ("https://irreal.org/blog/?feed=rss2" emacs)
          ("https://karthinks.com/tags/emacs/index.xml" emacs)
          ("https://lucumr.pocoo.org/feed.atom" python ai)
          "https://maggieappleton.com/rss.xml"
          "https://martinfowler.com/feed.atom"
          ("https://mitchellh.com/feed.xml" ghostty)
          ("https://overreacted.io/atom.xml" webdev)
          ("https://paolino.me/feed.xml" ruby ai)
          ("https://protesilaos.com/codelog.xml" emacs)
          ("https://railsatscale.com/feed.xml" rails)
          ("https://sachachua.com/blog/category/emacs-news/feed/" newsletter emacs)
          ("https://simonwillison.net/atom/entries/" python ai)
          "https://sive.rs/en.atom"
          "https://vickiboykis.com/index.xml"
          ("https://webkit.org/feed/atom/" webdev)
          ("https://words.filippo.io/rss/" security)
          "https://world.hey.com/dhh/feed.atom"
          "https://world.hey.com/jason/feed.atom"
          "https://world.hey.com/jorge/feed.atom"
          ("https://world.hey.com/this.week.in.rails/feed.atom" rails)
          ("https://www.joshwcomeau.com/rss.xml" webdev)
          ("https://www.masteringemacs.org/feed" emacs)
          ("https://www.mikeperham.com/index.xml" sidekiq)
          ("https://www.mitchellhanberg.com/feed.xml" elixir)
          "https://www.ryansinger.co/rss/"
          "https://www.seangoedecke.com/rss.xml"
          ("https://xenodium.com/feed" emacs)
          ("https://yiming.dev/rss.xml" friend)
          ))

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
      (cl-case major-mode
        (elfeed-search-mode
         (my/elfeed-browse-with-eww #'elfeed-search-browse-url))
        (elfeed-show-mode
         (my/elfeed-browse-with-eww #'elfeed-show-visit)))))

  (defun my/elfeed-browse-with-eww (browse-function)
    "Browse using EWW and enable readable mode."
    (funcall browse-function)
    (add-hook 'eww-after-render-hook #'eww-readable nil t))

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

(use-package citar
  :pin melpa
  :bind ("C-c E" . citar-open)
  :init
  (setq org-cite-global-bibliography `(,my-bibliography-file))
  (setq org-cite-insert-processor 'citar)
  (setq org-cite-follow-processor 'citar)
  (setq org-cite-activate-processor 'citar)
  :custom
  (citar-bibliography org-cite-global-bibliography)
  (citar-at-point-function #'embark-act)
  (citar-open-entry-function #'citar-open-entry-in-zotero)
  :config
  (add-to-list 'citar-file-open-functions '("pdf" . citar-file-open-external))
  (add-to-list 'citar-file-open-functions '("epub" . citar-file-open-external)))

(use-package citar-embark
  :pin melpa
  :after (citar embark)
  :config
  (citar-embark-mode 1))

(use-package nov
  :pin melpa
  :mode ("\\.epub\\'" . nov-mode))

(provide 'init-reading)
