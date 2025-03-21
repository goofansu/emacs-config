(use-package elfeed
  :pin melpa
  :bind ("C-c e" . elfeed)
  :custom
  (elfeed-feeds
   '("https://protesilaos.com/codelog.xml"
     "https://karthinks.com/tags/emacs/index.xml")))

(provide 'init-elfeed)
