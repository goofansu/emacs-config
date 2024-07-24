(use-package wombag
  :vc (wombag :url "https://github.com/karthink/wombag.git")
  :commands (wombag-add-entry)
  :defer t
  :init
  (defsubst +wombag/url (url)
    "Add URL to Wombag."
    (message "Sending to Wombag: %s" url)
    (wombag-add-entry url ""))
  :config
  (message "wombag is loaded")
  :custom
  (wombag-host "https://app.wallabag.it")
  (wombag-username "goofansu")
  (wombag-password (auth-source-pass-get 'secret "app.wallabag.it"))
  (wombag-client-id "25398_849l75lzo6ww004w480w0kscc0gwssww04csgogscs8s0wskg")
  (wombag-client-secret (auth-source-pass-get 'secret "api-key/wallabag"))
  :bind ( :map embark-url-map
          ("R" . +wombag/url)))

(provide 'init-wombag)
