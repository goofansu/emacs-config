(defvar my-bibliography-file (expand-file-name "reference.bib" my-sync-directory))

(use-package elfeed
  :pin melpa
  :init
  (setq elfeed-db-directory (expand-file-name ".elfeed/" my-sync-directory))
  :bind
  (("C-c e" . elfeed)
   :map elfeed-search-mode-map
   ("B" . my/elfeed-eww)
   :map elfeed-show-mode-map
   ("B" . my/elfeed-eww))

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

(use-package elfeed-org
  :pin melpa
  :after elfeed
  :config (elfeed-org))

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
