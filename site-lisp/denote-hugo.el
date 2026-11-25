;;; denote-hugo.el --- Publish Denote notes -*- lexical-binding: t; -*-

(require 'denote)
(require 'ox-hugo)

(defun denote-hugo-files ()
  "Return a list of Hugo-compatible files in `denote-directory'."
  (let ((default-directory (denote-directory)))
    (process-lines "rg" "-i" "-l" "^#\\+hugo_base_dir" "--glob" "*.org")))

(defun denote-hugo-find-file ()
  "Search Hugo-compatible files in `denote-directory' and visit the result."
  (interactive)
  (let* ((default-directory (denote-directory))
         (selected-file (consult--read
                         (denote-hugo-files)
                         :prompt (format "Select FILE in %s: "  default-directory)
                         :sort nil
                         :require-match t
                         :category 'file
                         :state (consult--file-preview)
                         :history 'denote-file-history)))
    (find-file selected-file)))

(defun denote-hugo--astro-site (info)
  "Return the Astro project directory for this export, if any."
  (when-let* ((base (plist-get info :hugo-base-dir))
              (dir (expand-file-name base)))
    (when (file-exists-p (expand-file-name "astro.config.mjs" dir)) dir)))

(defun denote-hugo--shiki-fence (original block contents info)
  "Translate ox-hugo's generated line ranges on the opening fence only."
  (let ((fence (funcall original block contents info)))
    (if (and (denote-hugo--astro-site info)
             (string-match
              (rx string-start (group (+ "`") (+ (not (any " \n")))) " { "
                  (group (*? (not (any "\n")))) "hl_lines=["
                  (group (+ (any digit "," "\"" "-" " "))) "]"
                  (group (* (not (any "\n")))) " }" line-end)
              fence))
        (let* ((lang (match-string 1 fence))
               (raw-ranges (match-string 3 fence))
               (attrs (list (match-string 2 fence) (match-string 4 fence)))
               (body (substring fence (match-end 0)))
               (ranges (replace-regexp-in-string "[\" ]" "" raw-ranges))
               (other (delq nil (mapcar (lambda (part)
                                          (org-string-nw-p (string-trim part "[ ,]+" "[ ,]+")))
                                        attrs))))
          (concat lang " {" ranges "}" (when other (concat " " (string-join other ", "))) body))
      fence)))

(defun denote-hugo--astro-route (info)
  "Return the website route for this file-based ox-hugo export."
  (let* ((name (downcase (file-name-base (org-export-output-file-name ".md"))))
         (zh (string-suffix-p ".zh" name))
         (slug (string-remove-suffix ".zh" name))
         (parts (split-string
                 (string-join (delq nil (list (org-hugo--get-section-path info)
                                              (plist-get info :hugo-bundle)
                                              (unless (equal slug "_index") slug))) "/")
                 "/" t)))
    (concat (if zh "/zh/" "/")
            (mapconcat (lambda (part) (concat (url-hexify-string (downcase part)) "/")) parts ""))))

(defun denote-hugo--note-link (link description site)
  "Resolve LINK using the same file name and section as ox-hugo."
  (pcase-let* ((`(,file ,id ,search)
                (denote-link--ol-resolve-link-to-target link :full-data))
               (label (or description (concat "denote:" id))))
    (if (not (and file (string-equal (file-name-extension file) "org"))) label
      (with-temp-buffer
        (insert-file-contents file)
        (setq buffer-file-name file
              default-directory (file-name-directory file))
        (delay-mode-hooks (org-mode))
        (let ((info (org-export-get-environment 'hugo)))
          (cond
           ;; Other Hugo sites retain the original relref link format.
           ((null site)
            (format "[%s]({{< relref \"%s\" >}})" label
                    (or (cadr (assoc "EXPORT_FILE_NAME" (org-collect-keywords '("EXPORT_FILE_NAME"))))
                        (file-name-nondirectory file))))
           ((and (plist-get info :hugo-base-dir)
                 (file-equal-p site (expand-file-name (plist-get info :hugo-base-dir))))
            (let* ((route (denote-hugo--astro-route info))
                   (anchor (cond ((null search) "")
                                 ((string-prefix-p "#" search) search)
                                 ((org-link-search search) (org-hugo--get-anchor-at-point info)))))
              (format "[%s](%s%s)" label route (or anchor ""))))
           ;; A personal note without a page on this site has no public URL.
           (t label)))))))

(defun denote-hugo--figure (src link info)
  "Render a standalone Org image using its exported attachment URL SRC."
  (let* ((parent (org-export-get-parent-element link))
         (attrs (org-export-read-attribute :attr_html parent))
         (caption (or (org-export-data-with-backend (org-export-get-caption parent) 'html info)
                      (plist-get attrs :caption)))
         (image (format "<img %s>" (org-html--make-attribute-string
                                    (list :src src :alt (or (plist-get attrs :alt) "")
                                          :width (plist-get attrs :width)
                                          :height (plist-get attrs :height)
                                          :title (plist-get attrs :title))))))
    (format "<figure%s>%s%s</figure>"
            (if-let* ((class (plist-get attrs :class)))
                (concat " " (org-html--make-attribute-string (list :class class))) "")
            (if-let* ((href (plist-get attrs :link)))
                (format "<a %s>%s</a>" (org-html--make-attribute-string
                                         (list :href href :target (plist-get attrs :target)
                                               :rel (plist-get attrs :rel))) image)
              image)
            (if (org-string-nw-p caption) (concat "<figcaption>" caption "</figcaption>") ""))))

(defun denote-hugo--astro-links (original link description info)
  "Emit site URLs for Denote links and HTML for standalone Astro images."
  (let ((site (denote-hugo--astro-site info)))
    (if (equal (org-element-property :type link) "denote")
        (denote-hugo--note-link (org-element-property :path link) description site)
      (let ((result (funcall original link description info)))
        ;; ox-hugo already copied the attachment and computed its public URL.
        (if (and site (stringp result)
                 (string-match (rx string-start "{{< figure src=\"" (group (+ (not (any "\"")))) "\"")
                               result))
            (denote-hugo--figure (match-string 1 result) link info)
          result)))))

(advice-add 'org-blackfriday-src-block :around #'denote-hugo--shiki-fence)
(advice-add 'org-hugo-link :around #'denote-hugo--astro-links)

(provide 'denote-hugo)
