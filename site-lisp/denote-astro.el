;;; denote-astro.el --- Export Denote notes to Astro via ox-hugo -*- lexical-binding: t; -*-

(require 'denote)
(require 'ox-hugo)

(defconst denote-astro--heading-helper
  (expand-file-name "denote-astro-headings.mjs"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defvar denote-astro--collecting-headings nil)

(defun denote-astro-export ()
  "Export the current Denote Org file to its configured website.
Export the whole note, without evaluating Babel blocks.  This writes
local Markdown and attachments; it does not deploy the website."
  (interactive)
  (unless (and (derived-mode-p 'org-mode) buffer-file-name)
    (user-error "Visit a Denote Org note first"))
  (unless (plist-get (org-export-get-environment 'hugo) :hugo-base-dir)
    (user-error "Add #+hugo_base_dir (the post or page Tempel template) first"))
  (save-buffer)
  (let ((org-export-use-babel nil))
    (save-restriction
      (widen)
      (deactivate-mark)
      (message "Exported %s" (org-hugo-export-to-md)))))

(defun denote-astro-files ()
  "Return notes configured for website export in `denote-directory'."
  (let ((default-directory (denote-directory)))
    (process-lines "rg" "-i" "-l" "^#\\+hugo_base_dir" "--glob" "*.org")))

(defun denote-astro-find-file ()
  "Find a note configured for website export in `denote-directory'."
  (interactive)
  (let* ((default-directory (denote-directory))
         (selected-file (consult--read
                         (denote-astro-files)
                         :prompt (format "Select FILE in %s: "  default-directory)
                         :sort nil
                         :require-match t
                         :category 'file
                         :state (consult--file-preview)
                         :history 'denote-file-history)))
    (find-file selected-file)))

(defun denote-astro--site (info)
  "Return the Astro project directory for this export, if any."
  (when-let* ((base (plist-get info :hugo-base-dir))
              (dir (expand-file-name base)))
    (when (file-exists-p (expand-file-name "astro.config.mjs" dir)) dir)))

(defun denote-astro--export (original &rest args)
  "Disable Babel evaluation for Astro exports via ORIGINAL ARGS."
  (let ((org-export-use-babel
         (unless (denote-astro--site (org-export-get-environment 'hugo))
           org-export-use-babel)))
    (apply original args)))

(defun denote-astro--front-matter (original info)
  "Use YAML and opt-in automatic update dates for Astro in ORIGINAL INFO."
  (if (not (denote-astro--site info)) (funcall original info)
    (let ((info (copy-sequence info)))
      (plist-put info :hugo-front-matter-format "yaml")
      ;; Astro update dates come only from an explicit automatic-date opt-in.
      (plist-put info :hugo-lastmod nil)
      (plist-put info :logbook-lastmod nil)
      ;; Require the note's explicit opt-in, even if the global option is t.
      (plist-put info :hugo-auto-set-lastmod
                 (equal "t" (downcase
                             (or (cadr (assoc "HUGO_AUTO_SET_LASTMOD"
                                              (org-collect-keywords '("HUGO_AUTO_SET_LASTMOD"))))
                                 ""))))
      (funcall original info))))

(defun denote-astro--compact-front-matter (text backend info)
  "Remove ox-hugo's extra blank line after Astro YAML front matter."
  (if (and (org-export-derived-backend-p backend 'hugo)
           (denote-astro--site info)
           (string-match "\\`---\n\\(?:.*\n\\)*?---\n\\(\n\\)" text))
      (replace-match "" t t text 1)
    text))

(defun denote-astro--front-matter-data (original data info)
  "Adapt ORIGINAL front-matter DATA to the Astro collection using INFO."
  (let ((data (funcall original data info)))
    (if (not (denote-astro--site info)) data
      (let* ((custom (org-hugo--parse-property-arguments
                      (plist-get info :hugo-custom-front-matter)))
             (published (or (alist-get 'published custom)
                            (alist-get 'publishDate data)
                            (alist-get 'date data)))
             (name (or (plist-get info :export-file-name)
                       (cadr (assoc "EXPORT_FILE_NAME"
                                    (org-collect-keywords '("EXPORT_FILE_NAME"))))
                       (file-name-base (or buffer-file-name ""))))
             (lang (or (alist-get 'lang custom)
                       (cadr (assoc "LANGUAGE"
                                    (org-collect-keywords '("LANGUAGE"))))))
             (result (list (assq 'title data))))
        (dolist (key '(description tags))
          (when-let* ((pair (assq key data))) (push pair result)))
        (when (member lang '("en" "zh")) (push (cons 'lang lang) result))
        (dolist (pair `((published . ,published)
                        (lastmod . ,(and (plist-get info :hugo-auto-set-lastmod)
                                         (alist-get 'lastmod data)))))
          (when (cdr pair)
            (let ((date (format "%s" (cdr pair))))
              (unless (string-match "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" date)
                (user-error "Invalid %s date: %s" (car pair) date))
              (push (cons (car pair) (match-string 0 date)) result))))
        (when (and (string-prefix-p "posts/" (org-hugo--get-section-path info))
                   (not (equal (file-name-base name) "_index"))
                   (not published))
          (user-error "Posts need #+hugo_publishdate or #+date"))
        (dolist (key '(draft toc))
          (when (member (alist-get key data) '(t true "true"))
            (push (cons key "true") result)))
        (nreverse result)))))

(defun denote-astro--heading-ids (info)
  "Return Astro's generated IDs, keyed by Org heading position in INFO.
Ask the site's installed Markdown processor so formatting, Unicode,
punctuation and duplicate headings follow the same rules as the build."
  (or (plist-get info :denote-astro-heading-ids)
      (let ((denote-astro--collecting-headings t)
            (render-info (org-combine-plists
                          info (list :exported-data (make-hash-table :test #'eq))))
            (ids (make-hash-table :test #'eql))
            positions markdown)
        (org-element-map (plist-get info :parse-tree) 'headline
          (lambda (heading)
            (unless (or (org-element-property :footnote-section-p heading)
                        (org-export-low-level-p heading info))
              (push (org-element-property :begin heading) positions)
              (push (org-hugo-heading heading nil render-info) markdown))))
        (with-temp-buffer
          (insert (string-join (nreverse markdown) "\n"))
          (unless (zerop (call-process-region
                          (point-min) (point-max) "node" t t nil
                          denote-astro--heading-helper (denote-astro--site info)))
            (user-error "Cannot get Astro heading IDs (run npm ci in the site): %s"
                        (string-trim (buffer-string))))
          (let ((slugs (json-parse-string (buffer-string) :array-type 'list)))
            (unless (= (length slugs) (length positions))
              (user-error "Astro heading count differs from the Org export"))
            (cl-mapc (lambda (position slug) (puthash position slug ids))
                     (nreverse positions) slugs)))
        (plist-put info :denote-astro-heading-ids ids)
        ids)))

(defun denote-astro--anchor (original element info)
  "Use CUSTOM_ID or Astro's generated heading ID for ELEMENT in INFO."
  (if (not (denote-astro--site info)) (funcall original element info)
    (or (org-element-property :CUSTOM_ID element)
        (gethash (org-element-property :begin element) (denote-astro--heading-ids info))
        (user-error "No exported Astro heading at %s" (org-element-property :begin element)))))

(defun denote-astro--heading (original heading contents info)
  "Emit ordinary Markdown headings, with a target only for CUSTOM_ID."
  (if (not (denote-astro--site info))
      (funcall original heading contents info)
    (let* ((org-hugo-headline-anchor nil)
           (info (org-combine-plists info '(:md-headline-style atx)))
           (text (funcall original heading contents info))
           (custom-id (org-element-property :CUSTOM_ID heading)))
      (when (and text (not (org-export-low-level-p heading info)))
        ;; Org already separates elements; ox-hugo adds another newline.
        (setq text (string-remove-prefix "\n" text))
        (let ((target
               (when (and custom-id (not denote-astro--collecting-headings)
                          (not (equal custom-id
                                      (gethash (org-element-property :begin heading)
                                               (denote-astro--heading-ids info)))))
                 (concat "<span " (org-html--make-attribute-string (list :id custom-id))
                         "></span>"))))
          ;; Trim ox-hugo's space reserved for its {#anchor} attribute.
          (when (string-match "^#+ .*" text)
            (let ((end (match-end 0)))
              (setq text (concat (string-trim-right (substring text 0 end)) target
                                 (substring text end)))))))
      text)))

(defun denote-astro--literal-shortcodes (original code lang)
  "Keep CODE examples literal for Astro; let ORIGINAL escape Hugo sites."
  (if (denote-astro--site (org-export-get-environment 'hugo)) code
    (funcall original code lang)))

(defun denote-astro--shiki-fence (original block contents info)
  "Translate ox-hugo's generated line ranges on the opening fence only."
  (let ((fence (funcall original block contents info)))
    (if (and (denote-astro--site info)
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

(defun denote-astro--route (info)
  "Return the website route for this file-based ox-hugo export."
  (let* ((slug (downcase (file-name-base
                         (org-export-output-file-name ".md"))))
         (parts (split-string
                 (string-join (delq nil (list (org-hugo--get-section-path info)
                                              (plist-get info :hugo-bundle)
                                              (unless (equal slug "_index") slug))) "/")
                 "/" t)))
    (concat "/"
            (mapconcat (lambda (part) (concat (url-hexify-string (downcase part)) "/")) parts ""))))

(defun denote-astro--search-anchor (search)
  "Resolve heading SEARCH in a copy prepared with Org's export rules."
  (org-export-with-buffer-copy
   (let* ((org-export-use-babel nil)
          (backend (org-export-get-backend 'hugo))
          (info (org-export--annotate-info
                 backend
                 (org-combine-plists
                  (org-export--get-export-attributes backend nil nil)
                  (org-export--get-buffer-attributes)))))
     (org-link-search search)
     (org-hugo--get-anchor-at-point info))))

(defun denote-astro--note-link (link description site)
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
            (let* ((route (denote-astro--route info))
                   (anchor (cond ((null search) "")
                                 ((string-prefix-p "#" search) search)
                                 (t (denote-astro--search-anchor search)))))
              (format "[%s](%s%s)" label route (or anchor ""))))
           ;; A personal note without a page on this site has no public URL.
           (t label)))))))

(defun denote-astro--figure (src link info)
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

(defun denote-astro--links (original link description info)
  "Emit site URLs for Denote links and HTML for standalone Astro images."
  (let ((site (denote-astro--site info)))
    (cond
     (denote-astro--collecting-headings
      (or description (org-element-property :raw-link link)))
     ((equal (org-element-property :type link) "denote")
      (denote-astro--note-link (org-element-property :path link) description site))
     (t
      (let ((result (funcall original link description info)))
        ;; ox-hugo already copied the attachment and computed its public URL.
        (if (and site (stringp result)
                 (string-match (rx string-start "{{< figure src=\"" (group (+ (not (any "\"")))) "\"")
                               result))
            (denote-astro--figure (match-string 1 result) link info)
          result))))))

(advice-add 'org-blackfriday-src-block :around #'denote-astro--shiki-fence)
(advice-add 'org-hugo--escape-hugo-shortcode :around #'denote-astro--literal-shortcodes)
(advice-add 'org-hugo-link :around #'denote-astro--links)
(advice-add 'org-hugo-export-to-md :around #'denote-astro--export)
(advice-add 'org-hugo--get-front-matter :around #'denote-astro--front-matter)
(advice-add 'org-hugo--replace-keys-maybe :around #'denote-astro--front-matter-data)
(advice-add 'org-hugo--get-anchor :around #'denote-astro--anchor)
(advice-add 'org-hugo-heading :around #'denote-astro--heading)
(add-hook 'org-export-filter-final-output-functions #'denote-astro--compact-front-matter)

(provide 'denote-astro)
;;; denote-astro.el ends here
