;;; denote-astro-test.el --- Website export integration tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'denote-astro)

(defmacro denote-astro-test--site (&rest body)
  "Run BODY with temporary notes and an Astro site."
  (declare (indent 0))
  `(let* ((root (make-temp-file "denote-astro-test-" t))
          (site (expand-file-name "site" root))
          (denote-directory (expand-file-name "notes" root))
          (org-hugo-front-matter-format "yaml")
          (org-hugo-default-static-subdirectory-for-externals "attachments")
          (org-export-use-babel nil)
          (org-export-with-toc nil)
          (org-id-locations-file (expand-file-name "ids" root)))
     (unwind-protect
         (progn
           (make-directory site t)
           (make-directory (expand-file-name "static" site) t)
           (make-directory denote-directory t)
           (make-symbolic-link
            (expand-file-name "node_modules" (or (getenv "YEJUN_SITE") "~/code/yejun.dev"))
            (expand-file-name "node_modules" site))
           (write-region "" nil (expand-file-name "astro.config.mjs" site) nil 'silent)
           ,@body)
       (delete-directory root t))))

(defun denote-astro-test--export (site name headers body)
  "Export a note named NAME with HEADERS and BODY into SITE."
  (let ((file (expand-file-name (concat "20261127T120000--" name ".org") denote-directory)))
    (with-temp-file file
      (insert "#+title: Export test\n#+date: [2025-03-04 Tue 17:17]\n"
              "#+filetags: :emacs:writing:\n#+identifier: 20261127T120000\n"
              "#+hugo_base_dir: " site "\n#+hugo_section: posts\n"
              "#+export_file_name: " name "\n" headers "\n" body))
    (with-temp-buffer
      (insert-file-contents file)
      (setq buffer-file-name file default-directory (file-name-as-directory denote-directory))
      (org-mode)
      (let ((output (org-hugo-export-to-md)))
        (cons output (with-temp-buffer (insert-file-contents output) (buffer-string)))))))

(ert-deftest denote-astro-metadata-and-literal-filename ()
  (denote-astro-test--site
    (pcase-let ((`(,file . ,text)
                 (denote-astro-test--export
                  site "sample.zh.md"
                  "#+hugo_publishdate: 2026-11-27\n#+hugo_custom_front_matter: :toc false :state seedling"
                  "* Hello\nContent.")))
      (should (string-suffix-p "/content/posts/sample.zh.md" file))
      (should (string-match-p "published: 2026-11-27" text))
      (should-not (string-match-p "^lang:" text))
      (should (string-match-p "tags: \\[\"emacs\", \"writing\"\\]" text))
      (should-not (string-match-p "^\\(?:date\\|author\\|state\\|lastmod\\|draft\\|toc\\):" text))
      (should-not (string-match-p "{#" text))
      (should (string-match-p "^## Hello$" text))
      (should-not (string-match-p "<span" text)))))

(ert-deftest denote-astro-explicit-dates-and-flags ()
  (denote-astro-test--site
    (let ((text (cdr (denote-astro-test--export
                      site "sample"
                      "#+language: zh\n#+hugo_draft: true\n#+hugo_custom_front_matter: :toc true"
                      "Body."))))
      (should (string-match-p "published: 2025-03-04" text))
      (should (string-match-p "lang: \"zh\"" text))
      (should (string-match-p "draft: true" text))
      (should (string-match-p "toc: true" text)))))

(ert-deftest denote-astro-automatic-lastmod-is-date-only ()
  (denote-astro-test--site
    (cl-letf (((symbol-function 'org-current-time)
               (lambda () (encode-time 56 34 12 27 11 2026))))
      (dolist (headers '("#+hugo_auto_set_lastmod: t"
                         "#+hugo_auto_set_lastmod: t\n#+hugo_lastmod: 2020-01-01"))
        (let ((text (cdr (denote-astro-test--export site "updated" headers "Body."))))
          (should (string-match-p "^lastmod: 2026-11-27$" text))
          (should (string-match-p "^published: 2025-03-04$" text)))))))

(ert-deftest denote-astro-automatic-lastmod-requires-note-opt-in ()
  (denote-astro-test--site
    (let ((org-hugo-auto-set-lastmod t))
      (dolist (headers '("" "#+hugo_auto_set_lastmod: nil" "#+hugo_auto_set_lastmod: false"
                         "#+hugo_lastmod: 2020-01-01"))
        (let ((text (cdr (denote-astro-test--export site "unchanged" headers "Body."))))
          (should-not (string-match-p "^lastmod:" text)))))))

(ert-deftest denote-astro-compact-spacing-preserves-code ()
  (denote-astro-test--site
    (let ((text (cdr (denote-astro-test--export
                      site "spacing" ""
                      (concat "Introduction.\n\n* First\nBody.\n\n"
                              "** Nested\n- Item\n\n* Second\n"
                              "#+begin_src markdown\n## Code heading\n\n\nKeep these blanks.\n#+end_src\n")))))
      (should (string-match-p (regexp-quote "---\nIntroduction.\n\n## First\n") text))
      (should (string-match-p (regexp-quote "Body.\n\n### Nested\n") text))
      (should-not (string-match-p "\n\n\n#+ \\(?:First\\|Nested\\|Second\\)" text))
      (should (string-match-p (regexp-quote "## Code heading\n\n\nKeep these blanks.") text)))))

(ert-deftest denote-astro-links-attachments-and-code ()
  (denote-astro-test--site
    (with-temp-file (expand-file-name "20261126T120000--target.org" denote-directory)
      (insert "#+title: Target\n#+identifier: 20261126T120000\n"
              "#+hugo_base_dir: " site "\n#+hugo_section: posts\n"
              "#+export_file_name: target\n#+language: zh\n* Target heading\n"
              ":PROPERTIES:\n:CUSTOM_ID: target-id\n:END:\n"
              "* Hello + World\n* Hello + World\n"
              "* Hidden :noexport:\n* COMMENT Hidden too\n"
              "* 中文 *标题*\n"))
    (with-temp-file (expand-file-name "20261125T120000--private.org" denote-directory)
      (insert "#+title: Private\n#+identifier: 20261125T120000\n"))
    (write-region "image" nil (expand-file-name "image.png" denote-directory) nil 'silent)
    (let ((text (cdr (denote-astro-test--export
                      site "sample" ""
                      (concat "[[denote:20261126T120000][Target]]\n\n"
                              "[[denote:20261126T120000::#target-id][Detail]]\n\n"
                              "[[denote:20261126T120000::*Hello + World][Heading]]\n\n"
                              "[[denote:20261126T120000::*中文 *标题*][Chinese heading]]\n\n"
                              "[[denote:20261125T120000][Private]]\n\n"
                              "* Hello + World\n[[*Hello + World][Local]]\n\n"
                              "#+attr_html: :alt Example image\n[[file:image.png]]\n\n"
                              "#+begin_src emacs-lisp :hl_lines 1\n(message \"hello\")\n#+end_src\n")))))
      (should (string-match-p (regexp-quote "[Target](/posts/target/)") text))
      (should (string-match-p (regexp-quote "[Detail](/posts/target/#target-id)") text))
      (should (string-match-p (regexp-quote "[Heading](/posts/target/#hello--world)") text))
      (should (string-match-p (regexp-quote "[Chinese heading](/posts/target/#中文-标题)") text))
      (should-not (string-match-p "denote:\\|{{<\\|/zh/\\|\\.org)" text))
      (should (string-match-p (regexp-quote "[Local](#hello--world)") text))
      (should-not (string-match-p "<span" text))
      (should (string-match-p "<figure><img src=\"/attachments/image.png\" alt=\"Example image\"" text))
      (should (file-exists-p (expand-file-name "static/attachments/image.png" site)))
      (should (string-match-p (regexp-quote "```emacs-lisp {1}") text)))))

(ert-deftest denote-astro-generated-and-custom-heading-ids ()
  (denote-astro-test--site
    (let ((text (cdr (denote-astro-test--export
                      site "headings" ""
                      (concat
                       "[[id:second-heading][Second]]\n\n"
                       "[[#stable-anchor][Custom]]\n\n"
                       "[[*Don't repeat yourself][Apostrophe]]\n\n"
                       "[[*中文 *标题*][Chinese]]\n\n"
                       "* Repeat\n* Repeat\n:PROPERTIES:\n:ID: second-heading\n:END:\n"
                       "* COMMENT Repeat\n* Repeat :noexport:\n"
                       "* Visible *title*\n:PROPERTIES:\n:CUSTOM_ID: stable-anchor\n:END:\n"
                       "* Same\n:PROPERTIES:\n:CUSTOM_ID: same\n:END:\n"
                       "* Don't repeat yourself\n* 中文 *标题*\n")))))
      (should (string-match-p (regexp-quote "[Second](#repeat-1)") text))
      (should (string-match-p (regexp-quote "[Custom](#stable-anchor)") text))
      (should (string-match-p (regexp-quote "[Apostrophe](#dont-repeat-yourself)") text))
      (should (string-match-p (regexp-quote "[Chinese](#中文-标题)") text))
      (should (string-match-p "^## Visible \\*\\*title\\*\\*<span id=\"stable-anchor\"></span>$" text))
      (should (string-match-p "^## Same$" text))
      (should (= (with-temp-buffer
                   (insert text)
                   (count-matches "<span" (point-min) (point-max))) 1)))))

(ert-deftest denote-astro-keeps-shortcode-examples-literal ()
  (denote-astro-test--site
    (let* ((body (concat "#+begin_src emacs-lisp\n"
                         "(message \"{{< relref example >}} {{% example %}}\")\n"
                         "#+end_src\n"))
           (astro (cdr (denote-astro-test--export site "example" "" body))))
      (should (string-match-p (regexp-quote "{{< relref example >}} {{% example %}}") astro))
      (delete-file (expand-file-name "astro.config.mjs" site))
      (let ((hugo (cdr (denote-astro-test--export site "example" "" body))))
        (should (string-match-p (regexp-quote "{{</* relref example */>}} {{%/* example */%}}") hugo))))))

(ert-deftest denote-astro-leaves-hugo-sites-alone ()
  (denote-astro-test--site
    (delete-file (expand-file-name "astro.config.mjs" site))
    (pcase-let ((`(,file . ,text)
                 (denote-astro-test--export site "sample" "" "* Hello\nBody.")))
      (should (string-suffix-p "/sample.md" file))
      (should (string-match-p "^date:" text))
      (should-not (string-match-p "^published:" text))
      (should (string-match-p "{#hello}" text)))))

(ert-deftest denote-astro-command-exports-whole-note-without-babel ()
  (denote-astro-test--site
    (let* ((file (expand-file-name "20261127T120000--command.org" denote-directory))
           (sentinel (expand-file-name "babel-ran" root))
           (output (expand-file-name "content/posts/command.md" site)))
      (with-temp-file file
        (insert "#+title: Command\n#+date: 2026-11-27\n#+export_file_name: command\n"
                "#+hugo_base_dir: " site "\n#+hugo_section: posts\n\n"
                "* First\nFirst body.\n* Second\nSecond body.\n"
                "#+begin_src emacs-lisp :exports both :eval yes\n"
                (format "(write-region \"ran\" nil %S)\n" sentinel)
                "#+end_src\n"))
      (let ((buffer (find-file-noselect file)))
        (unwind-protect
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward "* Second")
              (beginning-of-line)
              (narrow-to-region (point) (point-max))
              (denote-astro-export)
              (should-not (file-exists-p sentinel))
              (let ((text (with-temp-buffer (insert-file-contents output) (buffer-string))))
                (should (string-match-p "First body" text))
                (should (string-match-p "Second body" text))))
          (kill-buffer buffer))))))

;;; denote-astro-test.el ends here
