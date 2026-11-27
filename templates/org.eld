org-mode

;; yejun.dev (Org -> ox-hugo -> Astro Markdown)
(fn "* Footnotes
* COMMENT Local Variables :ARCHIVE:
# Local Variables:
# eval: (org-hugo-auto-export-mode)
# End:")

(page "#+export_file_name: " p n
      "#+hugo_base_dir: " (p "~/code/yejun.dev") n
      "#+hugo_section: /")

(post "#+export_file_name: " p n
      "#+hugo_base_dir: " (p "~/code/yejun.dev") n
      "#+hugo_section: posts" n
      "#+hugo_publishdate: " (p (format-time-string "%Y-%m-%d")) n
      "#+hugo_draft: true")

(lastmod "#+hugo_auto_set_lastmod: t")
(toc "#+hugo_custom_front_matter: :toc true")
(zh "#+language: zh")

(cap "#+caption: " p)
(attr "#+attr_html: " p)
