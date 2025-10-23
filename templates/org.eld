org-mode

;; Hugo
(fn "* Footnotes
* COMMENT Local Variables :ARCHIVE:
# Local Variables:
# eval: (org-hugo-auto-export-mode)
# End:")

(page "#+export_file_name: " p n
      "#+hugo_base_dir: " (p "~/code/yejun.dev") n
      "#+hugo_section: /" n
      "#+hugo_auto_set_lastmod: t")

(post "#+export_file_name: " p n
      "#+hugo_base_dir: " (p "~/code/yejun.dev") n
      "#+hugo_section: " (p "posts") n
      "#+hugo_draft: true" n
      "#+hugo_custom_front_matter: " (p ":toc false") n
      "#+hugo_auto_set_lastmod: t")

(devlog "#+export_file_name: " p n
        "#+hugo_base_dir: " (p "~/code/yejun.dev") n
        "#+hugo_section: " (p "devlog") n
        "#+hugo_custom_front_matter: " (p ":toc true") n
        "#+hugo_custom_front_matter: " (p ":state seedling") n
        "#+hugo_auto_set_lastmod: t")

(til "#+hugo_base_dir: ~/code/yejun.dev" n
     "#+hugo_section: til")

(link "#+hugo_base_dir: ~/code/yejun.dev" n
      "#+hugo_section: links")

(cap "#+caption: " p)
(alt "#+attr_html: :alt " p)
(id "#+attr_html: :id " p)
