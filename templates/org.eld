org-mode

;; Hugo
(fn "* Footnotes
* COMMENT Local Variables :ARCHIVE:
# Local Variables:
# eval: (org-hugo-auto-export-mode)
# End:")

(post "#+export_file_name: " p n
      "#+hugo_base_dir: " p n
      "#+hugo_section: posts" n
      "#+hugo_custom_front_matter: " p)

(til "#+hugo_base_dir: ~/src/yejun.dev
#+hugo_section: til")

(link "#+hugo_base_dir: ~/src/yejun.dev
#+hugo_section: links")

(cap "#+caption: " p)
(alt "#+attr_html: :alt " p)
