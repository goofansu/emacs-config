(use-package modus-themes
  :init
  (defun my/load-modus-theme ()
    "Load a Modus theme according to current macOS appearance.
Load modus-vivendi theme if in macOS dark mode, otherwise load
modus-operandi theme."
    (interactive)
    (if (macos-dark-mode-p)
        (load-theme 'modus-vivendi :no-confirm)
      (load-theme 'modus-operandi :no-confirm)))
  :hook (after-init . my/load-modus-theme)
  :bind ("<f9>" . modus-themes-toggle)
  :custom
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui t)
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-completions '((t . (extrabold))))
  (modus-themes-prompts '(extrabold))
  (modus-themes-headings
   '((agenda-structure . (variable-pitch light 2.2))
     (agenda-date . (variable-pitch regular 1.3))
     (t . (regular 1.15)))))

(use-package fontaine
  :hook after-init
  :bind ("C-c F" . fontaine-set-preset)
  :custom
  (fontaine-presets
   '((regular
      :default-height 160)
     (medium
      :default-family "Aporetic Serif Mono"
      :default-height 200
      :fixed-pitch-family "Aporetic Serif Mono"
      :variable-pitch-family "Aporetic Sans")
     (large
      :default-height 240)
     (t
      :default-family "Aporetic Sans Mono"
      :default-weight regular
      :default-slant normal
      :default-width normal
      :default-height 100
      :fixed-pitch-family "Aporetic Sans Mono"
      :variable-pitch-family "Aporetic Serif")
     ))
  :config
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))

(use-package spacious-padding
  :hook after-init
  :custom
  (spacious-padding-subtle-mode-line t))

(provide 'init-ui)
