(use-package modus-themes
  :init
  (setq current-theme (if (display-graphic-p) 'modus-operandi 'modus-vivendi))
  :config
  (message "modus-themes is loaded")
  (load-theme current-theme :no-confirm)
  :custom
  (modus-themes-custom-auto-reload nil)
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui t)
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs nil)
  (modus-themes-org-blocks nil)
  (modus-themes-completions
   '((matches . (underline))
     (selection . (extrabold))))
  (modus-themes-prompts '(extrabold))
  (modus-themes-headings
   '((agenda-structure . (variable-pitch light 1.8))
     (agenda-date . (1.3))
     (t . (regular 1.15)))))

(use-package spacious-padding
  :config
  (message "spacious-padding is loaded")
  (spacious-padding-mode 1)
  :custom
  (spacious-padding-subtle-mode-line t))

(use-package fontaine
  :demand t
  :config
  (message "fontaine is loaded")
  (fontaine-set-preset 'regular)
  :custom
  (fontaine-presets
   '((regular
      :default-family "JetBrains Mono"
      :default-weight normal
      :default-height 160
      :variable-pitch-family "Iosevka Comfy Motion Duo"
      :variable-pitch-weight normal
      :variable-pitch-height 1.2
      :line-spacing 1)
     (large
      :inherit regular
      :default-height 180
      :variable-pitch-height 1.3)
     (presentation
      :inherit regular
      :default-height 240
      :variable-pitch-height 1.6)))
  :hook (modus-themes-after-load-theme . fontaine-apply-current-preset)
  :bind ("C-c f" . fontaine-set-preset))

(provide 'init-ui)
