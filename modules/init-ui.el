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
   '((matches . ( underline))
     (selection . (extrabold))))
  (modus-themes-prompts '(extrabold))
  (modus-themes-headings
   '((agenda-structure . (variable-pitch light 2.2))
     (agenda-date . (variable-pitch regular 1.3))
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
   '((regular)
     (presentation
      :default-weight semilight
      :default-height 240
      :bold-weight extrabold)
     (t
      :default-family "JetBrains Mono"
      :default-weight regular
      :default-height 160
      :fixed-pitch-family nil
      :fixed-pitch-weight nil
      :fixed-pitch-height 1.0
      :variable-pitch-family "Iosevka"
      :variable-pitch-weight regular
      :variable-pitch-height 1.0)))
  :hook (modus-themes-after-load-theme . fontaine-apply-current-preset)
  :bind ("C-c f" . fontaine-set-preset))

(provide 'init-ui)
