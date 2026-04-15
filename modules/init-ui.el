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
  (spacious-padding-subtle-frame-lines t))

(use-package modus-themes-exporter
  :ensure nil ; do not try to install because we get it from source in the `:init'
  :commands (modus-themes-exporter-export)
  :init
  ;; Then upgrade it with the command `package-vc-upgrade' or `package-vc-upgrade-all'.
  (unless (package-installed-p 'modus-themes-exporter)
    (package-vc-install "https://github.com/protesilaos/modus-themes-exporter.git"))
  :config
  ;; TODO remove when upgraded to modus-themes 5.3
  (defun modus-themes-retrieve-palette-value (color palette)
  "Return COLOR from PALETTE.
Use recursion until COLOR is retrieved as a string.  Refrain from
doing so if the value of COLOR is not a key in the PALETTE.

Return `unspecified' if the value of COLOR cannot be determined.
This symbol is accepted by faces and is thus harmless.

This function is used in the macro `modus-themes-theme'"
  (let ((value (car (alist-get color palette))))
    (cond
     ((or (stringp value)
          (eq value 'unspecified))
      value)
     ((and (symbolp value)
           value)
      (modus-themes-retrieve-palette-value value palette))
     (t
      'unspecified))))
  (defun modus-themes-color-dark-p (hex-color)
  "Return non-nil if hexadecimal RGB HEX-COLOR is dark.
Test that HEX-COLOR has more contrast against white than black."
  (> (modus-themes-contrast hex-color "#ffffff")
     (modus-themes-contrast hex-color "#000000"))))

(provide 'init-ui)
