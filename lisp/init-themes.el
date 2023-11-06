(use-package modus-themes
  :config
  (message "modus-themes is loaded")
  (let ((theme (if (display-graphic-p) 'modus-operandi 'modus-vivendi)))
    (load-theme theme :no-confirm))
  :custom
  (modus-themes-completions
   '((matches . (extrabold underline))
     (selection . (semibold italic))))
  (modus-themes-headings
   '((1 . (variable-pitch 1.2))
     (2 . (variable-pitch 1.1))
     (3 . (variable-pitch 1.05))
     (4 . (1.0))
     (agenda-date . (1.2))
     (agenda-structure . (variable-pitch light 1.6))
     (t . (1.1)))))

(provide 'init-themes)
