(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(use-package ace-window
  :config
  (message "ace-window is loaded")
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind ([remap other-window] . ace-window))

(provide 'init-gui-frames)
