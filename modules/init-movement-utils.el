;; Window
(use-package window
  :ensure nil
  :bind ("M-o" . other-window))

(use-package windmove
  :ensure nil
  :config
  (windmove-default-keybindings)
  (windmove-delete-default-keybindings))

(use-package transpose-frame
  :pin melpa
  :bind
  ( :map window-prefix-map
    ("w" . transpose-frame)
    ("r" . rotate-frame-clockwise)
    ("R" . rotate-frame-anticlockwise)))

(use-package ace-window
  :init
  ;; Open any buffer by splitting any window
  ;; https://karthinks.com/software/fifteen-ways-to-use-embark/#open-any-buffer-by-splitting-any-window
  (eval-when-compile
    (defmacro embark-aw-action (fn)
      `(defun ,(intern (concat "embark-aw-" (symbol-name fn))) ()
         ,(format "Open %s buffer selected with ace-window." (symbol-name fn))
         (interactive)
         (with-demoted-errors "%s"
           (require 'ace-window)
           (let ((aw-dispatch-always t))
             (aw-switch-to-window (aw-select nil))
             (call-interactively (symbol-function ',fn)))))))

  (defvar embark-aw-find-file (embark-aw-action find-file))
  (defvar embark-aw-switch-to-buffer (embark-aw-action switch-to-buffer))
  (defvar embark-aw-bookmark-jump (embark-aw-action bookmark-jump))

  :bind
  (("C-x o" . ace-window)
   :map embark-file-map
   ("o" . embark-aw-find-file)
   :map embark-buffer-map
   ("o" . embark-aw-switch-to-buffer)
   :map embark-bookmark-map
   ("o" . embark-aw-bookmark-jump))

  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'frame))

(use-package popper
  :hook after-init
  :init
  (setq popper-reference-buffers
        (append '(compilation-mode
                  rspec-compilation-mode
                  exunit-compilation-mode
                  inf-ruby-mode
                  devdocs-mode)
                '("^\\*Messages\\*$"
                  ("^\\*Async Shell Command\\*$" . hide)
                  "^\\*Shell Command Output\\*$")))
  :bind
  (("C-`"   . popper-toggle)
   ("M-`"   . popper-cycle)
   ("C-M-`" . popper-toggle-type))
  :custom
  (popper-group-function #'selected-frame)
  (popper-display-control 'user))

(use-package popper-echo
  :ensure nil
  :after popper
  :config
  (popper-echo-mode 1))

(use-package shackle
  :pin melpa
  :custom
  (shackle-inhibit-window-quit-on-same-windows t)
  (shackle-rules
   `((calendar-mode :align below)
     ;; Org capture windows
     ("*Org Select*" :align below)
     ("*Capture*" :align below)
     ("^CAPTURE-.+$" :regexp t :align below)
     ;; Modes
     (calibre-library-mode :select t :same t)))
  :config
  (shackle-mode 1))

;; Frame
(use-package frame
  :ensure nil
  :bind
  (("C-s-f" . select-frame-by-name)
   ("s-w" . my/tab-close-or-delete-frame))
  :config
  (defun my/tab-close-or-delete-frame ()
    "Close the current tab if there are multiple tabs, otherwise delete the frame."
    (interactive)
    (if (and (bound-and-true-p tab-bar-mode)
             (> (length (tab-bar-tabs)) 1))
        (tab-close)
      (delete-frame))))

(use-package beframe
  :if (display-graphic-p)
  :hook after-init
  :bind-keymap ("C-c b" . beframe-prefix-map)
  :custom
  (beframe-functions-in-frames '(project-prompt-project-dir))
  :config
  (defun my/beframe-buffer-names-sorted (&optional frame)
    "Return the list of buffers from `beframe-buffer-names' sorted by visibility.
With optional argument FRAME, return the list of buffers of FRAME."
    (beframe-buffer-names frame :sort #'beframe-buffer-sort-visibility))

  (defvar consult--source-beframe
    `( :name     "Buffer (current frame)"
       :narrow   ?F
       :category buffer
       :face     consult-buffer
       :history  beframe-history
       :items    ,#'my/beframe-buffer-names-sorted
       :action   ,#'switch-to-buffer
       :state    ,#'consult--buffer-state
       :default  t)
    "Beframe buffer source for `consult-buffer'.")

  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources 'consult--source-beframe))

;; Tab bar
(use-package tab-bar
  :ensure nil
  :bind
  (("s-t" . tab-new)
   ("s-T" . tab-undo)
   ("s-{" . tab-previous)
   ("s-}" . tab-next))
  :custom
  (tab-bar-show 1)
  :config
  ;; bind s-1 through s-9 to switch tabs
  (dolist (i (number-sequence 1 9))
    (bind-key (format "s-%d" i)
              `(lambda ()
                 (interactive)
                 (when (<= ,i (length (tab-bar-tabs)))
                   (tab-bar-select-tab ,i))))))

(provide 'init-movement-utils)
