;; Making custom-file disposable
(setq custom-file (make-temp-file "emacs-custom-"))

;; Directories
(defvar my-src-directory "~/src/"
  "Directory for source code repos.")

(defvar my-sync-directory "~/Library/CloudStorage/Dropbox/"
  "Directory for syncing files.")

;; Keymaps
(defvar my-insert-map (make-sparse-keymap)
  "A keymap for insert commands.")

(defvar my-toggle-map (make-sparse-keymap)
  "A keymap for toggle commands.")

(keymap-set global-map "C-c i" my-insert-map)
(keymap-set global-map "C-c t" my-toggle-map)

(provide 'init-custom)
