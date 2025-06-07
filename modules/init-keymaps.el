(defvar my-insert-map (make-sparse-keymap)
  "A keymap for insert commands.")

(defvar my-toggle-map (make-sparse-keymap)
  "A keymap for toggle commands.")

(keymap-set global-map "C-c i" my-insert-map)
(keymap-set global-map "C-c t" my-toggle-map)

(provide 'init-keymaps)
