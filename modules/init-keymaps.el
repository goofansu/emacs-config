(defvar my-insert-map (make-sparse-keymap)
  "A keymap for insert commands.")

(defvar my-toggle-map (make-sparse-keymap)
  "A keymap for toggle commands.")

(define-key global-map (kbd "C-c i") my-insert-map)
(define-key global-map (kbd "C-c t") my-toggle-map)

(provide 'init-keymaps)
