;;; gh.el -- Code for GitHub CLI -*- lexical-binding: t -*-

;;; Commentary:
;;
;; https://cli.github.com/manual/

;;; Code:

(defun gh-gist-create ()
  (interactive)
  (let ((filename (if buffer-file-name
                      (file-relative-name buffer-file-name)
                    (buffer-name)))
        (output-buffer "*gist-output*"))
    (shell-command-on-region
     (if (use-region-p) (region-beginning) (point-min))
     (if (use-region-p) (region-end) (point-max))
     (format "gh gist create --filename \"%s\" -" filename)
     output-buffer)
    (with-current-buffer output-buffer
      (goto-char (point-max))
      (forward-line -1)
      (kill-new (thing-at-point 'url)))
    (kill-buffer output-buffer)))

(provide 'gh)
;;; gh.el ends here
