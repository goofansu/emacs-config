(blink-cursor-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent))

;; Boost startup performance
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs started in %s" (emacs-init-time))
            (setq gc-cons-threshold (* 1000 1000 8)
                  gc-cons-percentage 0.1)))

(provide 'early-init)
