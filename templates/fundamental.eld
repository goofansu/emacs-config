fundamental-mode

(today (format-time-string "[%Y-%m-%d]"))
(now (format-time-string "[%Y-%m-%d %a %H:%M]"))
(yesterday (format-time-string "[%Y-%m-%d %a]" (time-subtract nil (* 24 60 60))))
(tomorrow (format-time-string "[%Y-%m-%d %a]" (time-add nil (* 24 60 60))))
(backup (format "Auto-backup on %s at %s" (format-time-string "%Y-%m-%d") (format-time-string "%H:%M:%S")))
