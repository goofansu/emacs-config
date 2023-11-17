(add-to-list 'auth-sources "~/.password-store/authinfo.gpg")

(use-package pass
  :pin melpa
  :defer t
  :config
  (message "pass is loaded"))

(defun yejun/otp-key-uri (issuer secret)
  "Create and copy the OTP key URI consisting of issuer and secret."
  (interactive (list (read-string "Issuer: ")
                     (read-passwd "Secret: " t)))
  (let* ((secret (replace-regexp-in-string "\\s-" "" secret))
         (otp-uri (format "otpauth://totp/totp-secret?secret=%s&issuer=%s" secret issuer)))
    (kill-new otp-uri)
    (message "OTP key URI created and copied")))

(provide 'init-pass)
