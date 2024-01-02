;;; gptel-kagi.el --- Kagi support for gptel     -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: hypermedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file adds support for the Kagi FastGPT LLM API to gptel

;;; Code:
(require 'gptel)
(require 'cl-generic)
(eval-when-compile
  (require 'cl-lib))

;;; Kagi
(cl-defstruct (gptel-kagi (:constructor gptel--make-kagi)
                            (:copier nil)
                            (:include gptel-backend)))

(cl-defmethod gptel--parse-response ((_backend gptel-kagi) response info)
  (let* ((data (plist-get response :data))
         (output (plist-get data :output))
         (references (plist-get data :references)))
    (when references
      (setq references
            (cl-loop with linker =
                     (pcase (buffer-local-value 'major-mode
                                                (plist-get info :buffer))
                       ('org-mode
                        (lambda (text url)
                          (format "[[%s][%s]]" url text)))
                       ('markdown-mode
                        (lambda (text url)
                          (format "[%s](%s)" text url)))
                       (_ (lambda (text url)
                            (gptel--button-buttonize
                             text (lambda (data) (browse-url data))
                             url))))
                     for ref across references
                     for title = (plist-get ref :title)
                     for snippet = (plist-get ref :snippet)
                     for url = (plist-get ref :url)
                     for n upfrom 1
                     collect
                     (concat (format "[%d] " n)
                             (funcall linker title url) ": "
                             (replace-regexp-in-string
                              "</?b>" "*" snippet))
                     into ref-strings
                     finally return
                     (concat "\n\n" (mapconcat #'identity ref-strings "\n")))))
        (concat output references)))

(cl-defmethod gptel--request-data ((_backend gptel-kagi) prompts)
  "JSON encode PROMPTS for sending to ChatGPT."
  `(,@prompts :web_search t :cache t))

(cl-defmethod gptel--parse-buffer ((_backend gptel-kagi) &optional _max-entries)
  (let ((prompts)
        (prop (text-property-search-backward
               'gptel 'response
               (when (get-char-property (max (point-min) (1- (point)))
                                        'gptel)
                 t))))
    (if (and (prop-match-p prop)
             (prop-match-value prop))
        (user-error "No user prompt found!")
      (setq prompts (list
                     :query
                     (if (prop-match-p prop)
                         (string-trim
                          (buffer-substring-no-properties (prop-match-beginning prop)
                                                          (prop-match-end prop))
                          (format "[\t\r\n ]*\\(?:%s\\)?[\t\r\n ]*"
                                  (regexp-quote (gptel-prompt-prefix-string)))
                          (format "[\t\r\n ]*\\(?:%s\\)?[\t\r\n ]*"
                                  (regexp-quote (gptel-response-prefix-string))))
                       "")))
      prompts)))

;;;###autoload
(cl-defun gptel-make-kagi
    (name &key stream key
          (host "kagi.com")
          (header (lambda () `(("Authorization" . ,(concat "Bot " (gptel--get-api-key))))))
          (models '("fastgpt"))
          (protocol "https")
          (endpoint "/api/v0/fastgpt"))
  "Register a Kagi FastGPT backend for gptel with NAME.

Keyword arguments:

HOST is the Kagi host (with port), defaults to \"kagi.com\".

MODELS is a list of available Kagi models: only fastgpt is supported.

STREAM is a boolean to toggle streaming responses, defaults to
false.  Kagi does not support a streaming API yet.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/api/v0/fastgpt\".

HEADER (optional) is for additional headers to send with each
request.  It should be an alist or a function that retuns an
alist, like:
((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key.

Example:
-------

(gptel-make-kagi \"Kagi\" :key my-kagi-key)"
  (let ((backend (gptel--make-kagi
                  :name name
                  :host host
                  :header header
                  :key key
                  :models models
                  :protocol protocol
                  :endpoint endpoint
                  :url (if protocol
                           (concat protocol "://" host endpoint)
                         (concat host endpoint)))))
    (prog1 backend
      (setf (alist-get name gptel--known-backends
                       nil nil #'equal)
                  backend))))

(provide 'gptel-kagi)
;;; gptel-kagi.el ends here
