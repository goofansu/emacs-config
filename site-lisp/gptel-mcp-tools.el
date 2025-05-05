(require 'gptel)
(require 'mcp-hub)

(define-minor-mode gptel-mcp-tools-mode
  "Toggle MCP tools integration with gptel.
When enabled, all MCP tools are selected in gptel.
When disabled, all MCP tools are unselected in gptel."
  :global t
  :init-value nil
  :group 'gptel
  (if gptel-mcp-tools-mode
      (gptel-mcp-tools-activate)
    (gptel-mcp-tools-deactivate)))

(defun gptel-mcp-tools-register ()
  "Register MCP tools to gptel."
  (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
    (mapcar #'(lambda (tool)
                (apply #'gptel-make-tool
                       tool))
            tools)))

(defun gptel-mcp-tools-activate ()
  "Activate MCP tools in gptel."
  (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
    (mapcar #'(lambda (tool)
                (let ((path (list (plist-get tool :category)
                                  (plist-get tool :name))))
                  (push (gptel-get-tool path)
                        gptel-tools)))
            tools)))

(defun gptel-mcp-tools-deactivate ()
  "Deactivate MCP tools in gptel."
  (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
    (mapcar #'(lambda (tool)
                (let ((path (list (plist-get tool :category)
                                  (plist-get tool :name))))
                  (setq gptel-tools
                        (cl-remove-if #'(lambda (tool)
                                          (equal path
                                                 (list (gptel-tool-category tool)
                                                       (gptel-tool-name tool))))
                                      gptel-tools))))
            tools)))

(provide 'gptel-mcp-tools)
