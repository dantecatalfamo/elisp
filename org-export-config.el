;;; org-export-config --- Export hosts and ssh config files from org buffer.

;;; Commentary:

;;; Code:

(require 'org-element)
(require 'subr-x)

(defvar my/org-export-config-ssh-header ""
  "Optional text to be inserted at the top of SSH config.")

(defun my/org-export-config-get-addrs (buffer)
  "Parse org BUFFER for headlines with IP or URL properties, and optional SSH properties.
Example:
* server
    :PROPERTIES:
    :IP:          192.168.0.2
    :SSH_USER:    john
    :SSH_PORT:    2222
    :SSH_FORWARD: t
    :END:
becomes:
Host server
  HostName 192.168.0.2
  ForwardAgent yes
  Port 2222
  User john"
  (with-current-buffer buffer
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (node)
        (let ((url (org-element-property :URL node))
              (ip (org-element-property :IP node))
              (host (car (org-element-property :title node)))
              (ssh-forward (org-element-property :SSH_FORWARD node))
              (ssh-port (org-element-property :SSH_PORT node))
              (user (org-element-property :SSH_USER node)))
          (when (or ip url)
            (list :HOST (substring-no-properties host)
                  :IP ip
                  :URL url
                  :SSH_FORWARD ssh-forward
                  :SSH_PORT ssh-port
                  :SSH_USER user)))))))

(defun my/org-export-config-hosts (buffer &optional tld)
  "Format BUFFER as hosts file with optional TLD appended to hostnames."
  (with-temp-buffer
    (let ((hosts (my/org-export-config-get-addrs buffer))
          (dot-tld (if tld (concat "." tld) "")))
      (dolist (host hosts)
        (if-let ((ip (plist-get host :IP))
                 (hostname (plist-get host :HOST)))
            (insert ip "\t" hostname dot-tld "\n")))
      (buffer-string))))

(defun my/org-export-config-ssh (buffer)
  "Format BUFFER as an SSH config file."
  (with-temp-buffer
    (let ((hosts (my/org-export-config-get-addrs buffer)))
      (insert my/org-export-config-ssh-header "\n")
      (dolist (host hosts)
        (let ((hostname (plist-get host :HOST))
              (addr (or (plist-get host :IP)
                        (plist-get host :URL)))
              (ssh-forward (plist-get host :SSH_FORWARD))
              (ssh-port (plist-get host :SSH_PORT))
              (ssh-user (plist-get host :SSH_USER)))
          (insert "Host " hostname "\n")
          (insert "  HostName " addr "\n")
          (when ssh-forward
              (insert "  ForwardAgent yes\n"))
          (when ssh-port
              (insert "  Port " ssh-port "\n"))
          (when ssh-user
              (insert "  User " ssh-user "\n"))
          (insert "\n"))))
    (buffer-string)))

(provide 'org-export-config)
;;; org-export-config.el ends here
