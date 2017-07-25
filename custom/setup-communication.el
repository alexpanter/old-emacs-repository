(provide 'setup-communication)

;; quoted from tuhdo.github.io/emacs-tutor3.html :
;; "This group allows to customize communications, networking, and remote access to files.
;; For example, ftp, ldap, dig, whois, netstat... I only enable goto-address-mode to enable
;; URL highlighting and able to click on it in any buffer. Very useful. Sample configuration:


;; go-to-address-mode
(add-hook 'prog-mode-hook 'goto-address-mode)
(add-hook 'text-mode-hook 'goto-address-mode)
