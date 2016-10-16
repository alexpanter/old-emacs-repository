(provide 'setup-help)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: info+                     ;;
;;                                    ;;
;; GROUP: Help -> info+               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'info+)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: discover-my-major         ;;
;;                                    ;;
;; GROUP: Help -> major-mode          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A quick major mode help with discover-my-major
;; press "C-h h m" to see the key bindings of the current major mode.
(global-unset-key (kbd "C-h h")) ; original "C-h h" displays "hello" in different languages
(define-key 'help-command (kbd "h m") 'discover-my-major)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: rainbow-mode              ;;
;;                                    ;;
;; GROUP: Help -> Rainbow             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This minor mode sets background color to strings that match color names,
;; e.g. #0000ff is displayed in white with a blue background.
(add-hook 'html-mode-hook 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package: help+                     ;;
;;                                    ;;
;; GROUP: Help                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'help+)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package: help-fns+                 ;;
;;                                    ;;
;; GROUP: Help                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'help-fns+)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package: help-mode+                ;;
;;                                    ;;
;; GROUP: Help                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'help-mode+)
