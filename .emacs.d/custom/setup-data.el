(provide 'setup-data)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Data -> Saveplace ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; saveplace remembers your location in a file when saving files
(require 'saveplace)
(setq-default save-place t)
;; use the following command with Emacs above 24.5 (I have 24.5.1)
(toggle-save-place-globally 1)
