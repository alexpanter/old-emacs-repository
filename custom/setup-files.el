;;; package --- Summary
;;;
;;; Commentary:
;;;
;;; Code:



;; FILE SIZE WARNING
;;
;; "Maximum size of file above which a confirmation is requested".
(setq large-file-warning-threshold 100000000) ;; (size in bytes)



;; BACKUP FILES
;;
;; define a backups directory and generate _a lot_ of backups!
;; (which is a really good thing)
(defvar backup-directory "~/.backups")
(if (not (file-exists-p backup-directory))
    (make-directory backup-directory t))
;;
(setq
 make-backup-files t    ; backup a file the first time it is saved
 backup-directory-alist `((".*" . ,backup-directory)) ; save files in chosen backup folder
 backup-by-copying t    ; copy the current file into backup directory
 version-control t      ; version numbers for backup files
 delete-old-versions t  ; delete unnecessary versions
 kept-old-versions 6    ; oldest versions to keep when a new numbered backup is made (default: 2)
 kept-new-versions 9    ; newest versions to keep when a new numbered backup is made (default: 2)
 auto-save-default t    ; auto-save every buffer that visits a file
 auto-save-timeout 40   ; number of seconds idle time before auto-save (default: 30)
 auto-save-interval 400 ; number of keystrokes between auto-saves (default: 300)
 )



;; RECENTF
;;
;; builds a list of recently visited files
(recentf-mode)
(setq
 recentf-max-menu-items 30
 recentf-max-saved-items 5000
 )
(require 'recentf-ext)



;; SAVEHIST
;;
;; savehist saves minibuffer history by defaults
(setq savehist-additional-variables '(search ring regexp-search-ring) ; also save your regexp search queries
      savehist-autosave-interval 60     ; save every minute
      )



;; GARBAGE COLLECTION
;;
;; Start garbage collection every 100MB to improve Emacs performance
(setq gc-cons-threshold 100000000)



(provide 'setup-files)
;;; setup-files ends here
