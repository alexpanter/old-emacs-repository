(provide 'setup-data)

;; after a lot of testing I discovered that enabling the package
;; `saveplace-mode' causes the activated mode hooks to not be
;; re-initialized when starting emacs with a saved session.
;; Therefore this mode has been disabled.
;; Also, because it's kind of pointless - if I quit emacs,
;; it's because I'm done editing the files.
;;
;; Alternative : use workgroups.
;;
