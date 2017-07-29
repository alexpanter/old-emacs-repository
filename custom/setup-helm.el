;;; package --- Summary
;;;
;;; Commentary:
;;;
;;; This file customizes all helm settings.
;;; For better overview, no helm related settings should take
;;; place outside this file.
;;;
;;; Check this website "http://tuhdo.github.io/helm-intro.html" for more info.
;;; Some settings are not enabled yet so that I can get used to the features one at a time.
;;;
;;; According to tuhdo, an alternative to using helm is enabling the following packages:
;;; ido + ido-ubiquitous + flx-ido + smex
;;;
;;; Code:

(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change 'helm-command-prefix-key' once 'helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-config-prefix)
(global-unset-key (kbd "C-x c"))

;; rebind tab to run persistent action
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

;; make TAB work in terminal
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)

;; list actions using C-z
(define-key helm-map (kbd "C-z") 'helm-select-action)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))


(setq
 ;; open helm buffer inside current window, not occupy whole other window
 helm-split-window-in-side-p           t

 ;; move to end or beginning of source when reaching top or bottom
 helm-move-to-line-cycle-in-source     t

 ;; search for library in 'require' and 'declare-function' sexp.
 helm-ff-search-library-in-sexp        t

 ;; scroll 8 lines other window using M-<next>/M-<prior>
 helm-scroll-amount                    8
 helm-ff-file-name-history-use-recentf t
 )

;; by enabling auto-resize, Helm will resize its buffer automatically
;; to fit the number of search candidates.
(helm-autoresize-mode t)

;; Command: helm-M-x
(global-set-key (kbd "M-x") 'helm-M-x)

;; if this is activated, helm-M-x will fuzzy match candidates
(setq helm-M-x-fuzzy-match t) ; optional fuzzy match for helm-M-x

;; enable the helm kill-ring with binding it to M-y
(global-set-key (kbd "M-y") 'helm-show-kill-ring)


;; helm-mini:
(global-set-key (kbd "C-x b") 'helm-mini)

;; to enable fuzzy matching with helm-mini, add the following settings:
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)


;; helm-find-files - file navigation on steroids!
(global-set-key (kbd "C-x C-f") 'helm-find-files)
;; if the point is located on a folder path, C-x C-f will
;; start in that folder. (Use C-l C-r to navigate directories)


(helm-mode 1)



(provide 'setup-helm)
;;; setup-helm ends here
