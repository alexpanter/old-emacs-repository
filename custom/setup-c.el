;;; package --- setup-applications
;;;
;;; Code:
;;;
;;; Commentary:



;; COMPANY
;;
(use-package company
  :init
  (global-company-mode 1)
  (setq company-backends (delete 'company-semantic company-backends))
  (define-key c++-mode-map [(C-tab)] 'company-complete)
  (define-key c-mode-map [(C-tab)] 'company-complete)
  )



;; COMPANY-C-HEADERS
;;
(use-package company-c-headers
  :init
  (add-to-list 'company-backends 'company-c-headers))
;; Header file completion with company-c-headers :
;; (add-to-list 'company-backends 'company-c-headers)

;; Allowing source code folding with `hs-minor-mode`, which allows users to fold and
;; hide blocks of text. Blocks are defined by regular expressions which match the
;; start and end of a text region. E.g. anything between { and } is a block.
;; The regular expressions are defined in `hs-special-modes-alist`.
;; (add-hook 'c-mode-common-hook 'hs-minor-mode)
;; KEY BINDINGS :
;; C-c @ C-c -> `hs-toggle-hiding` (toggle hiding/showing of a block)
;; C-c @ C-h -> `hs-hide-block` (select current block at point and hide it)
;; C-c @ C-l -> `hs-hide-level` (hide blocks with indentation levels below this block)
;; C-c @ C-s -> `hs-show-block` (select current block at point and show it)
;; C-c @ C-M-h -> `hs-hide-all` (hide all top level blocks, displaying only first and last lines)
;; C-c @ C-M-s -> `hs-show-all` (show everything)



;; CC-MODE
;;
(use-package cc-mode
  :init
  (define-key c-mode-map  (kbd "M-<RET>") 'company-complete)
  (define-key c++-mode-map (kbd "M-<RET>") 'company-complete))



;; (require 'semantic)
;; (global-semanticdb-minor-mode 1)
;; (global-semantic-idle-scheduler-mode nil)
;; (semantic-mode 1)
(use-package semantic
  :init
  (semantic-mode 1)
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (global-semantic-stickyfunc-mode 0))

(semantic-add-system-include "/usr/local/include")
(semantic-add-system-include "~/linux/include")
(semantic-add-system-include "/usr/include/c++/8/")



(use-package function-args
  :init
  (fa-config-default))
;;
;; (require 'function-args)
;; (fa-config-default)
;;
;; useful shortcuts:
;;
;; commands (M-x) starting with either 'moo-' or 'fa-'
;; belong to this awesome package.
;;
;; M-i/M-u : show/hide function signature
;; M-o : complete at point



;; Enable EDE only in C/C++
(require 'ede)


(defun alexott/cedet-hook ()
  (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
  (local-set-key "\C-c\C-s" 'semantic-ia-show-summary)
  (global-ede-mode))

(add-hook 'c-mode-common-hook 'alexott/cedet-hook)
(add-hook 'c-mode-hook 'alexott/cedet-hook)
(add-hook 'c++-mode-hook 'alexott/cedet-hook)


;; setup ede to work with company-c-headers
(defun ede-object-system-include-path ()
  "Return the system include path for the current buffer."
  (when ede-object
    (ede-system-include-path ede-object)))

;; (setq company-c-headers-path-system 'ede-object-system-include-path)
(lambda () (add-to-list 'company-c-headers-path-system "-std=c++17"))
(lambda () (add-to-list 'company-c-headers-path-system "/usr/include/c++/5/"))
(lambda () (add-to-list 'company-c-headers-path-system "/usr/include/c++/5.4.0/"))
(lambda () (add-to-list 'company-c-headers-path-system "/usr/include/c++/8/"))
(lambda () (add-to-list 'company-clang-arguments "-stdlib=libc++"))
(lambda () (add-to-list 'company-clang-arguments "-std=c++17"))

(provide 'setup-c)
;;; setup-c ends here
