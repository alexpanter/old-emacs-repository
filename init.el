;;; package --- setup-applications
;;;
;;; Code:
;;;
;;; Commentary:



;;; ===== PACKAGE LIST ===== ;;;

; list the packages you want
(setq package-list
      '(anaphora cider seq spinner queue pkg-info epl clojure-mode
                 clean-aindent-mode company-c-headers company company-jedi
                 jedi-core python-environment deferred epc ctable concurrent
                 csharp-mode diff-hl dired+ discover-my-major makey
                 expand-region f dash s flycheck-tip popup flycheck let-alist
                 fsharp-mode pos-tip company-quickhelp helm-gtags helm
                 helm-core async help+ help-fns+ help-mode+ highlight-numbers
                 parent-mode highlight-symbol ibuffer-git ibuffer-vc info+
                 magit magit-popup git-commit with-editor markdown-mode
                 nyan-mode projectile rainbow-mode recentf-ext shell-pop
                 smartparens sr-speedbar undo-tree volatile-highlights
                 yasnippet zenburn-theme ztree function-args))

;; Add and enable the MELPA package archive
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))



;;; ===== PACKAGE CUSTOMIZATION ===== ;;;

;; add modules path
(add-to-list 'load-path "~/.emacs.d/custom/")
(mapc 'load (directory-files "~/.emacs.d/custom" t ".*\.el"))

;; load custom modules
(require 'helm-config)
(require 'setup-gtags)
(require 'setup-programming)
(require 'setup-applications)
(require 'setup-communication)
(require 'setup-data)
(require 'setup-development)
(require 'setup-editing)
(require 'setup-environment)
(require 'setup-external)
(require 'setup-faces-and-ui)
(require 'setup-files)
(require 'setup-help)
(require 'setup-text)
(require 'setup-local)
(require 'setup-convenience)




;;; ===== AUTOMATIC CONFIGURATION ===== ;;;

;; custom configurations
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("72c255b5d12f35d7207c04d49670fc5811a60f6141a6b3a49c68da111a461bc9" "67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" default)))
 '(doc-view-continuous t)
 '(safe-local-variable-values
   (quote
    ((companye-clang-arguments "-I/home/alexander/Documents/C++Docs")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#050505" :foreground "#DCDCCC" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "PfEd" :family "Inconsolata"))))
 '(hl-line ((t (:background "#111111"))))
 '(linum ((t (:background "#050505" :foreground "#9FC59F"))))
 '(region ((t (:background "dark green"))))
 '(trailing-whitespace ((t (:background "#F53525")))))
