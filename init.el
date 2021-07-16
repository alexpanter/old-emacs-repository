;;; package --- setup-applications
;;;
;;; Code:
;;;
;;; Commentary:



;;; =====  PACKAGE LIST  ===== ;;;

;; list the packages you want
(defvar-local package-list
  '(anaphora cider seq spinner queue pkg-info epl clojure-mode
             clean-aindent-mode company-c-headers company company-jedi
             jedi-core python-environment deferred epc ctable concurrent
             csharp-mode diff-hl dired+ discover-my-major makey
             expand-region f dash s flycheck-tip popup flycheck let-alist
             fsharp-mode pos-tip company-quickhelp helm-gtags helm
             helm-core async help+ help-fns+ help-mode+ highlight-numbers
             parent-mode highlight-symbol ibuffer-git ibuffer-vc info+
             magit magit-popup git-commit with-editor markdown-mode
             projectile rainbow-mode recentf-ext shell-pop ggtags
             smartparens sr-speedbar undo-tree volatile-highlights
             yasnippet zenburn-theme ztree function-args
             use-package rich-minority))

;; Add and enable the MELPA package archive
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; faster loading of packages:
;; https://github.com/jwiegley/use-package
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))



;;; =====  PACKAGE CUSTOMIZATION  ===== ;;;

;; add modules path
(add-to-list 'load-path "~/.emacs.d/custom/")
(mapc 'load (directory-files "~/.emacs.d/custom" t ".*\.el"))

;; load custom modules
(require 'setup-helm)
(require 'setup-helm-gtags)
(require 'setup-c)
;; (require 'setup-ggtags) ; WHY outcommented!!?

(require 'setup-editing)
(require 'setup-appearance)

(require 'setup-languages)
(require 'setup-compilation)

(require 'setup-applications)
(require 'setup-communication)
(require 'setup-shell)
(require 'setup-files)

(require 'setup-navigation)


;; TODO: Add hotfix that actually works and fixes the *very* annoying PROBLEM !
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (global-set-key (kbd "C-c C-r") 'comment-or-uncomment-region)))


;;; =====  AUTOMATIC CONFIGURATION  ===== ;;;

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
 '(column-number-mode t)
 '(company-c-headers-path-system
   (quote
    ("/usr/include/" "/usr/local/include/" "/usr/include/c++/5.4.0/" "/usr/include/c++/8/")))
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("ec5f697561eaf87b1d3b087dd28e61a2fc9860e4c862ea8e6b0b77bd4967d0ba" "e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" "72c255b5d12f35d7207c04d49670fc5811a60f6141a6b3a49c68da111a461bc9" "67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" default)))
 '(doc-view-continuous t)
 '(flycheck-c/c++-clang-executable nil)
 '(package-selected-packages
   (quote
    (hl-todo lua-mode ztree zenburn-theme yasnippet volatile-highlights use-package undo-tree swiper sr-speedbar smartparens shell-pop rich-minority recentf-ext rainbow-mode nyan-mode markdown-mode magit info+ ibuffer-vc ibuffer-git highlight-symbol highlight-numbers help-mode+ help-fns+ help+ helm-make helm-gtags haskell-mode ggtags function-args fsharp-mode flycheck-tip f expand-region discover-my-major dired+ diminish diff-hl csharp-mode company-jedi company-glsl company-c-headers clean-aindent-mode cider anaphora)))
 '(safe-local-variable-values
   (quote
    ((flycheck-c/c++-clang-executable . "/usr/bin/clang++-6.0")
     (company-clang-executable . "/usr/bin/clang++-6.0")
     (company-c-headers-path-user . "/home/alexander/Documents/Viking-Village/src")
     (company-c-headers-path-system . "/home/alexander/Documents/Viking-Village/src")
     (company-c-headers-path-user . "/home/alexander/Documents/Viking-Village/src/")
     (company-c-headers-path-system . "/home/alexander/Documents/Viking-Village/src/")
     (company-clang-arguments . "-std=c++17")
     (company-clang-arguments "-I/usr/include/c++/5" "-I/usr/include/c++/8" "-I/home/alexander/Documents/Viking-Village/src" "/usr/include/x86_64-linux-gnu/c++/5" "/usr/include/c++/5/backward" "/usr/lib/gcc/x86_64-linux-gnu/5/include" "/usr/local/include" "/usr/lib/gcc/x86_64-linux-gnu/5/include-fixed" "/usr/include/x86_64-linux-gnu" "-I/usr/include/x86_64-linux-gnu/c++/5" "-I/usr/lib/gcc/x86_64-linux-gnu/5/include" "-I/usr/lib/gcc/x86_64-linux-gnu/5/include-fixed" "-I/usr/include/x86_64-linux-gnu" "-I/usr/include/c++/5/backward" "/usr/include" "-I/usr/local/include" "-stdlib=libc++" "-std=c++17")
     (flycheck-gcc-language-standard . c++17)
     (company-clang-arguments "-I/home/alexander/Documents/Graphics/GameEngine/")
     (company-clang-arguments "-I/home/alexander/Documents/Viking-Village/src/")
     (companye-clang-arguments "-I/home/alexander/Documents/C++Docs"))))
 '(shell-pop-shell-type
   (quote
    ("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell)))))
 '(shell-pop-window-size 25)
 '(sp-highlight-pair-overlay nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#050505" :foreground "#DCDCCC" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "PfEd" :family "Inconsolata"))))
 '(fringe ((t (:background "gray7" :foreground "#DCDCCC"))))
 '(header-line ((t (:background "#283b3d" :foreground "#F0DFAF" :box (:line-width -1 :style released-button)))))
 '(helm-ff-directory ((t (:background "#050505" :foreground "steel blue" :weight bold))))
 '(helm-ff-dotted-directory ((t (:background "#050505" :foreground "orange red"))))
 '(helm-ff-dotted-symlink-directory ((t (:background "DimGray" :foreground "DarkOrange"))))
 '(helm-ff-executable ((t (:background "#050505" :foreground "yellow green" :weight normal))))
 '(helm-ff-file ((t (:background "#050505" :foreground "#DCDCCC" :weight normal))))
 '(helm-ff-symlink ((t (:background "#050505" :foreground "#F0DFAF" :weight bold))))
 '(highlight-numbers-number ((t (:inherit font-lock-constant-face :foreground "#00c5c5"))))
 '(hl-line ((t (:background "#111111"))))
 '(linum ((t (:background "#050505" :foreground "#9FC59F"))))
 '(mode-line ((t (:background "#8B2B2B" :foreground "#8FB28F" :box (:line-width -1 :style released-button)))))
 '(region ((t (:background "dark green"))))
 '(trailing-whitespace ((t (:background "#F53525"))))
 '(undo-tree-visualizer-active-branch-face ((t (:foreground "chocolate3" :weight bold))))
 '(undo-tree-visualizer-current-face ((t (:foreground "sky blue" :weight bold))))
 '(vertical-border ((t (:foreground "dark slate gray"))))
 '(vhl/default-face ((t (:background "dark goldenrod" :foreground "black")))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
