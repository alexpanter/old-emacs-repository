;;; package --- setup-languages
;;;
;;; Commentary:
;;;
;;; Code:


;; GENERAL MODE
;;
(defun my-prog-mode-hook ()
  "Settings for general programming modes.
Will default if not overriden in specific mode hooks."

  (setq indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq company--auto-completion nil)
  (setq-default indent-tabs-mode nil)
  )

(add-hook 'prog-mode-hook 'my-prog-mode-hook)



;; MARKDOWN MODE
;;
;; (defun my-markdown-mode-hook ()
;;   "Settings chosen when using markdown mode."

;;   (use-package markdown-mode
;;                :ensure t
;;                :commands (markdown-mode gfm-mode)
;;                :mode (("README\\.md\\'" . gfm-mode)
;;                       ("\\.md\\'" . markdown-mode)
;;                       ("\\.markdown\\'" . markdown-mode))
;;                :init (setq markdown-command "multimarkdown"))
;;   )

;; (add-hook 'markdown-mode-hook 'my-markdown-mode-hook)



;; EMACS LISP MODE
;;
(defun my-emacs-lisp-mode-hook ()
  "Custom e-lisp settings."
  (smartparens-global-mode 0)
  (global-set-key (kbd "C-<left>") 'previous-buffer)
  (global-set-key (kbd "C-<right>") 'next-buffer)
  )

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

;; eldoc for lisp-derived languages:
;; show signature for definition at point in minibuffer
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)



;; CLOJURE MODE
;;
(unless (package-installed-p 'clojure-mode)
  (package-install 'clojure))

(defun my-clojure-mode-hook ()
  "Custom Clojure settings."
  ;; https://github.com/clojure-emacs/clojure-mode

  (eval-after-load 'clojure-mode
    '(define-key clojure-mode-map (kbd "C-c C-w") 'cider-r)
  ))

(add-hook 'clojure-mode-hook 'my-clojure-mode-hook)



;; CIDER REPL MODE
;;
(defun my-cider-repl-mode-hook ()
  "Custom settings for Clojure REPL mode."

  (define-key cider-repl-mode-map (kbd "C-c C-w") 'cider-repl-clear-buffer)
  )

(add-hook 'cider-repl-mode-hook 'my-cider-repl-mode-hook)



;; COMMON C MODE
;;
(defun my-c-mode-common-hook ()
  "Custom settings for c-mode-common."

  (setq company-backends (delete 'company-semantic company-backends))
  (define-key c-mode-map (kbd "M-o") 'company-complete)
  (define-key c++-mode-map (kbd "M-o") 'company-complete)

  (add-to-list 'company-backends 'company-c-headers)
  (lambda () (add-to-list 'company-c-headers-path-system "/usr/include/"))
  (lambda () (add-to-list 'company-c-headers-path-system "/usr/local/include/"))
  (lambda () (add-to-list 'company-c-headers-path-system "/usr/include/c++/5.4.0/"))
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)



;; C MODE
;;
;; Available C style:
;; "gnu": The default style for GNU projects
;; "k&r": What Kernighan and Ritchie, the authors of C used in their book.
;; "bsd": What BSD developers use, aka "Allman style" after Eric Allman.
;; "whitesmith": Popularized by the examples that came with Whitesmiths C,
;;               an early commercial C compiler.
;; "stroustrup": What Stroustrup, the author of C++ used in his book.
;; "ellemtel": Popular C++ coding standards as defined by "Programming in C++,
;;             Rules and Recommendations,", Erik Nyquist and Mats Henricson,
;;             Ellemtel.
;; "linux": What the Linux developers use for kernel modules.
;; "python": What Python developers use for extension modules.
;; "java": The default style for java-mode (see below)
;; "user": When you want to define your own style

; (add-to-list 'auto-mode-alist '("\\.c\\'" . c-mode))

;; CUSTOMIZABLE INDENTATION SETTINGS :
;; place the point on a line, then press C-c C-o to explore or set the variable
;; that defines the indent offset of that line.
(defvar my-custom-c-style "comsys")

(defun my-c-mode-hook ()
  "Hook for custom c-mode-settings."

  (electric-pair-mode 1) ; automatic pairing of parantheses
  (yas-minor-mode 1)     ; snippets

  ;; CUSTOM FORMATTING SETTINGS - only to be applied when
  ;; a specific c-style is enabled.

  ;; LINUX KERNEL STYLE :
  (when (equal my-custom-c-style "linux")
    (setq-default c-basic-offset 8
		  tab-width 8
		  indent-tabs-mode t)
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'comment-intro 0)
    (c-set-offset 'defun-block-intro 8)
    (c-set-offset 'statement-block-intro 8)
    (c-set-offset 'substatement 8)
    (c-set-offset 'topmost-intro 0)
    (c-set-offset 'statement-cont 0)
    (c-set-offset 'func-decl-cont 0)
    )
  ;; COMSYS CODE STYLE :
  (when (equal my-custom-c-style "comsys")
    (setq-default c-basic-offset 2
		  tab-width 2
		  indent-tabs-mode nil)
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'comment-intro 0)
    (c-set-offset 'defun-block-intro 2)
    (c-set-offset 'statement-block-intro 2)
    (c-set-offset 'substatement 2)
    (c-set-offset 'topmost-intro 0)
    (c-set-offset 'statement-cont 0)
    (c-set-offset 'func-decl-cont 0)
    )
  ;; KUDOS CODE STYLE :
  (when (equal my-custom-c-style "kudos")
	(setq-default c-basic-offset 2
				  tab-width 2
				  indent-tabs-mode nil)
  (c-set-offset 'defun-block-intro 2)
  (c-set-offset 'statement-block-intro 2)
  (c-set-offset 'comment-intro 0)
  (c-set-offset 'func-decl-cont 0)
   )
  )

(add-hook 'c-mode-hook 'my-c-mode-hook)



;; C++ MODE
;;
(defvar c++-default-style "Mine")

(defun my-c++-mode-hook ()
  "Custom hook for `c++-mode' settings."

  (electric-pair-mode 1) ; automatic pairing of parantheses

  (when (equal c++-default-style "Mine")
	(setq-default c-basic-offset 4
		      tab-width 4
		      indent-tabs-mode nil)
	(c-set-offset 'substatement-open 0)
    (c-set-offset 'func-decl-cont 0)
    (c-set-offset 'inline-open 0)
	)
  )

(add-hook 'c++-mode-hook 'my-c++-mode-hook)



;; GLSL MODE
;;
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.shd\\'" . glsl-mode))

(defun my-glsl-mode-hook ()
  "Custom OpenGL shader language(glsl) settings."

  (setq-default c-basic-offset 4
                tab-width 4
                indent-tabs-mode nil)
  (c-set-offset 'defun-block-intro 4)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'statement-block-intro 4)
  (c-set-offset 'knr-argdecl -100) ;; DIRTY HACK ??!
  )

(add-hook 'glsl-mode-hook 'my-glsl-mode-hook)



;; PYTHON MODE
;;
(defun my-python-mode-hook ()
  "Custom hook for `python-mode` settings."
  (add-to-list 'company-backends 'company-jedi)

  (setq-default python-indent-offset 4)
  )

(add-hook 'python-mode-hook 'my-python-mode-hook)



;; F# MODE
;;
(unless (package-installed-p 'fsharp-mode)
  (package-install 'fsharp-mode))

(defun my-fsharp-mode-hook ()
  "Custom hook for `fsharp-mode` settings."
  (require 'fsharp-mode)
  (add-to-list 'load-path "~/.emacs.d/fsharp-mode/")
  (autoload 'fsharp-mode "fsharp-mode"
    "Major mode for editing F# code." t)
  (add-to-list 'auto-mode-alist '("\\.fs[iylx]?$" . fsharp-mode))
  (setq inferior-fsharp-program "/usr/bin/fsharpi --readline-")
  (setq fsharp-compiler "/usr/bin/fsharpc")
  )

(add-hook 'fsharp-mode-hook 'my-fsharp-mode-hook)

;; COMMON COMMANDS :
;; C-c C-t -> display tooltip (for the symbol at point)
;; C-c C-r -> evaluate region
;; C-c C-s -> show interactive buffer
;; C-c C-c -> compile with fsc (fsharpc ?)
;; C-c x   -> run the executable
;; C-c C-q -> quit current background compiler process
;; C-c C-c -> interrupt the interactive mode (useful for long computations
;;            or infinite loops)

;; For key commands more familiar to Visual Studio :
(defun fsharp-visual-studio-key-bindings ()
  "Enables fsharp key-commands similar to Visual Studio key bindings.
M-RET will invoke fsharp-eval-region, C-SPC will invoke fsharp-ac/complete-at-point."
  (interactive)
  (add-hook 'fsharp-mode-hook
            (lambda ()
              (define-key fsharp-mode-map (kbd "M-RET") 'fsharp-eval-region)
              (define-key fsharp-mode-map (kbd "C-SPC") 'fsharp-ac/complete-at-point)
              )))



;; C# MODE
;;
(defun my-c-sharp-mode-hook ()
  "Enable the stuff you want for C# here."
  (electric-pair-mode 1)
  (c-set-offset 'func-decl-cont 0)
  (c-set-offset 'inline-open 0)
  (c-set-offset 'substatement 0)
  (c-set-offset 'substatement-open 0)
  ;; (c-set-offset 'comment-offset 0)
  )

(add-hook 'csharp-mode-hook 'my-c-sharp-mode-hook)



;; LATEX MODE
;;
(defun my-latex-mode-hook ()
  "Hook for custom latex-mode-settings."

  (electric-pair-mode 1) ; automatic pairing of parantheses
  (clean-aindent-mode 0)
  (flyspell-mode 0)
  (setq electric-indent-inhibit 1)
  (setq tex-indent-basic 0)
  (setq tex-indent-item 0)
  )

(add-hook 'latex-mode-hook 'my-latex-mode-hook)



;; GDB MODE
;;
(setq gdb-many-windows t ; use gdb-many-windows by default
      gdb-show-main t)   ; Non-nil means display source file containing
                         ; the main routine at startup.



;; MAKEFILE MODE
;;
;; taken from prelude-c.el:48:
;; https://github.com/bbatsov/prelude/blob/master/modules/prelude-c.el

(defun prelude-makefile-mode-defaults ()
  (whitespace-toggle-options '(tabs))
  (whitespace-mode  0)
  (setq indent-tabs-mode t))

(setq prelude-makefile-mode-hook 'prelude-makefile-mode-defaults)
(add-hook 'makefile-mode-hook (lambda ()
                                (run-hooks 'prelude-makefile-mode-hook)))


(provide 'setup-languages)
;;; setup-languages ends here
