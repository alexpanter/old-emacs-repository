;;; package --- setup-programming
;;;
;;; Commentary:
;;;
;;; Code:




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Programming - General               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-prog-mode-hook ()
  "Settings for general programmin modes.
Will default if not overriden in specific mode hooks."

  (setq indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq company--auto-completion nil)
  (setq-default indent-tabs-mode nil)
  )

(add-hook 'prog-mode-hook 'my-prog-mode-hook)

(require 'speedbar)
(setq speedbar-show-unknown-files t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Programming - Markdown              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Programming - Languages -> Emacs Lisp ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-emacs-lisp-mode-hook ()
  "Custom e-lisp settings."

  )

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Programming -> Tools -> Clojure     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (package-installed-p 'clojure-mode)
  (package-install 'clojure))

(defun my-clojure-mode-hook ()
  "Custom Clojure settings."
  ;; https://github.com/clojure-emacs/clojure-mode

  (eval-after-load 'clojure-mode
    '(define-key clojure-mode-map (kbd "C-c C-w") 'cider-r)
  ))

(add-hook 'clojure-mode-hook 'my-clojure-mode-hook)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Programming -> Tools -> Cider REPL    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-cider-repl-mode-hook ()
  "Custom settings for REPL mode."

  (define-key cider-repl-mode-map (kbd "C-c C-w") 'cider-repl-clear-buffer)
  )

(add-hook 'cider-repl-mode-hook 'my-cider-repl-mode-hook)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Programming - Languages -> c-mode-common ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Available C style:
;; "gnu": The default style for GNU projects
;; "k&r": What Kernighan and Ritchie, the authors of C used in their book.
;; "bsd": What BSD developers use, aka "Allman style" after Eric Allman.
;; "whitesmith": Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; "stroustrup": What Stroustrup, the author of C++ used in his book.
;; "ellemtel": Popular C++ coding standards as defined by "Programming in C++, Rules and Recommendations,"
;;             Erik Nyquist and Mats Henricson, Ellemtel.
;; "linux": What the Linux developers use for kernel modules.
;; "python": What Python developers use for extension modules.
;; "java": The default style for java-mode (see below)
;; "user": When you want to define your own style


;; (defun my-c-mode-common-hook ()
;;   "This hook contain customizations for helm-gtags, company-c-headers."

;;   ;;
;;   ;; Before using ggtags or helm-gtags, remember to create a GTAGS database by
;;   ;; running gtags at your project root in terminal.
;;   ;; $ cd /path/to/project/root
;;   ;; $ gtags
;;   ;;
;;   ;; After this, a few files are created:
;;   ;; $ ls G*
;;   ;; GPATH   GRTAGS  GTAGS
;;   ;;
;;   ;; GPATH:  path name database
;;   ;; GRTAGS: reference database
;;   ;; GTAGS:  definition database
;;   ;;
;;   (setq
;;    helm-gtags-ignore-case t
;;    helm-gtags-auto-update t
;;    helm-gtags-use-input-at-cursor t
;;    helm-gtags-pulse-at-cursor t
;;    helm-gtags-prefix-key "\C-cg"
;;    helm-gtags-suggested-key-mapping t
;;    )

;;   (require 'helm-gtags)
;;   ;; Enable helm-gtags-mode
;;   (add-hook 'dired-mode-hook 'helm-gtags-mode)
;;   (add-hook 'eshell-mode-hook 'helm-gtags-mode)
;;   (add-hook 'c-mode-hook 'helm-gtags-mode)
;;   (add-hook 'c++-mode-hook 'helm-gtags-mode)
;;   (add-hook 'asm-mode-hook 'helm-gtags-mode)

;;   (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
;;   (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
;;   (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
;;   (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
;;   (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
;;   (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

;;   ;; BASIC KEY COMMANDS: (`expression` is defined as a pair or a symbol)
;;   ;; C-M-f runs forward-sexp, move forward over a balanced expression.
;;   ;; C-M-b runs backward-sexp. move backward over a balanced expression.
;;   ;; C-M-k runs kill-sexp. kill balanced expression forward.
;;   ;; C-M-<SPC> or C-M-@ runs mark-sexp, put mark after following expression.
;;   ;; C-M-a runs beginning-of-defun, which moves point to beginning of a function.
;;   ;; C-M-e runs end-of-defun, which moves point to end of a function.
;;   ;; C-M-h runs mark-defun, which put a region around whole current of following function.

;;   ;; To use company-mode with Clang, add this comfiguration:
;;   (setq company-backends (delete 'company-semantic company-backends))
;;   (define-key c-mode-map [(C-tab)] 'company-complete)
;;   (define-key c++-mode-map [(C-tab)] 'company-complete)

;;   ;; To retrieve completion candidates for your projects,
;;   ;; you will have to tell Clang where your include paths are.
;;   ;; Create a file named `.dir-locals.el` at your project root:
;;   ;; ((nil . ((company-clang-arguments . ("-I/home/<user>/project_root/include1/"
;;   ;;                                      "-I/home/<user>/project_root/include2/")))))
;;   ;;
;;   ;; If you put a file with a special name, e.g. `.dir-locals-el` in a directory,
;;   ;; Emacs will read it when it visits any file in that directory or any of its
;;   ;; subdirectories, and apply the settings it specifies to the file's buffer.
;;   ;;
;;   ;; If you use Helm, you can easily insert absolute path by C-c i at the current path
;;   ;; in helm-find-files (which should optimally be bound to C-x C-f).

;;   ;; Header file completion with company-c-headers :
;;   (add-to-list 'company-backends 'company-c-headers)

;;   ;; Allowing source code folding with `hs-minor-mode`, which allows users to fold and
;;   ;; hide blocks of text. Blocks are defined by regular expressions which match the
;;   ;; start and end of a text region. E.g. anything between { and } is a block.
;;   ;; The regular expressions are defined in `hs-special-modes-alist`.
;;   (add-hook 'c-mode-common-hook 'hs-minor-mode)
;;   ;; KEY BINDINGS :
;;   ;; C-c @ C-c -> `hs-toggle-hiding` (toggle hiding/showing of a block)
;;   ;; C-c @ C-h -> `hs-hide-block` (select current block at point and hide it)
;;   ;; C-c @ C-l -> `hs-hide-level` (hide all block with indentation levels below this block)
;;   ;; C-c @ C-s -> `hs-show-block` (select current block at point and show it)
;;   ;; C-c @ C-M-h -> `hs-hide-all` (hide all top level blocks, displaying only first and last lines)
;;   ;; C-c @ C-M-s -> `hs-show-all` (show everything)
;;   (defun demodemo ()
;;     (interactive)
;;     (message "hej med dig"))

;;   ;; (require 'cc-mode)
;;   (require 'semantic)
;;   (global-semanticdb-minor-mode 1)
;;   (global-semantic-idle-scheduler-mode nil)
;;   (semantic-mode 1)
;;   )

;; (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Programming - Languages -> C        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.c\\'" . c-mode))

;; enabling formatting style for c-code :
;; (setq c-default-style "linux")
(setq c-default-style "kudos")

;; CUSTOMIZABLE INDENTATION SETTINGS :
;; place the point on a line, then press C-c C-o to explore or set the variable
;; that defines the indent offset of that line.

(defun my-c-mode-hook ()
  "Hook for custom c-mode-settings."

  (electric-pair-mode 1) ; automatic pairing of parantheses
  (yas-minor-mode 1)     ; snippets

  ;; CUSTOM FORMATTING SETTINGS - only to be applied when
  ;; a specific c-style is enabled.

  ;; LINUX KERNEL STYLE :
  (when (equal c-default-style "linux")
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
  (when (equal c-default-style "comsys")
    (setq-default c-basic-offset 4
		  tab-width 4
		  indent-tabs-mode t)
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'comment-intro 0)
    (c-set-offset 'defun-block-intro 4)
    (c-set-offset 'statement-block-intro 4)
    (c-set-offset 'substatement 4)
    (c-set-offset 'topmost-intro 0)
    (c-set-offset 'statement-cont 0)
    (c-set-offset 'func-decl-cont 0)
    )
  ;; KUDOS CODE STYLE :
  (when (equal c-default-style "kudos")
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



;;   ;; when enabled, this mode displays function interface in the minibuffer.
;;   ;; Not practical for C++ though, since only one interface can be shown at
;;   ;; a time, which is impractical for overloaded functions, e.g..
;;   (global-semantic-idle-summary-mode 1)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Programming - Languages -> C++      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar c++-default-style "Mine")

(defun my-c++-mode-hook ()
  "Custom hook for `c++-mode' settings."
  ;; (lambda () (add-to-list 'company-c-headers-path-system "/usr/include/c++/5/"))

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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Programming - Languages -> GLSL       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Programming - Languages -> Python   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-python-mode-hook ()
  "Custom hook for `python-mode` settings."
  (add-to-list 'company-backends 'company-jedi)

  (setq-default python-indent-offset 4)
  )

(add-hook 'python-mode-hook 'my-python-mode-hook)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Programming - Languages -> F#       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (package-installed-p 'fsharp-mode)
              (package-install 'fsharp-mode))

(add-hook 'fsharp-mode-hook
          (lambda ()
            (require 'fsharp-mode)
            (add-to-list 'load-path "~/.emacs.d/fsharp-mode/")
            (autoload 'fsharp-mode "fsharp-mode"
              "Major mode for editing F# code." t)
            (add-to-list 'auto-mode-alist '("\\.fs[iylx]?$" . fsharp-mode))
            (setq inferior-fsharp-program "/usr/bin/fsharpi --readline-")
            (setq fsharp-compiler "/usr/bin/fsharpc")
            ))
;; COMMON COMMANDS :
;; C-c C-t -> display tooltip (for the symbol at point)
;; C-c C-r -> evaluate region
;; C-c C-s -> show interactive buffer
;; C-c C-c -> compile with fsc (fsharpc ?)
;; C-c x   -> run the executable
;; C-c C-q -> quit current background compiler process
;; C-c C-c -> interrupt the interactive mode (useful for long computations or infinite loops)

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Programming - Languages -> C#       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Programming - Languages -> LaTeX    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Programming - Languages -> Fasto    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'fasto-mode)
(add-to-list 'auto-mode-alist '("\\.fo\\'" . fasto-mode))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Programming -> Tools -> Gdb         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq gdb-many-windows t ; use gdb-many-windows by default
      gdb-show-main t)   ; Non-nil means display source file containing the main routine at startup




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Programming -> Tools -> Compilation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation from Emacs
(defun prelude-colorize-compilation-buffer ()
  "Colorize a compilation mode buffer."
  (interactive)
  ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

;; setup compilation-mode used by `compile` command
(require 'compile)
(setq compilation-ask-about-save nil          ; Just save before compiling
      compilation-always-kill t               ; Just kill old compile processes before starting the new one
      compilation-scroll-output 'first-error) ; Automatically scroll to first

(defun my-compile-function ()
  "Custom function to define standard compile settings relative to active `major-mode`."
  (interactive)
  (defvar filename)
  (defvar comp-flags)
  (defvar output)

  (setq filename (file-name-nondirectory buffer-file-name))

  ;; latex-mode
  (when (member major-mode '(latex-mode tex-mode))
    (setq compile-command (concat "pdflatex " filename)))

  ;; c-mode
  (when (member major-mode '(c-mode))
    (setq comp-flags "-std=gnu11 -Wall -Werror -pedantic ")
    (setq output (concat "-o " (file-name-sans-extension filename) " "))
    (setq compile-command (concat "gcc " comp-flags output filename)))

  ;; c++-mode
  (when (member major-mode '(c++-mode))
    (setq comp-flags "-std=c++11 ")
    (setq output (concat "-o " (file-name-sans-extension filename) " "))
    (setq compile-command (concat "g++ " comp-flags output filename)))

  ;; fsharp-mode
  (when (member major-mode '(fsharp-mode))
    (setq compile-command (concat "fsharpc " filename)))

  ;; csharp-mode
  (when (member major-mode '(csharp-mode))
    (setq compile-command (concat "mcs " filename)))

  ;; COMPILE:
  (call-interactively 'compile)
  )

(global-set-key (kbd "<f5>") 'my-compile-function)

(defun my-make-compile-function ()
  "Custom function to compile using a local `Makefile`, if such a file exists."
  (interactive)
  (setq compile-command "make")
  (call-interactively 'compile)
  )

(global-set-key (kbd "<f6>") 'my-make-compile-function)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Programming -> Tools -> Makefile    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; taken from prelude-c.el:48: https://github.com/bbatsov/prelude/blob/master/modules/prelude-c.el
(defun prelude-makefile-mode-defaults ()
  (whitespace-toggle-options '(tabs))
  (whitespace-mode  0)
  (setq indent-tabs-mode t))

(setq prelude-makefile-mode-hook 'prelude-makefile-mode-defaults)
(add-hook 'makefile-mode-hook (lambda ()
                                (run-hooks 'prelude-makefile-mode-hook)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Programming -> Tools -> Ediff       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: diff-hl                             ;;
;;                                              ;;
;; GROUP: Programming -> Tools -> Vc -> Diff Hl ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-diff-hl-mode)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: magit                             ;;
;;                                            ;;
;; GROUP: Programming -> Tools -> Magit       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'magit)
(setq-default magit-stage-all-confirm nil)
(add-hook 'magit-mode-hook 'magit-load-config-extensions)

;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(global-unset-key (kbd "C-x g"))
(global-set-key (kbd "C-x g h") 'magit-log)
(global-set-key (kbd "C-x g f") 'magit-file-log)
(global-set-key (kbd "C-x g b") 'magit-blame-mode)
(global-set-key (kbd "C-x g m") 'magit-branch-manager)
(global-set-key (kbd "C-x g c") 'magit-branch)
(global-set-key (kbd "C-x g s") 'magit-status)
(global-set-key (kbd "C-x g r") 'magit-reflog)
(global-set-key (kbd "C-x g t") 'magit-tag)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: flyckeck                          ;;
;;                                            ;;
;; GROUP: Programming -> Tools -> Flycheck    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: flycheck-tip                                      ;;
;;                                                            ;;
;; GROUP: Flycheck Tip, but just consider it part of Flycheck ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flycheck-tip)
(flycheck-tip-use-timer 'verbose)



(provide 'setup-programming)
;;; setup-programming.el ends here
