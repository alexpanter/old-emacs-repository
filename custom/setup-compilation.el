;;; package --- setup-compilation
;;;
;;; Commentary:
;;;
;;; Code:



;; CUSTOMIZING THE COMPILATION BUFFER
;;
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



;; COMPILE SOURCE FILE
;;
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
    (setq comp-flags "-std=c++17 ")
    (setq output (concat "-o " (file-name-sans-extension filename) " "))
    (setq compile-command (concat "clang++-6.0 " comp-flags output filename)))

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



;; COMPILE FROM MAKEFILE
;;
(defun my-make-compile-function ()
  "Custom function to compile using a local `Makefile`, if such a file exists."
  (interactive)
  (setq compile-command "make")
  (call-interactively 'compile)
  )

(global-set-key (kbd "<f6>") 'my-make-compile-function)



(provide 'setup-compilation)
;;; setup-compilation ends here
