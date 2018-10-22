;;;
;;; Before using ggtags or helm-gtags, remember to create a GTAGS database by
;;; running gtags at your project root in terminal.
;;; $ cd /path/to/project/root
;;; $ gtags -c
;;;
;;; The flag `-c' indicates that compact database format is used (saves a lot
;;; of hard disk space). After this, a few files are created:
;;; $ ls G*
;;; GPATH   GRTAGS  GTAGS
;;;
;;; GPATH:  path name database
;;; GRTAGS: reference database
;;; GTAGS:  definition database
;;;
;;;
;;; Usage:
;;;
;;; `expression` is defined as a pair or a symbol:
;;; C-M-f runs forward-sexp, move forward over a balanced expression.
;;; C-M-b runs backward-sexp. move backward over a balanced expression.
;;; C-M-k runs kill-sexp. kill balanced expression forward.
;;; C-M-<SPC> or C-M-@ runs mark-sexp, put mark after following expression.
;;; C-M-a runs beginning-of-defun, which moves point to beginning of a function.
;;; C-M-e runs end-of-defun, which moves point to end of a function.
;;; C-M-h runs mark-defun, which put a region around whole current of following function.
;;;
;;;
;;; Clang backend:
;;;
;;; To use company-mode with Clang, add this comfiguration:
;;; (setq company-backends (delete 'company-semantic company-backends))
;;; (define-key c-mode-map [(C-tab)] 'company-complete)
;;; (define-key c++-mode-map [(C-tab)] 'company-complete)
;;;
;;; To retrieve completion candidates for your projects,
;;; you will have to tell Clang where your include paths are.
;;; Create a file named `.dir-locals.el` at your project root:
;;; ((nil . ((company-clang-arguments . ("-I/home/<user>/project_root/include1/"
;;;                                      "-I/home/<user>/project_root/include2/")))))
;;;
;;; If you put a file with a special name, e.g. `.dir-locals-el` in a directory,
;;; Emacs will read it when it visits any file in that directory or any of its
;;; subdirectories, and apply the settings it specifies to the file's buffer.
;;;
;;; If you use Helm, you can easily insert absolute path by C-c i at the current path
;;; in helm-find-files (which should optimally be bound to C-x C-f).
;;;
;;;
;;; Code:
;;;



;; this variables must be set before load helm-gtags
;; you can change to any prefix key of your choice
(setq helm-gtags-prefix-key "\C-cg")

(use-package helm-gtags
  :init
  (progn
    (setq helm-gtags-ignore-case t
          helm-gtags-auto-update t
          helm-gtags-use-input-at-cursor t
          helm-gtags-pulse-at-cursor t
          helm-gtags-prefix-key "\C-cg"
          helm-gtags-suggested-key-mapping t)

    ;; Enable helm-gtags-mode in Eshell for the same reason as above
    (add-hook 'eshell-mode-hook 'helm-gtags-mode)

    ;; Enable helm-gtags-mode in languages that GNU Global supports
    (add-hook 'c-mode-hook 'helm-gtags-mode)
    (add-hook 'c++-mode-hook 'helm-gtags-mode)
    (add-hook 'java-mode-hook 'helm-gtags-mode)
    (add-hook 'asm-mode-hook 'helm-gtags-mode)

    ;; key bindings
    (with-eval-after-load 'helm-gtags
      (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
      (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
      (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
      (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
      (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
      (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history))))



(provide 'setup-helm-gtags)
;;; setup-helm-gtags ends here
