;;; package --- Summary
;;;
;;; Commentary:
;;;
;;; Code:



;; AUTO REVERT
;;
;; update any change made on file to the current buffer
(global-auto-revert-mode)



;; SMARTPARENS
;;
;; automatically completes missing parantheses or wraps a selected region
;; in the entered open/close parantheses.
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
;;
(use-package smartparens
  :init
  (smartparens-mode t))
;;



;; SHOW TRAILING WHITESPACE
;;
;; whenever you create useless whitespace, the whitespace is highlighted
(add-hook 'prog-mode-hook
          (lambda ()
            (interactive)
            (setq show-trailing-whitespace 1)))



;; DELETE TRAILING WHITE SPACE
;;
;; on save, automatically strip file of trailing white space
;; (add-hook 'after-save-hook
;;           (lambda ()
;;             (delete-trailing-whitespace)))



;; WHITESPACE MODE
;;
;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)
;;
;; show important whitespace in diff-mode
(add-hook 'diff-mode-hook
          (lambda ()
            (setq-local whitespace-style
                        '(face
                          tabs
                          tab-mark
                          spaces
                          space-mark
                          trailing
                          indentation::space
                          indentation::tab
                          newline
                          newline-mark))
            (whitespace-mode 1)))



;; UNDO TREE
;;
;; undo tree gives a visual representation (GUI) of recent copy/paste
;; operations. To activate undo-tree press C-x u.
;; Press C-/ to undo.
;; Press C-_ to redo.
(use-package undo-tree
  :init
  (global-undo-tree-mode))



;; EXPAND REGION
;;
(use-package expand-region
  :init
  (global-set-key (kbd "M-m") 'er/expand-region))



;; COMMENT/OUTCOMMENT REGION
;;
;; SOME MAGICAL HOTFIX THAT ACTUALLY WORKS !!!
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)



;; INSERT SPECIAL CHARS
;;
;; SOME CHARS THAT NEEDED MANUAL FUNCTIONS AFTER UBUNTU 18 INSTALLATION
(global-set-key (kbd "<dead-circumflex>") (lambda ()
                                            (interactive) (insert-char 94)))
(global-set-key (kbd "<dead-tilde>") (lambda ()
                                       (interactive) (insert-char 126)))



;; DELETE SELECTION
;;
;; when this mode is active, marked text will be replaced with newly written
(delete-selection-mode)



;; VOLATILE HIGHLIGHTS
;;
;; dynamically highlights changes with copy/paste operations in the buffer
(use-package volatile-highlights
  :init
  (volatile-highlights-mode t))



;; NEWLINE AND INDENT
;; always do newline and indent after <RET> key press
(global-set-key (kbd "RET") 'newline-and-indent)



;; CLEAN AINDENT
;;
;; cleans up unused whitespace
(use-package clean-aindent-mode
  :init
  (add-hook 'prog-mode-hook 'clean-aindent-mode))



;; INDENT REGION OR BUFFER
;;
;; bind C-c i to indent a selected region or, if none selected, the whole buffer
(defcustom prelude-indent-sensitive-modes
  '(coffee-mode python-mode slim-mode haml-mode yaml-mode fsharp-mode)
  "Modes for which auto-indenting is suppressed."
  :type 'list)
;;
(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member major-mode prelude-indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (indent-buffer)
          (message "Indented buffer.")))
      (whitespace-cleanup))))
;;
(global-set-key (kbd "C-c i") 'indent-region-or-buffer)



;; MOVE TO BEGINNING OF LINE/TEXT
;;
;; rebind C-a to move to the beginning of text instead of beginning of line
(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
;;
(global-set-key (kbd "C-a") 'prelude-move-beginning-of-line)



;; HIGHLIGHT SYMBOL AT-POINT
;;
;; hover with mouse over a symbol or definition and press
;; C-S-mouse-1 and see the definition highlighted all through
;; the file. Really useful.
(require 'highlight-symbol)
;;
(highlight-symbol-nav-mode)
;;
(add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
(add-hook 'org-mode-hook (lambda () (highlight-symbol-mode)))
;;
(setq highlight-symbol-idle-delay 0.2
      highlight-symbol-on-navigation-p t)
;;
(global-set-key [(control shift mouse-1)]
                (lambda (event)
                  (interactive "e")
                  (defvar last-point)
                  (setq last-point (point))
                  (goto-char (posn-point (event-start event)))
                  (highlight-symbol-at-point)
                  (goto-char last-point)))
;;
(global-set-key (kbd "M-n") 'highlight-symbol-next)
(global-set-key (kbd "M-p") 'highlight-symbol-prev)



;; UTF-8 EDITING
;;
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)



;; KILL RING
;;
(setq global-mark-ring-max 5000      ; increase mark ring to contain 5000 entries
      mark-ring-max 5000             ; increase kill ring to contain 5000 entries
      mode-require-final-newline t   ; add a newline to end of file
      )
;;
(setq
 kill-ring-max 5000   ; increase kill-ring capacity
 kill-whole-line t    ; if NIL, kill whole line and move the next line up
 )



;; KILL ACTIVE BUFFER
;;
;; rebind C-x k to kill the active buffer instead of asking which one to kill
(defun kill-default-buffer ()
  "Kill the currently active buffer -- set to C-x k so that users are not asked
which buffer they want to kill."
  (interactive)
  (let (kill-buffer-query-functions) (kill-buffer)))
;;
(global-set-key (kbd "C-x k") 'kill-default-buffer)



;; KILL SEVERAL BUFFERS
;;
;; kill several buffers at the same time, asking for confirmation.
(global-set-key (kbd "C-x C-k") 'kill-some-buffers)



;; FLYSPELL
;;
(if (executable-find "aspell")
    (progn
      (setq ispell-program-name "aspell")
      (setq ispell-extra-args '("--sug-mode=ultra")))
  (setq ispell-program-name "ispell"))
;;
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)



;; FLYCHECK
;;
(use-package flycheck
  :init
  (global-flycheck-mode)
  (setq flycheck-c/c++-clang-executable "/usr/bin/clang++-6.0")
  (setq flycheck-clang-language-standard "c++17"))

(use-package flycheck-tip)


(provide 'setup-editing)
;;; setup-editing ends here
