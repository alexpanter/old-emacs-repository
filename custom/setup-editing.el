(provide 'setup-editing)


;; GROUP: Editing -> Killing
(setq global-mark-ring-max 5000             ; increase mark ring to contain 5000 entries
      mark-ring-max 5000                    ; increase kill ring to contain 5000 entries
      mode-require-final-newline t          ; add a newline to end of file
      )

(setq
 kill-ring-max 5000       ; increase kill-ring capacity
 kill-whole-line t        ; if NIL, kill whole line and move the next line up
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Editing -> Editing Basics ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; when point is on a paired paranthesis, the other is highlighted.
;; delay is measured in seconds.
(setq show-paren-delay 0.1)
(show-paren-mode 1)



;; when this mode is active, marked text will be replaced with newly written
(delete-selection-mode)

;; always do newline and indent after <RET> key press
(global-set-key (kbd "RET") 'newline-and-indent)

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customized functions               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(global-set-key (kbd "C-a") 'prelude-move-beginning-of-line)


;; rebind C-x k to kill the active buffer instead of asking which one to kill
(defun kill-default-buffer ()
  "Kill the currently active buffer -- set to C-x k so that users are not asked which buffer they want to kill."
  (interactive)
  (let (kill-buffer-query-functions) (kill-buffer)))

(global-set-key (kbd "C-x k") 'kill-default-buffer)


;; bind C-c i to indent a selected region or, if none selected, the whole buffer
(defcustom prelude-indent-sensitive-modes
  '(coffee-mode python-mode slim-mode haml-mode yaml-mode fsharp-mode)
  "Modes for which auto-indenting is suppressed."
  :type 'list)

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

(global-set-key (kbd "C-c i") 'indent-region-or-buffer)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package: volatile-highlights          ;;
;;                                       ;;
;; GROUP: Editing -> Volatile Highlights ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; dynamically highlights changes with copy/paste operations in the buffer
(require 'volatile-highlights)
(volatile-highlights-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package: Smartparens                  ;;
;;                                       ;;
;; GROUP: Editing -> Smartparens         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; automatically completes missing parantheses or wraps a selected region
;; in the entered open/close parantheses.
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)
(smartparens-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package: clean-aindent-mode               ;;
;;                                           ;;
;; GROUP: Editing -> Indent -> Clean Aindent ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cleans up unused whitespace
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package: undo-tree                  ;;
;;                                     ;;
;; GROUP: Editing -> Undo -> Undo Tree ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; undo tree gives a visual representation (GUI) of recent copy/paste
;; operations. To activate undo-tree press C-x u.
;; Press C-/ to undo.
;; Press C-_ to redo.
(require 'undo-tree)
(global-undo-tree-mode)

