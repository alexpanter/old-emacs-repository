(provide 'setup-convenience)

;; GROUP: Convenience -> Revert


;; update any change made on file to the current buffer
(global-auto-revert-mode)


;; GROUP: Convenience -> Hippe Expand
;; hippe-expand is a better version of dabbrev-expand.
;; While dabbrev-expand searches for words you already typed in current
;; buffer and other buffers, hippe-expand includes for sources,
;; such as filenames, kill ring...
(global-set-key (kbd "M-/") 'hippe-expand) ;; replace dabbrev-expand
(setq
 hippe-expand-try-functions-list
 '(try-expand-dabbrev ;; Try to expand word "dynamically", searching the current buffer
   try-expand-dabbrev-all-buffers ;; Try to expand word "dynamically", searching all other buffers.
   try-expand-dabbrev-from-kill ;; Try to expand word "dynamically", searching the kill ring.
   try-complete-file-name-partially ;; Try to complete text as a file name, as many characters as unique.
   try-complete-file-name ;; Try to complete text as a file name.
   try-expand-all-abbrevs ;; Try to expand word before point according to all abbrev tables.
   try-expand-list ;; Try to complete the current line to an entire line in the buffer
   try-expand-line ;; Try to complete the current line to an entire line in the buffer.
   try-complete-lisp-symbol-partially ;; Try to complete as an Emacs Lisp symbol, as many characters as unique.
   try-complete-lisp-symbol ;; Try to complete word as an Emacs Lisp symbol.
   ))


;; GROUP: Convenience -> HL Line
(global-hl-line-mode) ;; toggle line highlighting in all buffers

;; write `list-colors-display` to see a list of Emacs colors.
(set-face-background hl-line-face "#202120")





;; y or n is enough when prompting, e.g. when installing packages
(defalias 'yes-or-no-p 'y-or-n-p)


;; GROUP: Convenience -> Ibuffer
(setq ibuffer-use-other-window t) ;; always display ibuffer in another window


;; GROUP: Convenience -> Linum
;; enable only in programming modes
(add-hook 'prog-mode 'linum-mode
          (setq-default linum-format "%d "))
(global-linum-mode t)


;; GROUP: Convenience -> Whitespace
;; whenever you create useless whitespace, the whitespace is highlighted
(add-hook 'prog-mode-hook
          (lambda ()
            (interactive)
            (setq show-trailing-whitespace 1)))

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)


;; GROUP: Convenience -> windmove
;; easier window navigation
;; press S-{left,right,up,down} to switch window
(windmove-default-keybindings)  ; causes problems with shift used to mark text


;; GROUP: Convenienve -> bufmove
;; easier buffer navigation
;; press C-{left,right} to switch buffers
(defun bufmove-default-keybindings (&optional modifier)
  "Set up keybindings for switching between buffers in current window."
  (interactive)
  (unless modifier (setq modifier 'control))
  (global-set-key (vector (list modifier 'left)) 'previous-buffer)
  (global-set-key (vector (list modifier 'right)) 'next-buffer)
  )

(bufmove-default-keybindings)


;; GROUP: Convenience -> win-resize
;; easier manual resize of windows
;; press M-{left,right,up,down} to resize active window
(defun win-resize-top-or-bot ()
  "Figure out if the current window is on top, bottom or in the middle"
  (let* ((win-edges (window-edges))
	 (this-window-y-min (nth 1 win-edges))
	 (this-window-y-max (nth 3 win-edges))
	 (fr-height (frame-height)))
    (cond
     ((eq 0 this-window-y-min) "top")
     ((eq (- fr-height 0) this-window-y-max) "bot")
     (t "mid"))))

(defun hej ()
  (interactive)
  (message (win-resize-top-or-bot)))

(defun win-resize-left-or-right ()
  "Figure out if the current window is to the left, right or in the middle"
  (let* ((win-edges (window-edges))
	 (this-window-x-min (nth 0 win-edges))
	 (this-window-x-max (nth 2 win-edges))
	 (fr-width (frame-width)))
    (cond
     ((eq 0 this-window-x-min) "left")
     ((eq (+ fr-width 2) this-window-x-max) "right")
     (t "mid"))))

(defun win-resize-default-left ()
  "When resizing a window with M-<Left>."
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (shrink-window-horizontally 1))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally 1))
   ;; I have not yet figured out how "middle" case should work!
      )
  )

(defun win-resize-default-right ()
  "When resizing a window with M-<Right>."
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally 1))
   ((equal "right" (win-resize-left-or-right)) (shrink-window-horizontally 1))
   ;; I have not yet figured out how "middle" case should work!
   )
  )

(defun win-resize-default-up ()
  "When resizing a window with M-<Up>."
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (shrink-window 1))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window 1))
   ;; I have not yet figured out how "middle" case should work!
   )
  )

(defun win-resize-default-down ()
  "When resizing a window with M-<Up>."
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window 1))
   ((equal "bot" (win-resize-top-or-bot)) (shrink-window 1))
   ;; I have not yet figured out how "middle" case should work!
   )
  )

(defun win-resize-default-keybindings (&optional modifier)
  "Set up keybindings for interactively resizing the current window."
  (interactive)
  (unless modifier (setq modifier 'meta))
  (global-set-key (vector (list modifier 'left)) 'win-resize-default-left)
  (global-set-key (vector (list modifier 'right)) 'win-resize-default-right)
  (global-set-key (vector (list modifier 'up)) 'win-resize-default-up)
  (global-set-key (vector (list modifier 'down)) 'win-resize-default-down)
  )

(win-resize-default-keybindings)


;; GROUP: Convenience -> comment-region
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: company              ;;
;;                               ;;
;; GROUP: Convenience -> Company ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; Useful commands:
;;
;; Search through completion list with C-s, C-r, and C-o.
;; Move up and down with M-n and M-p.
;; Press <f1> to display documentation for the selected candidate,
;; or C-w to see its source.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package: expand-region                       ;;
;;                                              ;;
;; GROUP: Convenience -> Abbreviation -> Expand ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'expand-region)
(global-set-key (kbd "M-m") 'er/expand-region)


;; ibuffer-vc - package to show version control system status
;; in the ibuffer.
(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-vc-set-filter-groups-by-vc-root)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))

(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (vc-status 16 16 :left)
              " "
              filename-and-process)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES: projectile              ;;
;;                                   ;;
;; GROUP: Convenience -> Projectile  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; project interaction library for Emacs.
;; git-projects are considered projectile projects by default.
;; a project can be marked as projectile-project by adding an
;; empty .projectile file in the projects root folder.
;; see "https://github.com/bbatsov/projectile" for more info.
(projectile-global-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: function-args               ;;
;;                                      ;;
;; GROUP: Convenience -> Navigation     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'function-args)
(fa-config-default)

;; useful shortcuts:
;;
;; commands (M-x) starting with either 'moo-' or 'fa-'
;; belong to this awesome package.
;;
;; M-i/M-u : show/hide function signature
;; M-o : complete at point


