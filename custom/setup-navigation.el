;;; package --- Summary
;;;
;;; Commentary:
;;;
;;; Code:


;; EASY WINDOW NAVIGATION
;;
;; press S-{left,right,up,down} to switch window
(windmove-default-keybindings)  ; causes problems with shift used to mark text



;; EASY BUFFER NAVIGATION
;;
;; press C-{left,right} to switch buffers
(defun bufmove-default-keybindings (&optional modifier)
  "Set up keybindings for switching between buffers in current window."
  (interactive)
  (unless modifier (setq modifier 'control))
  (global-set-key (vector (list modifier 'left)) 'previous-buffer)
  (global-set-key (vector (list modifier 'right)) 'next-buffer)
  (global-set-key (kbd "C-<left>") 'previous-buffer)
  (global-set-key (kbd "C-<right>") 'next-buffer)
  )

(bufmove-default-keybindings)



;; EASY MANUAL RESIZE OF WINDOWS
;;
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


(provide 'setup-navigation)
;;; setup-navigation ends here
