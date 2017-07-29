;;; package --- Summary
;;;
;;; Commentary:
;;;
;;; Code:



;; INHIBIT STARTUP SCREEN
;;
(setq inhibit-startup-screen t)



;; REMOVE MENUS AND BARS
;;
;; you won't need any of the bar thingies
;; turn it off to save screen estate
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))



;; MODE LINE
;;
;; Hide the ridiculously long list of activated minor modes.
(use-package rich-minority
  :init
  (rich-minority-mode 1)
   (setf rm-blacklist ""))



;; FRAME TITLE
;;
;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
;; taken from prelude-ui.el, modified a little to capitalize "Emacs".
(defvar frame-custom-title (capitalize(invocation-name)))
;;
(setq frame-title-format
      '("" frame-custom-title " - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))



;; NO BLINKING CURSOR
;;
;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)



;; BETTER SCROLLING THOUGH FILE
;;
;; cursor at the top line and pressing key up won't move the file
;; a ridiculous amount of lines upwards.
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position nil)



;; BUFFER SIZE
;;
;; display the buffer size in the mode line
(size-indication-mode t)



;; HL-LINE MODE
;;
;; toggle line highlighting in all buffers
(global-hl-line-mode)



;; LINUM MODE
;;
;; enable only in programming modes
(add-hook 'prog-mode 'linum-mode
          (setq-default linum-format "%d "))
; (global-linum-mode t)



;; COLUMN NUMBERS
;;
;; show column numbers in the mode line
(column-number-mode)



;; BATTERY INFORMATION
;;
;; display battery charge in mode line
(display-battery-mode)



;; HIGHLIGHT NUMBERS
;;
(add-hook 'prog-mode-hook 'highlight-numbers-mode)



;; HIGHLIGHT MATCHING PARANTHESES
;;
;; when point is on a paired paranthesis, the other is highlighted.
;; delay is measured in seconds.
(setq show-paren-delay 0.1)
(show-paren-mode 1)



;; INCONSOLATA FONT
;;
;; change font to inconsolata for better looking text
;; remember to install the font Inconsolata first
(setq default-frame-alist '((font . "Inconsolata-10")))
;; set italic font for italic face, since Emacs does not set italic
;; face automatically
(set-face-attribute 'italic nil
                    :family "Inconsolata-Italic")
(set-face-attribute 'default (selected-frame)
					:height 98)



(provide 'setup-appearance)
;;; setup-appearance ends here
