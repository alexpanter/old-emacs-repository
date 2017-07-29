;;; package --- Summary
;;;
;;; Commentary:
;;;
;;; Code:



;; YES-OR-NO
;;
;; y or n is enough when prompting, e.g. when installing packages
(defalias 'yes-or-no-p 'y-or-n-p)



;; IBUFFER
;;
;; always display ibuffer in another window
(setq ibuffer-use-other-window t)
;;
;; ibuffer-vc - package to show version control system status
;; in the ibuffer.
(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-vc-set-filter-groups-by-vc-root)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))
;;
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



;; PROJECTILE
;;
;; project interaction library for Emacs.
;; git-projects are considered projectile projects by default.
;; a project can be marked as projectile-project by adding an
;; empty .projectile file in the projects root folder.
;; see "https://github.com/bbatsov/projectile" for more info.
(projectile-global-mode)



(provide 'setup-convenience)
;;; setup-convenience ends here
