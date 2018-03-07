;; -----------------------------------------------------------------------------

;; Cut-Paste like Windows
(cua-mode t)
;; Standard Windows behaviour
(setq cua-keep-region-after-copy t)
;; Don't tabify after rectangle commands
(setq cua-auto-tabify-rectangles nil)

;; Backup files copying them into a subdirectory
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; Typed text deletes selected text
(delete-selection-mode t)

;; No region when it is not highlighted
(transient-mark-mode 1)

;; Do not use tabs by default
(setq-default indent-tabs-mode nil)

;; Use 4 spaces by default
(setq-default tab-width 4)

;; Do not indent using tab
(setq-default indent-tabs-mode nil)

;; Auto-save before compiling
(setq compilation-ask-about-save nil)

;; Never prompt to kill a compilation session.
(setq compilation-always-kill t)

;; Always scroll to the bottom.
(setq compilation-scroll-output t)


;; Show trailing whitespaces, tabs, lines
;;(use-package whitespace :ensure t)
;;(global-whitespace-mode t)
;;(setq whitespace-style '(face empty tabs lines-tail trailing))

;; Delete trailing whitespaces before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)

;; -----------------------------------------------------------------------------

;; Setup multiple cursors. With this package you can have multiple cursors
;; and type the same data into multiple positions at the same time
;; Ctrl-D is used by Emacs su we unset it before binding.

(use-package multiple-cursors
  :ensure t)

;; Move and copy line like Eclipse and Netbeans
(use-package move-dup
  :ensure t
  :config
  (global-move-dup-mode))

(use-package helm-swoop
  :ensure t
  :after (helm))
