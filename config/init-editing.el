;;; ----------------------------------------------------------------------------
;;; Code:

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

;; Delete trailing whitespaces before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)

;; -----------------------------------------------------------------------------

;; Auto-save before compiling
(setq compilation-ask-about-save nil)

;; Never prompt to kill a compilation session.
(setq compilation-always-kill t)

;; Always scroll to the bottom.
(setq compilation-scroll-output t)

;; -----------------------------------------------------------------------------

;; Guides and whitespaces

;; Show trailing whitespaces, tabs, lines
(use-package whitespace
  :ensure t
  :config
  (global-whitespace-mode t)
  )

(setq whitespace-line-column 80)
(setq whitespace-tab-width 4)
;;(setq whitespace-display-mappings '(
;;    (space-mark   ?\ [?\u00B7] [?.])                 ; space - centered dot
;;    (space-mark   ?\xA0 [?\u00A4] [?_])            ; hard space - currency
;;    (newline-mark ?\n [?$ ?\n])                      ; eol - dollar sign
;;    (tab-mark     ?\t [?\u00BB ?\t] [?\\ ?\t])       ; tab - left quote mark
;;    ))
(setq whitespace-style '(face empty tabs lines-tail trailing))
;;(setq whitespace-style '(face tabs newline space-mark tab-mark newline-mark))

;; Show indent guides
(use-package highlight-indent-guides :ensure t)

(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-character ?\┆)

;; -----------------------------------------------------------------------------

;; Setup multiple cursors. With this package you can have multiple cursors
;; and type the same data into multiple positions at the same time
;; Ctrl-D is used by Emacs su we unset it before binding.

(use-package multiple-cursors
  :ensure t
  :config
  (setq mc/always-run-for-all t))

;; Move and copy line like Eclipse and Netbeans
(use-package move-dup
  :ensure t
  :config
  (global-move-dup-mode))

(use-package helm-swoop
  :ensure t
  :after (helm))

(provide 'init-editing)
;;; init-editing.el ends here
