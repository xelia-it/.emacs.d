;;; package --- Emacs configuration with batteries included

;;; ----------------------------------------------------------------------------
;;; Commentary:

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
  ;; Set default values as above
  (setq whitespace-line-column 80)
  (setq whitespace-tab-width 4)
  (global-whitespace-mode t)
  ;; make whitespace-mode use just basic coloring
  (setq whitespace-style (quote
                          (face spaces tabs newline
                                ;;space-mark tab-mark newline-mark
                                empty tabs lines-tail trailing)))
  (set-face-attribute 'whitespace-space nil :background nil :foreground "gray30")
  (set-face-attribute 'whitespace-newline nil :background nil :foreground "gray30")
  (set-face-attribute 'whitespace-tab nil :background nil :foreground "gray30")
  )

;; TODO: explore mark for space, tabs and newline
;; all numbers are Unicode codepoint in decimal. ⁖ (insert-char 182 1)
;;(setq whitespace-display-mappings
;;  '(
;;    (space-mark 32 [183] [46]) ; 32 SPACE 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
;;    (newline-mark 10 [182 10]) ; 10 LINE FEED
;;    (tab-mark 9 [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
;;    ))
;;

;; Show indent guides
(use-package highlight-indent-guides
  :ensure t
  :config
  ;; Activate indent guides for all programming languages
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?\┆)
  )

;; -----------------------------------------------------------------------------
;; Search and replace

(use-package helm
  :ensure t)

;; A better search package that works with helm
(use-package helm-swoop
  :ensure t
  :after (helm))

;; -----------------------------------------------------------------------------
;; Miscellaneous

;; Setup multiple cursors. With this package you can have multiple cursors
;; and type the same data into multiple positions at the same time
;; Ctrl-D is used by Emacs su we unset it before binding.
(use-package multiple-cursors
  :ensure t
  :config
  (setq mc/always-run-for-all t)
  ;; will make <return> insert a newline; multiple-cursors-mode can still
  ;; be disabled with C-g / ESC
  (define-key mc/keymap (kbd "<return>") nil)
  )

;; Package to easy select region
(use-package expand-region
  :ensure t)

;; Move and copy line like Eclipse and Netbeans
(use-package move-dup
  :ensure t
  :config
  (global-move-dup-mode))

(provide 'init-editing)
;;; init-editing.el ends here
