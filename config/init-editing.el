;;; package --- Emacs configuration with batteries included

;;; ---------------------------------------------------------------------------------
;;; Commentary:

;;; ----------------------------------------------------------------------------
;;; Code:

;; -----------------------------------------------------------------------------
;; Basic editing preferences

;; Cut-Paste like Windows
(cua-mode t)
;; Standard Windows behaviour
(setq-default cua-keep-region-after-copy t)
;; Don't tabify after rectangle commands
(setq-default cua-auto-tabify-rectangles nil)

;; Typed text deletes selected text
(delete-selection-mode t)

;; No region when it is not highlighted
(transient-mark-mode 1)

;; Do not use tabs by default
(setq-default indent-tabs-mode nil)

;; Use 4 spaces by default
(setq-default tab-width 4)
(setq-default ruby-indent-level 2)
(setq-default css-indent-offset 4)

;; -----------------------------------------------------------------------------
;; Backup files

;; Backup files copying them into a subdirectory
(setq backup-directory-alist `(("." . "~/.saves")))

(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )

;; Save without messages
(setq-default save-silently t)

;; Delete trailing whitespaces before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)

;; -----------------------------------------------------------------------------
;; Compiling related options

;; Auto-save before compiling
(setq compilation-ask-about-save nil)

;; Never prompt to kill a compilation session.
(setq-default compilation-always-kill t)

;; Always scroll to the bottom.
(setq-default compilation-scroll-output t)

;; -----------------------------------------------------------------------------
;; Guides and whitespaces

;; Do not word-wrap lines
(setq-default truncate-lines t)

;; Fix mouse scroll
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; Show trailing whitespaces, tabs, lines
(use-package whitespace
  :ensure t
  :defer t
  :config
  ;; Set default values as above
  (setq whitespace-line-column 80)
  (setq-default whitespace-tab-width 4)
  (add-hook 'prog-mode-hook 'whitespace-mode)

  ;; make whitespace-mode use just basic coloring
  (setq whitespace-style (quote
	                 (face spaces tabs newline
                               ;; space-mark
			       tab-mark newline-mark
                                empty tabs lines-tail trailing)))
  (set-face-attribute 'whitespace-space nil :background nil :foreground "#3C4350")
  (set-face-attribute 'whitespace-newline nil :background nil :foreground "#3C4350")
  (set-face-attribute 'whitespace-tab nil :background nil :foreground "#3C4350")
  (set-face-attribute 'whitespace-line nil :background "#2C323C" :foreground nil)
  (set-face-attribute 'whitespace-trailing nil :background nil :foreground "#3C4350")

  (setq whitespace-display-mappings
  '(
    (space-mark 32 [183] [46]) ; 32 SPACE 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
    (newline-mark 10 [182 10]) ; 10 LINE FEED
    (tab-mark 9 [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
    ))
  )

;; Show indent guides
(use-package highlight-indent-guides
  :ensure t
  :defer t
  :config
  (setq highlight-indent-guides-method 'character)
  (set-face-attribute 'highlight-indent-guides-character-face
                      (face-attribute 'font-lock-comment-face :foreground))
  ;; Activate indent guides for all programming languages
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  )

;; -----------------------------------------------------------------------------
;; Bookmarks

;; From package documentation
;; https://github.com/joodland/bm
(use-package bm
  :ensure t
  :defer t
  :demand t

  :init
  ;; restore on load (even before you require bm)
  (setq bm-restore-repository-on-load t)


  :config
  ;; Allow cross-buffer 'next'
  (setq bm-cycle-all-buffers t)

  ;; where to store persistant files
  (setq bm-repository-file "~/.emacs.d/bm-repository")

  ;; save bookmarks
  (setq-default bm-buffer-persistence t)

  ;; Change the look
  (set-face-attribute 'bm-persistent-face nil :background "#202830" :foreground "#5ABAC6")

;; Loading the repository from file when on start up.
  (add-hook' after-init-hook 'bm-repository-load)

  ;; Restoring bookmarks when on file find.
  (add-hook 'find-file-hooks 'bm-buffer-restore)

  ;; Saving bookmarks
  (add-hook 'kill-buffer-hook #'bm-buffer-save)

  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when Emacs is killed, so we
  ;; must save all bookmarks first.
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))

  ;; The `after-save-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state.
  (add-hook 'after-save-hook #'bm-buffer-save)

  ;; Restoring bookmarks
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)

  ;; The `after-revert-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state. This hook might cause trouble when using packages
  ;; that automatically reverts the buffer (like vc after a check-in).
  ;; This can easily be avoided if the package provides a hook that is
  ;; called before the buffer is reverted (like `vc-before-checkin-hook').
  ;; Then new bookmarks can be saved before the buffer is reverted.
  ;; Make sure bookmarks is saved before check-in (and revert-buffer)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
  )

;; -----------------------------------------------------------------------------
;; Miscellaneous

;; Setup multiple cursors. With this package you can have multiple cursors
;; and type the same data into multiple positions at the same time
;; Ctrl-D is used by Emacs su we unset it before binding.
(use-package multiple-cursors
  :ensure t
  :config
  ;; Always run commands
  (setq mc/always-run-for-all t)
  ;; Always run commands
  (setq mc/always-repeat-command t)
  ;; Safety ceil
  (setq mc/max-cursors 30)

  ;; The :bind key do not works well with multiple-cursors
  (global-set-key (kbd "M-S-<down>") 'mc/mark-next-like-this)
  (global-set-key (kbd "M-S-<up>") 'mc/mark-previous-like-this)
  (global-unset-key (kbd "C-d"))
  (global-set-key (kbd "C-d") 'mc/mark-all-symbols-like-this-in-defun)
  (global-set-key (kbd "S-C-d") 'mc/mark-next-like-this-word)
  (global-set-key (kbd "M-S-<mouse-1>") 'mc/add-cursor-on-click)
  (define-key mc/keymap (kbd "<escape>") 'mc/keyboard-quit)

  ;; will make <return> insert a newline; multiple-cursors-mode can still
  ;; be disabled with C-g / ESC
  (define-key mc/keymap (kbd "<return>") nil)
  )

;; Visual search and replace
(use-package visual-regexp
  :ensure t
  :defer t)

;; Package to easy select region
(use-package expand-region
  :ensure t
  :defer t
  :bind (("C-l" . er/expand-region))
  )

;; Move and copy line like Eclipse and Netbeans
(use-package move-dup
  :ensure t
  :config
  (global-move-dup-mode))

(use-package string-inflection
  :ensure t
  :defer t)

(provide 'init-editing)
;;; init-editing.el ends here
