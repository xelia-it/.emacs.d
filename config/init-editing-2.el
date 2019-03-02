;;; package --- Emacs configuration with batteries included

;;; ---------------------------------------------------------------------------------
;;; Commentary:

;;; ----------------------------------------------------------------------------
;;; Code:

;; -----------------------------------------------------------------------------
;; Basic editing preferences

(defun my-editing-preferences ()
  "Apply my edititing preference."

  ;; Cut-Paste like Windows
  (cua-mode t)

  ;; Standard Windows behaviour
  ;;(setq-default cua-keep-region-after-copy t)

  ;; Don't tabify after rectangle commands
  ;;(setq-default cua-auto-tabify-rectangles nil)

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
  )

(defun my-backup-preferences ()
  "Apply my backup preference."

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
  )

(defun my-compiling-preferences()
  "Configure compiler."

  ;; Auto-save before compiling
  (setq compilation-ask-about-save nil)

  ;; Never prompt to kill a compilation session.
  (setq-default compilation-always-kill t)

  ;; Always scroll to the bottom.
  (setq-default compilation-scroll-output t)
  )

;; Package multiple-cursors is the main editiging package.
;; With this package you can have multiple cursors
;; and type the same data into multiple positions at the same time
;; Ctrl-D is used by Emacs su we unset it before binding.
(use-package multiple-cursors
  :ensure t
  :config
  ;; Always run commands
  (setq-default mc/always-run-for-all t)
  ;; Always run commands
  (setq-default mc/always-repeat-command t)
  ;; Safety ceil
  (setq-default mc/max-cursors 30)

  ;; The :bind key do not works well with multiple-cursors
  (global-set-key (kbd "M-S-<down>") 'mc/mark-next-like-this)
  (global-set-key (kbd "M-S-<up>") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-d") 'mc/mark-all-symbols-like-this-in-defun)
  (global-set-key (kbd "S-C-d") 'mc/mark-next-like-this-word)
  (global-set-key (kbd "M-S-<mouse-1>") 'mc/add-cursor-on-click)
  (define-key mc/keymap (kbd "<escape>") 'mc/keyboard-quit)

  ;; will make <return> insert a newline; multiple-cursors-mode can still
  ;; be disabled with C-g / ESC
  (define-key mc/keymap (kbd "<return>") nil)

  (my-editing-preferences)
  (my-backup-preferences)
  )

(provide 'init-editing)
;;; init-editing.el ends here
