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

(defun kill-start-of-line ()
  "Kill from point to start of line."
  (interactive)
  (kill-line 0)
)

(defun kill-end-of-line ()
  "Kill from point to end of line."
  (interactive)
  (kill-line nil)
)

;; ------------------------------------------------------------------------------
;; Packages

;; Move and copy line like Eclipse and Netbeans
(use-package move-dup
  :ensure t
  :config
  ;; Use this package to trigger my editing preference setup
  (my-editing-preferences)
  (my-backup-preferences)
  ;; Activate move-up
  (global-move-dup-mode)
  )

;; With this package you can have multiple cursors
;; and type the same data into multiple positions at the same time
;; Ctrl-D is used by Emacs su we unset it before binding.
(use-package multiple-cursors
  :ensure t
  :after (move-dup)
  :config
  ;; Always run commands
  (setq-default mc/always-run-for-all t)
  ;; Always run commands
  (setq-default mc/always-repeat-command t)
  ;; Safety ceil
  (setq-default mc/max-cursors 30)

  ;; Disable annoying minor modes when editing with multiple cursor
  (add-to-list 'mc/unsupported-minor-modes 'company-mode)
  (add-to-list 'mc/unsupported-minor-modes 'flycheck-mode)

  ;; The :bind key do not works well with multiple-cursors
  (global-set-key (kbd "S-M-<up>") 'mc/mark-previous-like-this)
  (global-set-key (kbd "S-M-<down>") 'mc/mark-next-like-this)
  (global-set-key (kbd "S-C-d") 'mc/mark-next-like-this-word)
  (global-set-key (kbd "C-d") 'mc/mark-all-symbols-like-this-in-defun)
  (global-set-key (kbd "M-S-<mouse-1>") 'mc/add-cursor-on-click)
  ;; Exit using escape
  (define-key mc/keymap (kbd "<escape>") 'mc/keyboard-quit)

  ;; will make <return> insert a newline; multiple-cursors-mode can still
  ;; be disabled with C-g / ESC
  (define-key mc/keymap (kbd "<return>") nil)

  ;; Define other keybindingd
  (global-set-key (kbd "M-<left>") 'beginning-of-line)
  (global-set-key (kbd "M-<right>") 'end-of-line)

  ;; Delete chars
  (global-set-key (kbd "C-<backspace>") 'backward-kill-word)
  (global-set-key (kbd "S-C-<backspace>") 'kill-start-of-line)
  (global-set-key (kbd "C-<delete>") 'kill-word)
  (global-set-key (kbd "S-C-<delete>") 'kill-end-of-line)
  (global-set-key (kbd "M-<backspace>") 'kill-whole-line)
  (global-set-key (kbd "M-<delete>") 'kill-whole-line)

  (global-set-key (kbd "C-g") 'goto-line)
  )

;; Package to easy select region
(use-package expand-region
  :ensure t
  :defer t
  :bind (
         ("C-l" . er/expand-region)
         )
  )

;; Package to easy select region
(use-package visual-regexp
  :ensure t
  :defer t
  :init
  (setq vr/auto-show-help t)
  :bind (
         ("S-C-r" . vr/mc-mark)
         ("C-r" . vr/replace)
         )
  )

(use-package indent-guide
  :ensure t
  :defer t
  :hook (prog-mode . indent-guide-mode)
  :config
  (setq indent-guide-char "⋮")
  (set-face-attribute 'indent-guide-face nil :foreground (face-foreground 'font-lock-comment-face))
  )

(provide 'init-editing)
;;; init-editing.el ends here
