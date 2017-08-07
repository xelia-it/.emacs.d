;;; package --- Emacs configuration with batteries included

;;; ----------------------------------------------------------------------------
;;; Commentary:


;;; ----------------------------------------------------------------------------
;;; Code:

;; -----------------------------------------------------------------------------
;;  Package repositories

;; Use MELPA repisitory for download packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; Downloads new packages in case of a fresh install
(package-initialize)

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)

;; -----------------------------------------------------------------------------
;; Custom set variable

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a4c9e536d86666d4494ef7f43c84807162d9bd29b0dfd39bdf2c3d845dcc7b2e" default)))
 '(package-selected-packages
   (quote
    (helm-projectile flycheck projectile multiple-cursors helm use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; -----------------------------------------------------------------------------
;; Setup color theme and window

;; Set the default font
;; (set-default-font "noto mono 11")

;; Load theme
(use-package atom-one-dark-theme
  :ensure t
  :init (load-theme 'atom-one-dark))

;; Basic settings
(toggle-frame-maximized)          ;; Start with maximized frame
(setq inhibit-startup-message t)  ;; No more Emacs splash screen
(tool-bar-mode -1)                ;; Disable toolbar on top
;; (menu-bar-mode -1)             ;; Uncomment to disable menu bar on top
(scroll-bar-mode -1)              ;; No more scrollbars
(blink-cursor-mode t)             ;; Blink cursor
(setq-default cursor-type 'bar)   ;; Cursor like a bar
(set-cursor-color "#ffffff")      ;; Color white
(global-hl-line-mode)             ;; Hightlight current line

;; Add line numbers to the right
(require 'linum)
(global-linum-mode 1)
(setq linum-format " %d ")

;; Makes *scratch* empty.
(setq initial-scratch-message "")

;; Slip window in vertical
(split-window-right 50)

;; Removes *scratch* from buffer after the mode has been set.
;; TODO: delete when ok!
;;(defun remove-scratch-buffer ()
;;  (if (get-buffer "*scratch*")
;;      (kill-buffer "*scratch*")))
;;(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; Removes *messages* from the buffer list.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer)))))

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; Show completion in a separate window
(add-to-list 'special-display-buffer-names '("*Completions*" my-display-completions))

(defun my-display-completions (buf)
  "Put the buffer on the right of the frame.  BUF is the buffer."
  (let ((windows (delete (minibuffer-window) (window-list))))
    (if (eq 1 (length windows))
        (progn
          (select-window (car windows))
          (split-window-horizontally)))
    (let ((target-window (window-at (- (frame-width) 2) 0))
          (pop-up-windows t))
      (set-window-buffer target-window buf)
      target-window)))

;; -----------------------------------------------------------------------------
;;  Editing

(cua-mode t)
(delete-selection-mode t)              ;; Typed text deletes selected text
(setq cua-auto-tabify-rectangles nil)  ;; Don't tabify after rectangle commands
(transient-mark-mode 1)                ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t)    ;; Standard Windows behaviour

;; Delete trailing whitespace when save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Do not indent using tab
(setq-default indent-tabs-mode nil)

;; Do not create *~ backup files
(setq make-backup-files nil)

;; No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)

;; Setup multiple cursors. With this package you can have multiple cursors
;; and type the same data into multiple positions at the same time
;; Ctrl-D is used by Emacs su we unset it before binding.
(global-unset-key "\C-d")
(use-package multiple-cursors
  :ensure t
  :init
  :bind (("M-S-<down>" . mc/mark-next-like-this)
	 ("M-S-<up>" . mc/mark-previous-like-this)
	 ("C-d" . mc/mark-all-like-this))
  )


;; ----------------------------------------------------------------------------
;;  Advanced: incremental completion (Helm)

;; (global-unset-key "\C-<tab>")
(global-unset-key "\C-o")

(use-package helm
  :ensure t
  :init
  :bind (
	 ("C-<tab>" . helm-multi-files)
      	 ("C-o" . helm-find-files)
         ("C-x C-f" . helm-find-files)
	 )
  )

(add-to-list 'special-display-buffer-names '("*helm buffers*" my-display-completions))
(add-to-list 'special-display-buffer-names '("*helm multi files*" my-display-completions))
(add-to-list 'special-display-buffer-names '("*helm find files*" my-display-completions))

;; ----------------------------------------------------------------------------
;;  Advanced: incremental completion (Projectile)

(use-package projectile
  :ensure t
  :init (projectile-mode)
  )

(use-package helm-projectile
  :ensure t
  :init (helm-projectile-on)
  :bind (
         ("C-p" . helm-projectile-find-file-dwim)
         ("S-C-a" . helm-projectile-switch-project)
         ("S-C-f" . helm-projectile-grep)
	 )
  )

;; ----------------------------------------------------------------------------
;;  Advanced: incremental completion (Flycheck)

(use-package flycheck
  :ensure t
  :init
  )
(add-hook 'after-init-hook #'global-flycheck-mode)

(provide 'init)
;;; init.el ends here
