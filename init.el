;;; Emacs config

;; -----------------------------------------------------------------------------
;;  Package repositories

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
 '(package-selected-packages (quote (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; -----------------------------------------------------------------------------
;; Setup color theme and window

;; Set the default font
(set-default-font "noto mono 11")

;; Load theme
(use-package atom-one-dark-theme
  :ensure t
  :init (load-theme 'atom-one-dark))


(toggle-frame-maximized)          ;; Start with maximized frame
(setq inhibit-startup-message t)  ;; No more Emacs splash screen
(tool-bar-mode -1)                ;; Disable toolbar on top
;; (menu-bar-mode -1)             ;; Uncomment to disable menu bar on top
(scroll-bar-mode -1)              ;; No more scrollbars
(blink-cursor-mode t)             ;; Blink cursor
(delete-selection-mode t)         ;; Typed text deletes selected text
(global-hl-line-mode)             ;; Hightlight current line

;; -----------------------------------------------------------------------------
;;  Editing

(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

;; -----------------------------------------------------------------------------
;;  Advanced: incremental completion (Helm)

(use-package helm
  :ensure t
  :init
  )
