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
;; Setup color theme and window

(set-default-font "noto mono 11")

;; -----------------------------------------------------------------------------
;;  Advanced: incremental completion (Helm)

(use-package helm
  :ensure t
  :init)
;; (require 'helm-config)

;; Editing

(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
