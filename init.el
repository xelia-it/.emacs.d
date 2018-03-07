;;; package --- Emacs configuration with batteries included

;;; ----------------------------------------------------------------------------
;;; Commentary:

;;; ----------------------------------------------------------------------------
;;; Code:

;; -----------------------------------------------------------------------------
;;  Package repositories

;; Setup repository
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

;; Downloads new packages in case of a fresh install
(package-initialize)

;; Install use-package packege if not present
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; -----------------------------------------------------------------------------
;; Load script

;; Our scripts are into a subdirectory
(add-to-list 'load-path "~/.emacs.d/config")

;; Set the path variable
(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

;; -----------------------------------------------------------------------------
;; Other config scripts

(load "init-face.el")
(load "init-editing.el")
(load "init-projects.el")
(load "init-completion.el")
(load "init-keybinding.el")

;; -----------------------------------------------------------------------------
;; Custom set variable

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote nil))
 '(package-selected-packages (quote (atom-one-dark-theme use-package))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Some variables can be put into .dir-locals-el scripts
(put 'company-clang-arguments 'safe-local-variable (lambda(xx) t))
(put 'flycheck-clang-args 'safe-local-variable (lambda(xx) t))
(put 'projectile-project-compilation-cmd 'safe-local-variable (lambda(xx) t))
(put 'c-default-style 'safe-local-variable (lambda(xx) t))
(put 'c-offsets-alist 'safe-local-variable (lambda(xx) t))

(provide 'init)
;;; init.el ends here
