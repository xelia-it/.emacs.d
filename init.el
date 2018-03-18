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
 '(custom-safe-themes
   (quote
    ("e9460a84d876da407d9e6accf9ceba453e2f86f8b86076f37c08ad155de8223c" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "b01e2d02a7bd9a67e8824bf1501f4fb9d5dce57941808f0af7020b47aaa9b294" "c620ce43a0b430dcc1b06850e0a84df4ae5141d698d71e17de85e7494377fd81" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "c2f49c919c31c7de1ace6f10eea91f64c6f2338a82a203eca2588e3447082e76" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb")))
 '(package-selected-packages
   (quote
    (ac-helm dracula-theme atom-one-dark-theme use-package))))

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
