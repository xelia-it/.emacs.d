;;; package --- Emacs configuration with batteries included

;;; ----------------------------------------------------------------------------
;;; Commentary:

;;; ----------------------------------------------------------------------------
;;; Code:

;; -----------------------------------------------------------------------------
;;  UTF-8

;; Used in every file
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; -----------------------------------------------------------------------------
;;  Security

(require 'cl)
(setq tls-checktrust t)

(setq python (or (executable-find "py.exe")
                 (executable-find "python")
                 ))

(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string (concat python " -m certifi"))))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile)))
  (setq gnutls-verify-error t)
  (setq gnutls-trustfiles (list trustfile)))

;; -----------------------------------------------------------------------------
;;  Package repositories

;; Setup repository
(require 'package)
(setq package-enable-at-startup nil)

(defvar gnu '("gnu" . "https://elpa.gnu.org/packages/"))
(defvar melpa '("melpa" . "https://melpa.org/packages/"))
(defvar melpa-stable '("melpa-stable" . "https://stable.melpa.org/packages/"))
(defvar org-elpa '("org" . "http://orgmode.org/elpa/"))

(setq package-archives nil)
(add-to-list 'package-archives melpa-stable t)
(add-to-list 'package-archives melpa t)
(add-to-list 'package-archives gnu t)
(add-to-list 'package-archives org-elpa t)

;; Downloads new packages in case of a fresh install
(package-initialize)

;; Install use-package package if not present
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; -----------------------------------------------------------------------------
;; Configuration path

;; Write a separate custom.el
;;(setq custom-file (concat init-dir "custom.el"))
;;(load custom-file :noerror)

;; Our scripts are into a subdirectory
(add-to-list 'load-path "~/.emacs.d/config")

;; Set the path variable
;;(use-package exec-path-from-shell
;;  :ensure t
;;  :config
;;  (when (memq window-system '(mac ns x))
;;	(exec-path-from-shell-initialize))
;;	)


;; -----------------------------------------------------------------------------
;; Other config scripts

(load "init-face.el")
(load "init-editing.el")
(load "init-projects.el")
(load "init-completion.el")
(load "init-keybinding.el")

;; -----------------------------------------------------------------------------
;; Custom set variable

;; Some variables can be put into .dir-locals-el scripts
(put 'company-clang-arguments 'safe-local-variable (lambda(xx) t))
(put 'flycheck-clang-args 'safe-local-variable (lambda(xx) t))
(put 'projectile-project-compilation-cmd 'safe-local-variable (lambda(xx) t))
(put 'c-default-style 'safe-local-variable (lambda(xx) t))
(put 'c-offsets-alist 'safe-local-variable (lambda(xx) t))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (magit yasnippet-snippets yasnippet all-the-icons mode-icons web-mode use-package string-inflection spaceline rainbow-mode multiple-cursors move-dup js2-mode impatient-mode highlight-indent-guides helm-swoop helm-projectile helm-gtags helm-emmet flycheck expand-region exec-path-from-shell company-web company-quickhelp company-irony-c-headers company-irony company-inf-ruby company-c-headers atom-one-dark-theme ac-html-csswatcher ac-html-bootstrap))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
