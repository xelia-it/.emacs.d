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

(defvar repo-gnu '("gnu" . "https://elpa.gnu.org/packages/"))
(defvar repo-melpa '("melpa" . "https://melpa.org/packages/"))
(defvar repo-melpa-stable '("melpa-stable" . "https://stable.melpa.org/packages/"))
(defvar repo-org-elpa '("org" . "http://orgmode.org/elpa/"))

(setq package-archives nil)
(add-to-list 'package-archives repo-melpa-stable t)
(add-to-list 'package-archives repo-melpa t)
(add-to-list 'package-archives repo-gnu t)
(add-to-list 'package-archives repo-org-elpa t)

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
;; (Works only on Linux/Mac)
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; -----------------------------------------------------------------------------
;; Other config scripts

(load "init-face.el")
(load "init-editing.el")
(load "init-projects.el")
(load "init-completion.el")
(load "init-utils.el")
(load "init-keybinding.el")

;; -----------------------------------------------------------------------------
;; Custom set variable

;; Some variables can be put into .dir-locals-el scripts
;; BEWARE: THIS CAN BE DANGEROUS
(setq enable-local-variables :all)

;; Use a separate custom file
(setq custom-file "~/.emacs.d/custom.el")
(cond
 ((file-exists-p custom-file) (load custom-file))
 )

(provide 'init)
;;; init.el ends here
