;; Early disable annoying features

;; Scollbars, menu bars, splash screen are distracting and occupies space.
;; No more default Emacs splash screen
(setq inhibit-splash-screen t)
;; Disable toolbar on top
(tool-bar-mode -1)
;; Disable menu bar on top
(menu-bar-mode -1)
;; No more scrollbars
(scroll-bar-mode -1)
;; Start maximized
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; Define vars here
(defvar my-vendor-dir
  (expand-file-name "packages/" user-emacs-directory)
  "Directory with packages that are not yet available in ELPA (or MELPA).")

(defvar my-config-dir
  (expand-file-name "config/" user-emacs-directory)
  "Directory containing configuration files.")

(defvar my-init-basename
  "emacs-init"
  "The base name for all configuration files.")

(defvar my-init-org-file
  (expand-file-name (concat my-init-basename ".org") my-config-dir)
  "Configurations source organized in sections and subsections.")

(defvar my-init-file
  (expand-file-name (concat my-init-basename ".el") my-config-dir)
  "All configurations tangled from org file.")

(defvar my-init-compiled-file
  (expand-file-name (concat my-init-basename ".elc") my-config-dir)
  "Byte compiled configurations file.")

;; Setup repository.


;; There are other repository:
;; - GNU ELPA: https://elpa.gnu.org/packages/
;; - MELPA STABLE: https://stable.melpa.org/packages/
;; - ORG http://orgmode.org/elpa/
;; I use only MELPA packages.
;;  
(defvar repo-melpa '("melpa" . "https://melpa.org/packages/"))
(defvar repo-elpa '("elpa" . "https://elpa.gnu.org/packages/"))
(defvar repo-stable-melpa '("melpa" . "https://stable.melpa.org/packages/"))
(setq package-archives nil)
(add-to-list 'package-archives repo-elpa t)
(add-to-list 'package-archives repo-melpa t)

;; Package compile

;; (eval-when-compile (require 'use-package))

;; Native compilation

;; I strongly suggest to enable  Native Compilation whenever possible.
(setq native-comp-deferred-compilation t)
(setq package-native-compile t)
(setq comp-async-report-warnings-errors nil)

;; Fix compile warnings
;;(setq personal-keybindings ())
