;;; package --- Emacs configuration with batteries included

;;; ----------------------------------------------------------------------------
;;; Commentary:

;;; ----------------------------------------------------------------------------
;;; Code:

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

;; Native compilation
;; I strongly suggest to enable  Native Compilation whenever possible.
(setq native-comp-deferred-compilation t)
(setq package-native-compile t)
(setq comp-async-report-warnings-errors nil)

;; Fix compile warnings
(setq personal-keybindings ())

;; Workaround for (temporary?) Emacs error.
;; Details can be found here: https://emacs.stackexchange.com/questions/74289/emacs-28-2-error-in-macos-ventura-image-type-invalid-image-type-svg
;; (setq image-types (cons 'svg image-types))
;; (add-to-list 'image-types 'svg)

;; Load compiled Lisp file.
;; If this do not exists use the original org file to produce Lisp file.

(unless (file-exists-p my-init-compiled-file)
  (unless (file-exists-p my-init-file)
    (message "Extracting code from init file ...")
    (require 'ob-tangle)
    (org-babel-tangle-file my-init-org-file my-init-file)
    )
  (message "Byte-compiling init file ...")
  (byte-compile-file my-init-file)
  )

;; Then load it
;; Arguments meaning:
;;  1st) report error if file not found,
;;  2nd) do not print loading message,
;;  3rd) add suffix
(load my-init-compiled-file nil t t t)

;;; init.el ends here
