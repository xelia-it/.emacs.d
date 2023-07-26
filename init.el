;;; package --- Emacs configuration with batteries included

;;; ----------------------------------------------------------------------------
;;; Commentary:

;;; ----------------------------------------------------------------------------
;;; Code:

;; Define vars here
(defvar my-vendor-dir (expand-file-name "packages/" user-emacs-directory)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")

(defvar my-config-dir (expand-file-name "config/" user-emacs-directory)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")

(defvar my-init-file (expand-file-name "emacs-init.elc" my-config-dir)
  "All configurations stored in this file.")

(defvar my-org-file (expand-file-name "emacs-init.org" my-config-dir)
  "All configurations tangled from this file.")

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
(setq image-types (cons 'svg image-types))

;; Load compiled Lisp file.
;; If this do not exists use the original org file to produce Lisp file.
(if (file-exists-p my-init-file)
  ;; Load compiled config file
  ;; Arguments means: report error if file not found, do not print loading message, do not add suffix
  (load my-init-file nil t t)
  (progn
    (org-babel-load-file my-org-file t)
    )
  )

;;; init.el ends here
