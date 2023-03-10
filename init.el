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

(defvar my-init-file (expand-file-name "emacs-init.el" my-config-dir)
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

;; Load compiled Lisp file.
;; If this do not exists use the original org file to produce Lisp file.
(if (file-exists-p my-init-file)
  (load my-init-file t t)
  (progn
    (org-babel-load-file my-org-file)
    )
  )

;;; init.el ends here
