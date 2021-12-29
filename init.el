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

;; Disable startup screen
(setq-default startup-screen-inhibit-startup-screen t)

(if (file-exists-p my-init-file)
  ;; TOD: Just for debug.. DELETE!
  (delete-file my-init-file)
  (progn
    (org-babel-load-file my-org-file)
    )
    (load my-init-file t t)
  )

;;; init.el ends here
