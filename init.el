;; Define vars here
(defvar my-vendor-dir (expand-file-name "packages/" user-emacs-directory)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")

(defvar my-config-dir (expand-file-name "config/" user-emacs-directory)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")

(defvar my-init-file (expand-file-name "emacs-init.el" user-emacs-directory)
  "All configurations stored in this file.")

(defvar my-org-file (expand-file-name "emacs-init.org" my-config-dir)
  "All configurations tangled from this file.")

(if (file-exists-p my-init-file)
  (load my-init-file t t)
  (progn
    (org-babel-load-file my-org-file)
    )
  )
