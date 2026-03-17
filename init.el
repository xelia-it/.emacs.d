;;; package --- Emacs configuration with batteries included

;;; ----------------------------------------------------------------------------
;;; Commentary:

;;; ----------------------------------------------------------------------------
;;; Code:

;; Load compiled Lisp file.
;; If this do not exists use the original org file to produce Lisp file.

;; Step 1: tangling if needed
(when (or (not (file-exists-p my-init-file))
          (file-newer-than-file-p my-init-org-file my-init-file))
  (message "Extracting code from init file ...")
  (require 'ob-tangle)
  (org-babel-tangle-file my-init-org-file my-init-file))

;; Step 2: compile if needed
(when (or (not (file-exists-p my-init-compiled-file))
          (file-newer-than-file-p my-init-file my-init-compiled-file))
  (message "Byte-compiling init file...")
  (byte-compile-file my-init-file))

;; Step 3: load .elc
(load (file-name-sans-extension my-init-file) nil t)

;;; init.el ends here
