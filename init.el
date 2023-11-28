;;; package --- Emacs configuration with batteries included

;;; ----------------------------------------------------------------------------
;;; Commentary:

;;; ----------------------------------------------------------------------------
;;; Code:

;; Load compiled Lisp file.
;; If this do not exists use the original org file to produce Lisp file.

(unless (file-exists-p my-init-compiled-file)
  (unless (file-exists-p my-init-file)
    (message "Extracting code from init file ...")
    (require 'ob-tangle)
    (org-babel-tangle-file my-init-org-file my-init-file)
  )

  (message "Byte-compiling init file ...")
  (byte-compile-file my-init-file t)
)

;; Then load it
;; Arguments meaning:
;;  1st) report error if file not found,
;;  2nd) do not print loading message,
;;  3rd) add suffix
(load my-init-file nil t t t)

;;; init.el ends here
