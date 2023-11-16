;;; package --- Emacs configuration with batteries included

;;; ----------------------------------------------------------------------------
;;; Commentary:

;;; ----------------------------------------------------------------------------
;;; Code:

;; Setup packages

;;(require 'package)

;; Install use-package package if not present
;;(unless (package-installed-p 'use-package)
;;  (package-refresh-contents)
;;  (package-install 'use-package))

(eval-when-compile (require 'use-package))


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
  (byte-compile-file my-init-file t)
)

;; Then load it
;; Arguments meaning:
;;  1st) report error if file not found,
;;  2nd) do not print loading message,
;;  3rd) add suffix
(load my-init-file nil t t t)

;;; init.el ends here
