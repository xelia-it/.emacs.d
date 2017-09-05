;;; package --- Summary

;;; Commentary:

;;; Code:

;; ----------------------------------------------------------------------------
;;  RTAGS

(use-package rtags :ensure t)
(use-package flycheck-rtags :ensure t)
(use-package company-rtags :ensure t)
(use-package helm-rtags :ensure t)

;; Start rtags daemon automatically
;; It MUST be installed and accessible via $PATH
(add-hook 'c-mode-hook 'rtags-start-process-unless-running)
(add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
(add-hook 'objc-mode-hook 'rtags-start-process-unless-running)

;; ensure that we use only rtags checking
;; https://github.com/Andersbakken/rtags#optional-1
(defun setup-flycheck-rtags ()
  "Setup flycheck rtags."
  (interactive)
  (flycheck-select-checker 'rtags)
  ;; RTags creates more accurate overlays.
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-check-syntax-automatically nil))

;; only run this if rtags is installed
(when (require 'rtags nil :noerror)
  ;; make sure you have company-mode installed
  (require 'company)
  (define-key c-mode-base-map (kbd "C-b")
    (function rtags-find-symbol-at-point))
  (define-key c-mode-base-map (kbd "S-C-b")
    (function rtags-find-references-at-point))
  ;; disable prelude's use of C-c r, as this is the rtags keyboard prefix
  ;; (define-key prelude-mode-map (kbd "C-c r") nil)
  ;; install standard rtags keybindings. Do M-. on the symbol below to
  ;; jump to definition and see the keybindings.
  (rtags-enable-standard-keybindings)
  ;; comment this out if you don't have or don't use helm
  (setq rtags-display-result-backend 'helm)
  ;; company completion setup
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t)
  ;; (push 'company-rtags company-backends)
  (add-to-list 'company-backends 'company-rtags)

  (global-company-mode)
  ;; (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))
  ;; use rtags flycheck mode -- clang warnings shown inline
  (require 'flycheck-rtags)
  ;; c-mode-common-hook is also called by c++-mode
  (add-hook 'c-mode-common-hook #'setup-flycheck-rtags))


;;
;; -- Experiment with code from documentation

;; (define-key c-mode-base-map (kbd "M-.") (function tags-find-symbol-at-point))
;; (define-key c-mode-base-map (kbd "M-,") (function tags-find-references-at-point))
;; (define-key c-mode-base-map (kbd "M-;") (function tags-find-file))
;; (define-key c-mode-base-map (kbd "C-.") (function tags-find-symbol))
;; (define-key c-mode-base-map (kbd "C-,") (function tags-find-references))
;; (define-key c-mode-base-map (kbd "C-<") (function rtags-find-virtuals-at-point))
;; (define-key c-mode-base-map (kbd "M-i") (function tags-imenu))
;;
;; (define-key global-map (kbd "M-.") (function tags-find-symbol-at-point))
;; (define-key global-map (kbd "M-,") (function tags-find-references-at-point))
;; (define-key global-map (kbd "M-;") (function tags-find-file))
;; (define-key global-map (kbd "C-.") (function tags-find-symbol))
;; (define-key global-map (kbd "C-,") (function tags-find-references))
;; (define-key global-map (kbd "C-<") (function rtags-find-virtuals-at-point))
;; (define-key global-map (kbd "M-i") (function tags-imenu))

(provide 'init-rtags)
;;; init-rtags.el ends here
