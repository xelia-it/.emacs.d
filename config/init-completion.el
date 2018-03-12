;;; package --- Emacs configuration with batteries included

;;; ----------------------------------------------------------------------------
;;; Commentary:

;;; ----------------------------------------------------------------------------
;;; Code:

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  )

(use-package company
  :ensure t
  :config
  (add-to-list 'company-backends 'company-clang)
  (add-to-list 'company-backends 'company-gcc)
  (add-to-list 'company-backends 'company-cppcheck)
  (global-company-mode)
  )

;; -----------------------------------------------------------------------------
;; GNU Global

(use-package helm-gtags
  :ensure t
  :config

    ;; Enable helm-gtags-mode
    (add-hook 'c-mode-hook 'helm-gtags-mode)
    (add-hook 'c++-mode-hook 'helm-gtags-mode)
    (add-hook 'asm-mode-hook 'helm-gtags-mode)
    (setq
     helm-gtags-ignore-case t
     helm-gtags-auto-update t
     helm-gtags-use-input-at-cursor t
     helm-gtags-pulse-at-cursor t
     helm-gtags-prefix-key "\C-cg"
     helm-gtags-suggested-key-mapping t
     )
    ;; Enable helm-gtags-mode
    (add-hook 'dired-mode-hook 'helm-gtags-mode)
    (add-hook 'eshell-mode-hook 'helm-gtags-mode)
    (add-hook 'c-mode-hook 'helm-gtags-mode)
    (add-hook 'c++-mode-hook 'helm-gtags-mode)
    (add-hook 'asm-mode-hook 'helm-gtags-mode)
    )

;; -----------------------------------------------------------------------------
;; C/C++

(use-package company-c-headers
  :ensure t
  :after (company)
  :config
  (add-to-list 'company-backends 'company-c-headers)
  )

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  )

(use-package company-irony
  :ensure t
  :after (company irony)
  :config
  )

(use-package company-irony-c-headers
  :ensure t
  :after (company-irony)
  :config

  (add-to-list 'company-backends '(company-irony-c-headers company-irony))

  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))

  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  )

;; Debugging
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t
 gdb-enable-debug t
 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;; -----------------------------------------------------------------------------
;; HTML

(use-package company-web
  :ensure t
  :after (company)
  :config
  (add-to-list 'company-backends 'company-web-html)
  (add-to-list 'company-backends 'company-web-jade)
  (add-to-list 'company-backends 'company-web-slim)
  )

(use-package ac-html-bootstrap
  :ensure t
  :after (company))

(use-package ac-html-csswatcher
  :ensure t
  :after (company))

(use-package ac-html-csswatcher
  :ensure t
  :after (company))


;; -----------------------------------------------------------------------------

(provide 'init-completion)
;;; init-completion.el ends here
