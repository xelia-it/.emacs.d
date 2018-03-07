;;; package --- Emacs configuration with batteries included

;;; ----------------------------------------------------------------------------
;;; Commentary:

;;; ----------------------------------------------------------------------------
;;; Code:

;; -----------------------------------------------------------------------------

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  )

(use-package company
  :ensure t
  :config
  (add-to-list 'company-backends 'company-c-headers)
  (add-to-list 'company-backends 'company-clang)
  (add-to-list 'company-backends 'company-web-html)
  (add-to-list 'company-backends 'company-web-jade)
  (add-to-list 'company-backends 'company-web-slim)
  (global-company-mode)
  )
;; (setq company-backends (delete 'company-semantic company-backends))

(use-package company-c-headers
  :ensure t
  :after (company))

(use-package company-web
  :ensure t
  :after (company))

(use-package ac-html-bootstrap
  :ensure t
  :after (company))

(use-package ac-html-csswatcher
  :ensure t
  :after (company))

(use-package ac-html-csswatcher
  :ensure t
  :after (company))


;; -----------
;; C++

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

  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  (setq company-backends (delete 'company-semantic company-backends))
  (eval-after-load 'company
    '(add-to-list
      'company-backends 'company-irony))


  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))

  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  )

;;; init-completion.el ends here
