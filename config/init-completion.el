;;; package --- Emacs configuration with batteries included

;;; ----------------------------------------------------------------------------
;;; Commentary:

;;; ----------------------------------------------------------------------------
;;; Code:

;; -----------------------------------------------------------------------------
;; On the fly check


(use-package flycheck
  :ensure t
  :config
  ;; Disable ruby-reek checker: it's too verbose
  (setq-default flycheck-disabled-checkers '(ruby-reek))
  (global-flycheck-mode)
  )

;; -----------------------------------------------------------------------------
;; Autocomplete

(use-package company
  :ensure t
  :config
  (global-company-mode))

(use-package company-quickhelp
  :ensure t
  :after (company)
  :config
  (company-quickhelp-mode))

(setq company-idle-delay 0
      company-echo-delay 0
      company-dabbrev-downcase nil
      company-minimum-prefix-length 2
      company-selection-wrap-around t
      company-transformers '(company-sort-by-occurrence
                             company-sort-by-backend-importance))

;; -----------------------------------------------------------------------------
;; Code tagging and movement

;; Use GNU Global for code tagging
(use-package helm-gtags
  :ensure t
  :after (helm)
  :config

  ;; Enable helm-gtags-mode
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)
  (setq helm-gtags-ignore-case t
        helm-gtags-auto-update t
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t
        helm-gtags-prefix-key "\C-cg"
        helm-gtags-suggested-key-mapping t)

  ;; Enable helm-gtags-mode
  (add-hook 'dired-mode-hook 'helm-gtags-mode)
  (add-hook 'eshell-mode-hook 'helm-gtags-mode)
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)
  (add-hook 'ruby-mode-hook 'helm-gtags-mode)
  )

;; -----------------------------------------------------------------------------
;; Language: C/C++

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

(use-package cmake-ide
  :ensure t
  :config
  (cmake-ide-setup)
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
;; Language: HTML/CSS/Javascript

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


(use-package js2-mode
  :ensure t
  :config
  ;; open files ending in “.js” with js2-mode
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  )

(use-package rainbow-mode
  :ensure t)

(use-package emmet-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :config
  ;; associate extensions with web-mode
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.handlebars\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  ;;
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-block-face t)
  (setq web-mode-enable-part-face t)
  (setq web-mode-enable-comment-keywords t)
  (setq web-mode-enable-heredoc-fontification t)
  ;;
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)

  (setq web-mode-html-tag-unclosed-face t)
  )

(use-package impatient-mode
  :ensure t
  :config
  (add-hook 'c-mode-hook 'httpd-start))

;; (use-package skewer-mode
;;   :ensure t
;;   :config
;;   (add-hook 'js2-mode-hook 'skewer-mode)
;;   (add-hook 'css-mode-hook 'skewer-css-mode)
;;   (add-hook 'html-mode-hook 'skewer-html-mode)
;;   )

;; -----------------------------------------------------------------------------
;; Language: Ruby

(use-package company-inf-ruby
  :ensure t
  :after (company)
  :config
  (add-to-list 'company-backends 'company-inf-ruby)
  )

;; -----------------------------------------------------------------------------
;; Language: Angular

(use-package tide
  :ensure t
  :config

  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1))

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)

  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  )

(use-package ng2-mode
  :ensure t
  :after (tide)
  )

;; -----------------------------------------------------------------------------

(use-package yasnippet
  :ensure t
  :config
  )

(use-package yasnippet-snippets
  :ensure t
  :after (yasnippet)
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  )

;; -----------------------------------------------------------------------------

(provide 'init-completion)
;;; init-completion.el ends here
