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
  (setq company-idle-delay 0
        company-echo-delay 0
        company-dabbrev-downcase nil
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-transformers '(company-sort-by-occurrence
                               company-sort-by-backend-importance))
  (global-company-mode)
  )

(use-package company-quickhelp
  :ensure t
  :after (company)
  :config
  (company-quickhelp-mode))

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

  ;; If irony server was never installed, install it.
  (unless (irony--find-server-executable)
    (call-interactively #'irony-install-server))

  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)

  ;; Use compilation database first, clang_complete as fallback.
  (setq-default irony-cdb-compilation-databases '(irony-cdb-libclang
                                                  irony-cdb-clang-complete))

  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  )

(use-package company-irony
  :ensure t
  :after (company irony)
  )

(use-package company-irony-c-headers
  :ensure t
  :after (company-irony)
  :config

  ;; Configure company-irony and company-irony-c-headers
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  (setq company-backends (delete 'company-semantic company-backends))
  (add-to-list 'company-backends '(company-irony-c-headers company-clang))
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
  ;; (setq web-mode-enable-comment-keywords t)
  (setq web-mode-enable-heredoc-fontification t)
  ;;
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  ;;
  ;;(setq web-mode-html-tag-unclosed-face t)
  (set-face-attribute 'web-mode-block-face nil :background nil)
  (set-face-attribute 'web-mode-inlay-face nil :background nil)
  (set-face-attribute 'web-mode-current-column-highlight-face nil
                      :foreground "#ffffff" :background nil)
  (set-face-attribute 'web-mode-current-element-highlight-face nil
                      :foreground "#ffffff" :background nil :weight 'extra-bold)

  (add-hook 'web-mode-hook 'emmet-mode)
  ;; Use 2 spaces for indent markup
  (add-hook 'web-mode-hook
            (lambda () ""
              (setq web-mode-markup-indent-offset 2)))
  )

(use-package impatient-mode
  :ensure t
  :config
  (add-hook 'c-mode-hook 'httpd-start)
  )

;; -----------------------------------------------------------------------------
;; Language: Ruby

(use-package robe
  :ensure t
  :after (company)
  :config
  (add-to-list 'company-backends 'company-robe)
  )

;;(use-package company-inf-ruby
;;  :ensure t
;;  :after (company)
;;  :config
;;  (add-to-list 'company-backends 'company-inf-ruby)
;;  )

(use-package projectile-rails
  :ensure t
  :after (helm projectile)
  :config
  (projectile-rails-global-mode)
  )

;; -----------------------------------------------------------------------------
;; Language: Python

(use-package company-jedi
  :ensure t
  :config

  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))

  (add-hook 'python-mode-hook 'my/python-mode-hook)
  )

;; -----------------------------------------------------------------------------
;; Language: Angular

;; Web mode with Angular
;;(require 'web-mode)
;;(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
;;(add-hook 'web-mode-hook
;;          (lambda ()
;;            (when (string-equal "tsx" (file-name-extension buffer-file-name))
;;              (setup-tide-mode))))
;;;; enable typescript-tslint checker
;;(flycheck-add-mode 'typescript-tslint 'web-mode)

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package ng2-mode
  :ensure t
  :after (tide)
  )

;; -----------------------------------------------------------------------------
;; Language: Kivy

(use-package kivy-mode
  :ensure t)

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

(use-package company-restclient
  :ensure t
  :config
  (add-to-list 'company-backends 'company-restclient))

;; -----------------------------------------------------------------------------

(provide 'init-completion)
;;; init-completion.el ends here
