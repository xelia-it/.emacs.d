;;; .emacs.d --- Emacs configuration with batteries included

;;; ----------------------------------------------------------------------------
;;; Commentary:
;;;
;;; .

;;; ----------------------------------------------------------------------------
;;; Code:

;; -----------------------------------------------------------------------------
;; Language: HTML

(use-package web-mode
  :ensure t
  :defer t
  :config

  ;; Associate extensions with web-mode
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.handlebars\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))

  ;; Configuration
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-block-face t)
  (setq web-mode-enable-part-face t)
  (setq web-mode-enable-heredoc-fontification t)
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

  ;;(add-hook 'web-mode-hook 'emmet-mode)
  ;; Use 2 spaces for indent markup
  (add-hook 'web-mode-hook
            (lambda () ""
              (setq web-mode-markup-indent-offset 2)))
  )

;; -----------------------------------------------------------------------------
;; Language: Ruby

(use-package robe
  :ensure t
  :defer t
  :after (company)
  :config
  (add-to-list 'company-backends 'company-robe)
  ;;(push 'company-robe company-backends)
  :hook (ruby-mode . robe-mode)
  )

(use-package projectile-rails
  :ensure t
  :defer t
  :after (helm projectile)
  ;;:config
;;  (projectile-rails-global-mode)
  :hook (prog-mode . projectile-rails-mode)
  )

;; ------------------------------------------------------------------------------
;; Language: C++

(use-package irony
  :ensure t
  :init
  (message "company init start")
  :config
  (message "company config start")

  ;; Windows performance tweaks
  ;;
  (when (boundp 'w32-pipe-read-delay)
    (setq w32-pipe-read-delay 0))
  ;; Set the buffer size to 64K on Windows (from the original 4K)
  (when (boundp 'w32-pipe-buffer-size)
    (setq irony-server-w32-pipe-buffer-size (* 64 1024)))
  :hook ((c++-mode-hook . irony-mode)
         (c-mode-hook . irony-mode)
         (objc-mode-hook . irony-mode)
         (irony-mode-hook . irony-cdb-autosetup-compile-options)
         )
  )

(use-package flycheck-irony
  :ensure t
  :hook (flycheck-mode . flycheck-irony-setup)

  ;;(eval-after-load 'flycheck
  ;;  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
  )

;; -----------------------------------------------------------------------------
;; Language: YAML

(use-package yaml-mode
  :ensure t
  :defer t
  :config

  ;; TODO: can be used :hook from use-package?
  (require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
   ;;(add-hook 'yaml-mode-hook
   ;; '(lambda ()
   ;;    (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
  )

(use-package yaml-imenu
  :ensure t
  :defer t
  )
(provide 'init-languages)
;;; init-languages.el ends here
