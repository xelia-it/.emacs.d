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
  :mode (
         ("\\.html?\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.handlebars\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         )
  :config
  ;; Configuration
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-block-face t)
  (setq web-mode-enable-part-face t)
  (setq web-mode-enable-heredoc-fontification t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-markup-indent-offset 2)

  ;;(setq web-mode-html-tag-unclosed-face t)
  (set-face-attribute 'web-mode-block-face nil :background nil)
  (set-face-attribute 'web-mode-inlay-face nil :background nil)
  (set-face-attribute 'web-mode-current-column-highlight-face nil
                      :foreground "#ffffff" :background nil)
  (set-face-attribute 'web-mode-current-element-highlight-face nil
                      :foreground "#ffffff" :background nil :weight 'extra-bold)
  )

;; -----------------------------------------------------------------------------
;; Language: Ruby

(use-package robe
  :ensure t
  :defer t
  :after (company)
  :init

  ;; Setting rbenv path
  (setenv "PATH" (concat (getenv "HOME") "/.rbenv/shims:" (getenv "HOME") "/.rbenv/bin:" (getenv "PATH")))
  (setq exec-path (cons (concat (getenv "HOME") "/.rbenv/shims") (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))

  :config
  (push 'company-robe company-backends)

  :hook (
         (ruby-mode . robe-mode)
         (ruby-mode . robe-start)
         )
  )

(use-package projectile-rails
  :ensure t
  :defer t
  :after (helm projectile)
  :hook (prog-mode . projectile-rails-mode)
  )

;; ------------------------------------------------------------------------------
;; Language: C++

(use-package irony
  :ensure t
  :defer t
  :init
  :config

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
  )

;; -----------------------------------------------------------------------------
;; Language: Python

;; Auto complete with Python
;; (use-package anaconda-mode
;;   :ensure t
;;   :defer t
;;   :mode ("\\.py\\'" . python-mode)
;;   :interpreter "anaconda-mode"
;;   :hook (python-mode-hook . anaconda-mode)
;;   )
;;
;; (use-package company-anaconda
;;   :ensure t
;;   :init
;;   (eval-after-load "company"
;;     '(add-to-list 'company-backends 'company-anaconda))
;;   )

;; -----------------------------------------------------------------------------
;; Language: Python

(use-package jedi
  :ensure t
  :init
  ;; TODO: Set on Windows
  (setq-default py-python-command "/usr/bin/python3")
  (add-hook 'python-mode-hook 'jedi:setup)
  )

(use-package company-jedi
  :ensure t
  :config

  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))

  (add-hook 'python-mode-hook 'my/python-mode-hook)
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

;; -----------------------------------------------------------------------------
;; Language: Markdown

(use-package markdown-mode
  :ensure t
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  )

(provide 'init-languages)
;;; init-languages.el ends here
