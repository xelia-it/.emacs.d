;; -----------------------------------------------------------------------------
;; Language: HTML

(use-package web-mode
  :ensure t
  :defer t
  :config

  ;; associate extensions with web-mode
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.handlebars\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  
  ;; Configuration
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
  ;;(set-face-attribute 'web-mode-block-face nil :background nil)
  ;;(set-face-attribute 'web-mode-inlay-face nil :background nil)
  ;;(set-face-attribute 'web-mode-current-column-highlight-face nil
  ;;                    :foreground "#ffffff" :background nil)
  ;;(set-face-attribute 'web-mode-current-element-highlight-face nil
  ;;                    :foreground "#ffffff" :background nil :weight 'extra-bold)

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
;;  :after (company)
  :config
  ;;(add-to-list 'company-backends 'company-robe)
  )

