;;; .emacs.d --- Emacs configuration with batteries included

;;; ----------------------------------------------------------------------------
;;; Commentary:
;;;
;;; Packages for managing projects, navigating into project files,
;;; code completion and on-the-fly checks.

;;; ----------------------------------------------------------------------------
;;; Code:

;; -----------------------------------------------------------------------------
;; Code navigation

(use-package helm
  :ensure t
  :after (atom-one-dark-theme)
  :config
  (setq helm-display-function 'pop-to-buffer)
  (set-face-attribute 'isearch nil
                      :foreground "#ffffff" :background nil :weight 'bold)
  (set-face-attribute 'helm-match nil
                      :foreground "#ffffff" :background nil :weight 'bold)
  :bind (
         ("M-x" . helm-M-x)
         ("C-o" . helm-find-files)
         ("C-j" . helm-imenu)
         ("S-C-j" . helm-imenu-in-all-buffers)
         ("C-x b" . helm-buffers-list)
         ("C-<tab>" . helm-buffers-list)
         ("C-<iso-lefttab>" . helm-buffers-list)
         ("C-b" . helm-buffers-list)
         ("C-x C-b" . helm-buffers-list)
         ("S-C-v" . helm-show-kill-ring)
         ("C-f" . helm-occur)
         :map helm-map
         ("C-<tab>" . helm-next-line)
         ("C-<iso-lefttab>" . helm-previous-line)
         )
  )

(use-package projectile
  :ensure t
  :after (helm)
  :init
  (setq projectile-completion-system 'helm)
  (projectile-mode)
  :bind (
         ;; Compile Project
         ("<f9>" . projectile-compile-project)
         ("C-<f9>" . projectile-run-project)
         ("S-<f9>" . projectile-configure-project)
         ("M-<f9>" . projectile-test-project)
         ;; Move to the previous error found during compiling
         ("<f10>" . previous-error)
         ;; Move to the next error found during compiling
         ("<f11>" . next-error)
         ;; Show flycheck errors
         ("<f12>" . flycheck-list-errors)
         )
  )

(use-package helm-projectile
  :ensure t
;;  :defer t
  ;; TODO: this do not work
  ;; :after (helm projectile)
  :bind (
         ("C-S-f" . helm-projectile-grep)
         ("S-C-o" . helm-projectile-switch-project)
         ("C-t" . helm-projectile-find-file)
         )
  :config
  (helm-projectile-on)
  )

;; -----------------------------------------------------------------------------
;; On the fly check

(use-package flycheck
  :ensure t
;;  :defer t
  :config
  ;; Disable ruby-reek checker: it's too verbose
  (setq-default flycheck-disabled-checkers '(ruby-reek))
  ;; Change flycheck icons
  (define-fringe-bitmap 'flycheck-fringe-bitmap-ball
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00111000
            #b01111100
            #b11111110
            #b11111110
            #b01111100
            #b00111000
            #b00000000
            #b00000000
            #b00000000
            #b00000000))
  ;; Apply "ball" icon to errors ..
  (flycheck-define-error-level 'error
    :severity 100
    :compilation-level 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-error
    :error-list-face 'flycheck-error-list-error)
  ;; .. warnings ..
  (flycheck-define-error-level 'warning
    :severity 200
    :compilation-level 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-warning
    :error-list-face 'flycheck-error-list-warning)
  ;; .. and infos
  (flycheck-define-error-level 'info
    :severity 300
    :compilation-level 0
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-info
    :error-list-face 'flycheck-error-list-info)

  :hook (prog-mode . flycheck-mode)
  )

;; -----------------------------------------------------------------------------
;; Autocomplete

(use-package company
  :ensure t
  :diminish company-mode
  :after (atom-one-dark-theme)
  :config
  (message "Config company")
  (setq company-auto-complete nil
        company-idle-delay 0.05
        company-minimum-prefix-length 2
        company-tooltip-limit 10
        company-selection-wrap-around t
        )
  :bind (
         ("C-SPC" . company-complete)
         :map company-active-map
         ("<escape>" . company-abort)
         )
  :hook (prog-mode . company-mode)
  ;; (global-company-mode 1)
  )

;; Quick help during autocomplete
(use-package company-quickhelp
  :ensure t
  ;;:defer t
  :after (company)
  :config
  (company-quickhelp-mode))

;; -----------------------------------------------------------------------------
;; Git support

(use-package lsp-mode
  :ensure t
  :commands lsp
  :config

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("solargraph" "stdio"))
                    :major-modes '(ruby-mode)
                    :server-id 'solargraph))
  )

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package company-lsp
  :ensure t
  :commands company-lsp)

;; -----------------------------------------------------------------------------
;; Git support

(use-package magit
  :ensure t
  :config

  (defun my-save ()
    "Save current buffer without confirmation."
    (interactive)
    (save-buffer t)
    (magit-refresh-all)
    )

  (defun my-save-all ()
    "Save all buffers without confirmation and refresh magit."
    (interactive)
    (save-some-buffers t)
    (magit-refresh-all)
    )

  :bind (
         ("C-s" . my-save-buffer)
         ("C-s" . my-save-all)
         ("<f5>" . magit-status)
         ("<f6>" . magit-log-all)
         :map magit-mode-map
         ("C-w" . magit-mode-bury-buffer)
	     )
  )

(use-package git-gutter
  :ensure t
;;  :defer t

  :config
  ;; If you would like to use git-gutter.el and linum-mode
  ;; (git-gutter:linum-setup)

  ;; Modifies gutters "icons".
  ;; Alternatives: ("▐")
  (setq git-gutter:modified-sign "❙")
  (setq git-gutter:added-sign "❙")
  (setq git-gutter:deleted-sign "❙")
  (setq git-gutter:update-interval 2)
  (setq git-gutter:always-show-separator t)
  (setq git-gutter:unchanged-sign " ")

  (set-face-attribute 'git-gutter:modified nil
                      :foreground "#61AFEF" :background (face-background 'font-lock-builtin-face))
  (set-face-attribute 'git-gutter:added nil
                      :foreground "#E5C07B" :background (face-background 'font-lock-builtin-face))
  (set-face-attribute 'git-gutter:deleted nil
                      :foreground "#E06C75" :background (face-background 'font-lock-builtin-face))
  (set-face-background 'git-gutter:unchanged (face-background 'font-lock-builtin-face))

  ;; If you enable global minor mode
  ;;(global-git-gutter-mode t)
  :bind (
         ("<f7>" . git-gutter:previous-hunk)
         ("S-<f7>" . git-gutter:popup-hunk)
         ("<f8>" . git-gutter:next-hunk)
         ("S-<f8>" . git-gutter:popup-hunk)
	     )

  :hook (prog-mode . git-gutter-mode)
  )

(use-package git-timemachine
  :ensure t
  )

(provide 'init-projects)
;;; init-projects.el ends here
