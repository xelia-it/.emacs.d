;;; .emacs.d --- Emacs configuration with batteries included

;;; ----------------------------------------------------------------------------
;;; Commentary:
;;;
;;; Packages for managing projects, navigating into project files,
;;; code completion and on-the-fly checks.

;;; ----------------------------------------------------------------------------
;;; Code:

(use-package helm
  :ensure t
  :bind (
         ("M-x" . helm-M-x)
         ("C-o" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("C-r" . helm-imenu)
         ("S-C-r" . helm-imenu-in-all-buffers)
         ("S-C-V" . helm-kill-ring)
         )
  :init
  (setq helm-display-function 'pop-to-buffer)
  )

(use-package helm-swoop
  :ensure t
  :defer t
  :after (helm)
  :bind (
         ("C-f" . helm-swoop)
         :map isearch-mode-map
         ("C-f" . helm-swoop-from-isearch)
         :map helm-swoop-map
         ("C-f" . helm-next-line)
         :map helm-multi-swoop-map
         ("C-f" . helm-next-line)
         )
  :init

  ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
;;  (setq helm-swoop-split-direction 'split-window-vertically)
  )

(use-package projectile
  :ensure t
  :after (helm)
  :init
  (setq projectile-completion-system 'helm)
  (projectile-mode)
  :bind (
         ;; Compile Project
         ("S-<f9>" . projectile-configure-project)
         ("<f9>" . projectile-compile-project)
         ("C-<f9>" . projectile-run-project)
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
  :defer t
  ;; TODO: this do not work
  ;; :after (helm projectile)
  :bind (
         ("C-S-f" . helm-projectile-grep)
         ("S-C-o" . helm-projectile-switch-project)
         ("C-t" . helm-projectile-find-file)
         )
  :init
  (helm-projectile-on)
  )




;; On the fly check
(use-package flycheck
  :ensure t
  :defer t
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
  ;; Apply "ball" icon to errors, warnings and info
  (flycheck-define-error-level 'error
    :severity 100
    :compilation-level 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-error
    :error-list-face 'flycheck-error-list-error)

  (flycheck-define-error-level 'warning
    :severity 200
    :compilation-level 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-warning
    :error-list-face 'flycheck-error-list-warning)

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

(use-package company
  :ensure t
  :defer t
  :config
  (setq company-idle-delay 0
        company-echo-delay 0
        company-dabbrev-downcase nil
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-transformers '(company-sort-by-occurrence
                               company-sort-by-backend-importance))
  ;; (global-company-mode)
  :hook (prog-mode . company-mode)
  )

(use-package company-quickhelp
  :ensure t
  :defer t
  :after (company)
  :config
  (company-quickhelp-mode))

;; -----------------------------------------------------------------------------
;; Git support

(use-package magit
  :ensure t
  :bind (
         ("<f5>" . magit-status)
         ("<f6>" . magit-log)
	)
  )

(use-package git-gutter
  :ensure t
  :config
  ;; If you enable global minor mode
  ;;(global-git-gutter-mode t)

  ;; If you would like to use git-gutter.el and linum-mode
  ;; (git-gutter:linum-setup)

  ;;
  ;; Alternatives: ("▐")
  (setq git-gutter:modified-sign "❙")
  (setq git-gutter:added-sign "❙")
  (setq git-gutter:deleted-sign "❙")
  (setq git-gutter:update-interval 2)
 ;; (custom-set-variables
 ;;  '(git-gutter:modified-sign "❙")
 ;;  '(git-gutter:added-sign "❙")
 ;;  '(git-gutter:deleted-sign "❙")
 ;;  '(git-gutter:update-interval 2)
 ;;  '(git-gutter:visual-line t)
 ;;  )

  ;; If you enable git-gutter-mode for some modes
  (add-hook 'ruby-mode-hook 'git-gutter-mode)

  (global-set-key (kbd "C-x C-g") 'git-gutter)
  (global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)

  ;; Jump to next/previous hunk
  (global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
  (global-set-key (kbd "C-x n") 'git-gutter:next-hunk)

  (set-face-foreground 'git-gutter:modified "#61AFEF")
  (set-face-foreground 'git-gutter:added "#E5C07B")
  (set-face-foreground 'git-gutter:deleted "#E06C75")

  (global-git-gutter-mode +1)
  )

(provide 'init-projects)
;;; init-projects.el ends here
