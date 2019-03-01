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
  :hook (prog-mode . flycheck-mode)
  )

(provide 'init-projects)
;;; init-projects.el ends here
