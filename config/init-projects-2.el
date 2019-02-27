;;; package --- Emacs configuration with batteries included

;;; ----------------------------------------------------------------------------
;;; Commentary:

;;; ----------------------------------------------------------------------------
;;; Code:

(use-package helm
  :ensure t
  )

(use-package projectile
  :ensure t
  :after (helm)
  :bind (
         ;; Project navigation
         ("C-o" . helm-find-files)
         ("S-C-o" . helm-projectile-switch-project)
         ("C-p" . helm-projectile-find-file)
         )
  :config
  (setq projectile-completion-system 'helm)
  (projectile-mode)
  )

(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  :config
  (helm-projectile-on)
  )

(provide 'init-projects)
;;; init-projects.el ends here
