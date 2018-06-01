;;; package --- Emacs configuration with batteries included

;;; ----------------------------------------------------------------------------
;;; Commentary:

;;; ----------------------------------------------------------------------------
;;; Code:

(require 'use-package)

(use-package helm
  :ensure t
  :config
  (require 'helm-config)
  (setq
   helm-always-two-windows t
   helm-split-window-default-side (quote right))
  )

;; A better search package that works with helm
(use-package helm-swoop
  :ensure t
  :after (helm)
  :config
  (set-face-attribute 'helm-swoop-target-word-face nil
                      :background nil
                      :foreground "#ffffff")
  )

(use-package projectile
  :ensure t
  :after (helm)
  :config
  (setq projectile-completion-system 'helm)
  (projectile-mode))

(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  :config
  (helm-projectile-on))

(use-package magit
  :ensure t
  )

(provide 'init-projects)
;;; init-projects.el ends here
