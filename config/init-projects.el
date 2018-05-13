;;; package --- Emacs configuration with batteries included

;;; ----------------------------------------------------------------------------
;;; Commentary:

;;; ----------------------------------------------------------------------------
;;; Code:

(require 'use-package)

(use-package helm
  :ensure t
  :config
  (require 'helm-config))

(use-package projectile
  :ensure t
  :config
  (projectile-mode))

(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  :config
  (helm-projectile-on))

(use-package magit
  :ensure t
  )

(use-package projectile-rails
  :ensure t
  :after (helm projectile)
  :config
  (projectile-rails-global-mode))

(provide 'init-projects)
;;; init-projects.el ends here
