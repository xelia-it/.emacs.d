;; -----------------------------------------------------------------------------
;; Other config scripts

(require 'use-package)

(use-package helm
  :ensure t
  :config
  (require 'helm-config))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode))

(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  :config
  (helm-projectile-on))
