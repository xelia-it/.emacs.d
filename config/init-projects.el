;;; package --- Emacs configuration with batteries included

;;; ----------------------------------------------------------------------------
;;; Commentary:

;;; ----------------------------------------------------------------------------
;;; Code:

(require 'use-package)

(use-package helm
  :ensure t
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

(use-package shackle
  :ensure t
  :after (helm)
  :config
  (setq shackle-rules '(
                        ("\\`\\*[hH]elm.*?\\*\\'" :regexp t :align 'below :size 0.4)
                        ))
  (shackle-mode 1)
  )

;; -----------------------------------------------------------------------------
;; Git support

(use-package magit
  :ensure t
  )

(use-package git-gutter
  :ensure t
  :config
  ;; If you enable global minor mode
  (global-git-gutter-mode t)

  ;; If you would like to use git-gutter.el and linum-mode
  ;; (git-gutter:linum-setup)

  (custom-set-variables
   '(git-gutter:modified-sign "▐")
   '(git-gutter:added-sign "▐")
   '(git-gutter:deleted-sign "▐")
   '(git-gutter:update-interval 2)
   '(git-gutter:visual-line t)
   )

  ;; If you enable git-gutter-mode for some modes
  (add-hook 'ruby-mode-hook 'git-gutter-mode)

  (global-set-key (kbd "C-x C-g") 'git-gutter)
  (global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)

  ;; Jump to next/previous hunk
  (global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
  (global-set-key (kbd "C-x n") 'git-gutter:next-hunk)

  (set-face-foreground 'git-gutter:modified "lightblue")
  (set-face-foreground 'git-gutter:added "green")
  (set-face-foreground 'git-gutter:deleted "red")
  )

(provide 'init-projects)
;;; init-projects.el ends here
