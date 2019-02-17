;;; package --- Emacs configuration with batteries included

;;; ----------------------------------------------------------------------------
;;; Commentary:
;;;
;;; Keybindings

;;; ----------------------------------------------------------------------------
;;; Code:

;; M-x is better managed by helm
(global-set-key (kbd "M-x") 'helm-M-x)

;; Project navigation
(global-set-key (kbd "C-o") 'helm-find-files)
(global-set-key (kbd "S-C-o") 'helm-projectile-switch-project)

(global-set-key (kbd "C-p") 'helm-projectile-find-file)

;; Annoying
(global-unset-key (kbd "<f2> <f2>"))

;; Magit
(global-set-key (kbd "<f5>") 'magit-status)
(global-set-key (kbd "<f6>") 'magit-log-all)
(define-key magit-mode-map (kbd "C-w") 'magit-mode-bury-buffer)

;; Compile Project
(global-set-key (kbd "S-<f9>") 'projectile-configure-project)
(global-set-key (kbd "<f9>") 'projectile-compile-project)
(global-set-key (kbd "C-<f9>") 'projectile-run-project)
(global-set-key (kbd "M-<f9>") 'projectile-test-project)
;; Move to the previous error found during compiling
(global-set-key (kbd "<f10>") 'previous-error)
;; Move to the next error found during compiling
(global-set-key (kbd "<f11>") 'next-error)
;; Show flycheck errors
(global-set-key (kbd "<f12>") 'flycheck-list-errors)

;; Ctrl-<space>: code completion (with company)
(global-unset-key (kbd "C-SPC"))
(global-set-key (kbd "C-SPC") 'company-complete-common)

;; Set key bindings
(define-key c++-mode-map (kbd "C-.") 'helm-gtags-find-tag)
(define-key c++-mode-map (kbd "C-:") 'helm-gtags-pop-stack)
(define-key web-mode-map (kbd "C-.") 'helm-gtags-find-tag)
(define-key web-mode-map (kbd "C-:") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-.") 'helm-gtags-find-tag)
(define-key helm-gtags-mode-map (kbd "C-:") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-find-rtag)
(define-key helm-gtags-mode-map (kbd "C-M-.") 'helm-gtags-find-symbol)
(global-unset-key (kbd "C-j"))
(global-set-key (kbd "C-j") 'helm-imenu)

;; Save buffers
(global-unset-key (kbd "C-s"))
(global-set-key (kbd "C-s") 'my-save)

(global-unset-key (kbd "S-C-s"))
(global-set-key (kbd "S-C-s") 'my-save-all)

;; Change Search and Replace keys
(global-unset-key (kbd "C-f"))
(global-set-key (kbd "C-f") 'helm-occur)
(global-unset-key (kbd "C-S-f"))
(global-set-key (kbd "C-S-f") 'helm-projectile-grep)

(define-key global-map (kbd "C-r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; if you use multiple-cursors, this is for you:
;;(define-key global-map (kbd "C-m") 'vr/mc-mark)

;; Move between buffers
(global-unset-key (kbd "C-b"))
(global-set-key (kbd "C-b") 'helm-buffers-list)
(global-set-key (kbd "C-<tab>") 'next-buffer)
(global-set-key (kbd "C-<iso-lefttab>") 'previous-buffer)
(global-set-key (kbd "C-w") 'kill-this-buffer)
(global-set-key (kbd "S-C-w") 'kill-other-buffers)

;; Editing
(global-set-key (kbd "S-C-v") 'helm-show-kill-ring)
(global-set-key (kbd "M-<left>") 'move-beginning-of-line)
(global-set-key (kbd "M-<right>") 'move-end-of-line)
(global-unset-key (kbd "C-d"))


;; In C++ mode some keystrokes overlaps: force unset.


(defun my-disable-ctrl-d ()
  "Disable Ctrl-D."
  (local-unset-key (kbd "C-d"))
  (local-unset-key (kbd "C-c C-u"))
  )
(add-hook 'c++-mode-hook 'my-disable-ctrl-d)

;; Whitespace
(global-set-key (kbd "C-c C-w") 'whitespace-mode)
(global-set-key (kbd "C-c C-i") 'highlight-indent-guides-mode)

;; Change case
(global-set-key (kbd "C-c C-u") 'string-inflection-all-cycle)

;; Switch cpp <--> hpp
(global-set-key (kbd "S-C-a") 'ff-find-related-file)

;; Use ESC to quit command. This free Ctrl-G for moving to a specific line.
(global-unset-key (kbd "<escape>"))
(global-unset-key (kbd "C-g"))

;; Use escape for "abort" operations
;; (company needs a specific command)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(define-key company-mode-map (kbd "<escape>") 'company-abort)

;; Use goto-line for Ctrl-G
;; The mapping do not work in c-mode and c++ mode
;; so we need to force the behaviour
(define-key prog-mode-map (kbd "C-g") 'goto-line)
(define-key c-mode-map (kbd "C-g") 'goto-line)
(define-key c++-mode-map (kbd "C-g") 'goto-line)
(global-set-key (kbd "C-g") 'goto-line)

;; Bookmarks - using helm
(global-set-key (kbd "C-<f2>") 'bm-toggle)
(global-set-key (kbd "S-<f2>") 'bm-previous)
(global-set-key (kbd "<f2>") 'bm-next)

(provide 'init-keybinding)
;;; init-keybinding.el ends here
