;;; package --- Emacs configuration with batteries included

;;; ----------------------------------------------------------------------------
;;; Commentary:
;;;
;;; Keybindings

;;; ----------------------------------------------------------------------------
;;; Code:

;; Annoying
(global-unset-key (kbd "<f2> <f2>"))

;; Magit

(define-key magit-mode-map (kbd "C-w") 'magit-mode-bury-buffer)


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

;; Editing
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
