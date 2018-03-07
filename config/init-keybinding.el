;; -------------

;; ----------------------------------------------------------------------------
;;  Keybindings

(global-set-key (kbd "M-x") 'helm-M-x)

;; Project navigation
(global-set-key (kbd "C-o") 'helm-find-files)
(global-set-key (kbd "S-C-o") 'helm-projectile-switch-project)

(global-set-key (kbd "C-p") 'helm-projectile-find-file)

(global-set-key (kbd "C-j") 'helm-imenu)
(global-set-key (kbd "S-C-j") 'helm-occur)

;; Compile Project
(global-set-key (kbd "<f9>") 'projectile-compile-project)
;; Move to the previous error found during compiling
(global-set-key (kbd "<f10>") 'previous-error)
;; Move to the next error found during compiling
(global-set-key (kbd "<f11>") 'next-error)
;; Show flycheck errors
(global-set-key (kbd "<f12>") 'flycheck-list-errors)

;; Ctrl-<space>: code completion (with company)
(global-set-key (kbd "C-SPC") 'company-complete-common)
;;(define-key c-mode-map (kbd "C-SPC") 'company-complete)
;;(define-key c++-mode-map (kbd "C-SPC") 'company-complete)

;; Ctrl-S: save current file
(global-unset-key (kbd "C-s"))
(global-set-key (kbd "C-s") 'save-buffer)

;; Shift-Ctrl-S: save all files without confirmation
(defun save-all ()
  "Save buffer without confirmation."
  (interactive) (save-some-buffers t))
(global-unset-key (kbd "S-C-s"))
(global-set-key (kbd "S-C-s") 'save-all)

;; Close all buffers
(global-set-key (kbd "S-C-w") 'close-all-buffers)

;; Change Search and Replace keys
(global-unset-key (kbd "C-f"))
(global-set-key (kbd "C-f") 'helm-swoop)
(global-unset-key (kbd "C-S-f"))
(global-set-key (kbd "C-S-f") 'helm-projectile-grep)

(define-key global-map (kbd "C-r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; if you use multiple-cursors, this is for you:
;;(define-key global-map (kbd "C-m") 'vr/mc-mark)

;; Switch from .c/.h and vicevarsa
;; (global-set-key (kbd "C-S-a") 'ff-find-other-file)

;; Move between buffers
(global-set-key (kbd "M-<right>") 'next-buffer)
(global-set-key (kbd "M-<left>") 'previous-buffer)
(global-set-key (kbd "C-w") 'kill-this-buffer)
(global-set-key (kbd "C-<tab>") 'other-window)

;; Editing
(global-unset-key (kbd "C-d"))
(global-set-key (kbd "M-S-<down>") 'mc/mark-next-like-this)
(global-set-key (kbd "M-S-<up>") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-d") 'mc/mark-all-symbols-like-this-in-defun)
(global-set-key (kbd "S-C-d") 'mc/mark-next-like-this-word)

;; In C++ mode the Ctrl-D is reactivated: force unset.
(defun my-disable-ctrl-d ()
  "Disable Ctrl-D."
  (local-unset-key (kbd "C-d"))
  )
(add-hook 'c++-mode-hook 'my-disable-ctrl-d)



;; Activate whitespace-mode to view all whitespace characters
;;(global-set-key (kbd "C-c w") 'whitespace-mode)

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
;;(define-key prog-mode-map (kbd "C-g") 'goto-line)
;;(define-key c-mode-map (kbd "C-g") 'goto-line)
;;(define-key c++-mode-map (kbd "C-g") 'goto-line)
(global-set-key (kbd "C-g") 'goto-line)

;; Bookmarks - using helm
(global-set-key (kbd "C-*") 'bookmark-delete)
(global-set-key (kbd "C-,") 'bookmark-set)
(global-set-key (kbd "C-.") 'helm-filtered-bookmarks)
