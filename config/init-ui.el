;;; package --- Emacs configuration with batteries included

;;; ----------------------------------------------------------------------------
;;; Commentary:

;;; ----------------------------------------------------------------------------
;;; Code:

(use-package atom-one-dark-theme
  :ensure t

  :init
  ;; Removes *messages* from the buffer list.
  (setq-default message-log-max nil)
  (kill-buffer "*Messages*")

  ;; Removes *Completions* from buffer after you've opened a file.
  (add-hook 'minibuffer-exit-hook
            '(lambda ()
               (let ((buffer "*Completions*"))
                 (and (get-buffer buffer)
                      (kill-buffer buffer)))))


  ;; No more default Emacs splash screen
  (setq inhibit-splash-screen t)

  ;; Empty scratch buffer
  (setq initial-scratch-message nil)

  ;; Empty minibuffer message
  (defun display-startup-echo-area-message ()
    "Overwrite default startup message."
    (message ""))
  
  :config

  ;; Load default theme
  (load-theme 'atom-one-dark t)

  ;; Frame settings
  (tool-bar-mode -1)                ;; Disable toolbar on top
  (menu-bar-mode -1)                ;; Disable menu bar on top
  (scroll-bar-mode -1)              ;; No more scrollbars

  ;; A very light color compatible with atom-one-dark-theme
  (defvar near-to-white-color "#86e6f2")

  ;; Set cursor color
  (set-cursor-color near-to-white-color)
  ;; Blink cursor
  (blink-cursor-mode)
  ;; Cursor like a bar (works only on Linux/Mac)
  (if (memq window-system '(mac ns x))
      (setq-default cursor-type 'bar))

  (global-hl-line-mode)             ;; Hightlight current line

  ;; Set default font
  ;;(add-to-list 'default-frame-alist
  ;;             '(font . "DejaVu Sans Mono-12"))
      
  ;; Show parenthesis
  (require 'paren)
  ;;(set-face-background 'show-paren-match "#282C34")
  (set-face-background 'show-paren-match (face-background 'font-lock-builtin-face))
  (set-face-foreground 'show-paren-match near-to-white-color)
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
  (defvar match-paren--idle-timer nil)
  (defvar match-paren--delay 0.5)
  (setq match-paren--idle-timer
        (run-with-idle-timer match-paren--delay t #'blink-matching-open))
  (show-paren-mode 1)

  ;; Show line numbers
  (setq display-line-numbers-width 5)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)

  ;; Toggle full screen automatically
  (run-with-idle-timer 0.1 nil 'toggle-frame-fullscreen)

  ;; Use ESC to quit command. This free Ctrl-G for moving to a specific line.
  (global-unset-key (kbd "<escape>"))
  (global-unset-key (kbd "C-g"))
  ;; Use escape for "abort" operations
  ;; (company needs a specific command)
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  ;; (define-key company-mode-map (kbd "<escape>") 'company-abort)

  :bind (
         ("C-s" . 'save-buffer)
         ("S-C-s" . 'my-save-all)
         ("C-w" . 'kill-buffer-and-window)
         ("S-C-W" . 'my-kill-other-buffers)
         )
  )

(use-package doom-modeline
  :ensure t
  :init
 
  ;; Don’t compact font caches during GC.
  (setq inhibit-compacting-font-caches t)
  ;; Avoid strange name when visiting
  (setq find-file-visit-truename t)
  ;; Check VC info
  (setq auto-revert-check-vc-info t)

  :hook (after-init . doom-modeline-mode)
  )

(use-package shackle
  :ensure t
  :config
  (setq shackle-rules '(
                        ("\\`\\*[hH]elm.*?\\*\\'" :regexp t :align 'below :size 0.4)
                        ("\\`\\*Helm Swoop.*?\\*\\'" :regexp t :align 'below :size 0.2)
                        ("\\`\\*Flycheck.*?\\*\\'" :regexp t :align 'below :size 0.2)
                        ))
  (shackle-mode 1)
  )

(provide 'init.ui)
;; init-ui.el ends here
