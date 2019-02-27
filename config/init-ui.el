;;

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

  ;; Cursor settings
;;  (set-cursor-color "#fff")         ;; Set cursor color
  ;;(blink-cursor-mode)             ;; Blink cursor
  ;; Cursor like a bar (On Linux/Mac)
  ;;(if (memq window-system '(mac ns x))
  ;;    (setq-default cursor-type 'bar))

  (global-hl-line-mode)             ;; Hightlight current line

  ;; Set default font
  ;;(add-to-list 'default-frame-alist
  ;;             '(font . "DejaVu Sans Mono-12"))


  ;; Show parenthesis
  (require 'paren)
  ;;(set-face-background 'show-paren-match "#282C34")
  ;;(set-face-foreground 'show-paren-match "#fff")
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
  (defvar match-paren--idle-timer nil)
  (defvar match-paren--delay 0.5)
  (setq match-paren--idle-timer
        (run-with-idle-timer match-paren--delay t #'blink-matching-open))
  (show-paren-mode 1)
  
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
;;    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")
    )

  ;; Show line numbers
  (setq display-line-numbers "%4d \u2502 ")
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)

  ;; Toggle full screen automatically
  (run-with-idle-timer 0.1 nil 'toggle-frame-fullscreen)
  
  )


(use-package moody
  :ensure t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package minimap
  :ensure t
  :after (atom-one-dark)
  :config

  ;; Set minimap
  ;;(minimap-mode)

  )
