;;; package --- Emacs configuration with batteries included

;;; ----------------------------------------------------------------------------
;;; Commentary:

;;; ----------------------------------------------------------------------------
;;; Code:

;; No more default Emacs splash screen
(setq inhibit-splash-screen t)

;; Empty scratch buffer
(setq initial-scratch-message nil)

;; Empty minibuffer message
(defun display-startup-echo-area-message ()
  "Overwrite default startup message."
  (message ""))

;; Setup color theme and window
(use-package atom-one-dark-theme
  :ensure t
  :init
  :config

  ;; Frame settings
  (tool-bar-mode -1)                ;; Disable toolbar on top
  (menu-bar-mode -1)                ;; Disable menu bar on top
  (scroll-bar-mode -1)              ;; No more scrollbars
  (load-theme 'atom-one-dark t)     ;; Load theme (t = force without warning)
  (toggle-frame-maximized)          ;; Start with maximized frame
  (split-window-right)
  ;; Cursor settings
  (set-cursor-color "#fff")         ;; Set cursor color
  (blink-cursor-mode t)             ;; Blink cursor
  (setq-default cursor-type 'bar)   ;; Cursor like a bar
  (global-hl-line-mode)             ;; Hightlight current line
  )

;; Show parenthesis
(require 'paren)
(set-face-background 'show-paren-match "#282C34")
(set-face-foreground 'show-paren-match "#fff")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)
(defvar match-paren--idle-timer nil)
(defvar match-paren--delay 0.5)
(setq match-paren--idle-timer
      (run-with-idle-timer match-paren--delay t #'blink-matching-open))
(show-paren-mode 1)

;; -----------------------------------------------------------------------------
;; Powerline

(use-package powerline
  :ensure t)

(use-package spaceline
  :ensure t
  :after (powerline)
  :config

  ;; Show only a subset of all information available
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-buffer-encoding-off)
  (spaceline-toggle-buffer-encoding-abbrev-off)
  (spaceline-toggle-hud-off)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-battery-off)
  (spaceline-toggle-buffer-position-off)
  )

;; -----------------------------------------------------------------------------
;; Cleanup interface and buffers

;; Removes *messages* from the buffer list.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer)))))

;; -----------------------------------------------------------------------------

(provide 'init-face)
;;; init-face.el ends here
