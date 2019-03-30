;;; package --- Emacs configuration with batteries included

;;; ----------------------------------------------------------------------------
;;; Commentary:

;;; ----------------------------------------------------------------------------
;;; Code:

(defun my-unbind-keys ()
  "Unbind keys."
  (dolist (key-to-unset '("C-a" "C-b" "C-d" "C-e" "C-f" "C-g"
                          "C-h" "C-k" "C-l" "C-n" "C-o" "C-p"
                          "C-t" "C-v" "C-z"
                          "<escape>"
                          "M-<up>" "M-<down>"
                          "S-M-<up>" "S-M-<down>"
                          "S-C-<up>" "S-C-<down>"
                          "C-M-<up>" "C-M-<down>"
                          "C-<space>"
                          ))
    (global-unset-key (kbd key-to-unset)))
  )

(defun my-kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer
          (delq (current-buffer)
                (remove-if-not 'buffer-file-name (buffer-list))))
    (message "Killed other buffers")
    )

;; -----------------------------------------------------------------------------

;; The theme is very similar to Atom colors.
(use-package atom-one-dark-theme
  :ensure t
  :init

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

  ;; Removes *messages* from the buffer list.
;;  (setq-default message-log-max nil)
 ;; (kill-buffer "*Messages*")

  ;; Removes *Completions* from buffer after you've opened a file.
  (add-hook 'minibuffer-exit-hook
            '(lambda ()
               (let ((buffer "*Completions*"))
                 (and (get-buffer buffer)
                      (kill-buffer buffer)))))


  ;; A very light color compatible with atom-one-dark-theme
  (defvar near-to-white-color "#86e6f2")

  ;; Use ESC to quit command. This free Ctrl-G for moving to a specific line.
  ;; Use escape for "abort" operations
  ;; (company needs a specific command)
  (my-unbind-keys)

  ;; Set cursor color
  (set-cursor-color near-to-white-color)

  ;; Blink cursor
  (blink-cursor-mode)

  ;; TODO: multiple-cursor do not work with bar cursor
  ;; Cursor like a bar (works only on Linux/Mac)
  ;;(if (memq window-system '(mac ns x))
  ;;    (setq-default cursor-type 'bar))

  (global-hl-line-mode)             ;; Hightlight current line

  ;; Do not word-wrap lines
  (setq-default truncate-lines t)

  ;; Fix mouse scroll
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
  (setq mouse-wheel-progressive-speed nil)

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
  (set-face-foreground 'line-number (face-foreground 'font-lock-comment-face))
  (set-face-foreground 'line-number-current-line (face-foreground 'font-lock-builtin-face))
  (set-face-background 'line-number-current-line (face-background 'hl-line))
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)

  ;; Toggle full screen automatically
  (run-with-idle-timer 0.1 nil 'toggle-frame-fullscreen)

  :bind (
         ("C-w" . 'kill-buffer-and-window)
         ("S-C-W" . 'my-kill-other-buffers)
         ("<escape>" . 'keyboard-escape-quit)
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
  (setq-default auto-revert-check-vc-info t)

  (add-hook 'doom-modeline-mode-hook 'column-number-mode)

  :hook (after-init . doom-modeline-mode)
  )

(use-package shackle
  :ensure t
  :config
  (setq shackle-rules '(
                        ("\\`\\*Flycheck.*?\\*\\'" :regexp t :align 'below :size 0.2)
                        ("\\`\\*[hH]elm.*?\\*\\'" :regexp t :align 'below :size 0.3)
                        ))
  (shackle-mode 1)
  )

(provide 'init.ui)
;;; init-ui.el ends here
