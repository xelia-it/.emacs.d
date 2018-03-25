;;; package --- Emacs configuration with batteries included

;;; ----------------------------------------------------------------------------
;;; Commentary:

;;; ----------------------------------------------------------------------------
;;; Code:

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


;; No more default Emacs splash screen
(setq inhibit-splash-screen t)

;; Empty scratch buffer
(setq initial-scratch-message nil)

;; Empty minibuffer message
(defun display-startup-echo-area-message ()
  "Overwrite default startup message."
  (message ""))

;; -----------------------------------------------------------------------------
;; Theme

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

(use-package mode-icons
  :ensure t
  :config
  (mode-icons-mode t)
)

(use-package all-the-icons
  :ensure t
  )

(use-package powerline
  :ensure t
  :after (mode-icons all-the-icons)
  :config
  ;; TODO: experiments
  ;; Definitions
  (defvar mode-line-height
    24 "A little bit taller, a little bit baller.")
  (defvar mode-line-bar
    (eval-when-compile (pl/percent-xpm mode-line-height 100 0 100 0 3 "#909fab" nil)))
  (defvar mode-line-inactive-bar
    (eval-when-compile (pl/percent-xpm mode-line-height 100 0 100 0 3 "#333333" nil)))

  (defun my-powerline-default-theme()
    "Setup a nano-like mode-line."
    (interactive)
    (setq-default mode-line-format
                  '("%e"
                    (:eval
                     (let* ((active (powerline-selected-window-active))
                            (face0 (if active 'powerline-active0 'powerline-inactive0))
                            (lhs (list
                                  (propertize " " 'display (if active mode-line-bar mode-line-inactive-bar))

                                  (powerline-raw "%*" face0 'l)
                                  (powerline-major-mode face0 'l)
                                  (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
                                  (powerline-process face0 'l)
                                  ))
                            (rhs (list
                                  ;; (powerline-raw
                                  ;;  (propertize (all-the-icons-faicon "code-fork")
                                  ;;              'face `(:family ,(all-the-icons-octicon-family) :height 1.2)
                                  ;;              'display '(raise -0.1))
                                  ;;  face0 'l)
                                  (powerline-vc face0 'l)
                                  (powerline-raw "%4l" face0 'l)
                                  (powerline-raw ":" face0 'l)
                                  (powerline-raw "%3c" face0 'r)
                                  (powerline-fill face0 0)
                                  ))
                            )
                       (concat (powerline-render lhs)
                               (powerline-fill face0 (powerline-width rhs))
                               (powerline-render rhs)))))))

  (my-powerline-default-theme)
  )


;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------

(provide 'init-face)
;;; init-face.el ends here
