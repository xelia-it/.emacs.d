;;; package --- Emacs configuration with batteries included

;;; ----------------------------------------------------------------------------
;;; Commentary:

;;; ----------------------------------------------------------------------------
;;; Code:

(use-package markdown-mode
  :ensure t
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer
          (delq (current-buffer)
                (remove-if-not 'buffer-file-name (buffer-list))))
    (message "Killed other buffers")
    )

(defun my-save ()
  "Save current buffer without confirmation."
  (interactive)
  (save-buffer t)
  )

(defun my-save-all ()
  "Save all buffers without confirmation and refresh magit."
  (interactive)
  (save-some-buffers t)
  (magit-refresh-all)
  )

(use-package org-pomodoro
  :ensure t
  :defer t)

(provide 'init-utils)
;;; init-utils.el ends here
