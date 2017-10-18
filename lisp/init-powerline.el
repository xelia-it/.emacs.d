;;; package --- Summary

;;; ----------------------------------------------------------------------------
;;; Commentary:


;;; ----------------------------------------------------------------------------
;;; Code:

;; Change mode line
(use-package powerline :ensure t :init)

;;;###autoload
(defun powerline-my-theme ()
  "Setup a nano-like mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          ;; face2 is always inactive
                          (mode-line (if active 'mode-line 'mode-line-inactive))

                          (face1 (if active 'mode-line 'mode-line-inactive))
                          (face2 'mode-line-inactive)

                          (lhs (list
                                (powerline-raw (if (buffer-modified-p) " * " " - ") face1 'l)
                                (powerline-vc face1 'l)
                                ))

                          (rhs (list
                                (powerline-raw "%3l : %3c %6p" face1 'r)
                                ))

                          (center (list
                                   (powerline-raw " %b" face1)
                                   )))

                     (concat (powerline-render lhs)
                             (powerline-fill-center face1 (/ (powerline-width center) 2.0))
                             (powerline-render center)
                             (powerline-fill face1 (powerline-width rhs))
                             (powerline-render rhs))))))
  )

;; Load the above theme
(powerline-my-theme)

;; Experiment with powerline
;;(custom-set-faces
;; '(mode-line ((t (:foreground "#030303" :background "#f8d85c" :box nil))))
;; '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#030303" :box nil))))
;; )

;; ----------------------------------------------------------------------------

(provide 'init-powerline)
;;; init-powerline.el ends here
