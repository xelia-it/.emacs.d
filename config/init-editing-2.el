;;; package --- Emacs configuration with batteries included

;;; ---------------------------------------------------------------------------------
;;; Commentary:

;;; ----------------------------------------------------------------------------
;;; Code:

;; -----------------------------------------------------------------------------
;; Basic editing preferences

;; Cut-Paste like Windows
(cua-mode t)

;; Standard Windows behaviour
;;(setq-default cua-keep-region-after-copy t)

;; Don't tabify after rectangle commands
;;(setq-default cua-auto-tabify-rectangles nil)

;; Typed text deletes selected text
(delete-selection-mode t)

;; No region when it is not highlighted
(transient-mark-mode 1)

;; Do not use tabs by default
(setq-default indent-tabs-mode nil)

;; Use 4 spaces by default
(setq-default tab-width 4)
(setq-default ruby-indent-level 2)
(setq-default css-indent-offset 4)
