;;; package --- Emacs configuration with batteries included

;;; ----------------------------------------------------------------------------
;;; Commentary:

;;; ----------------------------------------------------------------------------
;;; Code:

;; -----------------------------------------------------------------------------
;;  Package repositories

;; Use MELPA repisitory for download packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; Downloads new packages in case of a fresh install
(package-initialize)

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)

;; -----------------------------------------------------------------------------
;; Custom set variable

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay nil)
 '(company-minimum-prefix-length 1)
 '(company-selection-wrap-around t)
 '(custom-safe-themes
   (quote
    ("a4c9e536d86666d4494ef7f43c84807162d9bd29b0dfd39bdf2c3d845dcc7b2e" default)))
 '(package-selected-packages
   (quote
    (git-gutter helm-gtags yasnippet emmet-mode yasnippet-snippets helm-rtags company company-rtags flycheck-rtags visual-regexp syntax-subword atom-one-dark-theme move-dup yaml-mode ac-html-csswatcher ac-html-bootstrap company-web expand-region highlight-indent-guides company-clang company-c-headers powerline ng2-mode helm-projectile flycheck projectile multiple-cursors helm use-package))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Some variables can be put into .dir-locals-el scripts
(put 'company-clang-arguments 'safe-local-variable (lambda(xx) t))
(put 'flycheck-clang-args 'safe-local-variable (lambda(xx) t))
(put 'projectile-project-compilation-cmd 'safe-local-variable (lambda(xx) t))
(put 'c-default-style 'safe-local-variable (lambda(xx) t))

;; -----------------------------------------------------------------------------
;; Custom scripts and environment variables

;; Our scripts are into a subdirectory
(add-to-list 'load-path "~/.emacs.d/lisp")

;; -----------------------------------------------------------------------------
;; Setup color theme and window

;; Set the default font
(cond
 ((find-font (font-spec :name "Noto Mono"))
  (set-frame-font "Noto Mono-11"))
 ((find-font (font-spec :name "DejaVu Sans Mono"))
  (set-frame-font "DejaVu Sans Mono-11"))
 ((find-font (font-spec :name "Lucida Console"))
  (set-frame-font "Lucida Console-11"))
)

;; Load theme
(use-package atom-one-dark-theme
  :ensure t
  :config (load-theme 'atom-one-dark))

;; Basic settings
(toggle-frame-maximized)          ;; Start with maximized frame
(setq inhibit-startup-message t)  ;; No more Emacs splash screen
(tool-bar-mode -1)                ;; Disable toolbar on top
;; (menu-bar-mode -1)             ;; Uncomment to disable menu bar on top
(scroll-bar-mode -1)              ;; No more scrollbars
(blink-cursor-mode t)             ;; Blink cursor
(setq-default cursor-type 'bar)   ;; Cursor like a bar
(set-cursor-color "#def")      ;; Color white
(global-hl-line-mode)             ;; Hightlight current line

;; Add line numbers to the right
(require 'linum)
;; (global-linum-mode 1)
(setq linum-format " %d ")
(add-hook 'prog-mode-hook #'linum-mode)

;; Show parenthesis
(require 'paren)
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#def")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)
(defvar match-paren--idle-timer nil)
(defvar match-paren--delay 0.5)
(setq match-paren--idle-timer (run-with-idle-timer match-paren--delay t #'blink-matching-open))
(show-paren-mode 1)

;; Makes *scratch* empty.
(setq initial-scratch-message "")
;; Initial minibuffer message
(defun display-startup-echo-area-message ()
  "Overwrite default startup message."
  (message ""))

;; Start with window splitted in vertical
(split-window-horizontally)

;; Removes *messages* from the buffer list.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer)))))

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; Show completion in a separate window
(add-to-list 'special-display-buffer-names '("*Completions*" my-display-completions))

(defun my-display-completions (buf)
  "Put the buffer on the right of the frame.  BUF is the buffer."
  (let ((windows (delete (minibuffer-window) (window-list))))
    (if (eq 1 (length windows))
        (progn
          (select-window (car windows))
          (split-window-horizontally)))
    (let ((target-window (window-at (- (frame-width) 2) 0))
          (pop-up-windows t))
      (set-window-buffer target-window buf)
      target-window)))

;; Load powerline settings
(load "init-powerline")

;; Show indent guides
(use-package highlight-indent-guides :ensure t)

(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-character ?\┆)
;; (setq highlight-indent-guides-character ?\¦)
;; (set-face-foreground 'highlight-indent-guides-character-face "#345")
;; (setq highlight-indent-guides-auto-odd-face-perc 5)
;; (setq highlight-indent-guides-auto-even-face-perc 10)
;; (setq highlight-indent-guides-auto-character-face-perc 15)

(defun close-all-buffers ()
  "Close all buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(global-set-key (kbd "S-C-w") 'close-all-buffers)

;; Move between words
(use-package syntax-subword :ensure t)
(syntax-subword-mode t)

;; Bookmarks - using helm
(global-set-key (kbd "C-*")     'bookmark-delete)
(global-set-key (kbd "C-,")     'bookmark-set)
(global-set-key (kbd "C-.")     'helm-filtered-bookmarks)

;; Load theme
(use-package git-gutter :ensure t)

;; Use git-gutter.el and linum-mode
(git-gutter:linum-setup)

;; Enable git-gutter-mode for some modes
(add-hook 'prog-mode-hook 'git-gutter-mode)

;; Update git-gutter every 2 seconds
(custom-set-variables
 '(git-gutter:update-interval 2))

;; -----------------------------------------------------------------------------
;;  Editing (and movement between files)

;; Setup Ctrl-C, Ctrl-V, Ctrl-X like Windows
(cua-mode t)

;; Standard Windows behaviour
(setq cua-keep-region-after-copy t)

;; Don't tabify after rectangle commands
(setq cua-auto-tabify-rectangles nil)

;; Typed text deletes selected text
(delete-selection-mode t)

;; No region when it is not highlighted
(transient-mark-mode 1)
;; Delete trailing whitespace when save
;; Show trailing whitespaces, tabs, lines
(use-package whitespace :ensure t)
(global-whitespace-mode t)
(setq whitespace-style '(face empty tabs lines-tail trailing))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Do not indent using tab
(setq-default indent-tabs-mode nil)

;; Do not create *~ backup files
(setq make-backup-files nil)

;; No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)

;; Setup multiple cursors. With this package you can have multiple cursors
;; and type the same data into multiple positions at the same time
;; Ctrl-D is used by Emacs su we unset it before binding.

(global-unset-key (kbd "C-d"))
(use-package multiple-cursors
  :ensure t
  :init
  :bind (("M-S-<down>" . mc/mark-next-like-this)
         ("M-S-<up>" . mc/mark-previous-like-this)
         ("S-C-d" . mc/mark-all-symbols-like-this)
         ("C-d" . mc/mark-all-symbols-like-this-in-defun))
  )

;; In C++ mode the Ctrl-D is reactivated: force unset.
(defun my-disable-ctrl-d ()
  "Disable Ctrl-D."
  (local-unset-key (kbd "C-d"))
  )
(add-hook 'c++-mode-hook 'my-disable-ctrl-d)

;; Move and copy line like Eclipse and Netbeans
(use-package move-dup :ensure t)
(global-move-dup-mode)

;; Expand region
(use-package expand-region
  :ensure t
  :bind (("C-l" . er/expand-region))
  )

;; Visual feedback for search and replace
(use-package visual-regexp :ensure t)

;; Do not use tabs by default
(setq-default indent-tabs-mode nil)

;; Use 4 spaces by default
(setq-default tab-width 4)

;; ----------------------------------------------------------------------------
;;  Advanced: incremental completion (Support for AngularJS 2+)

(use-package ng2-mode :ensure t)

;; ----------------------------------------------------------------------------
;;  Advanced: support for YAML

(use-package yaml-mode :ensure t)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; ----------------------------------------------------------------------------
;;  Advanced: incremental completion (Helm)

(global-unset-key (kbd "C-o"))
(global-unset-key (kbd "C-j"))

(use-package helm
  :ensure t
  :init
  :bind (("C-<tab>" . helm-multi-files)
         ("C-o" . helm-find-files)
         ("C-x C-f" . helm-find-files)
         ("C-j" . helm-imenu)
         ("S-C-j" . helm-occur)
         ("M-x" . helm-M-x))
  )
(require 'helm)
(require 'helm-config)

;; Open helm on right side
(setq helm-split-window-default-side 'right)

;; Redefine some helm keys: TAB run persistent action
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(define-key helm-map (kbd "<escape>")  'keyboard-escape-quit)

;; ----------------------------------------------------------------------------
;;  Advanced: incremental completion (Projectile)

(use-package projectile
  :ensure t
  :init (projectile-mode)
  )

(use-package helm-projectile
  :ensure t
  :init (helm-projectile-on)
  :bind (("C-p" . helm-projectile-find-file)
         ("S-C-o" . helm-projectile-switch-project)
         ("S-C-f" . helm-projectile-grep))
  )

;; ----------------------------------------------------------------------------
;;  Advanced: incremental completion (Flycheck)

(use-package flycheck :ensure t)

;; flycheck with Google C Style
;; (eval-after-load 'flycheck
;;   '(progn
;;      (require 'flycheck-google-cpplint)
;;      ;; Add Google C++ Style checker.
;;      ;; In default, syntax checked by Clang and Cppcheck.
;;      (flycheck-add-next-checker 'c/c++-clang
;;                                 'c/c++-googlelint 'append)))

(add-hook 'after-init-hook #'global-flycheck-mode)

;; -----------------------------------------------------------------------------
;;  Advanced: autocomplete framework (Company)

(use-package company :ensure t)

;; Add some autocomplete engines
(use-package company-c-headers :ensure t)
(use-package company-web :ensure t)
(use-package ac-html-bootstrap :ensure t)
(use-package ac-html-csswatcher :ensure t)

;; Use clang for backends
(setq company-backends (delete 'company-semantic company-backends))
(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-backends 'company-clang)
(add-to-list 'company-backends 'company-web-html)
(add-to-list 'company-backends 'company-web-jade)
(add-to-list 'company-backends 'company-web-slim)

(add-hook 'after-init-hook 'global-company-mode)

;; ----------------------------------------------------------------------------
;;  Advanced: Indexer and symbol (rtags)

(use-package helm-gtags :ensure t)

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

;; Enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "C-b") 'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "S-C-b") 'helm-gtags-pop-stack)
     (define-key helm-gtags-mode-map (kbd "C-t") 'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "S-C-t") 'helm-gtags-find-symbol)
     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
     (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
     ))

;; ----------------------------------------------------------------------------
;;  Advanced: Snippets

(use-package yasnippet :ensure t)
(use-package yasnippet-snippets :ensure t)
(yas-global-mode 1)

;; For HTML and CSS use emmet
(use-package emmet-mode :ensure t)
 ;; Auto-start on any markup modes
(add-hook 'sgml-mode-hook 'emmet-mode)
;; enable Emmet's css abbreviation
(add-hook 'css-mode-hook  'emmet-mode)

;; ----------------------------------------------------------------------------
;;  Keybindings

;; Compile Project
(global-set-key (kbd "<f9>") 'projectile-compile-project)
;; Move to the previous error found during compiling
(global-set-key (kbd "<f10>") 'previous-error)
;; Move to the next error found during compiling
(global-set-key (kbd "<f11>") 'next-error)
;; Show flycheck errors
(global-set-key (kbd "<f12>") 'flycheck-list-errors)

;; Ctrl-<space>: code completion (with company)
(global-set-key (kbd "C-SPC") 'company-complete)
(define-key c-mode-map (kbd "C-SPC") 'company-complete)
(define-key c++-mode-map (kbd "C-SPC") 'company-complete)

;; Ctrl-S: save current file
(global-unset-key (kbd "C-s"))
(global-set-key (kbd "C-s") 'save-buffer)

;; Shift-Ctrl-S: save all files without confirmation
(defun save-all ()
  "Save buffer without confirmation."
  (interactive) (save-some-buffers t))
(global-unset-key (kbd "S-C-s"))
(global-set-key (kbd "S-C-s") 'save-all)

;; Change Search and Replace keys
(global-unset-key "\C-f")
(global-set-key (kbd "\C-f") 'isearch-forward)
(define-key isearch-mode-map "\C-f" 'isearch-repeat-forward)

(define-key global-map (kbd "C-r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; if you use multiple-cursors, this is for you:
;;(define-key global-map (kbd "C-m") 'vr/mc-mark)

;; Switch from .c/.h and vicevarsa
(global-set-key (kbd "C-S-a") 'ff-find-other-file)

;; Move between buffers
(global-set-key (kbd "M-<right>") 'next-buffer)
(global-set-key (kbd "M-<left>") 'previous-buffer)

;; Close current buffer and window
(global-set-key (kbd "C-w") 'kill-this-buffer)
(global-set-key (kbd "<C-iso-lefttab>") 'other-window)

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; Use ESC to quit command. This free Ctrl-G for moving to a specific line.
(global-unset-key (kbd "<escape>"))
(global-unset-key (kbd "C-g"))
(define-key key-translation-map (kbd "<escape>") (kbd "C-g"))
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-g") 'goto-line)

;; ----------------------------------------------------------------------------

(provide 'init)
;;; init.el ends here
