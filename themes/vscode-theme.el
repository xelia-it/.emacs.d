;; Color variables
(defvar vscode-bg-dark "#181818")
(defvar vscode-bg "#252526")
(defvar vscode-fg "#D4D4D4")
(defvar vscode-fg-light "#EAEAEA")
(defvar vscode-cursor "#AEAFAD")
(defvar vscode-comment "#6A9955")
(defvar vscode-keyword "#569CD6")
(defvar vscode-string "#CE9178")
(defvar vscode-function "#DCDCAA")
(defvar vscode-variable "#9CDCFE")
(defvar vscode-constant "#4EC9B0")
(defvar vscode-type "#4EC9B0")
(defvar vscode-warning "#D7BA7D")
(defvar vscode-error "#F44747")
(defvar vscode-selection "#264F78")
(defvar vscode-line-number "#858585")
(defvar vscode-line-number-current "#FFFFFF")
(defvar vscode-region "#3E4451")
(defvar vscode-border-color "#505050", "Color for borders")
(defvar vscode-shadow "#707070", "Shadow color")

;; Define theme
(deftheme vscode "VSCode Modern Dark Theme for Emacs")

(custom-theme-set-faces
 'vscode

 ;; Default
 `(default ((t (:background ,vscode-bg :foreground ,vscode-fg))))
 `(cursor ((t (:background ,vscode-cursor))))
 `(fringe ((t (:background ,vscode-bg-dark))))
 `(region ((t (:background ,vscode-region))))
 `(highlight ((t (:background ,vscode-selection))))
 `(shadow ((t (:foreground ,vscode-shadow))))

 ;; Syntax highlighting
 `(font-lock-builtin-face ((t (:foreground ,vscode-keyword))))
 `(font-lock-comment-face ((t (:foreground ,vscode-comment))))
 `(font-lock-keyword-face ((t (:foreground ,vscode-keyword))))
 `(font-lock-constant-face ((t (:foreground ,vscode-constant))))
 `(font-lock-string-face ((t (:foreground ,vscode-string))))
 `(font-lock-function-name-face ((t (:foreground ,vscode-function))))
 `(font-lock-variable-name-face ((t (:foreground ,vscode-variable))))
 `(font-lock-constant-face ((t (:foreground ,vscode-constant))))
 `(font-lock-type-face ((t (:foreground ,vscode-type))))
 `(font-lock-warning-face ((t (:foreground ,vscode-warning :weight bold))))

 `(warning ((t (:foreground ,vscode-warning :weight bold))))
 `(error ((t (:foreground ,vscode-error :weight bold))))

 ;; Line numbers
 `(line-number ((t (:foreground ,vscode-line-number :background ,vscode-bg-dark))))
 `(line-number-current-line
   ((t (:background nil
                    :box (:line-width (-1 . -1) :color ,vscode-border-color :style 'unspecified)))))
 `(hl-line
   ((t (:background nil
                    :box (:line-width (-1 . -1) :color ,vscode-border-color :style 'unspecified)))))

 ;; Verical line for max size
 `(fill-column-indicator ((t (:foreground ,vscode-border-color :background ,vscode-bg-dark))))

 ;; Minibuffer
 `(minibuffer-prompt ((t (:foreground ,vscode-line-number :background ,vscode-bg-dark))))
 `(vertical-border ((t (:foreground ,vscode-bg :background ,vscode-bg-dark))))

 ;; Mode line
 `(mode-line ((t (:foreground ,vscode-fg :background ,vscode-bg-dark))))
 `(mode-line-inactive ((t (:foreground ,vscode-fg :background ,vscode-bg-dark))))

 ;; Treemacs
 `(treemacs-root-face ((t (:background ,vscode-bg-dark :foreground ,vscode-fg-light :weight bold))))
 `(treemacs-directory-face ((t (:background ,vscode-bg-dark :foreground ,vscode-fg))))
 `(treemacs-file-face ((t (:background ,vscode-bg-dark :foreground ,vscode-fg))))

 ;; Helm
 `(helm-selection ((t (:foreground ,vscode-fg :background ,vscode-selection))))
 `(helm-source-header ((t (:foreground ,vscode-fg :background ,vscode-bg :weight bold))))
;; `(helm-minibuffer-prompt ((t (:foreground ,vscode-fg :background ,vscode-bg :weight bold))))

 ;; Org Mode
 `(org-level-1 ((t (:foreground ,vscode-keyword :weight bold :height 1.05))))
 `(org-level-2 ((t (:foreground ,vscode-function :weight bold :height 1.05))))
 `(org-level-3 ((t (:foreground ,vscode-variable :weight bold :height 1.05))))
 `(org-level-3 ((t (:foreground ,vscode-string :weight bold :height 1.05))))
 `(org-block  ((t (:background ,vscode-bg))))
 `(org-code  ((t (:background ,vscode-bg))))
 `(org-quote  ((t (:background ,vscode-bg))))
 `(org-todo ((t (:foreground ,vscode-warning :weight bold))))
 `(org-done ((t (:foreground ,vscode-comment :weight bold))))

 ;; Highlight indendt guide
 `(highlight-indent-guides-character-face  ((t (:foreground ,vscode-border-color))))
 `(highlight-indent-guides-even-face  ((t (:foreground ,vscode-border-color))))
 `(highlight-indent-guides-odd-face  ((t (:foreground ,vscode-border-color))))
 `(highlight-indent-guides-top-character-face  ((t (:foreground ,vscode-shadow))))
 `(highlight-indent-guides-top-even-face  ((t (:foreground ,vscode-shadow))))
 `(highlight-indent-guides-top-odd-face  ((t (:foreground ,vscode-shadow))))

 ;; Matching parenthesis
 `(show-paren-match
   ((t (:foreground ,vscode-function
        :background ,vscode-bg
        :box (:line-width (-1 . -1) :color ,vscode-border-color :style nil)
        :weight bold))))
 `(show-paren-mismatch
   ((t (:foreground ,vscode-bg-dark
        :background ,vscode-error
        :box (:line-width (-1 . -1) :color ,vscode-border-color :style nil)
        :weight bold))))
 )

(provide-theme 'vscode)
;;(provide 'vscode)
