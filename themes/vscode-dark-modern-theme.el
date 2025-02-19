;;; vscode-dark-modern-theme.el --- A Visual Studio Code Dark Modern theme for Emacs -*- lexical-binding: t; -*-

(deftheme vscode-dark-modern "A theme inspired by Visual Studio Code Dark Modern")

(let ((class '((class color) (min-colors 89)))
      ;; Palette
      (vscode-bg-dark             "#1e1e1e")
      (vscode-bg-light            "#252526")
      (vscode-fg-light            "#d4d4d4")
      (vscode-fg-dim              "#a6a6a6")
      (vscode-cursor              "#c586c0")
      (vscode-highlight           "#264f78")
      (vscode-region              "#3e4451")
      (vscode-comment             "#6a9955")
      (vscode-keyword             "#569cd6")
      (vscode-string              "#ce9178")
      (vscode-variable            "#9cdcfe")
      (vscode-function-name       "#dcdcaa")
      (vscode-constant            "#4ec9b0")
      (vscode-type-face           "#4fc1ff")
      (vscode-warning             "#f44747")
      (vscode-error               "#d16969")
      (vscode-success             "#608b4e")
      (vscode-line-number         "#858585")
      (vscode-line-number-current "#FFFFFF")
      (vscode-border              "#404040")
      (vscode-shadow              "#707070")
      (vscode-border-dark         "#282828") ;; Original is #282828
      (vscode-git-added           "#2ea043")
      (vscode-git-modified        "#0078d4")
      (vscode-parenthesis         "#e7c303")
      (vscode-css-selector        "#d7ba7d")
      (vscode-css                 "#b5cea8")
      )

  (custom-theme-set-faces
   'vscode-dark-modern

   ;; Default settings
   `(default ((,class (:background ,vscode-bg-dark :foreground ,vscode-fg-light))))
   `(fringe ((,class (:background ,vscode-bg-dark))))
   `(cursor ((,class (:background ,vscode-cursor))))
   `(region ((,class (:background ,vscode-region))))
   `(highlight ((,class (:background ,vscode-highlight))))
   `(shadow ((,class (:foreground ,vscode-fg-dim))))
   `(vscode-success ((,class (:foreground ,vscode-success))))
   `(vscode-warning ((,class (:foreground ,vscode-warning))))
   `(error ((,class (:foreground ,vscode-error))))

   ;; Font lock (syntax highlighting)
   `(font-lock-builtin-face ((,class (:foreground ,vscode-keyword))))
   `(font-lock-comment-face ((,class (:foreground ,vscode-comment))))
   `(font-lock-keyword-face ((,class (:foreground ,vscode-keyword))))
   `(font-lock-vscode-constant-face ((,class (:foreground ,vscode-constant))))
   `(font-lock-vscode-function-name-face ((,class (:foreground ,vscode-function-name))))
   `(font-lock-variable-name-face ((,class (:foreground ,vscode-variable))))
   `(font-lock-string-face ((,class (:foreground ,vscode-string))))
   `(font-lock-vscode-type-face ((,class (:foreground ,vscode-type-face))))
   `(font-lock-vscode-warning-face ((,class (:foreground ,vscode-warning :weight bold))))

   ;; Line numbers
   `(line-number ((t (:foreground ,vscode-line-number :background ,vscode-bg-dark))))
   `(line-number-current-line
     ((t (:background unspecified
          :box (:line-width (-1 . -1) :color ,vscode-border-dark :style 'unspecified)))))

   ;; Current line
   `(hl-line
     ((t (:background unspecified
                      :box (:line-width (-1 . -1) :color ,vscode-border-dark :style 'unspecified)))))

   ;; Vertical line for max size
   `(fill-column-indicator ((t (:foreground ,vscode-border-dark :background ,vscode-bg-dark))))

   ;; Minibuffer
   `(minibuffer-prompt ((t (:foreground ,vscode-line-number :background ,vscode-bg-dark))))
   `(vertical-border ((t (:foreground ,vscode-bg-light :background ,vscode-bg-dark))))

   ;; UI Elements
   `(mode-line ((,class (:background ,vscode-bg-light :foreground ,vscode-fg-light :box nil))))
   `(mode-line-inactive ((,class (:background ,vscode-bg-dark :foreground ,vscode-fg-dim :box nil))))
   `(header-line ((,class (:background ,vscode-bg-light :foreground ,vscode-fg-light :box nil))))
   `(minibuffer-prompt ((,class (:foreground ,vscode-keyword :weight bold))))

   ;; Search
   `(isearch ((,class (:background ,vscode-highlight :foreground ,vscode-fg-light))))
   `(lazy-highlight ((,class (:background ,vscode-region :foreground ,vscode-fg-light))))
   `(match ((,class (:background ,vscode-highlight :foreground ,vscode-fg-light))))

   ;; Org-mode
   `(org-level-1 ((,class (:foreground ,vscode-keyword :weight bold :height 1.05))))
   `(org-level-2 ((,class (:foreground ,vscode-function-name :weight bold :height 1.05))))
   `(org-level-3 ((,class (:foreground ,vscode-variable :weight bold))))
   `(org-block ((,class (:background ,vscode-bg-light :foreground ,vscode-fg-light))))
   `(org-code ((,class (:background ,vscode-bg-light :foreground ,vscode-fg-light))))
   `(org-quote  ((t (:background ,vscode-bg-light))))
   `(org-todo ((t (:foreground ,vscode-warning :weight bold))))
   `(org-done ((t (:foreground ,vscode-comment :weight bold))))

   ;; Dired
   `(dired-directory ((,class (:foreground ,vscode-keyword :weight bold))))
   `(dired-symlink ((,class (:foreground ,vscode-constant :weight bold))))

   ;; Treemacs
   `(treemacs-root-face ((,class (:foreground ,vscode-fg-light :weight bold :height 1.2))))
   `(treemacs-directory-face ((,class (:foreground ,vscode-keyword))))
   `(treemacs-file-face ((,class (:foreground ,vscode-fg-light))))
   `(treemacs-tags-face ((,class (:foreground ,vscode-constant))))
   `(treemacs-git-ignored-face ((,class (:foreground ,vscode-shadow))))

   ;; Helm
   `(helm-selection ((t (:foreground ,vscode-fg-light :background ,vscode-highlight))))
   `(helm-source-header ((t (:foreground ,vscode-fg-light :background ,vscode-bg-light :weight bold))))
   `(helm-buffer-directory ((,class (:foreground ,vscode-keyword :background unspecified))))
   `(helm-buffer-file ((,class (:foreground ,vscode-fg-light :background unspecified))))

   `(helm-ff-directory ((,class (:foreground ,vscode-keyword :background unspecified))))
   `(helm-ff-dirs ((,class (:foreground ,vscode-function-name :background unspecified))))
   `(helm-ff-dotted-directory ((,class (:foreground ,vscode-shadow :background unspecified))))
   `(helm-ff-file ((,class (:foreground ,vscode-fg-light :background unspecified))))
   `(helm-ff-file-extension ((,class (:foreground ,vscode-fg-light :background unspecified))))
   `(helm-candidate-number ((,class (:foreground ,vscode-parenthesis :background unspecified))))
   `(helm-header-line-left-margin ((,class (:foreground ,vscode-parenthesis :background unspecified))))
   `(helm-ff-prefix ((,class (:foreground ,vscode-parenthesis :background unspecified))))

   ;; Highlight indent guide
   ;; The normal color should be "vscode-border" in order to be equal
   ;; to Visual Studio Code theme.
   `(highlight-indent-guides-character-face  ((t (:foreground ,vscode-border-dark))))
   `(highlight-indent-guides-even-face  ((t (:foreground ,vscode-border-dark))))
   `(highlight-indent-guides-odd-face  ((t (:foreground ,vscode-border-dark))))
   `(highlight-indent-guides-stack-character-face  ((t (:foreground ,vscode-shadow))))
   `(highlight-indent-guides-stack-even-face  ((t (:foreground ,vscode-shadow))))
   `(highlight-indent-guides-stack-odd-face  ((t (:foreground ,vscode-shadow))))
   `(highlight-indent-guides-top-character-face  ((t (:foreground ,vscode-shadow))))
   `(highlight-indent-guides-top-even-face  ((t (:foreground ,vscode-shadow))))
   `(highlight-indent-guides-top-odd-face  ((t (:foreground ,vscode-shadow))))

   `(web-mode-html-tag-face ((t (:foreground ,vscode-keyword))))   ;; Color for HTML tags
   `(web-mode-html-attr-name-face ((t (:foreground ,vscode-variable)))) ;; HTML attributes
   `(web-mode-html-attr-value-face ((t (:foreground ,vscode-string)))) ;; Values
   `(web-mode-comment-face ((t (:foreground ,vscode-comment :slant italic)))) ;; Comments
   `(web-mode-docvscode-type-face ((t (:foreground ,vscode-cursor)))) ;; !DOCTYPE
   `(web-mode-css-selector-face ((t (:foreground ,vscode-css-selector)))) ;; CSS selectors
   `(web-mode-css-property-name-face ((t (:foreground ,vscode-variable)))) ;; CSS Properties
   `(web-mode-css-color-face ((t (:foreground ,vscode-css))))
   `(web-mode-javascript-string-face ((t (:foreground ,vscode-string)))) ;; Orange for string like JS

   ;; Git gutter
   `(git-gutter:added  ((t (:foreground ,vscode-git-added))))
   `(git-gutter:deleted  ((t (:foreground ,vscode-error))))
   `(git-gutter:modified  ((t (:foreground ,vscode-git-modified))))

   ;; Matching parenthesis
   `(show-paren-match
     ((t (:foreground ,vscode-function-name
          :background ,vscode-bg-light
          :box (:line-width (-1 . -1) :color ,vscode-border :style nil)
          :weight bold))))
   `(show-paren-mismatch
     ((t (:foreground ,vscode-bg-dark
          :background ,vscode-error
          :box (:line-width (-1 . -1) :color ,vscode-border :style nil)
          :weight bold))))
   )
  ) ;; closes let

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'vscode-dark-modern)
;;; vscode-dark-modern-theme.el ends here
