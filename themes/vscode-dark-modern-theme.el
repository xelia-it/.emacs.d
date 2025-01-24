;;; vscode-dark-modern-theme.el --- A Visual Studio Code Dark Modern theme for Emacs -*- lexical-binding: t; -*-

(deftheme vscode-dark-modern "A theme inspired by Visual Studio Code Dark Modern")

(let ((class '((class color) (min-colors 89)))
      ;; Palette
      (vscode-bg-dark             "#1e1e1e")
      (vscode-bg-light            "#252526")
      (vscode-fg-light            "#d4d4d4")
      (fg-dim                     "#a6a6a6")
      (cursor-color               "#c586c0")
      (highlight                  "#264f78")
      (region                     "#3e4451")
      (comment                    "#6a9955")
      (keyword                    "#569cd6")
      (string                     "#ce9178")
      (variable                   "#9cdcfe")
      (function-name              "#dcdcaa")
      (constant                   "#4ec9b0")
      (type-face                  "#4fc1ff")
      (warning                    "#f44747")
      (vscode-error               "#d16969")
      (success                    "#608b4e")
      (vscode-line-number         "#858585")
      (vscode-line-number-current "#FFFFFF")
      (vscode-border              "#404040")
      (vscode-shadow              "#707070")
      (vscode-border-dark         "#282828") ;; Original is #282828
      (vscode-git-added           "#2ea043")
      (vscode-git-modified        "#0078d4")
      (vscode-parenthesis         "#e7c303")
      )

  (custom-theme-set-faces
   'vscode-dark-modern

   ;; Default settings
   `(default ((,class (:background ,vscode-bg-dark :foreground ,vscode-fg-light))))
   `(fringe ((,class (:background ,vscode-bg-dark))))
   `(cursor ((,class (:background ,cursor-color))))
   `(region ((,class (:background ,region))))
   `(highlight ((,class (:background ,highlight))))
   `(shadow ((,class (:foreground ,fg-dim))))
   `(success ((,class (:foreground ,success))))
   `(warning ((,class (:foreground ,warning))))
   `(error ((,class (:foreground ,vscode-error))))

   ;; Font lock (syntax highlighting)
   `(font-lock-builtin-face ((,class (:foreground ,keyword))))
   `(font-lock-comment-face ((,class (:foreground ,comment))))
   `(font-lock-keyword-face ((,class (:foreground ,keyword))))
   `(font-lock-constant-face ((,class (:foreground ,constant))))
   `(font-lock-function-name-face ((,class (:foreground ,function-name))))
   `(font-lock-variable-name-face ((,class (:foreground ,variable))))
   `(font-lock-string-face ((,class (:foreground ,string))))
   `(font-lock-type-face ((,class (:foreground ,type-face))))
   `(font-lock-warning-face ((,class (:foreground ,warning :weight bold))))

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
   `(mode-line-inactive ((,class (:background ,vscode-bg-dark :foreground ,fg-dim :box nil))))
   `(header-line ((,class (:background ,vscode-bg-light :foreground ,vscode-fg-light :box nil))))
   `(minibuffer-prompt ((,class (:foreground ,keyword :weight bold))))

   ;; Search
   `(isearch ((,class (:background ,highlight :foreground ,vscode-fg-light))))
   `(lazy-highlight ((,class (:background ,region :foreground ,vscode-fg-light))))
   `(match ((,class (:background ,highlight :foreground ,vscode-fg-light))))

   ;; Org-mode
   `(org-level-1 ((,class (:foreground ,keyword :weight bold :height 1.05))))
   `(org-level-2 ((,class (:foreground ,function-name :weight bold :height 1.05))))
   `(org-level-3 ((,class (:foreground ,variable :weight bold))))
   `(org-block ((,class (:background ,vscode-bg-light :foreground ,vscode-fg-light))))
   `(org-code ((,class (:background ,vscode-bg-light :foreground ,vscode-fg-light))))
   `(org-quote  ((t (:background ,vscode-bg-light))))
   `(org-todo ((t (:foreground ,warning :weight bold))))
   `(org-done ((t (:foreground ,comment :weight bold))))

   ;; Dired
   `(dired-directory ((,class (:foreground ,keyword :weight bold))))
   `(dired-symlink ((,class (:foreground ,constant :weight bold))))

   ;; Treemacs
   `(treemacs-root-face ((,class (:foreground ,vscode-fg-light :weight bold :height 1.2))))
   `(treemacs-directory-face ((,class (:foreground ,keyword))))
   `(treemacs-file-face ((,class (:foreground ,vscode-fg-light))))
   `(treemacs-tags-face ((,class (:foreground ,constant))))
   `(treemacs-git-ignored-face ((,class (:foreground ,vscode-shadow))))

   ;; Helm
   `(helm-selection ((t (:foreground ,vscode-fg-light :background ,highlight))))
   `(helm-source-header ((t (:foreground ,vscode-fg-light :background ,vscode-bg-light :weight bold))))
   `(helm-buffer-directory ((,class (:foreground ,keyword :background nil))))
   `(helm-buffer-file ((,class (:foreground ,vscode-fg-light :background nil))))

   `(helm-ff-directory ((,class (:foreground ,keyword :background nil))))
   `(helm-ff-dirs ((,class (:foreground ,function-name :background nil))))
   `(helm-ff-dotted-directory ((,class (:foreground ,vscode-shadow :background nil))))
   `(helm-ff-file ((,class (:foreground ,vscode-fg-light :background nil))))
   `(helm-ff-file-extension ((,class (:foreground ,vscode-fg-light :background nil))))
   `(helm-candidate-number ((,class (:foreground ,vscode-parenthesis :background nil))))
   `(helm-header-line-left-margin ((,class (:foreground ,vscode-parenthesis :background nil))))
   `(helm-ff-prefix ((,class (:foreground ,vscode-parenthesis :background nil))))

   ;; Highlight indent guide
   ;; The nprmal color should be "vscode-border" in order to be equal
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


   ;; Git gutter
   `(git-gutter:added  ((t (:foreground ,vscode-git-added))))
   `(git-gutter:deleted  ((t (:foreground ,vscode-error))))
   `(git-gutter:modified  ((t (:foreground ,vscode-git-modified))))

   ;; Matching parenthesis
   `(show-paren-match
     ((t (:foreground ,function-name
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
