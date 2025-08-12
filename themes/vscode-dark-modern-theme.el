;;; vscode-dark-modern-theme.el --- A Visual Studio Code Dark Modern theme for Emacs -*- lexical-binding: t; -*-

(deftheme vscode-dark-modern
  "A theme inspired by Visual Studio Code Dark Modern")

;; Theme

(let ((class '((class color) (min-colors 89)))
      ;; Palette VSCode Dark Modern+
      (vscode-bg-extra-dark       "#181818")
      (vscode-bg-dark             "#1f1f1f")
      (vscode-bg-light            "#202020")
      (vscode-fg-light            "#cccccc")
      (vscode-fg-dim              "#6e7681")

      (vscode-border-dark         "#2b2b2b")
      (vscode-border-accent       "#0078d4")
      (vscode-cursor              "#aeafad")
      (vscode-selection-bg        "#264f78")
      (vscode-line-highlight      "#2a2a2a")

      (vscode-error               "#f85149")
      (vscode-warning             "#ff8800")
      (vscode-info                "#3794ff")

      (vscode-string              "#d69d85")
      (vscode-function            "#dcdcaa")
      (vscode-variable            "#9cdcfe")

      (vscode-keyword             "#569cd6")
      (vscode-constant            "#b5cea8")
      (vscode-type                "#4ec9b0")
      (vscode-comment             "#6a9955")
      (vscode-tag                 "#d7ba7d") ;; CSS Selector
      (vscode-git-added           "#2ea043")
      (vscode-git-modified        "#0078d4")

      (vscode-brackets            "#c586c0")
      (vscode-brackets-match      "#ffd700")
      (vscode-search-match        "#623315")
      (vscode-selection-bg-dark   "#04395e") ;; Vertico completion
      (vscode-highlight-read      "#474747") ;; Keyword highlight
      (vscode-highlight-write     "#093d5b") ;; Keyword assignment highlight

      ;; Palette extra VSCode
      (vscode-namespace         "#4ec9b0") ;; namespace/import
      (vscode-parameter         "#9cdcfe") ;; function parameters
      (vscode-operator          "#d4d4d4") ;; +, -, &&, etc.
      (vscode-enum              "#b8d7a3") ;; enum values
      (vscode-annotation        "#d7ba7d") ;; decorators
      (vscode-markup-heading    "#569cd6") ;; markdown headings
      (vscode-markup-bold       "#d7ba7d") ;; markdown bold
      (vscode-markup-italic     "#ce9178") ;; markdown italic
      (vscode-markup-inlinecode "#dcdcaa") ;; markdown inline code
      (vscode-highlight-strong  "#ffcc00") ;; very strong highlight
      )


  (custom-theme-set-faces
   'vscode-dark-modern

   ;; Base
   `(default ((,class (:background ,vscode-bg-dark :foreground ,vscode-fg-light))))
   `(cursor ((,class (:background ,vscode-cursor))))
   `(region ((,class (:background ,vscode-selection-bg))))
   `(fringe ((,class (:background ,vscode-bg-dark))))
   `(vertical-border ((,class (:foreground ,vscode-border-dark))))
   `(highlight ((,class (:background ,vscode-line-highlight))))
   `(minibuffer-prompt ((,class (:foreground ,vscode-keyword :weight normal))))
   `(link ((,class (:foreground ,vscode-info :underline t))))
   `(italic ((,class (:foreground ,vscode-fg-dim :underline nil))))

   ;; Font-lock faces completi (VSCode Dark Modern+)
   `(font-lock-bracket-face ((,class (:foreground ,vscode-brackets))))
   `(font-lock-builtin-face ((,class (:foreground ,vscode-function))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,vscode-comment :slant italic))))
   `(font-lock-comment-face ((,class (:foreground ,vscode-comment :slant italic))))
   `(font-lock-constant-face ((,class (:foreground ,vscode-constant))))
   `(font-lock-delimiter-face ((,class (:foreground ,vscode-fg-dim))))
   `(font-lock-doc-face ((,class (:foreground ,vscode-comment :slant italic))))
   `(font-lock-doc-markup-face ((,class (:foreground ,vscode-string))))
   `(font-lock-escape-face ((,class (:foreground ,vscode-constant :weight bold))))
   `(font-lock-function-call-face ((,class (:foreground ,vscode-function))))
   `(font-lock-function-name-face ((,class (:foreground ,vscode-function))))
   `(font-lock-keyword-face ((,class (:foreground ,vscode-keyword))))
   `(font-lock-misc-punctuation-face ((,class (:foreground ,vscode-fg-light))))
   `(font-lock-negation-char-face ((,class (:foreground ,vscode-warning :weight bold))))
   `(font-lock-number-face ((,class (:foreground ,vscode-constant))))
   `(font-lock-operator-face ((,class (:foreground ,vscode-keyword))))
   `(font-lock-preprocessor-face ((,class (:foreground ,vscode-type))))
   `(font-lock-property-name-face ((,class (:foreground ,vscode-variable))))
   `(font-lock-property-use-face ((,class (:foreground ,vscode-variable))))
   `(font-lock-punctuation-face ((,class (:foreground ,vscode-fg-light))))
   `(font-lock-regexp-face ((,class (:foreground ,vscode-string))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,vscode-string :weight bold))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,vscode-string :weight bold))))
   `(font-lock-string-face ((,class (:foreground ,vscode-string))))
   `(font-lock-type-face ((,class (:foreground ,vscode-type))))
   `(font-lock-variable-name-face ((,class (:foreground ,vscode-variable))))
   `(font-lock-variable-use-face ((,class (:foreground ,vscode-variable))))
   `(font-lock-warning-face ((,class (:foreground ,vscode-error :weight bold))))

   ;; Parameters
   `(font-lock-parameter-name-face ((,class (:foreground ,vscode-parameter))))
   ;; Namespace / imports
   `(font-lock-namespace-face ((,class (:foreground ,vscode-namespace))))
   ;; Enum constants
   `(font-lock-enum-face ((,class (:foreground ,vscode-enum))))
   ;; Decorators / annotations
   `(font-lock-annotation-face ((,class (:foreground ,vscode-annotation :slant italic))))
   ;; Operators
   `(font-lock-operator-face ((,class (:foreground ,vscode-operator))))

   ;; Line numbers
   `(line-number ((,class (:foreground ,vscode-fg-dim :background ,vscode-bg-dark))))
   `(line-number-current-line ((,class(:foreground ,vscode-fg-light :background ,vscode-bg-light
						    :box (:line-width (-1 . -1) :color ,vscode-border-dark :style nil)
						    ))))

   ;; Diff
   `(diff-added ((,class (:foreground ,vscode-git-added :background unspecified))))
   `(diff-removed ((,class (:foreground ,vscode-error :background unspecified))))
   `(diff-changed ((,class (:foreground ,vscode-git-modified :background unspecified))))

   ;; Git gutter
   `(git-gutter:added  ((t (:foreground ,vscode-git-added  :background unspecified))))
   `(git-gutter:deleted  ((t (:foreground ,vscode-error  :background unspecified))))
   `(git-gutter:modified  ((t (:foreground ,vscode-git-modified  :background unspecified))))

   ;; Current line
   `(hl-line
     ((t (:background unspecified
                      :box (:line-width (-1 . -1) :color ,vscode-border-dark :style nil)))))

   ;; Vertical line for max size
   `(fill-column-indicator ((t (:foreground ,vscode-border-dark :background unspecified))))

   ;; Search
   `(isearch ((,class (:background ,vscode-search-match))))
   `(lazy-highlight ((,class (:background ,vscode-search-match))))
   `(match ((,class (:background ,vscode-search-match))))

   ;; Treemacs
   `(treemacs-window-background-face ((,class (:foreground ,vscode-fg-light :background ,vscode-bg-extra-dark :height 0.95))))
   `(treemacs-root-face ((,class (:foreground ,vscode-fg-light :weight bold))))
   `(treemacs-directory-collapsed-face ((,class (:foreground ,vscode-fg-light))))
   `(treemacs-directory-face ((,class (:foreground ,vscode-fg-light))))

   `(treemacs-git-ignored-face ((,class (:foreground ,vscode-fg-dim))))
   `(treemacs-git-ignored-face ((,class (:foreground ,vscode-keyword))))

   ;; Vertico
   `(vertico-posframe-border ((,class (:background ,vscode-border-dark))))
   `(vertico-current ((,class (:background ,vscode-selection-bg-dark))))
   `(vertico-group-separator ((,class (:foreground ,vscode-fg-dim :strike-through t))))
   `(vertico-group-title ((,class (:foreground ,vscode-fg-dim))))

   ;; ;; Web Mode
   `(web-mode-html-tag-face ((t (:foreground ,vscode-keyword))))   ;; Color for HTML tags
   `(web-mode-html-attr-name-face ((t (:foreground ,vscode-variable)))) ;; HTML attributes
   `(web-mode-html-attr-value-face ((t (:foreground ,vscode-string)))) ;; Values
   `(web-mode-comment-face ((t (:foreground ,vscode-comment :slant italic)))) ;; Comments
   `(web-mode-docvscode-type-face ((t (:foreground ,vscode-cursor)))) ;; !DOCTYPE
   `(web-mode-css-selector-face ((t (:foreground ,vscode-tag)))) ;; CSS selectors
   `(web-mode-css-property-name-face ((t (:foreground ,vscode-variable)))) ;; CSS Properties
   `(web-mode-css-color-face ((t (:foreground ,vscode-tag))))
   `(web-mode-javascript-string-face ((t (:foreground ,vscode-string)))) ;; Orange for string like JS
   `(web-mode-block-face ((t (:background unspecified)))) ;; Use the same background of the rest of the HTML
;;   `(web-mode-current-element-highlight-face ((t (:foregrund ,vscode-keyword :background ,vscode-tag-background))))
;;   `(web-mode-current-column-highlight-face ((t (:foregrund ,vscode-keyword :background ,vscode-tag-background))))

   ;; LSP

   ;; Generic LSP
   `(lsp-face-highlight-read ((,class (:background ,vscode-highlight-read :foreground unspecified :underline nil))))
   `(lsp-face-highlight-textual ((,class (:background ,vscode-highlight-read :foreground unspecified :underline nil))))
   `(lsp-face-highlight-write ((,class (:background ,vscode-highlight-write :foreground unspecified :underline nil))))
   ;; LSP UI specific settings
   `(lsp-ui-doc-background ((t (:background ,vscode-bg-extra-dark))))
   `(lsp-ui-doc-text ((,class (:foreground ,vscode-fg-light))))
   `(lsp-ui-peek-peek ((,class (:background ,vscode-bg-extra-dark))))
   `(lsp-ui-peek-list ((t (:background ,vscode-bg-extra-dark))))
   `(lsp-ui-peek-selection ((t (:background ,vscode-selection-bg :foreground "#ffffff"))))
   `(lsp-ui-peek-highlight ((t (:background ,vscode-highlight-write :foreground "#ffffff"))))
   `(lsp-ui-peek-header
      ((t (:background ,vscode-bg-extra-dark :foreground "#ffffff"
                       :box (:line-width (-1 . -1) :color ,vscode-border-accent :style nil)))))
   `(lsp-ui-peek-footer
     ((t (:background ,vscode-bg-extra-dark :foreground "#ffffff"
                      :box (:line-width (-1 . -1) :color ,vscode-border-accent :style nil)))))
   `(lsp-ui-peek-filename ((t (:foreground ,vscode-variable))))
   `(lsp-ui-peek-line-number ((t (:foreground ,vscode-fg-dim))))
   ;; Semantic highlight (with treesitter)
   `(lsp-face-semhl-namespace      ((,class (:foreground ,vscode-namespace))))
   `(lsp-face-semhl-parameter      ((,class (:foreground ,vscode-parameter))))
   `(lsp-face-semhl-decorator      ((,class (:foreground ,vscode-annotation))))
   `(lsp-face-semhl-enumMember     ((,class (:foreground ,vscode-enum))))
   `(lsp-face-semhl-operator       ((,class (:foreground ,vscode-operator))))

   ;; Company box
   `(company-box-background ((,class (:background ,vscode-bg-extra-dark))))
   `(company-tooltip ((,class (:background ,vscode-bg-extra-dark :foreground ,vscode-fg-light))))
   `(company-tooltip-selection ((,class (:background ,vscode-selection-bg))))
   `(company-tooltip-annotation ((,class (:foreground ,vscode-function))))
   `(company-tooltip-scrollbar-thumb ((t (:background ,vscode-bg-light))))
   `(company-tooltip-scrollbar-track ((t (:background ,vscode-bg-extra-dark))))

   ;; Modeline

   ;; Active modeline
   `(mode-line ((t (:background ,vscode-bg-extra-dark :foreground ,vscode-fg-light :height 100 :box nil))))
   ;; Inactive modeline
   `(mode-line-inactive ((,class (:background ,vscode-bg-extra-dark :foreground ,vscode-fg-dim :height 100 :box nil))))
   `(mode-line-hightlight ((,class (:background unspecified :foreground ,vscode-fg-light :height 100 :box nil))))
   `(mode-line-buffer-id ((,class (:background unspecified :foreground ,vscode-fg-light :height 100 :weight normal))))
   `(mode-line-emphasis ((,class (:background unspecified :foreground ,vscode-fg-light :height 100 :weight normal))))
   ;; Doom modeline specific faces
   `(doom-modeline-bar ((,class (:background ,vscode-border-dark :foreground nil :box nil))))
   `(doom-modeline-bar-inactive ((,class (:background ,vscode-border-dark :foreground nil :box nil))))
   `(doom-modeline-highlight ((,class (:box nil :weight bold))))
   `(doom-modeline-info ((,class (:foreground ,vscode-brackets-match :weight normal))))

   ;; Matching parenthesis
   `(show-paren-match
     ((t (:foreground ,vscode-brackets-match
          :background unspecified
          :box (:line-width (-1 . -1) :color ,vscode-border-dark :style nil)
          :weight bold))))
   `(show-paren-match-expression
     ((t (:foreground ,vscode-brackets-match
          :background unspecified
          :box (:line-width (-1 . -1) :color ,vscode-border-dark :style nil)
          :weight bold))))
   `(show-paren-mismatch
     ((t (:foreground ,vscode-bg-dark
          :background ,vscode-error
          :box (:line-width (-1 . -1) :color ,vscode-border-dark :style nil)
          :weight bold))))
   )
  ) ;; closes let

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'vscode-dark-modern)
;;; vscode-dark-modern-theme.el ends here
