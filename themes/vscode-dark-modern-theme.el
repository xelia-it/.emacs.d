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
   `(minibuffer-prompt ((,class (:foreground ,vscode-keyword))))
   `(link ((,class (:foreground ,vscode-info :underline t))))
   `(italic ((,class (:foreground ,vscode-fg-dim :underline nil))))

   `(success ((,class (:foreground ,vscode-git-added :underline nil))))
   `(warning ((,class (:foreground ,vscode-warning :underline nil))))
   `(error ((,class (:foreground ,vscode-error :underline nil))))


   ;; Font-lock faces completi (VSCode Dark Modern+)
   `(font-lock-bracket-face ((,class (:foreground ,vscode-brackets))))
   `(font-lock-builtin-face ((,class (:foreground ,vscode-function))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,vscode-comment))))
   `(font-lock-comment-face ((,class (:foreground ,vscode-comment))))
   `(font-lock-constant-face ((,class (:foreground ,vscode-constant))))
   `(font-lock-delimiter-face ((,class (:foreground ,vscode-fg-dim))))
   `(font-lock-doc-face ((,class (:foreground ,vscode-comment))))
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
   `(font-lock-annotation-face ((,class (:foreground ,vscode-annotation))))
   ;; Operators
   `(font-lock-operator-face ((,class (:foreground ,vscode-operator))))

   ;; Line numbers
   `(line-number ((,class (:foreground ,vscode-fg-dim :background ,vscode-bg-dark))))
   `(line-number-current-line ((,class(:foreground ,vscode-fg-light :background ,vscode-bg-light
						    :box (:line-width (-1 . -1) :color ,vscode-border-dark :style nil)
						    ))))

   ;; Diff
   `(diff-added ((,class (:foreground ,vscode-git-added :background nil))))
   `(diff-removed ((,class (:foreground ,vscode-error :background nil))))
   `(diff-changed ((,class (:foreground ,vscode-git-modified :background nil))))

   ;; Git gutter
   `(git-gutter:added  ((t (:foreground ,vscode-git-added :background nil))))
   `(git-gutter:deleted  ((t (:foreground ,vscode-error :background nil))))
   `(git-gutter:modified  ((t (:foreground ,vscode-git-modified :background nil))))

   ;; Current line
   `(hl-line
     ((t (:background nil
                      :box (:line-width (-1 . -1) :color ,vscode-border-dark :style nil)))))

   ;; Vertical line for max size
   `(fill-column-indicator ((t (:foreground ,vscode-border-dark :background nil))))

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
   `(treemacs-hl-line-face ((,class (:background ,vscode-bg-light))))

   ;; Vertico
   `(vertico-posframe-border ((,class (:background ,vscode-border-dark))))
   `(vertico-current ((,class (:background ,vscode-selection-bg-dark))))
   `(vertico-group-separator ((,class (:foreground ,vscode-fg-dim :strike-through t))))
   `(vertico-group-title ((,class (:foreground ,vscode-fg-dim))))

   ;; Web Mode (HTML)
   `(web-mode-doctype-face ((,class (:foreground ,vscode-operator)))) ;; <!DOCTYPE>
   `(web-mode-html-tag-face ((,class (:foreground ,vscode-keyword)))) ;; <div>
   `(web-mode-html-tag-bracket-face ((,class (:foreground ,vscode-fg-dim)))) ;; <, >
   `(web-mode-html-attr-name-face ((,class (:foreground ,vscode-variable)))) ;; class, id
   `(web-mode-html-attr-equal-face ((t (:foreground ,vscode-operator)))) ;; =
   `(web-mode-html-attr-value-face ((t (:foreground ,vscode-string)))) ;; "value"
   `(web-mode-html-entity-face ((t (:foreground ,vscode-constant)))) ;; &nbsp;
   `(web-mode-comment-face ((t (:foreground ,vscode-comment)))) ;; <!-- comment -->
   `(web-mode-block-face ((t (:background ,vscode-bg-extra-dark)))) ;; <% %> / {{ }}
   `(web-mode-block-delimiter-face ((t (:foreground ,vscode-variable)))) ;; {{ }}
   ;; Web Mode (CSS)
   `(web-mode-css-selector-face ((t (:foreground ,vscode-tag)))) ;; .class, #id
   `(web-mode-css-property-name-face ((t (:foreground ,vscode-variable)))) ;; color, margin
   `(web-mode-css-pseudo-class-face ((t (:foreground ,vscode-namespace)))) ;; :hover, :before
   `(web-mode-css-color-face ((t (:foreground ,vscode-constant)))) ;; #fff, red
   `(web-mode-css-variable-face ((t (:foreground ,vscode-parameter)))) ;; --main-color
   ;; Web Mode (JS/TS inside HTML)
   `(web-mode-javascript-string-face ((t (:foreground ,vscode-string)))) ;; "string"
   `(web-mode-javascript-face ((t (:foreground ,vscode-variable)))) ;; JS variables
   `(web-mode-keyword-face ((t (:foreground ,vscode-keyword)))) ;; if, for, return
   `(web-mode-constant-face ((t (:foreground ,vscode-constant)))) ;; true, false, null
   `(web-mode-function-call-face ((t (:foreground ,vscode-function)))) ;; myFunction()
   ;; Web Mode (Highlighting)
   `(web-mode-current-element-highlight-face ((t (:background ,vscode-line-highlight))))
   `(web-mode-current-column-highlight-face ((t (:background ,vscode-bg-light))))

   ;; LSP

   ;; Generic LSP
   `(lsp-face-highlight-read ((,class (:background ,vscode-highlight-read :foreground nil :underline nil))))
   `(lsp-face-highlight-textual ((,class (:background ,vscode-highlight-read :foreground nil :underline nil))))
   `(lsp-face-highlight-write ((,class (:background ,vscode-highlight-write :foreground nil :underline nil))))
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
   `(mode-line ((t (:background ,vscode-bg-extra-dark :foreground ,vscode-fg-light :box nil))))
   ;; Inactive modeline
   `(mode-line-inactive ((,class (:background ,vscode-bg-extra-dark :foreground ,vscode-fg-dim :box nil))))
   `(mode-line-hightlight ((,class (:background nil :foreground ,vscode-fg-light :box nil))))
   `(mode-line-buffer-id ((,class (:background nil :foreground ,vscode-fg-light))))
   `(mode-line-emphasis ((,class (:background nil :foreground ,vscode-fg-light))))
   ;; Doom modeline specific faces
   `(doom-modeline-bar ((,class (:background ,vscode-border-dark :foreground nil :box nil))))
   `(doom-modeline-bar-inactive ((,class (:background ,vscode-border-dark :foreground nil :box nil))))
   `(doom-modeline-highlight ((,class (:box nil))))
   `(doom-modeline-info ((,class (:foreground ,vscode-brackets-match))))

   ;; Centaur tabs
   `(centaur-tabs-active-bar-face ((,class (:background ,vscode-border-accent))))
   `(centaur-tabs-selected ((,class (:background ,vscode-bg-dark :foreground ,vscode-fg-light
                                     :overline ,vscode-border-accent :underline nil))))
   `(centaur-tabs-selected-modified ((,class (:background ,vscode-bg-dark :foreground ,vscode-fg-light
                                              :overline ,vscode-border-accent :underline nil))))
   `(centaur-tabs-unselected ((,class (:background ,vscode-bg-extra-dark :foreground ,vscode-fg-dim
                                       :overline nil :underline ,vscode-border-dark))))
   `(centaur-tabs-unselected-modified ((,class (:background ,vscode-bg-extra-dark :foreground ,vscode-fg-dim
                                                :overline nil :underline ,vscode-border-dark))))

   ;; Matching parenthesis
   `(show-paren-match
     ((t (:foreground ,vscode-brackets-match
          :background nil
          :box (:line-width (-1 . -1) :color ,vscode-border-dark :style nil)))))
   `(show-paren-match-expression
     ((t (:foreground ,vscode-brackets-match
          :background nil
          :box (:line-width (-1 . -1) :color ,vscode-border-dark :style nil)))))
   `(show-paren-mismatch
     ((t (:foreground ,vscode-bg-dark
          :background ,vscode-error
          :box (:line-width (-1 . -1) :color ,vscode-border-dark :style nil)))))

   ;; Org-mode
   `(org-level-1 ((,class (:foreground ,vscode-markup-heading))))
   `(org-level-2 ((,class (:foreground ,vscode-markup-heading))))
   `(org-level-3 ((,class (:foreground ,vscode-markup-heading))))
   `(org-level-4 ((,class (:foreground ,vscode-markup-heading))))
   `(org-level-5 ((,class (:foreground ,vscode-markup-heading))))
   `(org-level-6 ((,class (:foreground ,vscode-markup-heading))))
   `(org-level-7 ((,class (:foreground ,vscode-markup-heading))))
   `(org-level-8 ((,class (:foreground ,vscode-markup-heading))))
   `(org-document-title ((,class (:foreground ,vscode-markup-heading))))
   `(org-document-info ((,class (:foreground ,vscode-variable))))
   `(org-document-info-keyword ((,class (:foreground ,vscode-comment))))
   `(org-meta-line ((,class (:foreground ,vscode-comment))))
   `(org-block ((,class (:background ,vscode-bg-extra-dark :foreground ,vscode-fg-light))))
   `(org-block-begin-line ((,class (:foreground ,vscode-comment))))
   `(org-block-end-line ((,class (:foreground ,vscode-comment))))
   `(org-code ((,class (:foreground ,vscode-markup-inlinecode :background ,vscode-bg-extra-dark))))
   `(org-verbatim ((,class (:foreground ,vscode-string))))
   `(org-link ((,class (:foreground ,vscode-info :underline t))))
   `(org-date ((,class (:foreground ,vscode-constant))))
   `(org-todo ((,class (:foreground ,vscode-warning))))
   `(org-done ((,class (:foreground ,vscode-git-added))))
   `(org-special-keyword ((,class (:foreground ,vscode-keyword))))
   `(org-checkbox ((,class (:foreground ,vscode-variable))))
   `(org-table ((,class (:foreground ,vscode-variable))))

   ;; Markdown-mode
   `(markdown-header-face-1 ((,class (:foreground ,vscode-markup-heading))))
   `(markdown-header-face-2 ((,class (:foreground ,vscode-markup-heading))))
   `(markdown-header-face-3 ((,class (:foreground ,vscode-markup-heading))))
   `(markdown-header-face-4 ((,class (:foreground ,vscode-markup-heading))))
   `(markdown-header-face-5 ((,class (:foreground ,vscode-markup-heading))))
   `(markdown-header-face-6 ((,class (:foreground ,vscode-markup-heading))))
   `(markdown-bold-face ((,class (:foreground ,vscode-markup-bold))))
   `(markdown-italic-face ((,class (:foreground ,vscode-markup-italic))))
   `(markdown-code-face ((,class (:foreground ,vscode-markup-inlinecode :background ,vscode-bg-extra-dark))))
   `(markdown-inline-code-face ((,class (:foreground ,vscode-markup-inlinecode :background ,vscode-bg-extra-dark))))
   `(markdown-pre-face ((,class (:foreground ,vscode-fg-light :background ,vscode-bg-extra-dark))))
   `(markdown-link-face ((,class (:foreground ,vscode-info :underline t))))
   `(markdown-url-face ((,class (:foreground ,vscode-info :underline t))))
   `(markdown-list-face ((,class (:foreground ,vscode-keyword))))

   ;; Magit
   `(magit-section-heading ((,class (:foreground ,vscode-markup-heading))))
   `(magit-section-highlight ((,class (:background ,vscode-line-highlight))))
   `(magit-diff-file-heading ((,class (:foreground ,vscode-variable))))
   `(magit-diff-file-heading-highlight ((,class (:background ,vscode-line-highlight :foreground ,vscode-variable))))
   `(magit-diff-hunk-heading ((,class (:foreground ,vscode-keyword :background ,vscode-bg-extra-dark))))
   `(magit-diff-hunk-heading-highlight ((,class (:foreground ,vscode-keyword :background ,vscode-line-highlight))))
   `(magit-diff-added ((,class (:foreground ,vscode-git-added :background nil))))
   `(magit-diff-added-highlight ((,class (:foreground ,vscode-git-added :background ,vscode-line-highlight))))
   `(magit-diff-removed ((,class (:foreground ,vscode-error :background nil))))
   `(magit-diff-removed-highlight ((,class (:foreground ,vscode-error :background ,vscode-line-highlight))))
   `(magit-diff-context ((,class (:foreground ,vscode-fg-dim :background nil))))
   `(magit-diff-context-highlight ((,class (:foreground ,vscode-fg-light :background ,vscode-line-highlight))))
   `(magit-branch-local ((,class (:foreground ,vscode-type))))
   `(magit-branch-remote ((,class (:foreground ,vscode-git-modified))))
   `(magit-tag ((,class (:foreground ,vscode-constant))))
   `(magit-hash ((,class (:foreground ,vscode-fg-dim))))

   ;; Which-key
   `(which-key-key-face ((,class (:foreground ,vscode-variable))))
   `(which-key-separator-face ((,class (:foreground ,vscode-fg-dim))))
   `(which-key-command-description-face ((,class (:foreground ,vscode-fg-light))))
   `(which-key-group-description-face ((,class (:foreground ,vscode-keyword))))

   ;; Dired
   `(dired-directory ((,class (:foreground ,vscode-keyword))))
   `(dired-symlink ((,class (:foreground ,vscode-info))))
   `(dired-marked ((,class (:foreground ,vscode-warning))))
   `(dired-flagged ((,class (:foreground ,vscode-error))))
   `(dired-header ((,class (:foreground ,vscode-markup-heading))))

   ;; Flycheck/Flymake
   `(flycheck-error ((,class (:underline (:style wave :color ,vscode-error)))))
   `(flycheck-warning ((,class (:underline (:style wave :color ,vscode-warning)))))
   `(flycheck-info ((,class (:underline (:style wave :color ,vscode-info)))))
   `(flymake-error ((,class (:underline (:style wave :color ,vscode-error)))))
   `(flymake-warning ((,class (:underline (:style wave :color ,vscode-warning)))))
   `(flymake-note ((,class (:underline (:style wave :color ,vscode-info)))))

   ;; Ivy/Counsel
   `(ivy-current-match ((,class (:background ,vscode-selection-bg-dark))))
   `(ivy-minibuffer-match-face-1 ((,class (:foreground ,vscode-fg-light))))
   `(ivy-minibuffer-match-face-2 ((,class (:foreground ,vscode-variable))))
   `(ivy-minibuffer-match-face-3 ((,class (:foreground ,vscode-type))))
   `(ivy-minibuffer-match-face-4 ((,class (:foreground ,vscode-function))))

   ;; Helm
   `(helm-selection ((,class (:background ,vscode-selection-bg-dark))))
   `(helm-match ((,class (:foreground ,vscode-variable))))
   `(helm-source-header ((,class (:foreground ,vscode-markup-heading))))
   )
  ) ;; closes let

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'vscode-dark-modern)
;;; vscode-dark-modern-theme.el ends here
