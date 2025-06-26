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

      (vscode-brackets            "#c586c0") ;; NEW
      (vscode-brackets-match      "#ffd700") ;; NEW
      (vscode-search-match        "#623315")
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
   `(minibuffer-prompt ((,class (:foreground ,vscode-keyword :weight bold))))
   `(link ((,class (:foreground ,vscode-info :underline t))))

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

   ;; ;; Minibuffer
   ;; `(minibuffer-prompt ((t (:foreground ,vscode-line-number :background ,vscode-bg-dark))))
   ;; `(vertical-border ((t (:foreground ,vscode-bg-light :background ,vscode-bg-dark))))

   ;; ;; UI Elements
   ;; `(mode-line ((,class (:background ,vscode-bg-light :foreground ,vscode-fg-light :box nil))))
   ;; `(mode-line-inactive ((,class (:background ,vscode-bg-dark :foreground ,vscode-fg-dim :box nil))))
   ;; `(header-line ((,class (:background ,vscode-bg-light :foreground ,vscode-fg-light :box nil))))
   ;; `(minibuffer-prompt ((,class (:foreground ,vscode-keyword :weight bold))))

   ;; ;; Org-mode
   ;; `(org-level-1 ((,class (:foreground ,vscode-keyword :weight bold :height 1.05))))
   ;; `(org-level-2 ((,class (:foreground ,vscode-function-name :weight bold :height 1.05))))
   ;; `(org-level-3 ((,class (:foreground ,vscode-variable :weight bold))))
   ;; `(org-block ((,class (:background ,vscode-bg-light :foreground ,vscode-fg-light))))
   ;; `(org-code ((,class (:background ,vscode-bg-light :foreground ,vscode-fg-light))))
   ;; `(org-quote  ((t (:background ,vscode-bg-light))))
   ;; `(org-todo ((t (:foreground ,vscode-warning :weight bold))))
   ;; `(org-done ((t (:foreground ,vscode-comment :weight bold))))

   ;; ;; Dired
   ;; `(dired-directory ((,class (:foreground ,vscode-keyword :weight bold))))
   ;; `(dired-symlink ((,class (:foreground ,vscode-constant :weight bold))))

   ;; ;; Treemacs
   ;; `(treemacs-root-face ((,class (:foreground ,vscode-fg-light :weight bold :height 1.2))))
   ;; `(treemacs-directory-face ((,class (:foreground ,vscode-keyword))))
   ;; `(treemacs-file-face ((,class (:foreground ,vscode-fg-light))))
   ;; `(treemacs-tags-face ((,class (:foreground ,vscode-constant))))
   ;; `(treemacs-git-ignored-face ((,class (:foreground ,vscode-shadow))))

   ;; Helm
   `(helm-selection ((,class (:foreground ,vscode-fg-light :background ,vscode-selection-bg)))) ;; Current selection
   `(helm-source-header ((,class (:foreground ,vscode-fg-light :background ,vscode-bg-light :weight bold))))
   ;; Text matching the search

   `(helm-match ((,class (:foreground ,vscode-keyword :weight bold :inherit nil))))
   `(helm-buffer-file ((,class (:foreground ,vscode-fg-light :background unspecified))))
   `(helm-file ((,class (:foreground ,vscode-fg-light :background unspecified))))   ;; Main text
   `(helm-ff-file-extension ((,class (:foreground ,vscode-fg-light :background unspecified))))  ;; File extensions
   `(helm-ff-directory ((,class (:foreground ,vscode-type :background unspecified))))
   `(helm-ff-dotted-directory ((,class (:foreground ,vscode-fg-dim :background unspecified))))
   `(helm-ff-executable ((,class (:foreground ,vscode-function :background unspecified))))
   `(helm-ff-invalid-symlink ((,class (:foreground ,vscode-error :background unspecified))))
   `(helm-ff-denied ((,class (:foreground ,vscode-error :background unspecified))))
   `(helm-ff-symlink ((,class (:foreground ,vscode-variable :background unspecified))))

   `(helm-grep-lineno ((,class (:foreground ,vscode-fg-dim :background unspecified))))

   `(helm-header-line-left-margin ((,class (:foreground ,vscode-brackets-match :background unspecified))))

   ;; ;;`(helm-candidate-number ((,class (:foreground ,vscode-parenthesis :background unspecified))))
   ;; ;;`(helm-ff-prefix ((,class (:foreground ,vscode-parenthesis :background unspecified))))
   ;; ;; `(helm-match ((,class (:foreground ,vscode-highlight :weight bold))))
   ;; ;;`(helm-visible-mark ((,class (:background ,vscode-region :foreground ,vscode-white))))

   ;; `(helm-selection ((,class (:foreground ,vscode-fg-light :background ,vscode-highlight ))))
   ;; `(helm-selection-line ((,class (:background ,vscode-highlight))))  ;; Riga selezionata


   ;; `(helm-ff-dirs ((,class (:foreground ,vscode-keyword :background unspecified))))  ;; Funzione per directory
   ;; `(helm-ff-dotted-directory ((,class (:foreground "#6A9955" :background unspecified))))  ;; Ombre per cartelle nascoste
   ;; `(helm-ff-file ((,class (:foreground ,vscode-fg-light :background unspecified))))
   ;; `(helm-ff-file-extension ((,class (:foreground ,vscode-fg-light :background unspecified))))  ;; File extensions
   ;; `(helm-candidate-number ((,class (:foreground "#C586C0" :background unspecified))))  ;; Parentesi/evidenziazione numeri
   ;; `(helm-source-header ((,class (:foreground "#DCDCAA" :background ,vs-code-bg-dark :weight bold)))) ;; Titolo della fonte


   ;; `(helm-M-x-key ((,class (:foreground "#d16969" :weight bold)))) ;; Keybinding for M-x

   ;; `(helm-buffer-directory ((,class (:foreground "#4FC1FF" :background ,vs-code-bg-dark))))  ;; Nome cartella in Helm buffer
   ;; `(helm-buffer-file ((,class (:foreground "#D4D4D4" :background ,vs-code-bg-dark)))) ;; File nei buffer di Helm
   ;; `(helm-buffer-size ((,class (:foreground "#6A9955" :background ,vs-code-bg-dark)))) ;; Dimensione buffer
   ;; `(helm-projectile-file ((,class (:foreground "#D4D4D4" :background ,vs-code-bg-dark))))
   ;; `(helm-projectile-dir ((,class (:foreground "#4FC1FF" :background ,vs-code-bg-dark))))
   ;; `(helm-ff-symlink ((,class (:foreground "#C586C0" :background unspecified)))) ;; Link simbolici
   ;; `(helm-ff-executable ((,class (:foreground "#4EC9B0" :background unspecified)))) ;; File eseguibili
   ;; `(helm-ff-invalid-symlink ((,class (:foreground "#F44747" :background unspecified)))) ;; Link simbolici rotti

   ;; ;; Highlight indent guide
   ;; ;; The normal color should be "vscode-border" in order to be equal
   ;; ;; to Visual Studio Code theme.
   ;; `(highlight-indent-guides-character-face  ((t (:foreground ,vscode-border-dark))))
   ;; `(highlight-indent-guides-even-face  ((t (:foreground ,vscode-border-dark))))
   ;; `(highlight-indent-guides-odd-face  ((t (:foreground ,vscode-border-dark))))
   ;; `(highlight-indent-guides-stack-character-face  ((t (:foreground ,vscode-shadow))))
   ;; `(highlight-indent-guides-stack-even-face  ((t (:foreground ,vscode-shadow))))
   ;; `(highlight-indent-guides-stack-odd-face  ((t (:foreground ,vscode-shadow))))
   ;; `(highlight-indent-guides-top-character-face  ((t (:foreground ,vscode-shadow))))
   ;; `(highlight-indent-guides-top-even-face  ((t (:foreground ,vscode-shadow))))
   ;; `(highlight-indent-guides-top-odd-face  ((t (:foreground ,vscode-shadow))))

   ;; ;; Web Mode
   ;; `(web-mode-html-tag-face ((t (:foreground ,vscode-keyword))))   ;; Color for HTML tags
   ;; `(web-mode-html-attr-name-face ((t (:foreground ,vscode-variable)))) ;; HTML attributes
   ;; `(web-mode-html-attr-value-face ((t (:foreground ,vscode-string)))) ;; Values
   ;; `(web-mode-comment-face ((t (:foreground ,vscode-comment :slant italic)))) ;; Comments
   ;; `(web-mode-docvscode-type-face ((t (:foreground ,vscode-cursor)))) ;; !DOCTYPE
   ;; `(web-mode-css-selector-face ((t (:foreground ,vscode-css-selector)))) ;; CSS selectors
   ;; `(web-mode-css-property-name-face ((t (:foreground ,vscode-variable)))) ;; CSS Properties
   ;; `(web-mode-css-color-face ((t (:foreground ,vscode-css))))
   ;; `(web-mode-javascript-string-face ((t (:foreground ,vscode-string)))) ;; Orange for string like JS
   ;; `(web-mode-block-face ((t (:background unspecified)))) ;; Use the same background of the rest of the HTML
   ;; `(web-mode-current-element-highlight-face ((t (:foregrund ,vscode-keyword :background ,vscode-tag-background))))
   ;; `(web-mode-current-column-highlight-face ((t (:foregrund ,vscode-keyword :background ,vscode-tag-background))))


   ;; ;; LSP
   `(lsp-face-highlight-read ((,class (:background "#474747" :foreground unspecified :underline nil))))
   `(lsp-face-highlight-textual ((,class (:background "#474747" :foreground unspecified :underline nil))))
   `(lsp-face-highlight-write ((,class (:background "#093d5b" :foreground unspecified :underline nil))))

   ;; `(lsp-ui-doc-background ((t (:background ,vscode-bg-dark))))
   ;; `(lsp-ui-doc-header ((t (:background ,vscode-highlight :foreground ,vscode-white :bold t))))
   ;; `(lsp-ui-doc-text ((t (:foreground ,vscode-fg-light))))
   ;; `(lsp-ui-peek-peek ((t (:background ,vs-code-bg-dark))))
   ;; `(lsp-ui-peek-list ((t (:background "#252526"))))
   ;; `(lsp-ui-peek-selection ((t (:background "#264f78" :foreground "#ffffff"))))
   ;; `(lsp-ui-peek-highlight ((t (:background "#094771" :foreground "#ffffff"))))
   ;; `(lsp-ui-peek-header
   ;;   ((t (:background ,vs-code-bg-dark :foreground "#ffffff"
   ;;                    :box (:line-width (-1 . -1) :color "#007acc" :style nil)))))
   ;; `(lsp-ui-peek-footer
   ;;   ((t (:background ,vs-code-bg-dark :foreground "#ffffff"
   ;;                    :box (:line-width (-1 . -1) :color "#007acc" :style nil)))))
   ;; `(lsp-ui-peek-filename ((t (:foreground ,vscode-variable))))
   ;; `(lsp-ui-peek-line-number ((t (:foreground ,vscode-line-number))))

   ;; ;; ----

   ;; ;; Company tooltip
   `(company-box-background ((,class (:background ,vscode-bg-dark))))
   `(company-box-border ((,class (:background ,vscode-border-dark))))
   `(company-tooltip ((,class (:background ,vscode-bg-dark :foreground ,vscode-fg-light))))
   `(company-tooltip-selection ((,class (:background ,vscode-selection-bg))))
   `(company-tooltip-annotation ((t (:foreground ,vscode-function))))

   ;; `(company-tooltip-annotation-selection ((t (:foreground "#ffd700"))))
   `(company-tooltip-scrollbar-thumb ((t (:background "#3e3e3e"))))
   `(company-tooltip-scrollbar-track ((t (:background "#252526"))))
   ;; `(company-tooltip-mouse ((t (:background "#569cd6" :foreground "#ffffff"))))


   ;; ;; Active modeline
   `(mode-line ((t (:background ,vscode-bg-extra-dark :foreground ,vscode-fg-light))))
   ;; Inactive modeline
   `(mode-line-inactive ((t (:background ,vscode-bg-light :foreground "#858585" :box nil))))

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
