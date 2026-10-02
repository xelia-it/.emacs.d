# External Packages

## Packages needed by LSP

In order to use LSP functionality we need to install Language Server separately.
The [Emacs LSP Github Project](https://emacs-lsp.github.io/lsp-mode/) contains
detailed information for all the supported Language Servers.

You can install Language Servers for this configuration using:
```bash
npm install -g @angular/language-service@next typescript @angular/language-server # Angular
npm install -g typescript-language-server typescript                              # TypeScript
npm install -g bash-language-server                                               # Bash
npm install -g dockerfile-language-server-nodejs                                  # Docker
npm install -g intelephense                                                       # PHP
npm install -g vscode-langservers-extracted                                       # HTML/Wev
rustup component add rust-analyzer rust-src                                       # Rust
pip install cmake-language-server                                                 # CMake
```

Please check also official documentation: https://emacs-lsp.github.io/

## Other packages

```bash
npm install -g @mermaid-js/mermaid-cli
```

## Markdown preview

`markdown-preview` (`C-c C-c p`) and `markdown-preview-mode` (`C-c C-v`) need a
Markdown processor in `PATH`:

```bash
sudo apt install pandoc   # or: multimarkdown
```

## Tree-sitter grammars

Debian ships libtree-sitter 0.22 (ABI 14). Grammars built from the latest
upstream (ABI 15) are reported as `version-mismatch`, so they are not loaded
and the classic major mode is used instead. Check with `M-x treesit-install-language-grammar`
using an older `revision`, or `(treesit-language-available-p 'rust t)`.
