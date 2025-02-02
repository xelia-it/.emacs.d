# LSP Configuration

In order to use LSP functionality we need to install Language Server separately.
The [Emacs LSP Github Project](https://emacs-lsp.github.io/lsp-mode/) contains
detailed information for all the supported Language Servers.

You can install Language Servers for this configuration using:
```
$ npm install -g @angular/language-service@next typescript @angular/language-server # Angular
$ npm install -g typescript-language-server typescript                              # TypeScript
$ npm install -g bash-language-server                                               # Bash
$ npm install -g dockerfile-language-server-nodejs                                  # Docker
$ npm install -g intelephense                                                       # PHP
$ npm install -g vscode-langservers-extracted                                       # HTML/Wev
$ rustup component add rust-analyzer rust-src                                       # Rust
$ pip install cmake-language-server                                                 # CMake
```

Please check also official documentation: https://emacs-lsp.github.io/
