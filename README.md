# emacs.d

An Emacs configuration with theme and keybinding similar to Visual Studio Code.

## Requirements

Tested on Debian 11 and Debian 12 with Emacs 28.1+.

## How to compile EMacs

```
$ sudo apt install build-essential xorg-dev libgtk2.0-dev libjpeg-dev libgif-dev libtiff-dev libncurses5-dev libjansson-dev libgccjit-10-dev
$ ./configure --prefix=/opt/emacs --with-native-compilation --with-mailutils
$ make
$ make install
```

Emacs will be installed in `/opt/emacs` folder.

## LSP Configuration

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

## Keybindings

* The [keybindings](docs/keybindings.md) equals to Visual Studio.

## Licence

This configuration is released as Public Domain (Unlicensed).
