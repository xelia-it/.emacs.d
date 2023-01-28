# emacs.d

An Emacs configuration with keybinding similar to Atom.

## Requirements

Tested on Debian 11 with Emacs 28.1.

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
The (Emacs LSP Github Project)[https://emacs-lsp.github.io/lsp-mode/] contains enough information for all the supported Language Servers.

I've installed Language Servers for this configuration using:
```
$ npm install -g @angular/language-service@next typescript @angular/language-server # Angular
$ npm install -g typescript-language-server typescript                              # TypeScript
$ npm i -g bash-language-server                                                     # Bash
$ npm i -g dockerfile-language-server-nodejs                                        # Docker
```

## Licence

This configuration is released as Public Domain (Unlicensed).

