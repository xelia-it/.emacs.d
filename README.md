# emacs.d

An Emacs configuration with theme and keybinding inspired to Visual Studio Code.

## Requirements

This configuration is compatible with *Emacs 28.1+* and
has test on *Debian 11* and *Debian 12*.

You also require [git](https://git-scm.com/) for cloning the repo.

## Quick Start

* Download Emacs: https://www.gnu.org/software/emacs/
* Go to your user root.
  It's `/home/<your username>` in Linux and `C:\Users\<your username>\AppData\Roaming` in Windows.
* Clone the repo: `git clone https://github.com/xelia-it/.emacs.d.git`
* Open Emacs. Emacs should show message "Byte-compyling init file..."
  and should start downloading all the packages.
  This can take a while. Time for a break :coffee:.
* At the end the main Emacs window should appear.
  The first time some compilation *warnings* can appear: you cna ignore them.
* Close and reopen Emacs. Now the window should open quickly with the new theme applied!

## Screenshot

![Editing Markdown](/docs/screenshot-theme.webp "Editing Markdown")

## See also

* Details about implemented [keybindings](docs/keybindings.md).
* Where to download [fonts](docs/fonts.md) for better experience.
* How to [configure LSP](docs/lsp-configuration.md).

## License

This configuration is released as Public Domain (Unlicensed).
