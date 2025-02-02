# How to compile Emacs

If you run Emacs under Debian Linux it's better to install the last version from source,
with all the otiimizations applied.

In a terminal:
```
$ sudo apt install build-essential xorg-dev libgtk2.0-dev libjpeg-dev libgif-dev libtiff-dev libncurses5-dev libjansson-dev libgccjit-10-dev
$ ./configure --prefix=/opt/emacs --with-native-compilation --with-mailutils
$ make
$ sudo make install
```

Emacs will be installed in `/opt/emacs` folder.
