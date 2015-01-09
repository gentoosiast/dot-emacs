This repository contains emacs configuration files

## Install

```
$ git clone https://github.com/uchida/dot-emacs.git ~/.emacs.d
$ make -C ~/.emacs.d
```

## Notices

On `/bin/sh != bash` systems, set SHELL environment variable.

```
$ make -e SHELL=bash -C ~/.emacs.d
```

On Windows, install with cygwin shell and note that

- Turn off core.autocrlf to prevents collapsion of shellscript.

  ```
  $ git config --global core.autocrlf false
  ```
