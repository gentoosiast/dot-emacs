PWD=$(shell pwd)
install:
	git submodule update --init
	test -f init-local.el || cp init-local.el{.sample,}
	mkdir -p skk
	test -f yatex.el || cp yatex.el{.sample,}
	test -f mew.el || cp mew.el{.sample,}
	emacs --script init.el

