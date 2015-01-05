.PHONY: install
install:
	git submodule update --init
	test -f init-local.el || cp init-local.el{.sample,}
	mkdir -p skk
	test -f skk/SKK-JISYO.L || curl -o skk/SKK-JISYO.L.gz http://openlab.jp/skk/dic/SKK-JISYO.L.gz
	test -f skk/SKK-JISYO.L || gunzip skk/SKK-JISYO.L.gz
	test -f yatex.el || cp yatex.el{.sample,}
	test -f mew.el || cp mew.el{.sample,}
	emacs --script init.el
