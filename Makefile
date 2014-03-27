PWD=$(shell pwd)
install:
	git submodule update --init
	mkdir -p $(HOME)/.emacs.d;
	rm -f $(HOME)/.emacs.d/init.el
	ln -s $(PWD)/init.el $(HOME)/.emacs.d
	rm -rf $(HOME)/.emacs.d/templates
	ln -s $(PWD)/tiny-templates $(HOME)/.emacs.d/templates
	rm -rf $(HOME)/.emacs.d/snippets
	ln -s $(PWD)/snippets $(HOME)/.emacs.d/snippets
	rm -rf $(HOME)/.emacs.d/recipes
	ln -s $(PWD)/recipes $(HOME)/.emacs.d/recipes
	rm -rf $(HOME)/.emacs.d/init
	ln -s $(PWD)/init $(HOME)/.emacs.d/init
	test -f $(HOME)/.emacs.d/init-local.el || cp $(PWD)/init-local.el.sample $(HOME)/.emacs.d/init-local.el
	mkdir -p $(HOME)/.emacs.d/skk
	test -f $(HOME)/.emacs.d/yatex.el || cp $(PWD)/yatex.el.sample $(HOME)/.emacs.d/yatex.el
	test -f $(HOME)/.emacs.d/mew.el || cp $(PWD)/mew.el.sample $(HOME)/.emacs.d/mew.el
	emacs --script $(HOME)/.emacs.d/init.el

