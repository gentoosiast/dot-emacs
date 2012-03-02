install:
	git submodule update --init
	mkdir -p ~/.emacs.d;
	rm -f ~/.emacs.d/init.el;
	ln -s `pwd`/init.el ~/.emacs.d
	rm -rf ~/.emacs.d/templates;
	ln -s `pwd`/tiny-templates ~/.emacs.d/templates
	rm -rf ~/.emacs.d/snippets;
	ln -s `pwd`/snippets ~/.emacs.d/snippets
	test -f ~/.emacs.d/init-local.el || cp `pwd`/init-local.el.sample ~/.emacs.d/init-local.el
	mkdir -p ~/.emacs.d/skk
	test -f ~/.emacs.d/yatex.el || cp `pwd`/yatex.el.sample ~/.emacs.d/yatex.el
	emacs --script ~/.emacs.d/init.el

