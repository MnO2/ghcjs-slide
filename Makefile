.PHONY: all install clean

all:
	stack exec -- pandoc -c tmpl/extra.css --filter app/Filter.hs -H tmpl/header.tmpl --smart --highlight-style zenburn -V theme:serif -s -f markdown -t revealjs hs15.markdown -o hs15.html

install:
	$(MAKE) -C src install
	git submodule update --init

clean:
	$(MAKE) -C src clean
