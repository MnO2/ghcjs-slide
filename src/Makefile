.PHONY: install clean

install:
	cabal sandbox init
	echo 'compiler: ghcjs' >> cabal.sandbox.config
	cabal install --ghcjs diagrams-contrib
	git clone "https://github.com/MnO2/diagrams-ghcjs"
	cabal install --ghcjs ./diagrams-ghcjs

clean:
	rm -f *.js_o *.js_hi
	rm -f M*.hs main.hs
	rm -rf main.jsexe

