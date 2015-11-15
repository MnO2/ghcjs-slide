.PHONY: install clean

install:
	cabal sandbox init
	echo 'compiler: ghcjs' >> cabal.sandbox.config
	cabal install --ghcjs diagrams-contrib
	git clone "https://github.com/MnO2/diagrams-ghcjs"
	cabal install --ghcjs ./diagrams-ghcjs

clean:
	find . -name "*.js_o" -o -name "*.js_hi" | xargs rm
	rm -rf main.jsexe
	rm -f M*.hs main.hs

