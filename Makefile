build:
	cabal install
	cp .cabal-sandbox/bin/WebStats ./Assignment4
	cp src/Main.hs ./Assignment4.hs

clean:
	rm Assignment4 Assignment4.hs
