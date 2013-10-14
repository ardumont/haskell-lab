install:
	sudo apt-get install -y haskell-platform

deps:
	cabal update && cabal install test-framework test-framework-hunit

test:
	cd src && runhaskell HuffmanTests
