install:
	sudo apt-get install -y haskell-platform

deps:
	cabal update && cabal install cabal-install test-framework test-framework-hunit test-framework-quickcheck2 multiset

test:
	cd src && runhaskell HuffmanTests
