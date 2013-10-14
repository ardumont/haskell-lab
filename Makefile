install:
	sudo aptitude install haskell-platform

deps:
	cabal install test-framework test-framework-hunit test-framework-quickcheck2 multiset

test:
	cd src && runhaskell HuffmanTests
