install:
	sudo apt-get install -y haskell-platform

deps:
	cabal update && cabal install test-framework test-framework-hunit test-framework-quickcheck2

tests: huffman-tests anagram-tests


huffman-tests:
	cd src && runhaskell HuffmanTests

anagram-tests:
	cd src && runhaskell AnagramTests

anagram-run-sample:
	cd src && runhaskell Anagram Linux rulez
