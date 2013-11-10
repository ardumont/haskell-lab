install:
	sudo apt-get install -y haskell-platform

deps:
	cabal update && cabal install test-framework test-framework-hunit test-framework-quickcheck2

tests: huffman-tests anagram-tests bst-tests rbt-tests


huffman-tests:
	cd src && runhaskell HuffmanTests

anagram-tests:
	cd src && runhaskell AnagramTests

bst-tests:
	cd src/tree && runhaskell BinarySearchTree

rbt-tests:
	cd src/tree && runhaskell RBT

anagram-run-sample:
	cd src && runhaskell Anagram Linux rulez
