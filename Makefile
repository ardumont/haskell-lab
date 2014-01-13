pull:
	git pull --rebase --prune origin master

push:
	git push origin master

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
	cd src/tree && runhaskell BSTTests

rbt-tests:
	cd src/tree && runhaskell RBTTests

anagram-run-sample:
	cd src && runhaskell Anagram Linux rulez
