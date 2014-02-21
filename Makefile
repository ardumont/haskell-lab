pull:
	git pull --rebase --prune origin master

push:
	git push origin master

install:
	sudo apt-get install -y haskell-platform

update:
	cabal update

init:
	cabal sandbox init && cabal install --enable-tests

deps:
	cabal install test-framework \
                      test-framework-hunit \
                      test-framework-quickcheck2 \
                      aeson \
                      process \
                      http-conduit \
                      authenticate-oauth \
                      persistent \
                      persistent-sqlite

tests: huffman-tests anagram-tests bst-tests rbt-tests ini-tests wifi-tests


huffman-tests:
	cd src && runhaskell HuffmanTests

anagram-tests:
	cd src && runhaskell AnagramTests

bst-tests:
	cd src/tree && runhaskell BSTTests

rbt-tests:
	cd src/tree && runhaskell RBTTests

ini-tests:
	cd src/ && runhaskell LoadAndUpdateIniTests

anagram-run-sample:
	cd src && runhaskell Anagram Linux rulez

wifi-tests:
	cd src/ && runhaskell WifiTests
