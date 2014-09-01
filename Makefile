pull:
	git pull --rebase --prune origin master

push:
	git push origin master

install:
	sudo apt-get install -y haskell-platform

update:
	cabal update

sandbox-init:
	cabal sandbox init && cabal install --enable-tests

sandbox-delete:
	cabal sandbox delete

deps:
	cabal install test-framework \
                      test-framework-hunit \
                      test-framework-quickcheck2

additional-deps:
	cabal install aeson \
                      process \
                      http-conduit \
                      authenticate-oauth \
                      persistent \
                      persistent-sqlite \
		      contravariant


tests: huffman-tests anagram-tests bst-tests rbt-tests ini-tests


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
