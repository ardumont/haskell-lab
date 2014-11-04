env-sandbox:
	nix-shell haskell-lab.nix

pull:
	git pull --rebase --prune origin master

push:
	git push origin master

install-ci:
	./install-platform.sh

install-dev:
# see ~/.nixpgs/config.nix - https://github.com/ardumont/dot-files/blob/master/.nixpkgs/config.nix
	nix-env -iA nixos.pkgs.hsEnv

update:
	cabal update

to-nix:
	cabal2nix haskell-lab.cabal --sha256 dummy-sha > default.nix

sandbox-init:
	cabal sandbox init && cabal configure --enable-tests

sandbox-delete:
	cabal sandbox delete

tests: huffman-tests anagram-tests bst-tests rbt-tests ini-tests


huffman-tests:
	./run-sandbox.sh "cd src && runhaskell HuffmanTests"

anagram-tests:
	./run-sandbox.sh "cd src && runhaskell AnagramTests"

bst-tests:
	./run-sandbox.sh "cd src/tree && runhaskell BSTTests"

rbt-tests:
	./run-sandbox.sh "cd src/tree && runhaskell RBTTests"

ini-tests:
	./run-sandbox.sh "cd src/ && runhaskell LoadAndUpdateIniTests"

anagram-run-sample:
	./run-sandbox.sh "cd src && runhaskell Anagram Linux rulez"
