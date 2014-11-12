# sandbox variable, `n` means no sandbox (by default)
# otherwise, runs in sandbox environment (for ci or for local dev not in env-sandbox)
SANDBOX=n

env-sandbox:
	nix-shell haskell-lab.nix

pull:
	git pull --rebase --prune origin master

push:
	git push origin master

install-ci:
	curl https://raw.githubusercontent.com/ardumont/sh/master/nix/install-nix.sh | bash

install-dev:
# see ~/.nixpgs/config.nix - https://github.com/ardumont/dot-files/blob/master/.nixpkgs/config.nix
	nix-env -i env-haskell

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
	./run.sh $(SANDBOX) "cd src && runhaskell HuffmanTests"

anagram-tests:
	./run.sh $(SANDBOX) "cd src && runhaskell AnagramTests"

bst-tests:
	./run.sh $(SANDBOX) "cd src/tree && runhaskell BSTTests"

rbt-tests:
	./run.sh $(SANDBOX) "cd src/tree && runhaskell RBTTests"

ini-tests:
	./run.sh $(SANDBOX) "cd src/ && runhaskell LoadAndUpdateIniTests"

anagram-run-sample:
	./run.sh $(SANDBOX) "cd src && runhaskell Anagram Linux rulez"
