pull:
	git pull --rebase --prune origin master

push:
	git push origin master

init:
	stack init

setup:
	stack setup

build:
	stack build

test:
	stack test

.PHONY: test
