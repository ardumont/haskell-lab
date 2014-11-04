#!/bin/bash -x

. $HOME/.profile

nix-shell --pure haskell-lab.nix --command "$*"
