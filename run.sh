#!/bin/bash
# Use: $0 <haskell-commands> <empty-for-normal-run-or-non-empty-for-sandbox-run>

SANDBOX=$1
shift
CMD=$*

if [ "$SANDBOX" = "n" ]; then
    eval $CMD && exit $?
fi

# Run in sandbox
. $HOME/.profile

nix-shell --pure shell.nix --command "$CMD"
