#!/bin/bash
if [ -n "$STACK" ]; then
  export STACKFILE=" --stack-yaml $STACK"
fi
if [ -n "$LLVM" ]; then
  export LLVMFLAG=" --ghc-options -fllvm --ghc-options -pgmlo opt-$LLVM --ghc-options"
fi
stack install --flag dindo-launch:$BUILDTAGGER $STACKFILE  --ghc-options -O2 $LLVMFLAG
