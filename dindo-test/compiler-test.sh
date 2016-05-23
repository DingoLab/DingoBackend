#!/bin/bash
if [ -n "$STACK" ]; then
  export STACKFILE=" --stack-yaml $STACK"
fi
if [ -n "$LLVM" ]; then
  export LLVMFLAG=" --ghc-options -fllvm --ghc-options -pgmlo --ghc-options opt-$LLVM --ghc-options -pgmlc --ghc-options llc-$LLVM"
fi
if [ "$THREADED" == "true" ]; then
  export THREADFLAG=" --ghc-options -threaded"
fi
stack install --flag dindo-launch:$BUILDTAGGER $STACKFILE  --ghc-options -O2 $LLVMFLAG $THREADFLAG
