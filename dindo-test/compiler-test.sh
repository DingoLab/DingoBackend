#!/bin/bash
if [ -n "$STACK" ]; then
  export STACKFILE=" --stack-yaml $STACK"
fi
if [ -n "$LLVM" ]; then
  export LLVMFLAG=" --ghc-options -fllvm -pgmlo opt-$LLVM -pgmlc llc-$LLVM"
fi
stack install --flag dindo-launch:$BUILDTAGGER $STACKFILE  --ghc-options -O2 $LLVMFLAG
