#!/bin/bash
if [ -n "$STACK" ]; then
  export STACKFILE=" --stack-yaml $STACK "
fi
if [ -n "$LLVM" ]; then
  export LLVMFLAG=" --ghc-options -fllvm --ghc-options -pgmlo --ghc-options opt-$LLVM --ghc-options -pgmlc --ghc-options llc-$LLVM "
fi
if [ -n "$THREADED" ]; then
  export THREADFLAG=" --ghc-options -threaded "
fi
if [ -n "$INSTALLTAGGER" ]; then
  export STACK_CMD=install
  export DINDO_LAUNCH_TAGGER=$INSTALLTAGGER
else
  export STACK_CMD=build
  export DINDO_LAUNCH_TAGGER=$BUILDTAGGER
fi
stack $STACK_CMD --flag dindo-launch:$DINDO_LAUNCH_TAGGER $STACKFILE  --ghc-options -O2 $THREADFLAG $LLVMFLAG
