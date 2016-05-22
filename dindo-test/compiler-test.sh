#!/bin/bash
if [ -n "$STACK" ]; then
  export STACKFILE=" --stack-yaml $STACK"
fi
stack install --flag dindo-launch:$BUILDTAGGER $STACKFILE
