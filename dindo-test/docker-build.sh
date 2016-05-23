#!/bin/bash
if [ -n "$TRAVIS_TAG" ]; then
  export DOCKER_IMAGE_TAG=$TRAVIS_TAG
else
  export DOCKER_IMAGE_TAG=${TRAVIS_COMMIT:0:7}
fi
if [ -n "$LLVM" ]; then
  export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-llvm-$LLVM
fi
if [ "$THREADED" == "true" ]; then
  export DOCKER_IMAGR_TAG=threaded-$DOCKER_IMAGE_TAG
fi
export DOCKER_IMAGE_TAG=$TRAVIS_BUILD_NUMBER-$DOCKER_IMAGE_TAG-GHC-$GHCVER-$BUILDTAGGER
echo build docker
mkdir dindo-docker/bin
echo echo \$SERVER_CONFIG \| $BUILDTAGGER \+RTS \-N > dindo-docker/bin/start.sh
chmod a+x dindo-docker/bin/start.sh
cp ~/.local/bin/$BUILDTAGGER dindo-docker/bin
cd dindo-docker && docker build -f Dockerfile -t qinka/dindo:$DOCKER_IMAGE_TAG . && cd ..
docker push  qinka/dindo
