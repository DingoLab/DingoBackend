#!/bin/bash
if [ -n "$INSTALLTAGGER" ]; then
  export DOCKER_IMAGE_TAG=docekr_$TRAVIS_BUILD_NUMBER
  if [ -n "$TRAVIS_TAG" ]; then
    export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-$TRAVIS_TAG
  else
    export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-${TRAVIS_COMMIT:0:7}
  fi
  export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-$(uname)_$Distributor\_$Codename-GHC_$GHCVER-$(lscpu | grep Architecture | awk '{print $2}')
  if [ -n "$LLVM" ]; then
    export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-llvm-$LLVM
  fi
  if [ -n "$THREADED" ]; then
    export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-threaded
  fi
  export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-$INSTALLTAGGER
  echo build docker
  mkdir dindo-docker/bin
  echo echo \$SERVER_CONFIG \| $INSTALLTAGGER \+RTS \-N > dindo-docker/bin/start.sh
  chmod a+x dindo-docker/bin/start.sh
  cp ~/.local/bin/$INSTALLTAGGER dindo-docker/bin
  cd dindo-docker && docker build -f Dockerfile -t qinka/dindo:$DOCKER_IMAGE_TAG . && cd ..
  docker push  qinka/dindo
fi
