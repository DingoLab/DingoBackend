#!/bin/bash
if [ -n "$INSTALLTAGGER" ]; then
  export DOCKER_IMAGE_TAG=docekr_$(date -u '+%Y_%m_%d_%H_%M_%S_%Z')
  export DOCKER_IMAGE_LASTEST=docker
  if [ -n "$TRAVIS_TAG" ]; then
    export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-$TRAVUS_BRANCH\_$TRAVIS_TAG
  else
    export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-$TRAVUS_BRANCH\_${TRAVIS_COMMIT:0:7}
  fi
  export DOCKER_IMAGE_LASTEST=$DOCKER_IMAGE_LASTEST-lastest
  export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-$(uname)_$Distributor\_$Codename-GHC_$GHCVER-$(lscpu | grep Architecture | awk '{print $2}')
  export DOCKER_IMAGE_LASTEST=$DOCKER_IMAGE_LASTEST-$(uname)_$Distributor\_$Codename-GHC_$GHCVER-$(lscpu | grep Architecture | awk '{print $2}')
  if [ -n "$LLVM" ]; then
    export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-llvm-$LLVM
    export DOCKER_IMAGE_LASTEST=$DOCKER_IMAGE_LASTEST-llvm-$LLVM
  fi
  if [ -n "$THREADED" ]; then
    export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-threaded
    export DOCKER_IMAGE_LASTEST=$DOCKER_IMAGE_LASTEST-threaded
  fi
  export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-$INSTALLTAGGER
  export DOCKER_IMAGE_LASTEST=$DOCKER_IMAGE_LASTEST-$INSTALLTAGGER
  echo build docker
  mkdir dindo-docker/bin
  echo echo \$SERVER_CONFIG \| $INSTALLTAGGER \+RTS \-N > dindo-docker/bin/start.sh
  chmod a+x dindo-docker/bin/start.sh
  cp ~/.local/bin/$INSTALLTAGGER dindo-docker/bin
  cd dindo-docker && docker build -f Dockerfile -t qinka/dindo:$DOCKER_IMAGE_TAG . && cd ..
  cd dindo-docker && docker build -f Dockerfile -t qinka/dindo:$DOCKER_IMAGE_LASTEST . && cd ..
  docker push  qinka/dindo
fi
