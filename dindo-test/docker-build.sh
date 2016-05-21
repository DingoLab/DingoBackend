#!/bin/bash
if [ -n "$TRAVIS_TAG" ]; then
  export DOCKER_IMAGE_TAG=$TRAVIS_BUILD_NUMBER-$TRAVIS_TAG-$GLOB_DT
else
  export DOCKER_IMAGE_TAG=$TRAVIS_BUILD_NUMBER-$TRAVIS_COMMIT-$GLOB_DT
fi
echo build docker
mkdir dindo-docker/bin
echo echo \$SERVER_CONFIG \| $BUILDTAGGER \+RTS \-N > dindo-docker/bin/start.sh
chmod a+x dindo-docker/bin/start.sh
cp ~/.local/bin/$BUILDTAGGER dindo-docker/bin
cd dindo-docker && docker build -f Dockerfile -t qinka/dindo:$BUILDTAGGER-$DOCKER_IMAGE_TAG . && cd ..
docker push  qinka/dindo
