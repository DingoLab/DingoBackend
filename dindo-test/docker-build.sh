#!/bin/bash
if [ -n "$TRAVIS_TAG" ]; then
  export DOCKER_IMAGE_TAG=$TRAVIS_BUILD_NUMBER-$TRAVIS_TAG-$BUILDTAGGER
else
  export DOCKER_IMAGE_TAG=$TRAVIS_BUILD_NUMBER-$TRAVIS_COMMIT-$BUILDTAGGER
fi
echo build docker
mkdir dindo-docker/bin
echo echo \$SERVER_CONFIG \| $BUILDTAGGER \+RTS \-N > dindo-docker/bin/start.sh
chmod a+x dindo-docker/bin/start.sh
cp ~/.local/bin/$BUILDTAGGER dindo-docker/bin
cd dindo-docker && docker build -f Dockerfile -t qinka/dindo:$DOCKER_IMAGE_TAG . && cd ..
docker push  qinka/dindo
