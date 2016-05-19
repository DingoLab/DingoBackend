#!/bin/bash
echo "build"
sudo apt-get update
if [ -n "$DOCUMENT" ]; then
  echo "for document"
  sudo apt-get install -y --no-install-recommends wget texlive-base texlive-xetex latex-xcolor
  sudo apt-get install -y --no-install-recommends texlive-fonts-recommended texlive-latex-extra lmodern texlive-latex-recommended
  sudo apt-get install -y ttf-wqy-microhei ttf-wqy-zenhei
  which xetex
else
  docker login -e="$DOCKER_EMAIL" -u="$DOCKER_USERNAME" -p="$DOCKER_PASSWORD"
  sudo add-apt-repository -y ppa:hvr/ghc
  sudo apt-get install -y ghc-$GHCVER cabal-install-$CABALVER
  sudo apt-get install -y ttf-wqy-microhei ttf-wqy-zenhei
  export PATH=/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:$PATH
fi
echo 'end'
