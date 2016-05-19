#!/bin/bash
if [ -n "$DOCUMENT" ]; then
  echo update docuemnt
  git config --global user.name "travis-auto"
  git config --global user.email "qinka@live.com"
  cd ~
  git clone https://qinka:$GITHUB_API_KEY@github.com/Dingo/DingoRelease.git
  cd DingoRelease
  export RELPATH=$(pwd)
  cp $TRAVIS_BUILD_DIR/document/*.pdf .
  git add .
  git commit -am"$travis-ci $(date)"
  git push origin master
  cd ~
  sudo rm -rf $RELPATH
  cd $TRAVIS_BUILD_DIR
else
  echo update docker
fi
