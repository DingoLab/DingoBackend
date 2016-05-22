#!/bin/bash
echo "build"
sudo apt-get update
if [ -n "$DOCUMENT" ]; then
  echo "for document"
  sudo apt-get update
  sudo apt-get install -y --no-install-recommends wget texlive-base texlive-xetex latex-xcolor xzdec
  sudo apt-get install -y --no-install-recommends texlive-fonts-recommended texlive-latex-extra lmodern texlive-latex-recommended
  cd ~ && mkdir texmf
  tlmgr init-usertree
  sudo tlmgr update --all
  sudo tlmgr install ctex l3kernel xecjk l3packages latex
  sudo tlmgr install geometry fontspec fandol ulem zhnumber
  cd $TRAVIS_BUILD_DIR
else
  export Codename=$(lsb_release -a | grep Codename | awk '{print $2}')
  sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442
  echo deb http://download.fpcomplete.com/ubuntu $Codename main|sudo tee /etc/apt/sources.list.d/fpco.list
  if [ -n "$LLVM" ]; then
    echo deb http://llvm.org/apt/$Codename/ llvm-toolchain-$Codename main | sudo tee -a /etc/apt/sources.list.d/llvm.list
    echo deb-src http://llvm.org/apt/$Codename/ llvm-toolchain-$Codename main | sudo tee -a /etc/apt/sources.list.d/llvm.list
    echo deb http://llvm.org/apt/$Codename/ llvm-toolchain-$Codename-$LLVM main | sudo tee -a /etc/apt/sources.list.d/llvm.list
    echo deb-src http://llvm.org/apt/$Codename/ llvm-toolchain-$Codename-$LLVM main | sudo tee -a /etc/apt/sources.list.d/llvm.list
  fi
  cat /etc/apt/sources.list
  docker login -e="$DOCKER_EMAIL" -u="$DOCKER_USERNAME" -p="$DOCKER_PASSWORD"
  sudo add-apt-repository -y ppa:hvr/ghc
  sudo apt-get update
  sudo apt-get install -y ghc-$GHCVER cabal-install-$CABALVER
  sudo apt-get install stack -y
  if [ -n "$LLVM" ]; then
    sudo apt-get install -y libllvm-$LLVM-ocaml-dev libllvm$LLVM libllvm$LLVM-dbg lldb-$LLVM llvm-$LLVM llvm-$LLVM-dev llvm-$LLVM-runtime lldb-$LLVM-dev
  fi
  export PATH=/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:$PATH
fi
echo 'end'
