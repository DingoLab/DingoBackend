#!/bin/bash
echo "build Dindo Documents"
cd document
tlmgr update
tlmgr install geometry
tlmgr install ctex
wget https://raw.githubusercontent.com/DingoLab/dingo.tex/master/dingo.cls
xelatex Dindo.tex
xelatex Dindo.tex
xelatex Dindo.tex
