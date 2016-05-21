#!/bin/bash
echo "build Dindo Documents"
cd document
which tlmgr
wget https://raw.githubusercontent.com/DingoLab/dingo.tex/master/dingo.cls
wget https://raw.githubusercontent.com/DingoLab/dingo.tex/master/geometry.sty
wget https://raw.githubusercontent.com/DingoLab/dingo.tex/master/geometry.cfg
xelatex Dindo.tex
xelatex Dindo.tex
xelatex Dindo.tex
