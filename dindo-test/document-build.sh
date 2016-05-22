#!/bin/bash
echo "build Dindo Documents"
cd document
wget https://raw.githubusercontent.com/DingoLab/dingo.tex/master/dingo.cls
xelatex Dindo.tex
xelatex Dindo.tex
xelatex Dindo.tex
