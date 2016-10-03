#! /bin/sh

# This script builds the Doxygen html refrence library
# and is not run automatically.

set -x
# run doxygen if present
if [ `whereis doxygen | wc -w` -gt 1 ]; then
   doxygen doxygen.conf
   if [ `whereis latex | wc -w` -gt 1 ]; then
      cd latex
      pdflatex refman.tex
      pdflatex refman.tex
      pdflatex refman.tex
      pdflatex refman.tex
      makeindex refman.idx
      pdflatex refman.tex
      mv refman.pdf ../HepMC2_reference_manual.pdf
      cd ..
      rm -rf latex
   fi
fi
