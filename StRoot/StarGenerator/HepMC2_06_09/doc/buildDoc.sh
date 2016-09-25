#! /bin/sh

# this script builds the pdf documents
# the bootstrap process executes this script

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
      rm -rf latex html
   fi
fi
# now run latex
if [ `whereis latex | wc -w` -gt 1 ]; then
   latex HepMC2_user_manual.tex 
   latex HepMC2_user_manual.tex
   dvipdf HepMC2_user_manual
   # cleanup latex files
   rm -f *.aux *.dvi *.log *.toc *.lot *.lof
fi

