#!/bin/csh

if ( $#argv != 2 ) then
  echo ""
  echo " Usage : $0 [embedding template xml file] [FSET number, 109 or something like that]"
  echo ""
  echo ""
  exit
endif

rm -rf Localmaker*

set template = "$1"

set FSET = "$2"

#star-submit-template-beta -template ${template} -entities FSET=$FSET -- -p bnl_condor_production
star-submit-template -template ${template} -entities FSET=$FSET
