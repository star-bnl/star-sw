#!/bin/csh

if ( "$HOST" !~ "*pdsf*" && "$HOST" !~ "*rcas*" ) then
   echo "this code can only be used for PDSF and RCF!"
   exit
endif

if ( $#argv != 1 ) then
  echo ""
  echo " Usage : $0 [FSET number, 109 or something like that]"
  echo ""
  echo ""
  exit
endif

if ( -f "Localmakerlibs$1.zip" ) then
  echo "CAUTION: existing sanbox file Localmakerlibs$1* will be removed!"
  rm -rf Localmakerlibs$1*
endif

set prod = `grep "\-production" $PWD/preparexmlslr.sh | awk -F"-production |-lib" '{print $2}'`
set prodname = `echo $prod`

set template = "embed_template_${prod}.xml"

if ( ! -f "$template" ) then
  echo "ERROR: $template does not exist! please run preparexmlslr.sh first!"
  exit
endif

echo "using the template file: $template"

set FSET = "$1"

#star-submit-template-beta -template ${template} -entities FSET=$FSET -- -p bnl_condor_production
star-submit-template -template ${template} -entities FSET=$FSET
