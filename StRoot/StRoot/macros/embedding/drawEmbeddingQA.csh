#!/bin/csh

if ( $#argv != 13 ) then
  echo ""
  echo " Usage : $0 [starlib] [embedding root] [real root] [geantid] [ptmax] [embedonly] [parentgeantid] [vzmax] [refmin] [refmax] [ptmin] [etamax] [ymax]"
  echo ""
  echo ""
  echo ""
  exit
endif

starver $1

set embedroot = "$2"
set realroot = "$3"
set geantid = "$4"
set ptmax = "$5"
set embedonly = "$6"
set parentgeantid = "$7"

set vzmax = "$8"
set refmin = "$9"
set refmax = "$10"
set ptmin = "$11"
set etamax = "$12"
set ymax = "$13"

root4star -b <<EOF
  .L StRoot/macros/embedding/drawEmbeddingQA.C
  drawEmbeddingQA("./","$embedroot", "$realroot",$geantid,$ptmax,$embedonly,$parentgeantid,$vzmax, $refmin, $refmax, $ptmin, $etamax, $ymax);
  .q
EOF

