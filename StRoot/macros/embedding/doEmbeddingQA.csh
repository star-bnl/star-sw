#!/bin/csh

if ( $#argv != 13 ) then
  echo ""
  echo " Usage : $0 [starlib] [year] [production] [input file list] [output filename] [is embedding] [vzmax] [refmin] [refmax] [ptmax] [ptmin] [etamax] [ymax]"
  echo ""
  echo ""
  echo ""
  exit
endif

starver $1

set year       = "$2"
set production = "$3"
set input      = "$4"
set output     = "$5"
set isembedding = "$6"

set vzmax = "$7"
set refmin = "$8"
set refmax = "$9"
set ptmax = "$10"
set ptmin = "$11"
set etamax = "$12"
set ymax = "$13"

root4star -b <<EOF
  .L StRoot/macros/embedding/doEmbeddingQAMaker.C
  doEmbeddingQAMaker($year, "$production", "$input", "$output", $isembedding, $vzmax, $refmin, $refmax, $ptmax, $ptmin, $etamax, $ymax);
  .q
EOF

