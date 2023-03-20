#!/bin/csh

if ( $#argv != 6 ) then
  echo ""
  echo " Usage: $0 [System] [Input file list] [Output filename] [type] [re-weighting correction (true or false)] [Unit weight (true or false)]"
  echo ""
  echo ""
  exit
endif

set system = "$1"
set input  = "$2"
set output = "$3"
set type   = "$4"
set table  = "./table"
set reweighting = "$5"
set unitweight = "$6"

starver SL16d

root4star -b <<EOF
  .L doAnalysisMaker.C
  doAnalysisMaker("$system", "$input", "$output", "$type", "$table", $reweighting, $unitweight);
  .q
EOF

