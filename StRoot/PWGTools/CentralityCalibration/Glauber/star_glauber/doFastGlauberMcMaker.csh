#!/bin/csh 

if ( $#argv != 6 ) then
  echo ""
  echo " Usage : $0 [Output filename] [Number of events] [system] [energy] [type] [deformation (true or false)]"
  echo ""
  exit
endif

set output  = "$1"
set nevents = "$2"
set system = "$3"
set energy = "$4"
set type = "$5"
set deformation = "$6"

starver SL19b

root4star -b <<EOF
  .L doFastGlauberMcMaker.C
  doFastGlauberMcMaker("$output", $nevents, "$system", $energy, "$type", $deformation);
  .q
EOF

