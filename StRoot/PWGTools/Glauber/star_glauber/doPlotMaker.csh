#!/bin/csh

if ( $#argv != 2 ) then
  echo ""
  echo " Usage : $0 [name] [energy]"
  echo "    Current available name's are:"
  echo "  ImpactParameter, Npart, Ncoll, Multiplicity, AreaRP, AreaPP, EccRP, EccPP"
  echo ""
  echo "  NOTE: name is case insensitive"
  echo ""
  exit
endif

starver SL16d

set name = "$1"
set energy = "$2"
#set mode = "$3"
set mode = "1"
set option = "-b"

root4star $option <<EOF
  .L doPlotMaker.C
  doPlotMaker("$name", "$energy", $mode);
  .q
EOF

