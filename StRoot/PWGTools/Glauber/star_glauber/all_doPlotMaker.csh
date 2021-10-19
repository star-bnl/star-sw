#!/bin/csh

if ( $#argv != 1 ) then
  echo ""
  echo " Usage : $0 [Energy]"
  echo ""
  echo ""
  exit
endif

#set list = ( "ImpactParameter" "Npart" "Ncoll" "Multiplicity" "AreaRP" "AreaPP" "EccRP" "EccPP" "EccPP_2" "EccPPM" )
set list = ( "ImpactParameter" "Npart" "Ncoll" "Multiplicity" "AreaRP" "AreaPP" "EccRP" "EccRPM" "EccPP_0" "EccPP_0_2" "EccPPM_0" "EccPPM_0_2" )
set energy = "$1"

foreach name ($list)
  doPlotMaker.csh $name $energy
end

