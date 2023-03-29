#!/bin/csh

if ( $#argv != 3 ) then
  echo ""
  echo " Usage: $0 [system] [re-weighting correction (true or false)] [Unit weight (true or false)]"
  echo ""
  echo ""
  exit
endif

#starnew
stardev
#set list=("default")
#set list = ( "smallNpp" "largeNpp" )
#set list = ( "lowrw" "highrw" )
#set list = ( "default" "small" "large" "smallXsec" "largeXsec" "gauss" "smallNpp" "largeNpp" "smallTotal" "largeTotal" "lowrw" "highrw" )
#set list = ( "default" "small" "large" "smallXsec" "largeXsec" "gauss" "smallNpp" "largeNpp" "smallTotal" "largeTotal" )
#set list = ( "default" "small" "large" "smallXsec" "largeXsec" "smallNpp" "largeNpp" "smallTotal" "largeTotal" )
#set list = ( "default" "small" "large" "smallXsec" "largeXsec" "gray" "gauss" "smallNpp" "largeNpp" "smallTotal" "largeTotal" )
#set list = ( "small" "large" "smallXsec" "largeXsec" "gray" "gauss" "smallNpp" "largeNpp" "smallTotal" "largeTotal" )

set list = ( "default" "small" "large" "smallXsec" "largeXsec" "gauss" "smallNpp" "largeNpp" "smallTotal" "largeTotal")
#set list = ( "smallTotal" "largeTotal" )

set system = "$1"
set reweighting = "$2"
set unitweight = "$3"

foreach type ($list)
  set input  = "./LIST/tree.$type.list"
  set output = "ana_${system}_${type}.root"
  if ( -f $input ) then
    set submit = "submit_condor.pl -v -s doAnalysisMaker.csh $system $input $output $type $reweighting $unitweight"
    echo $submit
    $submit
  else
    echo ""
    echo "can't find input $input. Skip"
    echo ""
  endif
end

