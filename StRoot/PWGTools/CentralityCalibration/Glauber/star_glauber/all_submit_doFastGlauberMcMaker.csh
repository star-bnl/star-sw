#!/bin/csh -f

if ( $#argv != 5 ) then
  echo ""
  echo "  Usage : $0 [system] [energy] [Begin jobIndex] [End jobIndex] [0=test, 1=submit]"
  echo ""
  echo ""
  exit
endif

#set nevents = 100000 # 100K
set nevents = 200000
set system = "$1"
set energy = "$2"
set begin  = "$3"
set end    = "$4"
##########################################################
#set types  = ( "default" )
#for systematic uncertainty, we need to submit the jobs with different types
#set types  = ( "gauss" ) 
#set types  = ( "smallNpp" "largeNpp" )
#set types  = ( "default" "large" "small" "largeXsec" "smallXsec" "smallNpp" "largeNpp" )
set types  = ( "default" "large" "small" "largeXsec" "smallXsec" "gauss" "smallNpp" "largeNpp" )
##########################################################

set flag = "$5"
set run = ""
if ( $flag == 1 ) then
  set run = "-run"
endif

foreach type ($types)
  submit_glauber.pl -sleep 0 -v $run -n $nevents -sys $system -e $energy -t $type $begin $end
end



