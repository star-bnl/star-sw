#!/bin/sh
logFile=$1
nBad=$2
mode=$3
echo "EEMC-SPY-SHELL  log=$logFile nBad=$nBad mode=$mode"

#mode: 1=balewski@rcf, 2=eemc@evp,3=operator@evp 
echo "eeSpy.sh : mode=$mode" 
case $mode in
    1)
	pathLog=/star/u/balewski/0x/defaultPanitkinOut/
	pathMacros=StRoot/StEEmcPool/muEztPanitkin/macros/
	break ;;
    3)
	pathLog=/onlineweb/www/eemc2005pplot/
	pathMacros=/home_local/eemc/defaultPanitkinSetup/macros/
	break ;;
    *)
	echo "eeSpy.sh : Internal error! mode=$mode" ; exit 1 ;;
esac

fullLog=$pathLog$logFile

runNo=`grep run $fullLog  |awk '{print $2}'`

echo "  $runNo "

# .... display warning box on the console
if [ $nBad -gt 0 ]; then    
    mailTit="eeSpy $runNo $nBad"
    #  echo display-box
    #wish $pathMacros/eeSpyBox.tcl $logFile  $nBad $pathLog  & 
    mail balewski@iucf.indiana.edu -s "$mailTit" < $fullLog
    exit
    mail relyea@bnl.gov -s " $mailTit" < $fullLog
    mail rfatemi@iucf.indiana.edu -s "$mailTit " < $fullLog
    mail jacobs@iucf.indiana.edu -s "$mailTit " < $fullLog
    mail wissink@iucf.indiana.edu -s "$mailTit " < $fullLog
   # wish $pathMacros/eeSpyBox.tcl -display 130.199.61.166:0.0  $logFile  $nBad $pathLog &

   

   # mail sowinski@iucf.indiana.edu -s "eeSpy nBad=$nBad $logDate " < $fullLog
   #mail msar@iucf.indiana.edu -s "eeSpy nBad=$nBad $logDate " < $fullLog
   #
   # mail jwebb@iucf.indiana.edu -s "eeSpy nBad=$nBad $logDate " < $fullLog
    #wish $pathMacros/eeSpyBox.tcl -display 130.199.60.25:0.0  $logFile  $nBad $pathLog &
    
fi
  echo "EEMC-SPY-SHELL  done"
exit


