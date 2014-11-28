#!/bin/sh

#listFull= ~/2012-Wana/run2012_long_trigID.csv


logPath=/star/institutions/mit/balewski/2012-Wsampler-pass2/
#logPath=/star/data05/scratch/balewski/W2012mu-A/
k=0
outPath=/star/data05/scratch/balewski/tmp2009/

read line

lastFill=1234 
nFills=0
# read from stdinp
while [ true ]; do
    read line
    if [ $? -ne 0 ]; then break; fi
    k=$[ $k+1 ]
    #echo "LINE $k IS: " $line
    #if [ $k -gt 10 ] ; then break ; fi
   
    Ffill=`echo $line | cut -f1 -d,`
    fill=`echo $Ffill | cut -f2 -dF`
    Rrun=`echo $line | cut -f3 -d,`
    run=`echo $Rrun | cut -f2 -dR`
    totSec=`echo $line | cut -f5 -d,`

    core=$Ffill\_$Rrun
    #if [  $fill -lt 16668 ]; then  continue; fi
    echo generate txt for $core  nRun=$k ...


#    if [ $run -le 13099000 ]; then  continue; fi
#    grep -ha track4beamLine $logPath/*/log/st_W_$run*.out >inp/globTr_$core.txt
    ls -l inp/globTr_$core.txt
#    if [ $? -ne 0 ]; then continue; fi
    
 

    
    #if [  $fill -gt 16693 ]; then  break; fi

    #echo $lastFill $fill  $Rrun $zeroB
    if [ $lastFill -eq $fill ]; then  
	continue; 
    fi

    if [ $totSec -lt 100 ] ; then echo skip short run $Rrun; continue; fi
    echo new fill $Ffill use run $Rrun scanning log files ...
    #lastFill=$fill; 
    
    #continue

    echo prepared text for core=$core,  fitting ...

    Log=out/$core.log
    time (./mainFitBeamLine3D $core X )>& $Log


    grep "Aborted" $Log
    if [ $? -eq 0  ]; then 
	echo $Rrun fit ABORTED, try next run if exit; lastFill=999; continue
    fi

    grep "#beamLine" $Log

    nFill=$[ $nFill +1 ]

    root -b <<EOF
    .x  pl3DbeamLine.C(1,"$core",1)
    .x  pl3DbeamLine.C(2,"$core",1)
    .x  pl3DbeamLine.C(3,"$core",1)
EOF

    #exit
done

echo total runs=$k  okFill=$nFill 
