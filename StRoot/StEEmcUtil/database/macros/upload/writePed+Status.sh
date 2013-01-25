#!/bin/bash
VER=Ver2004d  #don't touch

TM=$1
COM=$2
inpPath=$3
eemcDbPath=$4
FLAVOR="ofl"
nr=0
echo $inpPed
echo $inpMask

exit
for sec in 01 02 03 04 05  06 07 08  09 10 11 12 ; do 
    echo  Flavor = $FLAVOR
    echo  $sec $TM $COM
    echo  $inpPath/ped.sector${sec} 
    echo  $inpPath/stat-${sec} 
    echo ""
    exit 
    
# Execute eemcDb utility (compiled by user) and upload ped and status table #
    $eemcDbPath/eemcDb -s -p ${VER}/sector${sec}/eemcPMTped  -f  $inpPath/ped.sector${sec}  -c "ped, $FLAVOR, $COM" -t "$TM" -F "$FLAVOR" || exit
    $eemcDbPath/eemcDb -s -p ${VER}/sector${sec}/eemcPMTstat -f  $inpPath/stat-${sec}  -c "mask, $FLAVOR, $COM" -t "$TM" -F "$FLAVOR" || exit

     nr=$[$nr+1]
 done
 
echo " "
echo " "
echo " "
echo " TOTAL $nr records loaded "
echo " "
echo " "
echo " "

exit

xxxxxxxxxxxxxx  check ped xxxxxxxxxxx

mysql --host=robinson.star.bnl.gov --port=3306 Calibrations_eemc -e "select comment,elementID,dataID,flavor,beginTime from eemcDbPMTped where BeginTime>'2006-01-01' and  BeginTime<'2007-03-25' and flavor='ofl' and elementID=5 order by beginTime "



xxxxxxxxxxxxxx  check stat xxxxxxxxxxx
mysql --host=robinson.star.bnl.gov --port=3306 Calibrations_eemc -e "select comment,elementID,dataID,flavor,beginTime from eemcDbPMTstat where BeginTime>'2006-01-01' and  BeginTime<'2007-03-25' and flavor='ofl' and elementID=5 order by beginTime "

xxxxxxxxxxxxxx  DELETE stat  for one run xxxxxxxxxxx
mysql --host=robinson.star.bnl.gov --port=3306 Calibrations_eemc -e "delete  from eemcDbPMTped where BeginTime>'2005-01-01' and  BeginTime<'2006-03-25' and flavor='ofl'  and comment like '%R611107499%' order by beginTime "
