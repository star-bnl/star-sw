#!/bin/bash
VER=Ver2004d

###### Run 12 #######
#sim Ideal at   -> unixTS=1323475200
#ofl Startup at -> unixTS=1324339200

TM=1324339200 #unix time stamp 

COM="Initialize 2012 ideal EEMC P,Q,R,U,V gains, 23000ch/GeV, Justin"
inpPath="./idealGains"
FLAVOR="sim"

#COM="2012 Startup, use final 2006 gains, Justin"
#inpPath="/star/u/stevens4/runList2012/db-gains/oflStartup"
#FLAVOR="ofl"

eemcDbPath=yourPath/StRoot/StEEmcUtil/database/macros/upload/src/

for sec in 01 02 03 04 05 06 07 08 09 10 11 12 ; do

  echo $sec $TM $FLAVOR
  echo $COM
  echo $inpPath/sect${sec}PIXgains.dat
  exit

  #need to chose different flavor for DB entry use -F option

  $eemcDbPath/eemcDb  -s -p ${VER}/sector${sec}/eemcPIXcal  -f $inpPath/sect${sec}PIXgains.dat  -c "$COM" -t "$TM" -F "$FLAVOR"   || exit

done

exit


mysql --host=robinson.star.bnl.gov --port=3306 Calibrations_eemc -e "select comment,elementID,dataID,flavor,beginTime from eemcDbPIXcal where BeginTime>'2004-01-27'"
