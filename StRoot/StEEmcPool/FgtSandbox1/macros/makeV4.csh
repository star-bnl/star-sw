#!/bin/csh -f 

rm pams/geometry/isdmgeo/isdmgeo1.g

mgr/agmlParser.py --file=StarVMC/Geometry/IsdmGeo/IsdmGeo1.xml  --export=Mortran >pams/geometry/isdmgeo/isdmgeo1.g
echo $?
echo 'Isd.xml parsed ......'
sleep 3

rm pams/geometry/fgtdgeo/fgtdgeo2.g
mgr/agmlParser.py --file=StarVMC/Geometry/FgtdGeo/FgtdGeo3.xml --export=Mortran > pams/geometry/fgtdgeo/fgtdgeo2.g
echo $?
echo 'Fgt3.xml parsed ......'
sleep 3

modify FGTDGEO3 FGTDGEO2 pams/geometry/ftgdgeo/fgtdgeo2.g
modify FGAZ FGAS pams/geometry/ftgdgeo/fgtdgeo2.g

cons 

