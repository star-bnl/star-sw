#!/bin/bash
#path=xxx
queue=star_cas_short
#queue=star_cas_mem

for sec in 1 2 3 4 5 6 7 8 9 10 11 12 ; do
#for sec in 2  ; do
    echo working on sector $sec ...
    bsub -q $queue -o log.Hadd$sec root4star -b hadd.C\(${sec}\)
    #root4star -b -q hadd.C\(${sec}\)
   
   done
   exit	
       root.exe -b >&log.M$sec <<EOF 
   .x  doSmdGains.C( $sec , 0 )
   .x  doSmdGains.C( $sec , 1 )
    .q
EOF
#exit
    done
#exit
    cat Cmpv*ps | ps2pdf - Cmpv1-12.pdf
    mv Cmpv*ps old
    cat mpv*ps | ps2pdf - mpv1-12.pdf
    mv mpv*ps old
    mv *pdf /star/u/balewski/WWW-E/calibration/run5/absMipCal/iter2-out-absSmd_05MeVthr/

exit
================================================
    cat *$sec\U*ps | ps2pdf - sect$sec\U.pdf
    cat *$sec\V*ps | ps2pdf - sect$sec\V.pdf
    mv *pdf /star/u/balewski/WWW-E/calibration/run5/absMipCal/iter0-out-slopes



#    .x  hadd.C( $sec )