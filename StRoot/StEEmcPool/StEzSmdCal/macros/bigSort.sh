#!/bin/bash
path=outPPV-Z/

for run in 60490911 60490912  60490921 60490922 6049126 60491291 60491292  6049130  6049131 60500161 60500162 60500171 60500172 6050018 6050019 60500201 60500202 ; do
#for run in 6050018 6050019; do
   # echo working on run $run ... 
  
    bsub -q star_cas -o log.R$run root4star -b rdEztMuSmdCal.C\(${run}\)
#exit
    done

#----------- end of official scrypt ---------
exit

#    .x  hadd.C( $sec )