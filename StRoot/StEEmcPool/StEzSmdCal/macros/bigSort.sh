#!/bin/bash
path=outPPV-Z/

#CuCu200
#for run in 60490911 60490912  60490921 60490922 6049126 60491291 60491292  6049130  6049131 60500161 60500162 60500171 60500172 6050018 6050019 60500201 60500202 ; do

#pp200
for run in  61710371  61710372  61710373 61720911 61720912  61720913 61730681 61730682  61730683 61730771 61730772  61730773 ; do
    echo working on run $run ... 
  
    bsub -q star_cas_short -o log.R$run root4star -b rdEztMuSmdCal.C\(${run}\)
#exit
    done

#----------- end of official scrypt ---------
exit

#    .x  hadd.C( $sec )