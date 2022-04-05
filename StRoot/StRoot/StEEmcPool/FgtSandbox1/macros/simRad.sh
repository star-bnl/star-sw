#!/bin/sh
#............. FULL eta range ...........
njobs=0
delEta=3.
etaMin=0.
nEve=10000
zVer=0
for rMax in 37 55   ; do
   core=set$njobs
   time starsim -W 0 -b mat.kumac $nEve $rMax $etaMin $delEta $core $zVer
   h2root radlen_fgt_$core.rz 
   mv radlen_fgt_$core\.rz radlen_fgt
   mv radlen_fgt_$core\.root radlen_fgt
   njobs=$[ $njobs + 1 ]
done
echo "info: $njobs jobs submitted AAA "

