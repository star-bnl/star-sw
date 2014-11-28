#!/bin/sh

#............. FULL eta range , varied R ...........
njobs=0
delEta=3.
etaMin=0.
nEve=3000
zVer=0
for rMax in  55 12   ; do
   core=set$njobs
   time starsim -W 0 -b mat.kumac $nEve $rMax $etaMin $delEta $core $zVer
   h2root radlen_fgt_$core.rz 
   mv radlen_fgt_$core\.rz radlen_fgt
   mv radlen_fgt_$core\.root radlen_fgt
   njobs=$[ $njobs + 1 ]
done
echo "info: $njobs jobs submitted AAA "

exit
#.............  fixed R, varied eta range, takes much longer ...
njobs=0
delEta=0.5
etaMin=0.
nEve=100000
zVer=0
rMax=12
  for etaMin in  0. 0.5 1.0 1.5 2.   ; do
   core=R$rMax\set$njobs
   time starsim -W 0 -b mat.kumac $nEve $rMax $etaMin $delEta $core $zVer
   h2root radlen_fgt_$core.rz 
   mv radlen_fgt_$core\.rz radlen_fgt
   mv radlen_fgt_$core\.root radlen_fgt
   njobs=$[ $njobs + 1 ]
done
echo "info: $njobs jobs submitted AAA "


exit

