#!/bin/sh
nEve=5000
inpDir=/star/data13/reco/pp200/pythia6_205/11_15gev/cdf_a/y2004y/gheisha_on/p05ih/
core="rcf1227"
id1=95
id2=114

njobs=0
id=$id1
while [ $id -le $id2 ]; do
   name=$core\_$id\_4000evts
   echo "name=$name"
   #run interactive:

   #run using LSF
   bsub -q star_cas_big -o$name.log -J$name root4star -b -q rdMu2TrigSimu.C\($nEve,1,1,1,1,2006,1,0,1,\"$name.MuDst.root\",\"$inpDir\"\)

   njobs=$[ $njobs + 1 ]
   id=$[$id+1]	  
done
echo ""
echo "info: $njobs jobs submitted"

