#!/bin/sh
nEve=4000
id1=1
id2=60

njobs=0
id=$id1
while [ $id -le $id2 ]; do
   bsub_gstar_bfc.sh   $nEve $id

   njobs=$[ $njobs + 1 ]
   id=$[$id+1]	  
done
echo ""
echo "info: $njobs jobs submitted"

