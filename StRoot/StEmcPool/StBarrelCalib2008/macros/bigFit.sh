#!/bin/sh
nChan=-1
njobs=0
id=0
id2=127
while [ $id -le $id2 ]; do
    echo "job=$job id=$id"
    root.exe -b -q doBprsPed3D.C\($id,$nChan\) >&junk/LogCap$id
    id=$[ $id + 1 ]
    njobs=$[ $njobs + 1 ]
done
echo "info: $njobs jobs submitted"
