#!/bin/csh -x
set run=$1
set pathData=$2
set pathHisto=$3
set pathTmp=$4
set pathWeb=$5

set log1=$pathTmp\log.sortTrigData
set log2=$pathTmp\log.plotTrigData
#echo $log2
stardev
echo $run
rm -f  $pathTmp/*

root4star -b <<EOF  >& $log1
    .x rdTrg2Panitkin.C(100000,"$run","$pathData","$pathHisto")
    .q
EOF

#exit

starold
root -b << EOF2  >& $log2
  .x plPanitkin.C("$pathHisto$run.hist.root")
   eeJpQa(fd,pd1,0);
   pr("$pathTmp/fig-0jpfreq");
   plAllDsm("$pathTmp"); 
   .q
EOF2

cat $pathTmp/*ps |ps2pdf - $pathTmp/$run.pdf
mv $pathTmp/$run.pdf $pathWeb/pdf
exit
