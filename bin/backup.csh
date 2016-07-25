#! /usr/local/bin/tcsh -f
set dir = $1;
set time = $2;
set scratch = /scratch/zp/fisyak;
cd /afs/cern.ch/asis/$dir ; tar -N$time -zcvf $scratch/${dir}.tz . >& $scratch/${dir}.log &
#endscript
