#!/bin/tcsh -f
foreach f ( `ls -1d *Fit7.root` )
set b = `basename $f .Fit7.root`; root.exe -q -b 'makeAvLaserHits.C("'${f}'")' >& ${b}Av.log
end
# eod
