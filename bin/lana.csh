#!/bin/tcsh -f
foreach f ( `ls -1d ../ClnoW/*.event.root` )
   set b = `basename $f .event.root`;
   set r = ${b}.event.laser.root;
   if ( ! -r ${r}) root.exe -q -b  'lana.C("'${f}'")' >& ${b}.log
end
