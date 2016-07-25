#!/bin/csh -f
# more tmp.tmp
 set ccLIST = `ls $STAR/StEvent/*.hh`
 foreach file ($ccLIST)
   echo $file
   set fil2 = $STAR/StRoot/StRootEvent/`basename $file .hh`.h
   if (-f $fil2) then
     diff $file $fil2
 end
#end
