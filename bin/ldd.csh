#! /bin/csh -f 
set list=`ls $ROOTSYS/lib/*so`
foreach f ($list)
   echo $f
   ldd $f | grep stdc++
end
#e o d
