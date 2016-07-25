#! /usr/local/bin/tcsh -f
if ($#argv != 3) then
   echo Usage : $0 queue\(at_cas\) user\(fisyak\) date_range\(09/30,10/4\)
   echo          bhist.csh at_cas fisyak '09/30,10/4'
   exit
else 
/usr/local/lsf/bin/bhist -a -w  -n 0 -q $1 -u $2 -S $3
endif
# e o d
