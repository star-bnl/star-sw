#! /bin/tcsh -f
set opt = "_debug"
if ($?NODEBUG) set opt = "_opt";
foreach d ( 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23)
 if (! -d $d) mkdir $d; 
 cd $d; 
 $ROOTSYS/Build/test/stress 10000 >& ${d}_`date +%d%b%y`_${STAR_HOST_SYS}_${opt}.log &
 cd -;
end
# EOD 
