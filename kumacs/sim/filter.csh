#! /usr/local/bin/tcsh -f
#  save file counter, first and last file numbers in N
set dir=`basename $PWD`
set list=`ls *.*.fz`
rm -f N;  ls *.*.fz          | wc -l      >  N
echo $list[1]      | awk -F. '{print $2}' | sed -e 's/_.*//' >> N
echo $list[$#list] | awk -F. '{print $2}' | sed -e 's/_.*//' >> N
echo $list[1]      | awk -F. '{print $1}' >> N
echo $dir                                 >> N
rm -f $dir.log paw.metafile detm.rz detmsys.def
~nevski/gstdev/bin/staf -w 1 -g 30 -b $STAR/kumacs/sim/filter > $dir.log 
rm -f paw.metafile detm.rz detmsys.def
