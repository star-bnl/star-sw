#! /usr/local/bin/tcsh -f
#  save file counter, first and last file numbers in N
rm -f paw.metafile detm.rz detmsys.def
set list=`ls *.*.fz`
rm -f N;  ls *.*.fz          | wc -l      >  N
echo $list[1]      | awk -F. '{print $2}' >> N
echo $list[$#list] | awk -F. '{print $2}' >> N
echo $list[1]      | awk -F. '{print $1}' >> N
echo `basename $PWD`                      >> N
staf -w 1 -g 20 -b $STAR/kumacs/sim/filter > `basename $PWD`.log
rm -f paw.metafile detm.rz detmsys.def
