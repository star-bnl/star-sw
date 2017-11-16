#!/bin/sh
#the script to copy back the embedding data to PDSF embedding repository.
#usage: 
#%screen ./copyback.sh [Fset_begin#] [Fset_end#]
# or (if only one fset)
#%screen ./copyback.sh [Fset#] 
#input the PDSF NIM password
#use 'Ctrl+A Ctrl+D' to detach the screen session if the transfer starts normally. 
#memorize the cori node host name, so that login back again to monitor the screen session (with 'screen -ls' and 'screen -r' to find the detached session and bring it back to frontend).
#if the transfer doesn't start, remove screen in the command, and check what is going on.

if [[ ! $HOST =~ "cori" ]] ; then
   echo this code can only be used for Cori!
   exit
fi

if [[ $# -eq 0 || $# -gt 2 ]] ; then
   echo ""
   echo " Usage : $0 [Fset_begin#] [Fset_end#] "
   echo "      or $0 [Fset#] "
   echo ""
   echo ""
   exit
fi

begin=$1
if [ $# -eq 1 ] ; then
   end=$(($1+1))
else
   if [ $1 -gt $2 ] ; then
	echo "wrong parameters! the first fset# should not be larger than the second! "
	exit
   fi
   end=$(($2+1))
fi

cori="$CSCRATCH/embedding"
pdsf="/global/projecta/projectdirs/starprod/embedding"

part=`grep "\-particle" $PWD/preparexmlslr.sh | awk -F"-particle |-mode" '{print $2}'`
particle=`echo $part`
trg=`grep "\-trg" $PWD/preparexmlslr.sh | awk -F"-trg |-production" '{print $2}'`
trgset=`echo $trg`
reqid=`grep "\-r" $PWD/preparexmlslr.sh | awk '{print $2}'`

srcdirs=""
dest=$pdsf/$trgset

for (( ifset=$begin ; $ifset - $end ; ifset++ ))
do

   fsetp=$cori/$trgset/${particle}_${ifset}_${reqid}
   destp=$pdsf/$trgset/${particle}_${ifset}_${reqid}
   echo "CORI: $fsetp"
   echo "--->: $destp"
   if [ ! -d "$fsetp" ] ; then
	echo "CAUTION:"
	echo "$fsetp can not be found at Cori! please double check!"
	exit
   fi
   if [ -d "$destp" ] ; then
	echo "CAUTION:"
	echo "$destp is already at PDSF! please double check!"
	exit
   fi

   if [ "$ifset" -eq $begin ] ; then
	srcdirs="$fsetp"
   else
	srcdirs="$srcdirs $fsetp"
   fi
done

#echo $srcdirs
scp -rp $srcdirs $USER@dtn01.nersc.gov:$dest/

