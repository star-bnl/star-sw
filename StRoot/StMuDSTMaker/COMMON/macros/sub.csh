#! /afs/rhic.bnl.gov/star/users/laue/public/tcsh -f

# StRoot/StMuDstMaker/macros/sub.csh MuDst/centra/ReversedFullField/runs

set inputDir = $1
set outputDir = $2
set dataSet =  st_physics
set maxJobs = 100
set theDB = MuDstSubmitted.db

@ i=0
@ s=0
foreach f (`find $inputDir -name '*.event.root' | grep $dataSet | grep -v BadFile`)
@ i++
  set baseName = `basename $f .event.root`
  set dirName = `dirname $f`  
  set log = $baseName\.log
#  set outputDir = $dirName:s/MuDst/MuDST/
#  set outputDir = $dirName
  set checkfile = $outputDir/$baseName\.MuDst\.root
#  set outfile = /dev/null
  set outfile = test.out
  set time=`date -I`" "`date +%T`  
# $outDir $checkfile

  if (-e $baseName.out) rm $baseName.out  
  if ( 0 ) then  
#  if (-e $checkfile) then
#  if (`grep --count $baseName $theDB` != 0) then 
    echo " *** $i $s   $checkfile file already exist ***"
  else 
    echo " *** $i $s   $checkfile ***"
    set user = `/sbin/fuser -u $f`
    echo $user
    if ($user == "") then
      @ s++
#-o $outfile -e $outfile 
     bsub  -u $USER -q star_cas_short  -L /usr/local/bin/tcsh  -o $outfile -e $outfile root4star -q -b StRoot/StMuDSTMaker/COMMON/macros/StMuDstMaker.C\(100000,\"-\",\"$f:q\",\"$outputDir:q\"\)  >& /dev/null 
     echo $baseName $time >>& $theDB
#      echo "submitted"
    endif
  endif
  if ($s > $maxJobs ) break;  
end 
bjobs
      
