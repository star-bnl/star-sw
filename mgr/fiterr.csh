#
# Evaluation of hit errors.
# invoking:
# fiterr.csh daqfile.daq pp2009 <noPrepass>
#
# daqfile must be the fullname of daq file, including full path
# 
#  At the beginning empty working directory must be created
#  At the end in subdirectory StarDb/Calibrations/tracker/
#  will be created files:
# StarDb/Calibrations/tracker/ssdHitError.20050101.235959.C
# StarDb/Calibrations/tracker/svtHitError.20050101.235959.C
# StarDb/Calibrations/tracker/tpcInnerHitError.20050101.235959.C
# StarDb/Calibrations/tracker/tpcOuterHitError.20050101.235959.C
#
# these files could be used by Sti.
# file sti.log contains log of Sti
# file fit.log contains log of HitError fitting
#
# All questions to Victor Perev
# Victor Perev Feb 17, 2007
############################################################################
source ${GROUP_DIR}/.starver  DEV
#source ${GROUP_DIR}/.starver  .DEV
echo STAR = $STAR $ROOTSYS

#		Check input file
if ( ! ${%1} ) then
  echo "fiterr:  ***No input daq file ***"
  exit 13
endif

set daqFile = ${1}
if (!( -e ${daqFile} )) then
  echo "fiterr:  ***Daq file Not FOUND // ${daqFile} ***"
  exit 13
endif
echo "fiterr: Input daq file=${daqFile}"

set Opt = ${2}
set myOpt = ${3}
set noPrepass = 0
set x = (`echo $myOpt | grep -i noPrepass`) 
if ($#x) set noPrepass = 1




#		FloatPointException OFF
setenv STARFPE NO

#		Fit Sti errors
touch sti.log


#		Create DB directories
mkdir -p StarDb/Calibrations/tracker/
mkdir -p StarDb/Calibrations/rich/

#		Create link to fiterr.C for aclic
if ( ! ( -e fiterr.C  ) ) ln -sf $STAR/StRoot/macros/calib/fiterr.C .

@ iter = 0
#		Run prepass
if (${daqFile:e} == "fz") touch fiterrPrepass.DONE
if (${noPrepass}        ) touch fiterrPrepass.DONE

if (!(-e fiterrPrepass.DONE)) then
rm fit.log sti.log
touch sti.log
touch fit.log
setenv STARNODELETE YES

echo '*** Prepass Started *** '
echo '*** Prepass Started *** '>> sti.log

root4star -b  <<EOF  >>& sti.log
.L calib/prepass.C
#include <stdlib.h>
int ans =13;
ans =prepass("$daqFile","$Opt");
printf("ptrepass ans=%d\n",ans);\n");
if (ans != 99) exit(13);
printf("exit(0)\n");
exit(0);
EOF
set myerr = $status
echo '*** Prepass Ended *** Status=' $myerr
echo '*** Prepass Ended *** Status=' $myerr>> sti.log
if ($myerr) goto STIERR
touch fiterrPrepass.DONE

else

echo '*** Prepass allready DONE (fiterrPrepass.DONE exists)*** '
echo '*** Prepass allready DONE (fiterrPrepass.DONE exists)*** '>> sti.log

endif 


AGAIN:
@ iter = $iter + 1
if ( (-e pulls.root) && !(-e fit.log) ) goto FIT

if (-e pulls.root) mv pulls.root pulls.root.BAK
echo '*** STI Started *** Iter=' $iter
echo '*** STI Started *** Iter=' $iter >> sti.log

STI:
root4star -b  <<EOF  >>& sti.log
.L calib/fiterrSti.C
#include <stdlib.h>
int ans =13;
ans =runsti("$daqFile",999,"${Opt}");
if (ans != 99) exit(13);
ans =chain->Finish(); 
//		Clear all ROOT lists to avoid deleting at the end
.x ClearRoot.C

if (ans) exit(14);
exit(0);
EOF
set myerr = $status
echo '*** STI Ended *** Status=' $myerr
echo '*** STI Ended *** Iter=' $iter >> sti.log
if ($myerr) goto STIERR

FIT:
echo '*** FitErr Started *** Iter=' $iter
set timstamp = ( `grep 1stEventTimeStamp sti.log` )
echo $timstamp

touch fit.log
echo STAR = $STAR $ROOTSYS
echo LIBP = $LD_LIBRARY_PATH
root.exe -b <<EOF  >>& fit.log
#include <stdlib.h>
.L fiterr.C+
int ans = 13;
ans = fiterr("U ${timstamp[2]}");
exit(ans);
.q
EOF
set myerr = $status
echo '*** FitErr Ended *** Status=' $myerr
if ($myerr) goto FITERR

goto AGAIN

#
STIERR: 
echo "STIERR =" $myerr "Iter=" $iter
exit

FITERR: if (myerr==99) goto DONE
        echo "FITERR =" $myerr "Iter=" $iter
exit
DONE:   echo "DONE Iter=" $iter
