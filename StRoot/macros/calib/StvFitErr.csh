#
# Evaluation of hit errors.
# invoking:
# StvFitErr.csh daqfile.daq pp2009 <noPrepass>
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
# these files could be used by Stv.
# file stv.log contains log of Stv
# file fit.log contains log of HitError fitting
#
# All questions to Victor Perev
# Victor Perev Feb 17, 2007
############################################################################
source ${GROUP_DIR}/.starver  .DEV
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

#		Fit Stv errors
touch stv.log


#		Create DB directories
mkdir -p StarDb/Calibrations/tracker/
mkdir -p StarDb/Calibrations/rich/

#		Create link to fiterr.C for aclic
#if ( ! ( -e fiterr.C  ) ) ln -sf $STAR/StRoot/macros/calib/fiterr.C .

@ iter = 0
#		Run prepass
if (${daqFile:e} == "fz") touch fiterrPrepass.DONE
if (${noPrepass}        ) touch fiterrPrepass.DONE

if (!(-e fiterrPrepass.DONE)) then
  rm fit.log stv.log
  touch stv.log
  touch fit.log

  echo '*** Prepass Started *** '
  echo '*** Prepass Started *** '>> stv.log

  root4star -b  <<EOF  >>& stv.log
.L calib/prepass.C
#include <stdlib.h>
int ans =13;
ans =prepass("$daqFile","$Opt");
printf("ptrepass ans=%d\n",ans);
if (ans != 99) exit(13);
printf("exit(0)\n");
exit(0);
EOF
  set myerr = $status
  echo '*** Prepass Ended *** Status=' $myerr
  echo '*** Prepass Ended *** Status=' $myerr>> stv.log
  if ($myerr) goto STVERR
  touch fiterrPrepass.DONE

else

echo '*** Prepass allready DONE (fiterrPrepass.DONE exists)*** '
echo '*** Prepass allready DONE (fiterrPrepass.DONE exists)*** '>> stv.log

endif 


AGAIN:
@ iter = $iter + 1

set myroot_time = 0
if ( -e pulls.root) set myroot_time = `stat -L -c %X pulls.root`
set fitlog_time = 0
if ( -e fit.log) set fitlog_time = `stat -c %X fit.log`

if ($myroot_time > $fitlog_time) goto FIT

if (-e pulls.root) cp pulls.root pulls.root.BAK
echo '*** STV Started *** Iter=' $iter
echo '*** STV Started *** Iter=' $iter >> stv.log

STV:
root.exe -b  <<EOF  >>& stv.log
.L runStv.C
#include <stdlib.h>
const char *star = gSystem->Getenv("STAR");
const char *path = gSystem->Getenv("LD_LIBRARY_PATH");
printf("***  STAR = %s  ***\n",star);
printf("***  PATH = %s  ***\n",path);
int ans =13;
ans =runStv("$daqFile","${Opt}",200);
if (ans != 99) exit(13);
ans =chain->Finish(); 
if (ans) exit(14);
exit(0);
EOF

set myerr = $status
echo '*** STV Ended *** Status=' $myerr
echo '*** STV Ended *** Iter=' $iter >> stv.log
if ($myerr) goto STVERR
if (! -e pulls.root) ln -s *.tags.root pulls.root

FIT:
echo '*** FitErr Started *** Iter=' $iter
##set timstamp = ( `grep 1stEventTimeStamp stv.log` )
##echo $timstamp

touch fit.log

root.exe -b <<EOF  >>& fit.log
#include <stdlib.h>

gSystem->Load("libQuadp.so");
gSystem->Load("StvUtil");

.L StvFitErr.C+
int ans = 13;
ans = StvFitErr();
exit(ans);
.q
EOF
set myerr = $status
echo '*** FitErr Ended *** Status=' $myerr
if ($myerr) goto FITERR

goto AGAIN

#
STVERR: 
echo "STVERR =" $myerr "Iter=" $iter
exit

FITERR: if (myerr==99) goto DONE
        echo "FITERR =" $myerr "Iter=" $iter
exit
DONE:   echo "DONE Iter=" $iter
