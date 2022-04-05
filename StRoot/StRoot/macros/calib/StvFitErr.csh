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

set myFile = {$daqFile:s/@//}
setenv STV_FILE_LIST $myFile

if (!( -e ${myFile} )) then
  echo "fiterr:  ***Daq file Not FOUND // ${daqFile} ***"
  exit 13
endif
echo "fiterr: Input daq file=${daqFile}"
set nam = ${daqFile:t}
set nam = ${nam:r}
set nam = ${nam:s/@//}


set Opt = ${2}
setenv STV_OPTS $Opt
set myOpt = ${3}
set nFETracks = ${4};
if ( ! $nFETracks ) set nFETracks = 1000000;
echo "fiterr: *** $nFETracks tracks requested"
#		FloatPointException OFF
setenv STARFPE NO

#		Fit Stv errors


#		Create DB directories
mkdir -p StarDb/Calibrations/tracker/
mkdir -p StarDb/Calibrations/rich/

#		Create link to fiterr.C for aclic
#if ( ! ( -e fiterr.C  ) ) ln -sf $STAR/StRoot/macros/calib/fiterr.C .

@ iter = 0

AGAIN:
@ iter = $iter + 1

set myroot_time = 0
set fitlog_time = 0
if ( -e fit.log) set fitlog_time = `stat -c %X fit.log`
if ( -e stv.log) set myroot_time = `stat -c %X stv.log`
touch fit.log
touch stv.log
if ($myroot_time > $fitlog_time) goto FIT

echo '*** STV Started *** Iter=' $iter
echo '*** STV Started *** Iter=' $iter >> stv.log

## STV:
## gdb root.exe   <<EOF  >>& stv.log
## run -b 
## .L runStv.C
## #include <stdlib.h>
## const char *star = gSystem->Getenv("STAR");
## const char *path = gSystem->Getenv("LD_LIBRARY_PATH");
## printf("***  STAR = %s  ***\n",star);
## printf("***  PATH = %s  ***\n",path);
## int ans =13;
## ans =runStv("$daqFile","${Opt}",0,11);
## printf("*** runStv=%d\n ***",ans);
## ans =13;
## chain->SetAttr("fiterr",$nFETracks,"Stv");
## chain->SetAttr("HitLoadOpt"     ,0,"Stv");
## chain->SetAttr("useTracker"     ,1,"Stv");             
## chain->SetAttr("useVertexFinder",0,"Stv");             
## chain->SetAttr("useEventFiller" ,1,"Stv");            
## chain->SetAttr("makePulls"      ,1,"Stv");
## ans = chain->EventLoop(4000);
## bt
## printf("*** EventLoop=%d ***\n",ans);
## if (ans > 2) exit(13);
## ans =chain->Finish(); 
## if (ans) exit(14);
## exit(0);
## EOF
##########################
runCondor.csh
##########################
set myerr = $status
if ($myerr) goto STVERR
echo '*** STV Ended *** Status=' $myerr
echo '*** STV Ended *** Iter=' $iter >> stv.log
touch stv.log
if ($myerr) goto STVERR

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
ans = StvFitErr("condor/*.tags.root");
exit(ans);
.q
EOF
set myerr = $status
echo '*** FitErr Ended *** Status=' $myerr
if ($myerr) goto FITERR
if ( $iter > 9 ) goto MANY
goto AGAIN

#
STVERR: 
echo "STVERR =" $myerr "Iter=" $iter
exit

FITERR: if (myerr==99) goto DONE
        echo "FITERR =" $myerr "Iter=" $iter
exit
DONE:   echo "DONE Iter=" $iter
exit
MANY:   echo "TOO MANY Iters=" $iter
exit
