#
#	installExe script to create RL99.x STAF executable

# define input area where directories asu,sdd,... are
set INP = $cwd/asps/agi
if ( ! -d $INP ) then
  echo installExe: directory $INP does NOT exist
  exit 1
endif



# define output area where result will be placed
set OUT = $cwd

# define where all Make*.mk are. Here the place is the same as input area
if ( -e $cwd/MakeArch.mk ) then
  setenv STAF_MAKE_HOME $cwd
else
  setenv STAF_MAKE_HOME ${STAR}/mgr
endif


# define STAF SYS area
 setenv STAF_SYS $cwd

# setup stage: create OUTPUT directories and logon file makestaflogon.mk
#  info about INP_DIR and OUT_DIR is saved into this file

gmake -f ${STAF_MAKE_HOME}/MakeExe.mk INP_DIR=${INP} OUT_DIR=${OUT} setup


#real run of makefile. It creates executable


gmake -f ${STAF_MAKE_HOME}/MakeExe.mk INP_DIR=${INP} OUT_DIR=${OUT} Staf



#
#If this is official place remove some redundant directories
#But if it your own place do not do it. Next Make will be much faster

set OffPlace = `echo $OUT | grep /afs/rhic/common/`
if ( ${#OffPlace} ) then
  echo '*** CLEAN UP ***'
  
rm -rf ${OUT}/.@sys/dep/exe
rm -rf ${OUT}/.@sys/obj/exe
rm -rf ${OUT}/tmp
  echo '*** DONE ***'

endif


#
# All comments to perev@bnl.gov

