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
echo Use makefiles from $STAF_MAKE_HOME

# define STAF SYS area
 setenv STAF_SYS $cwd

# setup stage: create OUTPUT directories and logon file makestaflogon.mk
#  info about INP_DIR and OUT_DIR is saved into this file

gmake -f ${STAF_MAKE_HOME}/MakeExe.mk INP_DIR=${INP} OUT_DIR=${OUT} setup


#real run of makefile. It creates executable


gmake -f ${STAF_MAKE_HOME}/MakeExe.mk INP_DIR=${INP} OUT_DIR=${OUT} Staf



#
  echo '*** DONE ***'


#
# All comments to perev@bnl.gov

