#
#	installExe script to create  ROOT executable

# define input area where directories 
set INP = $cwd/asps/rexe
if ( ! -d $INP ) then
  echo installExe: directory $INP does NOT exist
  exit 1
endif



# define output area where result will be placed
set OUT = $cwd

# define where all Make*.mk are. Here the place is the same as input area
if ( ! ${?STAF_MAKE_HOME} ) then
  if ( -e $cwd/MakeRexe.mk ) then
    setenv STAF_MAKE_HOME $cwd
  endif
endif
if ( ! ${?STAF_MAKE_HOME} ) then
  if ( -e $cwd/mgr/MakeRexe.mk ) then
    setenv STAF_MAKE_HOME $cwd/mgr
  endif
endif
if ( ! ${?STAF_MAKE_HOME} ) then
  setenv STAF_MAKE_HOME ${STAR}/mgr
endif

echo Use makefiles from $STAF_MAKE_HOME

# define STAF SYS area
 setenv STAF_SYS $cwd

# setup stage: create OUTPUT directories and logon file makestaflogon.mk
#  info about INP_DIR and OUT_DIR is saved into this file

gmake -f ${STAF_MAKE_HOME}/MakeRexe.mk INP_DIR=${INP} OUT_DIR=${OUT} setup


#real run of makefile. It creates executable


gmake -f ${STAF_MAKE_HOME}/MakeRexe.mk INP_DIR=${INP} OUT_DIR=${OUT} 



#
  echo '*** DONE ***'


#
# All comments to perev@bnl.gov

