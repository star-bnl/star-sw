#
#	installRL script to create RL99.x STAF libraries

# define input area where directories asu,sdd,... are
set INP = $cwd/asps/staf
echo INP_DIR = $INP
if ( ! -d $INP) then
  echo *** $INP does not exist ***
  exit
endif

# define output area where result will be placed
set OUT = $cwd
echo OUT_DIR = $OUT

# define where all Make*.mk are. Here the place is the same as input area
#setenv STAF_MAKE_HOME ${INP}
if ( -e $cwd/MakeArch.mk ) then
  setenv STAF_MAKE_HOME $cwd
else
  setenv STAF_MAKE_HOME ${STAR}/mgr
endif

#
# setup stage: create OUTPUT directories and logon file makestaflogon.mk
#  info about INP_DIR and OUT_DIR is saved into this file

gmake -f ${STAF_MAKE_HOME}/MakeStaf.mk INP_DIR=${INP} OUT_DIR=${OUT} setup


#real run of makefile. It creates all the libraries ans all executables
# if you want only one ASP , "asu" for instance, add in line PKG=asu
# if you add "libs" only libraries will be created (no executables)
# if you add "exes" only executables will be created (no libraries)


gmake -f ${STAF_MAKE_HOME}/MakeStaf.mk 


# Makefile does not touch input area
# But in current environment should be "inc" directory on the same level
# as ASPs. So this directory is making by link to created "inc" directory 
#
  echo '*** DONE ***'

#
# All comments to perev@bnl.gov

