#!/bin/csh
#  $Log: login_star.csh,v $
#  Revision 1.1  1998/01/31 23:32:53  fisyak
#  New Environment variables
#
#  Revision 1.3  1998/01/30 12:42:16  fisyak
#  Save changes before moving to SL97b
#
#  Revision 1.2  1998/01/01 03:28:13  fisyak
#  New make.kumac
#
#  Revision 1.1.1.1  1997/12/31 14:35:23  fisyak
#
#             Last modification $Date: 1998/01/31 23:32:53 $ 
# login_star.csh
#------------------------------------------------------------------#
# This script will set up the STAR enviroment.                     #
# It should be "sourced" from your ~/.tcshrc or ~/.cshrc           #
#------------------------------------------------------------------#
umask 002
set ECHO = 1
if ($?LEVEL_STAR == 0) setenv LEVEL_STAR dev
if (($?ROOT_STAR == 1) || ($?prompt == 0)) set ECHO = 0 
#
#       determine the processor architecture...
#
########################################################################
set hostname_command = "hostname"
#
# Setting some STAR variables
#
########################################################################
                                                  if ($ECHO) cat    /afs/rhic/star/login/logo 
setenv DIR_STAR  /afs/rhic/star/packages;         if ($ECHO) echo   "Setting up DIR_STAR  = $DIR_STAR"
setenv VERSION_STAR `ls -l $DIR_STAR | grep "${LEVEL_STAR} ->" |cut -f2 -d">"`                                                 
setenv ROOT_STAR $DIR_STAR/dev;                   if ($ECHO) echo   "Setting up ROOT_STAR = $ROOT_STAR"
source $ROOT_STAR/mgr/SYS_STAR;    
setenv LIB_STAR  $ROOT_STAR/lib/${SYS_HOST_STAR}; if ($ECHO) echo   "Setting up LIB_STAR  = $LIB_STAR"
setenv BIN_STAR  $ROOT_STAR/bin/${SYS_HOST_STAR}; if ($ECHO) echo   "Setting up BIN_STAR  = $BIN_STAR"
setenv PAMS_STAR $ROOT_STAR/pams;                 if ($ECHO) echo   "Setting up PAMS_STAR = $PAMS_STAR"
setenv CVSROOT   $DIR_STAR/repository;            if ($ECHO) echo   "Setting up CVSROOT   = $CVSROOT"
#setenv STAR_REF  /afs/rhic/star/starlib/ref;      if ($ECHO) echo   "Setting up STAR_REF  = $STAR_REF"
#
# Setting WWW STAR homepage
#
########################################################################
setenv WWW_HOME "http://www.rhic.bnl.gov/STAR/star.html"
#
#       if CERN setup is not done, then do it...
#
########################################################################
if ($?CERN == 1) unsetenv CERN
if ($?CERN_LEVEL == 0) then
  setenv CERN_LEVEL pro
endif
if ($?CERN == 0) then 
    if ($ECHO) echo  "Setting up CERN CERN_LEVEL = $CERN_LEVEL "
    setenv CERN /afs/bnl.gov/cern/$SYS_STAR/cern
    setenv MGRTMP $CERN/$CERN_LEVEL/mgr
    if ( -e $MGRTMP/plienv.csh ) then
	# Try to detect if plienv.csh is already executed
	if ( (`echo $PATH | awk '{print index($0,"/cern/pro/bin")}' `) == 0 ) then
        	if ($ECHO) echo "Executing $MGRTMP/plienv.csh..."
        	source $MGRTMP/plienv.csh
	endif
    else
        if ($ECHO) echo "$MGRTMP/plienv.csh not found, CERN cernlib setup."
        setenv CERN /afs/cern.ch/asis/$SYS_STAR/cern
        if ( -e $CERN/NEW ) then
          setenv CERN_LEVEL NEW
        else if (-e /$CERN/98 ) then
          setenv CERN_LEVEL 98
        else if (-e /$CERN/97a) then
          setenv CERN_LEVEL 97a
        else
          setenv CERN_LEVEL 96b
        endif
        setenv CERN_ROOT $CERN/$CERN_LEVEL
#       setenv PATH `dropit cern`
        setenv PATH "$CERN_ROOT/mgr:$CERN_ROOT/bin:$PATH"
#        if ($ECHO) echo "$MGRTMP/plienv.csh not found, no CERN setup."
    endif
endif

#
# set path for STAR make utility
##########################################################################
if ($?MGR_STAR == 0) setenv MGR_STAR $ROOT_STAR/mgr
#
# set library path. Try to detect if STAR_LD_LIBRARY_PATH already included
#
###########################################################################
#
# set manpath. Try to detect if the MANPATH_STAR already included 
#
###########################################################################
set MANPATH_STAR = "$ROOT_STAR/man:/usr/afsws/man:/afs/rhic/local/man:/usr/local/man:/usr/man"
if ($?MANPATH == 0) setenv MANPATH "$MANPATH_STAR"
if ( (`echo $MANPATH | awk '{print index($0,"/afs/rhic/star/man")}' `) == 0 ) then
        setenv MANPATH "$MANPATH_STAR":"$MANPATH"
endif
unset MANPATH_STAR

#
# set path. Try to detect if the STAR_PATH already included
#
###########################################################################
set STAR_PATH = "/usr/afsws/bin:/usr/afsws/etc:/opt/rhic/bin:/afs/rhic/local/bin:/usr/local/bin:$ROOT_STAR/mgr:$BIN_STAR"
# check for /usr/ccs/bin/ld
if ( -e /usr/ccs/bin/ld ) set STAR_PATH = ( $STAR_PATH':'/usr/ccs/bin /usr/ccs/lib )
if ( (`echo $PATH | awk '{print index($0,"/afs/rhic/star/bin")}' `) == 0 ) then
        setenv PATH "$STAR_PATH":"$PATH"
endif
unset STAR_PATH
if ( -e $ROOT_STAR/mgr/init_star.csh) source $ROOT_STAR/mgr/init_star.csh
if ($ECHO) echo   "STAR library version "$VERSION_STAR" has been initiated"
if ($ECHO) echo   "with `which staf++`"
unset ECHO
alias makes "make -f $ROOT_STAR/mgr/Makefile"
alias maken "make -f $ROOT_STAR/mgr/Makefile.new MAKEFILE=$ROOT_STAR/mgr/Makefile.new"


