#  $Log: star_login.csh,v $
#  Revision 1.2  1998/01/01 03:28:13  fisyak
#  New make.kumac
#
#  Revision 1.1.1.1  1997/12/31 14:35:23  fisyak
#
#             Last modification $Date: 1998/01/01 03:28:13 $ 
#!/usr/bin/csh
# star_login.csh
#------------------------------------------------------------------#
# This script will set up the STAR enviroment.                     #
# It should be "sourced" from your ~/.tcshrc or ~/.cshrc           #
#------------------------------------------------------------------#
umask 002
set ECHO = 1
#if (($?STAR_LIB == 1) || ($?prompt == 0)) set ECHO = 0 

#
#       echo STAR logo 
#
if ($ECHO) then
cat /afs/rhic/star/login/logo 
endif

#
#       determine the processor architecture...
#
########################################################################
set hostname_command = "hostname"
#
# Setting some STAR variables
#
########################################################################
setenv STAR_DIR  /afs/rhic/star/packages;        if ($ECHO) echo   "Setting up STAR_DIR  = $STAR_DIR"
setenv STAR_ROOT $STAR_DIR/dev;           if ($ECHO) echo   "Setting up STAR_ROOT = $STAR_ROOT"
source $STAR_ROOT/mgr/STAR_SYS;    
setenv STAR_LIB $STAR_ROOT/lib/$STAR_SYS; if ($ECHO) echo   "Setting up STAR_LIB  = $STAR_LIB"
setenv STAR_BIN $STAR_ROOT/bin/$STAR_SYS; if ($ECHO) echo   "Setting up STAR_BIN  = $STAR_BIN"
setenv STAR_PAMS $STAR_ROOT/pams;         if ($ECHO) echo   "Setting up STAR_PAMS = $STAR_PAMS"
setenv CVSROOT $STAR_DIR/repository;      if ($ECHO) echo   "Setting up CVSROOT   = $CVSROOT"
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
#  if ( "$STAR_LEVEL" == "SL97a" ) setenv CERN_LEVEL 96a
#  if ( "$STAR_LEVEL" == "SL96b" ) setenv CERN_LEVEL 95a
# Currently, Sun/Solaris uses 95a for both:
#  if ( "$HOSTTYPE" == "sun4" ) setenv CERN_LEVEL 95a
endif
if ($?CERN == 0) then 
    if ($ECHO) echo -n "Setting up CERN $CERN_LEVEL : "
    setenv CERN /afs/bnl.gov/cern/$STAR_SYS/cern
    setenv MGRTMP $CERN/$CERN_LEVEL/mgr
    if ( -e $MGRTMP/plienv.csh ) then
	# Try to detect if plienv.csh is already executed
	if ( (`echo $PATH | awk '{print index($0,"/cern/pro/bin")}' `) == 0 ) then
        	if ($ECHO) echo "Executing $MGRTMP/plienv.csh..."
        	source $MGRTMP/plienv.csh
	endif
    else
        if ($ECHO) echo "$MGRTMP/plienv.csh not found, CERN cernlib setup."
        setenv CERN /afs/cern.ch/asis/$STAR_SYS/cern
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
if ($?SLM_DIR == 0) setenv SLM_DIR $STAR_ROOT/mgr
#
# set library path. Try to detect if STAR_LD_LIBRARY_PATH already included
#
###########################################################################
#
# set manpath. Try to detect if the STAR_MANPATH already included 
#
###########################################################################
set STAR_MANPATH = "$STAR_ROOT/man:/usr/afsws/man:/afs/rhic/local/man:/usr/local/man:/usr/man"
if ($?MANPATH == 0) setenv MANPATH "$STAR_MANPATH"
if ( (`echo $MANPATH | awk '{print index($0,"/afs/rhic/star/man")}' `) == 0 ) then
        setenv MANPATH "$STAR_MANPATH":"$MANPATH"
endif
unset STAR_MANPATH

#
# set path. Try to detect if the STAR_PATH already included
#
###########################################################################
set STAR_PATH = "/usr/afsws/bin:/usr/afsws/etc:/opt/rhic/bin:/afs/rhic/local/bin:/usr/local/bin:$STAR_ROOT/mgr:$STAR_BIN"
# check for /usr/ccs/bin/ld
if ( -e /usr/ccs/bin/ld ) set STAR_PATH = ( $STAR_PATH':'/usr/ccs/bin /usr/ccs/lib )
if ( (`echo $PATH | awk '{print index($0,"/afs/rhic/star/bin")}' `) == 0 ) then
        setenv PATH "$STAR_PATH":"$PATH"
endif
unset STAR_PATH
unset ECHO
if ( -e $STAR_ROOT/mgr/star_init.csh) source $STAR_ROOT/mgr/star_init.csh
alias makes "make -f $STAR_ROOT/mgr/Makefile"


