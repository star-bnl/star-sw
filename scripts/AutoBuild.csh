#!/bin/csh
#
# Wrapper to autobuild
# J.Lauret 2001 - 2011
#
# Current target for this script
#
# Linux based targets:
#   Linux61
#   Linux72    build a LinuxXX AutoBuild report (one version)
#   Linux80
#   Linux9
#   SL3
#   SL302
#   SL305
#   SL44
#   SL53
#   SL64
#   new        a generic target for new systems
#   
#              <--- all of those targets were consolidated into one block
#
#   64bits     build 64 bits verison of dev, whatever the OS
#   cal  [xx]  build "cal" version - xx can be other args like "gcc [v]"
#   eval [xx]  build "eval" version - xx can be other args like "gcc [v]"
#
#   gcc [v]    switch to some specific version of gcc
#   icc [v]    Builds with icc
#
#   Insure     Builds Insure++ compilation
#   gprof      Builds with gprof options
# 
#   inter      Builds as per the default target but do not perform
#              post compilation tasks (and do not send Email if
#              failure) and do NOT perform cvs co either (-s -i)
#   silent     Default mode + -s (no EMail at the end)
#
# Targets for Other platforms:
#   Solaris    Ditto for Solaris (does cache cleaning)
#   du         Digital Unix using hack space in 'cal' (dev only)
#
# Miscellaneous targets
#   Clean      Runs CleanLibs under the current OS and keeps 2
#              versions.
#
#
# Default is to run on the current platform both optimized and
# non optimized. The
#
#

# Grab it from env
if ( ! $?AFS_RHIC ) setenv AFS_RHIC /afs/rhic.bnl.gov

# In case of token failure, send an Email to
set EMAIL="jeromel@bnl.gov,didenko@bnl.gov"

# Path where to find the damned scripts.
set SCRIPTD=$AFS_RHIC/star/packages/scripts

# Loading of the star environment etc ...
setenv GROUP_DIR $AFS_RHIC/star/group
if ( -r  $GROUP_DIR/star_login.csh ) then
	source $GROUP_DIR/star_login.csh

	# The extra sed is because Solaris does not like the brakets
	# in the string, although we doubel quote it.
	if ( -e $HOME/bin/token.csh) then
	    $HOME/bin/token.csh
	    set STATUS=$status
	else
	    set STATUS=1
	endif

	if ($STATUS != 0) then
	    if ( ! -e /tmp/AutoBuild.info) then
		/bin/date >/tmp/AutoBuild.info
	    endif
	    echo "There is no token on `/bin/hostname` for `/usr/bin/id`" >>/tmp/AutoBuild.info
	    tokens >>/tmp/AutoBuild.info
	    /bin/cat /tmp/AutoBuild.info | mail -s "Token on `/bin/hostname`" $EMAIL
	else
	    # Check presence of a log directory
	    if( ! -d $HOME/log) then
		/bin/mkdir $HOME/log
	    endif

	    # Small global usage variable
	    set DAY=`/bin/date | /bin/sed "s/ .*//"`

	    setenv SILENT 1
	    if ($?INSURE)  unsetenv INSURE
	    if ($?NODEBUG) unsetenv NODEBUG
	    staradev
	    unset noclobber

	    switch ("$1")
	    case "Clean":
		cd $STAR
		mgr/CleanLibs obj 2 | /bin/grep Delete  >$HOME/log/CleanLibs.log
		mgr/CleanLibs OBJ 2 | /bin/grep Delete  >$HOME/log/CleanLibs.log
		breaksw

	    case "Insure2":
		starver .IDEV
	    case "Insure":
		cd $STAR
		$SCRIPTD/insbld.pl -c -s >$HOME/log/IN-$DAY.log
		mgr/CleanLibs IOBJ 1
		breaksw


	    # other library verison else that "dev"
	    case "eval":
		if ( "$3" == "") then
		    # update if no compiler version is specified
		    set XArgs="-u"
		endif
	    case "cal":
		if ( ! $?XArgs ) then
		    # no update otherwise
		    set XArgs="-i"
		endif
		# Commands uses whatever is found in 'adev' and compiles
		set LPATH=$AFS_RHIC/star/packages/$1
		set SPATH=$AFS_RHIC/star/doc/www/comp/prod/Sanity
		if ("$2" != "") then
		    # assumes to be args to setup
		    setup $2 $3
		    setenv AutoBuild_setup_cmd "setup $2 $3"
		endif
		$SCRIPTD/AutoBuild.pl -pst -k -d $XArgs -T $1$2$3 -R -1 -v $1 -t -B -p $LPATH
		if( -e $HOME/AutoBuild-$1$2$3-linux.html) then
		    /bin/mv -f $HOME/AutoBuild-$1$2$3-linux.html $SPATH/AutoBuild-$1$2$3-linux.html
		endif
		cd $LPATH
		echo "Cleaning older libraries"
		mgr/CleanLibs obj 1
		breaksw


	    # ***** THOSE BLOCKS ARE TEMPORARY *****
	    case "Solaris":
		set LPATH=$AFS_RHIC/star/packages/adev
		set SPATH=$AFS_RHIC/star/doc/www/comp/prod/Sanity
		$SCRIPTD/AutoBuild.pl -k -i -R -1 -t -p $LPATH
		if( -e $HOME/AutoBuild-solaris.html) then
		    /bin/mv -f $HOME/AutoBuild-solaris.html $SPATH/AutoBuild-solaris.html
		endif
		cd $LPATH
		# Clean this garbage
		echo "Cleaning up C++ demangling cache"
		/usr/bin/find . -type d -name SunWS_cache -exec rm -fr {} \;
		echo "Cleaning older libraries"
		mgr/CleanLibs obj 1
		breaksw
	    # ########################################


	    # By extending with -B, we can regroup targets
	    case "icc9":
		# Old backward compat
		# this was added to provide cross verison support
		# default tatrget becomes the latest
		setenv INTELC_VERSION 9

	    case "gprof":
	    case "gcc":
	    case "icc":
		set LPATH=$AFS_RHIC/star/packages/adev
		set SPATH=$AFS_RHIC/star/doc/www/comp/prod/Sanity
		
		# this is only for double checking. AutoBuild.pl is
		# impermeable to external env changes (start a new process)
		# so modifications has to be passed at command line level
		echo "Testing setup $1"
		setup $1 $2

		# add a test for icc
		set OKBUILD=1
		if ( "$1" == "icc") then
		    echo "Testing setup $1 ${INTELC_VERSION}"
		    set test=`which icc`
		    set sts=$status
		    if ( $sts != 0 ) then
			echo "Test returned status $sts"
			set OKBUILD=0
		    endif
		endif

		if ( $OKBUILD ) then
		    setenv AutoBuild_setup_cmd "setup $1 $2"
		    if ("$3" == "") then
			$SCRIPTD/AutoBuild.pl -1 -k -i -R -t -T $1$2 -B -p $LPATH 
		    else
			$SCRIPTD/AutoBuild.pl $3 -k -i -R -t -T $1$2 -B -p $LPATH 
		    endif
		    if( -e $HOME/AutoBuild-linux-$1$2.html) then
			/bin/mv -f $HOME/AutoBuild-linux-$1$2.html $SPATH/AutoBuild-$1$2.html
		    endif
		    cd $LPATH
		    echo "Cleaning older libraries"
		endif

		if ( "$1" == "gcc" || "$1" == "icc") then
		    mgr/CleanLibs 1
		    echo "Reverting to normal setup"
		    setup gcc
		else
		    mgr/CleanLibs GOBJ 1
		    echo "Reverting to normal setup"
		    setup nogprof
		endif
		breaksw


	    #case "SL5":
	    #	set XTRACMD="unsetenv PGI"
	    case "64bits":
		set XTRACMD="setup 64bits"
	    case "Linux61":
	    case "Linux72":
	    case "Linux80":
	    case "Linux9":
	    case "SL3":
	    case "SL302":
	    case "SL305":
	    case "SL44":
	    case "SL53":
	    case "SL64":
	    case "new":
		set LPATH=$AFS_RHIC/star/packages/adev
		set SPATH=$AFS_RHIC/star/doc/www/comp/prod/Sanity
		if ( ! $?XTRACMD ) then
		    $SCRIPTD/AutoBuild.pl -k -i -R -1 -T $1 -p $LPATH
		else
		    $SCRIPTD/AutoBuild.pl -k -s -i -R -1 -a "$XTRACMD" -T $1 -p $LPATH
		endif

		if( -e $HOME/AutoBuild-$1.html) then
		    /bin/mv -f $HOME/AutoBuild-$1.html $SPATH/AutoBuild-$1.html
		endif
		cd $LPATH
		echo "Cleaning older libraries"
		mgr/CleanLibs obj 1	
		if ( ! $?XTRACMD ) then
		    echo "Cleaning emacs flc files"
		    /usr/bin/find StRoot/ -name '*.flc' -exec rm -f {} \;
		endif
		breaksw



		# ****** This is the default action *****
	    case "inter":
		# Do not checkout, do not perform post-compilation
		$SCRIPTD/AutoBuild.pl -i -s    >$HOME/log/AB-d-$DAY.log
		breaksw
	    case "silent":
		# same as default with -s
		$SCRIPTD/AutoBuild.pl -s -u -R >$HOME/log/AB-$DAY.log
		breaksw
	    default
		# Is update mode, not checkout
		$SCRIPTD/AutoBuild.pl -u -R    >$HOME/log/AB-$DAY.log
	    endsw
	endif
endif
