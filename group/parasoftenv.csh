#!/usr/bin/csh -f

switch ($STAR_SYS)
    case "i386_sl*":
	# Starting at SL3, we have one version defined. Note the missing
	# breaksw to get the next block executed as well.
	if ( ! $?INSV ) then
	    setenv INSV insure-7.0
	endif

    case "i386_redhat8*":
	# Support for Insure++ -- This is set to allow other sites
	# to chose their version of INSURE.
	if ( ! $?INSV ) then
	    setenv INSV insure-6.1-gcc-3.2
	endif

	# use the defined version in pre-login
	if ( -e ${AFS_RHIC}/app/$INSV ) then
	    setenv PARASOFT ${AFS_RHIC}/app/$INSV
	else
	    # old default or soft-link
	    setenv PARASOFT ${AFS_RHIC}/app/insure
	endif

	set VER=`ls -ld ${PARASOFT}/bin* | sed "s/.*\.//"`
	if ("$VER" != "") set VER=".$VER"
	set path = ($path $PARASOFT/bin$VER)
	setenv LD_LIBRARY_PATH  ${LD_LIBRARY_PATH}:${AFS_RHIC}/app/${INSV}/lib$VER	    
	if ( -e ${PARASOFT}/man ) then
	   # no manpages starting from 4.2
	   setenv MANPATH `${GROUP_DIR}/dropit -p ${MANPATH} -p {$PARASOFT}/man`
	endif
	unset VER
	breaksw	


    case "i386_redhat6*":
    case "i386_linux2*":
	setenv PARASOFT $AFS_RHIC/app/insure-5.2
	setenv LD_LIBRARY_PATH `${GROUP_DIR}/dropit -p ${LD_LIBRARY_PATH} -p {$PARASOFT}/lib.linux2`
	if ( -x $GROUP_DIR/dropit) setenv PATH `dropit parasoft`
	set path = ($path $PARASOFT/bin.linux2)
	setenv MANPATH `${GROUP_DIR}/dropit -p ${MANPATH} -p {$PARASOFT}/man`
	breaksw

    case "i386_*":
	setenv PARASOFT $AFS_RHIC/i386_linux22/app/parasoft
	setenv LD_LIBRARY_PATH `${GROUP_DIR}/dropit -p ${LD_LIBRARY_PATH} -p {$PARASOFT}/lib.linux`
	if ( -x $GROUP_DIR/dropit) setenv PATH `dropit parasoft`
	set path = ($path $PARASOFT/bin.linux)
	setenv MANPATH `${GROUP_DIR}/dropit -p ${MANPATH} -p {$PARASOFT}/man`
	breaksw

    case "sun4*":
	setenv PARASOFT $AFS_RHIC/sun4x_56/app/parasoft
	setenv LD_LIBRARY_PATH `${GROUP_DIR}/dropit -p ${LD_LIBRARY_PATH} -p {$PARASOFT}/lib.solaris`
	if ( -x $GROUP_DIR/dropit) setenv PATH `dropit parasoft`
	set path = ($path $PARASOFT/bin.solaris)
	setenv MANPATH `${GROUP_DIR}/dropit -p ${MANPATH} -p {$PARASOFT}/man`
	breaksw 

    default:
	echo "No parasoft setup  defined for platform $STAR_SYS"
	breaksw
endsw
#___________
