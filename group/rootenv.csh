#!/bin/csh

#
# Setup ROOT environment according to STAR path standard
# Multiple version and scheme supported
#
#this may be sourced
if ( $?self ) then
    set pself=${self}
endif
set self="rootenv"
source $GROUP_DIR/unix_programs.csh

#if ($#argv > 0) setenv ROOT_LEVEL $1
#echo "rootenv 1 => $PATH"
if ($?STAR_HOST_SYS == 0) setenv STAR_HOST_SYS `sys`
set level = `echo $ROOT_LEVEL | $AWK -F. '{print $1$2}'`

if (! $?ROOT) setenv ROOT ${STAR_ROOT}/ROOT
if ($?ROOTROOT == 0)   setenv ROOTROOT ${ROOT}/${ROOT_LEVEL}
if ($level >= 305 )  then
    # all is sorted out here actually
    set p = ""
    set x = "deb"
    if ($?INSURE)  set p = "I"
    if ($?GPROF)   set p = "G"
    if ($?NODEBUG) set x = ""

    set ROOTBASE = "${ROOT}/${ROOT_LEVEL}/.${STAR_HOST_SYS}"
#echo "rootenv 3 => $PATH"

    if ( ! -r ${ROOT}/${ROOT_LEVEL}/.${STAR_HOST_SYS}/${p}root${x} && $?DECHO )then
	echo "$self :: Did not find ${p}root${x}"
    endif

    if (! -r ${ROOT}/${ROOT_LEVEL}/.${STAR_HOST_SYS}/${p}root${x} ) then
	# We set "a" default
	setenv ROOTSYS ${ROOT}/${ROOT_LEVEL}/.${STAR_HOST_SYS}/rootdeb
    else
	# we reset it according to what is defined
	setenv ROOTSYS ${ROOT}/${ROOT_LEVEL}/.${STAR_HOST_SYS}/${p}root${x}
	setenv TMVASYS $ROOTSYS
    endif
#echo "rootenv 4 => $PATH"

else
    if ($level  >= 224 )  then
	setenv ROOTSYS ${ROOT}/${ROOT_LEVEL}
	setenv TMVASYS $ROOTSYS
	set root = "/.${STAR_HOST_SYS}/root"
    else
	# not sure what that was but older version of
	# root we can probably get rid off
	setenv ROOTSYS ${ROOT}/${ROOT_LEVEL}/.${STAR_HOST_SYS}/root
	setenv TMVASYS $ROOTSYS
	set root   = ""
    endif
endif

#echo "rootenv 5 => $PATH"




# Treat the PATH and LD_LIBRARY_PATH
if ( -x ${GROUP_DIR}/dropit) then
    # the setenv at this stage would be a catastrophe
    # if dropit is not found
    # setenv LD_LIBRARY_PATH `${GROUP_DIR}/dropit -p "$LD_LIBRARY_PATH" ROOT`
    setenv PATH `${GROUP_DIR}/dropit -p ${ROOTSYS}/bin -p ${PATH}`
    if ($?LD_LIBRARY_PATH) setenv LD_LIBRARY_PATH `${GROUP_DIR}/dropit -p ${ROOTSYS}/lib -p ${LD_LIBRARY_PATH}`
    else                   setenv LD_LIBRARY_PATH ""
    if ($level  < 305 )  then
	if ($?NODEBUG) then
	    setenv PATH `${GROUP_DIR}/dropit -p ${ROOTSYS}/BIN -p ${PATH}`
	    setenv LD_LIBRARY_PATH `${GROUP_DIR}/dropit -p ${ROOTSYS}/LIB -p ${LD_LIBRARY_PATH}`
	endif
	if ($?INSURE) then
	    setenv PATH `${GROUP_DIR}/dropit -p ${ROOTSYS}/IBIN -p ${PATH}`
	    setenv LD_LIBRARY_PATH `${GROUP_DIR}/dropit -p ${ROOTSYS}/ILIB -p ${LD_LIBRARY_PATH}`
	endif
	if ($?GPROF) then
	    setenv PATH `${GROUP_DIR}/dropit -p ${ROOTSYS}/GBIN -p ${PATH}`
	    setenv LD_LIBRARY_PATH `${GROUP_DIR}/dropit -p ${ROOTSYS}/GLIB -p ${LD_LIBRARY_PATH}`
	endif
    else
	# version is greater
	if ($?NODEBUG) then
	    setenv PATH `${GROUP_DIR}/dropit -p ${ROOTBASE}/root/bin -p ${PATH}`
	    setenv  LD_LIBRARY_PATH `${GROUP_DIR}/dropit -p ${ROOTBASE}/root/lib -p ${LD_LIBRARY_PATH}`
	endif
	if ($?INSURE || $?GPROF ) then
	    setenv PATH `${GROUP_DIR}/dropit -p ${ROOTBASE}/${p}root/bin -p ${PATH}`
	    setenv LD_LIBRARY_PATH `${GROUP_DIR}/dropit -p ${ROOTBASE}/${p}root/lib -p ${LD_LIBRARY_PATH}`
	endif
    endif
else
    setenv PATH ${ROOTSYS}/bin:${PATH}
    setenv LD_LIBRARY_PATH ${ROOTSYS}/lib:${LD_LIBRARY_PATH}
endif

#
# ATTENTION -- XROOTD NOT VALID PRIOR TO THIS VERSION
#
if ($level >= 404  && $?XROOTDSYS ) then
    setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${XROOTDSYS}/.${STAR_HOST_SYS}/lib
    setenv PATH ${PATH}:${XROOTDSYS}/.${STAR_HOST_SYS}/bin

    # This indicates it will use pwdnetrc file and will not ask
    # for a password 
    setenv XrdSecPWDAUTOLOG  1

    # Several user/password may apply. If so, define this PRIOR 
    if ( ! $?XrdSecUSER ) then
	setenv XrdSecUSER starread
    endif

    # Point to default auth files where the info will be found
    #
    # The server will search for $HOME/.xrd/pwdnetrc which may cause some
    # information syncrhonization issues if $HOME and ${XROOTDSYS} are on 
    # different FS.
    if ( ! -r $HOME/.xrd/pwdnetrc && -r ${XROOTDSYS}/.xrd/pwdnetrc ) then
	setenv XrdSecPWDALOGFILE ${XROOTDSYS}/.xrd/pwdnetrc
    endif
    if ( ! -r $HOME/.xrd/pwdsrvpuk && -r ${XROOTDSYS}/.xrd/pwdsrvpuk ) then
	setenv XrdSecPWDSRVPUK   ${XROOTDSYS}/.xrd/pwdsrvpuk
    endif
endif


# attempt to check qt from ROOT
#if ( 0 ) then
#if ( -r ${ROOTSYS}/config.log ) then
    if ( -r ${OPTSTAR}/qt4 || -r ${OPTSTAR}/qt3 ) then
	# there is a possibility for an ambiguity to be
	# resolved
	# ->> this test is not safe - newer version of root config.log
        #     do not add this info
        #     set test1=`$GREP qt4 ${ROOTSYS}/config.log | /usr/bin/wc -l`
	set testq=`echo $LD_LIBRARY_PATH | $GREP 'qt/lib'`
	set test3=`echo $LD_LIBRARY_PATH | $GREP 'qt3/lib'`
	set test4=`echo $LD_LIBRARY_PATH | $GREP 'qt4/lib'`
	#if  ( $test1 != 0 ) then
	    # qt4 was used for ROOT but possibly an ambigous path
	    # reset
	    if ( "testq" != "") then
	    setenv LD_LIBRARY_PATH `echo $LD_LIBRARY_PATH | sed 's/qt\/lib/qt4\/lib/'`
	    endif
	    if ( "$test3" != "") then
	    setenv LD_LIBRARY_PATH `echo $LD_LIBRARY_PATH | sed 's/qt3\/lib/qt4\/lib/'`
	    endif
	    setenv QTDIR ${OPTSTAR}/qt4

	    # Beware that on Linux, the login may default to qt3 with diverse
	    # naming conventions
	    if ( -r /etc/profile.d/qt.csh ) then
		set test4=`echo $PATH | $GREP '/qt.*4/'`
		set test3=`echo $PATH | $GREP '/qt.*3/'`

		if ( "$test4" == "") then
		    setenv PATH $QTDIR/bin:${PATH}
		endif
		if ( "$test3" != "") then
		    if ( -x ${GROUP_DIR}/dropit) then
			setenv PATH `${GROUP_DIR}/dropit qt3 qt-3`
		    endif
		endif
	    endif

	#else 
	#    # assume qt3 - we have already tested we had $OPTSTAR/qt3
	#    if ( "testq" != "") then
	#    setenv LD_LIBRARY_PATH `echo $LD_LIBRARY_PATH | sed 's/qt\/lib/qt3\/lib/'`
	#    endif
	#    if ( "test4" != "") then
	#    setenv LD_LIBRARY_PATH `echo $LD_LIBRARY_PATH | sed 's/qt4\/lib/qt3\/lib/'`
	#    endif
	#    setenv QTDIR ${OPTSTAR}/qt3	    
	#endif 
    endif
#endif
#endif



# Manpages for ROOT
if ( -x ${GROUP_DIR}/dropit) then
    if ($?MANPATH == 0) setenv MANPATH
    setenv MANPATH `${GROUP_DIR}/dropit -p ${ROOTSYS}/man -p ${MANPATH}`
endif


# OpenGL
if ( -r $ROOTSYS/../Mesa) setenv OPENGL $ROOTSYS/../Mesa

# CINT
if ( -d ${ROOTSYS}/cint/cint ) then
    setenv CINTSYSDIR ${ROOTSYS}/cint/cint
else
    setenv CINTSYSDIR ${ROOTSYS}/cint
endif

if ( -x ${GROUP_DIR}/dropit && -d ${ROOTSYS}/cint/doc ) then
    setenv MANPATH `${GROUP_DIR}/dropit -p ${MANPATH} -p ${ROOTSYS}/cint/doc`
endif


# restore - this in case of a sourcing
if ( $?pself ) then
    set self=$pself
endif


# befeore leaving unset
unset p x
unset test1 testq test3 test4
unset level

