#!/bin/csh -f

#
# Setup for Intel compiler 
# (c) J. Lauret 2003-2009
#

set self="intelcc"

# get version as first argument
set vers=0
if ( $?1 ) then
    set vers=$1
endif
if ( $vers == "" || $vers == 0 ) then
    if ( $?DECHO ) then
	echo "$self :: Will check for INTELC_VERSION"
    endif

    if ( $?INTELC_VERSION ) then
	set vers=$INTELC_VERSION
    endif
    # if still empty, set to 0
    if ( $vers == "") set vers=0
endif

source $GROUP_DIR/unix_programs.csh


#set pathtointel = `/bin/ls -1d /usr/intel* | /usr/bin/tail -1`
#set pathtointel = `/bin/ls -1d /opt/intel* | /usr/bin/tail -1`
#set pathtointel = `/bin/ls -1d /opt/intel*/bin | grep -v hold | /usr/bin/tail -1`
#set pathtointel = /opt/intel
set pathtointel = /opt/intel_17
if ( $?DECHO ) then
   echo "$self :: Path found is [$pathtointel]"
endif
if ( -d "$pathtointel" ) then
#    if ( -x $GROUP_DIR/dropit ) then
#	setenv PATH            `$GROUP_DIR/dropit intel`
#	setenv LD_LIBRARY_PATH `$GROUP_DIR/dropit -p $LD_LIBRARY_PATH intel`
#	setenv MANPATH         `$GROUP_DIR/dropit -p $MANPATH intel`
#    endif
    # search for setup files and source later 
    set arch = ia32
    if ($USE_64BITS == 1) set arch = intel64
    set platform = linux
    source $pathtointel/bin/compilervars.csh $arch
endif
#    setenv PATH ${pathtointel}/bin:${PATH}
if ( $?DECHO ) then
   echo "$self :: Done [$seticc]"
endif
