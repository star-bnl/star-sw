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
set pathtointel = `/bin/ls -1d /opt/intel | grep -v hold | /usr/bin/tail -1`
#set pathtointel = /opt/intel
if ( $?DECHO ) then
   echo "$self :: Path found is [$pathtointel]"
endif




if ( "$pathtointel" != "") then
    # search for setup files and source later 
    if ( $vers == 0 ) then
       if ( $?DECHO ) then
          echo "$self :: No version specified, using the 'latest'"
       endif
       set seticc=`  find $pathtointel -type f -name iccvars.csh   | /usr/bin/tail -1`
       set setifc=`  find $pathtointel -type f -name ifcvars.csh   | /usr/bin/tail -1`
       set setifort=`find $pathtointel -type f -name ifortvars.csh | /usr/bin/tail -1`
       set setidb=`  find $pathtointel -type f -name idbvars.csh   | /usr/bin/tail -1`
    else
       if ( $?DECHO ) then
          echo "$self :: Version [$vers] specified, searching"
       endif
       set seticc=`  find $pathtointel -type f -name iccvars.csh   | $GREP $vers`
       set setifc=`  find $pathtointel -type f -name ifcvars.csh   | $GREP $1`
       set setifort=`find $pathtointel -type f -name ifortvars.csh | $GREP $1`
       set setidb=`  find $pathtointel -type f -name idbvars.csh   | $GREP $1`
    endif 
    if ( -x $GROUP_DIR/dropit ) then
	setenv PATH            `$GROUP_DIR/dropit intel`
	setenv LD_LIBRARY_PATH `$GROUP_DIR/dropit -p $LD_LIBRARY_PATH intel`
	setenv MANPATH         `$GROUP_DIR/dropit -p $MANPATH intel`
    endif
    # correct problem with version 10
    if ( $?INTEL_LICENSE_FILE)  unsetenv INTEL_LICENSE_FILE
    if ( "$seticc"   != "") source $seticc
    if ( "$setifc"   != "") source $setifc
    if ( "$setifort" != "") source $setifort
    if ( "$setidb"   != "") source $setidb
endif

if ( $?DECHO ) then
   echo "$self :: Done [$seticc]"
endif
