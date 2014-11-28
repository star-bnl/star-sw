#!/bin/bash
#       $Id: group_env.csh,v 1.1.1.1 2013/08/22 01:27:48 fisyak Exp $
#	Purpose:	STAR group csh setup
#
# Revisions & notes
#    2001-2009  Maintained J. Lauret
#    24 Apr 01  J. Lauret  Disabled echoing in ! prompt.
#                          DO NOT MODIFY THIS !!!
#     2 Apr 01  J. Lauret  Insure path added
#     3 Mar 98  T. Wenaus  HP Jetprint added (for sol)
#    17 Feb 98  Created Y.Fisyak (BNL)
#
# Should be loaded by star_login itself loaded executed by
# our individual .login files.
#
#
ECHO = 1;
FAIL = "";

export INTERACTIVE=0
if (( ! $?prompt)) ; then
    # when there is not prompt, we are usually in batch
    # or captive mode. Condor tends to copy env though
    ECHO = 0
else
    # otherwise, set this for convenience but make a few
    # more checks 
    if (( $?SSH_TTY || $?DISPLAY )) ; then
	export INTERACTIVE=1
    fi
fi
if (($?STAR == 1)) ; then   ECHO = 0 fi
if (($?SILENT == 1)) ; then ECHO = 0 fi

# This variable was added for the ECHOD debug mode
if (( $?self )) ; then
    GRPE_pself=${self}
fi
self="group_env"
if (( $?DECHO && $?STAR_LEVEL )) ; then
    # ECHO = 1
    echo "$self :: Receiving STAR_LEVEL $STAR_LEVEL"
fi

source $GROUP_DIR/unix_programs.csh

# possible path for utilities
export AFS=/usr/afsws


# check if AFS_RHIC is readable
READ_AFS=`echo $AFS_RHIC | $GREP Path_Not_Found`

if (( $?DECHO)) ; then echo "$self :: READ_AFS is [$READ_AFS]" fi

if ((! $?STAR_ROOT)) ; then
    if (( $?DECHO)) ; then echo "$self :: checking STAR_ROOT" fi
    if (( "$READ_AFS" == "")) ; then
	if (( $?DECHO)) ; then  echo "$self ::  Defining STAR_ROOT as AFS based if -d checks" fi
	if (( -d ${AFS_RHIC}/star )) ; then
	    export STAR_ROOT=${AFS_RHIC}/star
        fi
    else
       if (( -d /usr/local/star )) ; then
	    # this is valid
	    if (( $?DECHO) echo "$self ::  Defining STAR_ROOT as /usr/local/star"
	    export STAR_ROOT=/usr/local/star
       else
	    # We will fail (we know that)
	    echo "$self ::  Did not find a valid STAR_ROOT"
	    export STAR_ROOT=/Path_Not_Found_STAR_Login_Failure
	    FAIL="$FAIL STAR_ROOT"
       fi
    fi
fi

# Clear this out. First block STAF, second STAR
if (( $?DECHO)) ; then echo "$self :: Executing STAR_SYS" fi
source ${GROUP_DIR}/STAR_SYS;

# Define /opt/star (or equivalent)
if (( ! $?OPTSTAR)) ; then
   export OPTSTAR=/opt/star
   if ((-d ${OPTSTAR}/${STAR_HOST_SYS}) export XOPTSTAR=${OPTSTAR}/${STAR_HOST_SYS}
   else                                export XOPTSTAR=${OPTSTAR}
fi
# X indicates points to the AFS reference
if (( ! $?XOPTSTAR )) ; then
    if ((-d ${OPTSTAR}/${STAR_HOST_SYS}) export XOPTSTAR=${OPTSTAR}/${STAR_HOST_SYS}
#    # keep a reference to the AFS one
#    # this -e test may fail - don't do it
#    if (( "$READ_AFS" == "" )) ; then
##	export XOPTSTAR=${AFS_RHIC}/star/packages/.DEV2/misc/opt/star/${STAR_HOST_SYS}
#	export XOPTSTAR=$OPTSTAR #${AFS_RHIC}/opt/star
#    fi
fi

#if (( -r ${AFS_RHIC}/star/packages/.DEV2/misc/opt/star/${STAR_HOST_SYS} )) ; then
#    export  OPTSTAR=${AFS_RHIC}/star/packages/.DEV2/misc/opt/star/${STAR_HOST_SYS}
#fi
if (( ! $?OPTSTAR )) ; then
    # local first - BEWARE this may be a link over
    # AFS as well and as it turns out, -e test locks as well

    # there is not even a /opt, -e will be safe
    # if there is no star in /opt, also safe to do -e
    # note that ALL ls must be escaped to avoid argument aliasing
    # forcing color, fancy display etc ... all doing a form of stat
    # hence locking again
    IS_OPTSTAR_AFS=""
    TEST=""

    if (( -d /opt )) ; then
        TEST=`/bin/ls /opt/ | $GREP star`
	if (( "$TEST" == "star" )) ;  then
#            IS_OPTSTAR_AFS=`ls -ld /star/packages/.DEV2/misc/opt/star | grep afs`
            IS_OPTSTAR_AFS=`ls -ld /opt/star | $GREP afs`
	fi
    fi

    if (( "$IS_OPTSTAR_AFS" == "" || "$READ_AFS" == "")) ; then
	if (( $?DECHO)) ; then echo "$self :: Safe to test -e on /opt/star" fi
	if (( -e /opt/star )) ; then
	    export  OPTSTAR=/opt/star
	fi
	#else -> note that eventually, we could set blindly OPTSTAR if TEST!=""
    fi

    # remote second
    if (( $?DECHO)) ; then  echo "$self :: Not safe to check /opt/star OPTSTAR_AFS=[$IS_OPTSTAR_AFS] READ_AFS=[$READ_AFS]" fi
    if (( $?XOPTSTAR && ! $?OPTSTAR )) ; then
        export OPTSTAR=${XOPTSTAR}
    else
        export FAIL="$FAIL OPTSTAR"
    fi
fi

# define but feedback later
if (( $?DECHO)) ; then  echo "$self :: Defining GROUP_DIR STAR_PATH" fi
if (( ! $?GROUP_DIR )) ; then    export GROUP_DIR=${STAR_ROOT}/group fi    # Defined to AFS Group Dir
if (( $?STAR_PATH == 0)) ; then export STAR_PATH=${STAR_ROOT}/packages; fi


if (( $?DECHO)) ; then echo   "$self :: Value of GROUP_DIR = ${GROUP_DIR}" fi

# make this additional test ahead
if (( ! -e $STAR_PATH )) ; then
    FAIL="$FAIL STAR_PATH"
fi


if (( "$FAIL" != "")) ; then
    if (($?DECHO)) ; then  echo "$self :: FAIL is [$FAIL], something is not right (checking)" fi

    # we can add this only now because setup may be AFS-free
    if (( "$READ_AFS" != "" )) ; then
	FAIL="$FAIL AFS_RHIC"
    fi

    if (($ECHO)) ; then
	echo ""
	echo "***************************************************************"
	echo "  ERROR Cannot find a valid Path for                           "
	echo "    $FAIL                                                      "
	echo "  STAR Login is incomplete                                     "
	echo "                                                               "

	# we can try to guess the reason but it may not be the whole story
	failafs=0
	if (( `echo $FAIL | $GREP AFS` != "" &&  `echo $FAIL | $GREP STAR_PATH` != "")) ; then
	    # if AFS detection failed and STAR_PATH was not defined we have no options
	    failafs=1
	fi
	if (( `echo $STAR_ROOT | $GREP $AFS_RHIC` != "" &&  `echo $STAR_PATH | $GREP $STAR_ROOT` != "" && `echo $FAIL | $GREP STAR_PATH` != "")) ; then
	    # ! -e STAR_PATH but defined as AFS resident is the second sign of failure
	    # it does seem like the above but this second test is necessary due to client
	    # file caching
	    failafs=1
	fi

	if (( $failafs )) ; then
	echo "  Reason: It appears the AFS lookup has failed and             "
	else
	# any other reason, display a generic message
	echo "  Reason: Improper or incomplete installation                  "
	fi
	echo "          You do not have a local installation of the STAR     "
	echo "          software stack.                                      "
	echo "                                                               "
	echo "    If you are installing for the first time, ignore & proceed "
	echo "    with installation. Our documentation is available at       "
        echo "    http://drupal.star.bnl.gov/STAR/comp/sofi/installing       "
	echo "***************************************************************"
	echo ""
	# disable messages
	if( ! $?DECHO)) ; then ECHO = 0 fi
    fi
else
    if (($?DECHO)) ; then  echo "$self :: FAIL is NULL, we are fine so far" fi
    if (($ECHO)) ; then
	echo ""
	echo "         ----- STAR Group Login from $GROUP_DIR/ -----"
	echo ""
	echo "Setting up STAR_ROOT = ${STAR_ROOT}"
	echo "Setting up STAR_PATH = ${STAR_PATH}"
    fi
fi


export WWW_HOME=http://www.star.bnl.gov/
if (($ECHO)) ; then  echo   "Setting up WWW_HOME  = $WWW_HOME" fi


# Defined in CORE. GROUP_PATH/GROUPPATH are global
# while GROUP_DIR may be local
if (( ! $?GROUP_PATH )) ; then  export GROUP_PATH=${STAR_ROOT}/group fi
export GROUPPATH= $GROUP_PATH




# Default value (some if not already defined)
if (($?STAR_LEVEL == 0)); then export STAR_LEVEL=pro fi

if (( $?DECHO)) ; then  echo "$self :: Setting STAR_VERSION" fi

export STAR_VERSION=${STAR_LEVEL}
if (($STAR_LEVEL  == "old" || $STAR_LEVEL  == "pro" || $STAR_LEVEL  == "new" || $STAR_LEVEL  == "dev" || $STAR_LEVEL  == ".dev")) ; then
  # i.e. replace with link value instead
  if (( $?DECHO )) ; then  echo "$self :: Will test -e $STAR_PATH/${STAR_LEVEL}" fi
  # exit

  if( -e $STAR_PATH/${STAR_LEVEL})) ; then
    # be carefull, it may not be "seen" as a soft link
    # at all ... Some AFS client do not show the link.
    # No even speaking of absolute path ...
    if (( $?DECHO )) ; then  echo "$self :: Will ls -ld $STAR_PATH/${STAR_LEVEL}" fi
    a = `ls -ld $STAR_PATH/${STAR_LEVEL}`
    b = `ls -ld $STAR_PATH/${STAR_LEVEL} | cut -f2 -d">"`
    if (( $?DECHO )) ; then  echo "$self :: Checked $a $b" fi
    if (( "$a" != "$b")) ; then
	export STAR_VERSION=$b
    else
	export STAR_VERSION=$STAR_LEVEL
    fi
  fi
fi


if (( $?DECHO)) ; then  echo "$self :: Setting STAF_VERSION" fi

if (($?STAF_LEVEL == 0)) ; then
 if (( -e $STAR_PATH/StAF/${STAR_LEVEL})) ; then
    export STAF_LEVEL=$STAR_LEVEL
 else
    export STAF_LEVEL=pro
 fi
fi

export STAF_VERSION=${STAF_LEVEL}
if (($STAF_LEVEL  == "old" || $STAF_LEVEL  == "pro" || $STAF_LEVEL  == "new" || $STAF_LEVEL  == "dev" || $STAF_LEVEL  == ".dev")) ; then
  if( -e $STAR_PATH/StAF/${STAF_LEVEL})) ; then
    a = `ls -ld $STAR_PATH/StAF/${STAF_LEVEL}`
    b = `ls -ld $STAR_PATH/StAF/${STAF_LEVEL} | cut -f2 -d">"`
    if (( "$a" != "$b")) ; then
	export STAF_VERSION=$b
    else
	export STAF_VERSION=${STAF_LEVEL}
    fi
  fi
fi

#+
# use alternate gcc installations
# Needs to be here because STAR_SYS will set vars based on the
# command 'gcc'
#-
if (( $?USE_GCC_DIR )) ; then
    if (( -x $USE_GCC_DIR/bin/gcc && -d $USE_GCC_DIR/lib )) ; then
        # do not redefine it if already done to avoid having
	# a messed up path and ldpath
        if (( ! $?ALT_GCC )) ; then
            export ALT_GCC=$USE_GCC_DIR
	
            path=($USE_GCC_DIR/bin $path)
	    if (( $?LD_LIBRARY_PATH )) ; then
	        export LD_LIBRARY_PATH=$USE_GCC_DIR/lib:${LD_LIBRARY_PATH}
	    else
	        export LD_LIBRARY_PATH=$USE_GCC_DIR/lib
	    fi
	fi
    fi
fi



# Clear this out. First block STAF, second STAR
#if (( $?DECHO) echo "$self :: Executing STAR_SYS"
#source ${GROUP_DIR}/STAR_SYS;

#
# The above logic forces the creation of "a" compiler
# specific path prior to setting up $OPTSTAR1 . This was
# made on purpose so the environment would revert to a
# default $OPTSTAR in case things are not quite in place.
#

# There is a second chance to define XOPTSTAR
if (( $?DECHO)) ; then  echo "$self :: Checking  XOPTSTAR " fi
if (( ! $?XOPTSTAR )) ; then
#    if (( -e ${AFS_RHIC}/star/packages/.DEV2/misc/opt/star/${STAR_HOST_SYS} )) ; then
#	export XOPTSTAR=${AFS_RHIC}/star/packages/.DEV2/misc/opt/star/${STAR_HOST_SYS}
    if (( -e ${AFS_RHIC}/${STAR_SYS}/opt/star )) ; then
       export XOPTSTAR=${AFS_RHIC}/${STAR_SYS}/opt/star
    else
	# well, as good as anything else (we cannot find a
	# global reference)
	export XOPTSTAR=/dev/null
    fi
fi

if (( $?OPTSTAR )) ; then
    if ((!  $?optstar )) ; then export  optstar= ${OPTSTAR} fi
    if ((! $?xoptstar )) ; then  export xoptstar=${XOPTSTAR} fi

    if (( -e ${OPTSTAR}/${STAR_HOST_SYS} )) ; then
	# Redhat > 7.3  transition ; adding one level
	export OPTSTAR=${optstar}/${STAR_HOST_SYS}
    fi
    if (( -e ${xoptstar}/${STAR_HOST_SYS} )) ; then
	export XOPTSTAR=${xoptstar}/${STAR_HOST_SYS}
    fi
fi


# Display the messages here now
if ((  $?OPTSTAR )) ; then
    if (($ECHO)) ; then  echo   "Setting up OPTSTAR   = ${OPTSTAR}" fi
else
    # nothing found, so set it to nothing and the login
    # will be able to proceed (at least, repair will be
    # possible)...
    export OPTSTAR=""
fi
if ((  $XOPTSTAR == "/dev/null" )) ; then
    if (($ECHO)); then  echo   "WARNING : XOPTSTAR points to /dev/null (no AFS area for it)" fi
else
    if (($ECHO)) ; then echo   "Setting up XOPTSTAR  = ${XOPTSTAR}" fi
fi





# STAF
export STAF=${STAR_PATH}/StAF/${STAF_VERSION} ;   if (($ECHO)) ; then echo   "Setting up STAF      = ${STAF}" fi
export STAF_LIB= $STAF/.${STAR_HOST_SYS}/lib  ;   if (($ECHO)) ; then echo   "Setting up STAF_LIB  = ${STAF_LIB}" fi
export STAF_BIN= $STAF/.${STAR_HOST_SYS}/bin  ;   if (($ECHO)) ; then echo   "Setting up STAF_BIN  = ${STAF_BIN}" fi
# STAR
export STAR=     $STAR_PATH/${STAR_VERSION}   ;   if (($ECHO)) ; then echo   "Setting up STAR      = ${STAR}" fi
export STAR_LIB= $STAR/.${STAR_HOST_SYS}/lib  ;   if (($ECHO)) ; then echo   "Setting up STAR_LIB  = ${STAR_LIB}" fi
export STAR_OBJ= $STAR/.${STAR_HOST_SYS}/obj  ;   if (($ECHO)) ; then echo   "Setting up STAR_OBJ  = ${STAR_OBJ}" fi
export STAR_BIN= $STAR/.${STAR_HOST_SYS}/bin  ;   if (($ECHO)) ; then echo   "Setting up STAR_BIN  = ${STAR_BIN}" fi
export STARL=    $STAR_PATH/${STAR_VERSION}   
if (( $STAR_LEVEL == "cal" )) ; then
    # do not redefine STAR in this case - this is used in cons
    # heavily for finding includes and such. But define lib
    # and conditionally bin so PATH and LD path will be set 
    # properly. cd to STARL instead.
    if (( ! $?STAR_BIN )) ; then
	# make a default
	export STAR_BIN=$STAR_PATH/dev/.${STAR_HOST_SYS}/bin
    fi
    if (( -e $STAR_PATH/${STAR_VERSION}/.${STAR_HOST_SYS}/bin )) ; then
	# overwrite if exists
	export STAR_BIN=$STAR_PATH/${STAR_VERSION}/.${STAR_HOST_SYS}/lib 
    fi
    if (( ! $?STAR_LIB )) ; then
	export STAR_LIB=$STAR_PATH/dev/.${STAR_HOST_SYS}/lib
    fi
    export STAR_lib= $STAR_PATH/${STAR_VERSION}/.${STAR_HOST_SYS}/lib
    export MINE_lib= $STAR_PATH/${STAR_VERSION}/.${STAR_HOST_SYS}/lib
fi
                                                  if (($ECHO)) ; then echo   "Setting up STAR_LIB  = ${STAR_LIB}" fi
export MINE_LIB=       .${STAR_HOST_SYS}/lib
export MY_BIN=         .${STAR_HOST_SYS}/bin



# YP fix
if( ! $?DOMAINNAME)) ; then
    if (( -x "domainname" )) ; then
	export DOMAINNAME=`domainname`
    else
	export DOMAINNAME="(none)"
    fi

    # Fake it
    if (( "$DOMAINNAME" == "(none)")) ; then
       export DOMAINNAME=`hostname | sed 's/^[^\.]*\.//'`
    fi
fi




#
# ATTENTION - This support for $SITE need extending
# at each new site.
#
# Each Grid site should have an entry.
# Only sites having local DB rules could have an entry.
#
if (( ! $?SITE )) ; then
    switch ($DOMAINNAME)
	case "nersc.gov":    # <--- or whatever domainame returns
	    export SITE="LBL"
	    breaksw

	case "rhic.bnl.gov":
	case "rcf.bnl.gov":
	case "star.bnl.gov":
	case "starp.bnl.gov":
	    export SITE="BNL"
	    breaksw

	case "if.usp.br":
	    export SITE="USP"
	    breaksw

	case "cluster.phy.uic.edu":
	    export SITE="UIC"
	    breaksw

        case "sdfarm.kr":
            export SITE="KISTI"
            breaksw

	default:
	    # Not implemented
	    export SITE="generic"
	    breaksw
    endsw
fi




# db related
if (( $?SITE )) ; then
    #if (( ! $?DB_SERVER_LOCAL_CONFIG )) ; then
	if (( -e ${STAR_PATH}/conf/dbLoadBalancerLocalConfig_${SITE}.xml )) ; then
	    # 2008/08 new location and unique for all libraries - SL08e or above
	    export DB_SERVER_LOCAL_CONFIG=${STAR_PATH}/conf/dbLoadBalancerLocalConfig_${SITE}.xml
	else
	    # old method and value for backward compat - this is the part preventing
	    # from protecting against redefining. In fact, if not in the global
	    # area, we MUST redefine. File was removed from this path starting from
	    # SL10g
	    export DB_SERVER_LOCAL_CONFIG=${STAR}/StDb/servers/dbLoadBalancerLocalConfig_${SITE}.xml
	fi
    #fi
fi



# Options my alter *_BIN and/or add *_lib. All options should
# be treated here. Defaults hould be preserved above.
if (($?INSURE)) ; then
  # Do it conditional because this is a late addition.
  # The directory structure may not exist for all library version.
  if( -e $STAR/.${STAR_HOST_SYS}/ILIB)) ; then
   if ((-f $GROUP_DIR/parasoftenv.csh)) ; then
     source $GROUP_DIR/parasoftenv.csh
     export STAR_lib= $STAR/.${STAR_HOST_SYS}/ILIB ;  if (($ECHO)) ; then echo   "Setting up STAR_lib  = ${STAR_lib}" fi
     export MINE_lib=       .${STAR_HOST_SYS}/ILIB
     export STAR_BIN= $STAR/.${STAR_HOST_SYS}/IBIN
     export MY_BIN=         .${STAR_HOST_SYS}/IBIN
   else
     if (($ECHO)) ; then echo "Setting up STAR_lib  = Insure not found (not set)" fi
   fi
  else
   if (($ECHO)) ; then echo  "Setting up STAR_lib  = Cannot Set (missing tree)" fi
  fi

else if (($?GPROF)) ; then
  export STAR_lib= $STAR/.${STAR_HOST_SYS}/GLIB ;  if (($ECHO)) ; then echo   "Setting up STAR_lib  = ${STAR_lib}" fi
  export MINE_lib=       .${STAR_HOST_SYS}/GLIB
  export STAR_BIN= $STAR/.${STAR_HOST_SYS}/GBIN
  export MY_BIN=         .${STAR_HOST_SYS}/GBIN

else if (($?NODEBUG)) ; then
  export STAR_lib= $STAR/.${STAR_HOST_SYS}/LIB ;  if (($ECHO)) ; then echo   "Setting up STAR_lib  = ${STAR_lib}" fi
  export MINE_lib=       .${STAR_HOST_SYS}/LIB
  export STAR_BIN= $STAR/.${STAR_HOST_SYS}/BIN
  export MY_BIN=         .${STAR_HOST_SYS}/BIN

else
  if (( $STAR_LEVEL != "cal" )) ; then
    if (($?DECHO)) ; then    echo   "$self :: unseting STAR_lib and MINE_lib for Level=[$STAR_LEVEL]" fi
    if (($?STAR_lib)) ; then STAR_lib="" fi    if (($?MINE_lib)) ; then MINE_lib="" fi
  fi
fi

if (($ECHO)) ; then    echo   "Setting up STAR_BIN  = ${STAR_BIN}" fi

# Common stuff
export STAR_SCRIPTS=$STAR_PATH/scripts
export STAR_CGI= $STAR_PATH/cgi
export STAR_MGR= $STAR/mgr
export STAR_PAMS=$STAR/pams;            if (($ECHO)) ; then echo   "Setting up STAR_PAMS = ${STAR_PAMS}" fi

if (( -e ${STAR_ROOT}/data )) ; then
export STAR_DATA=${STAR_ROOT}/data;     if (($ECHO)) ; then echo   "Setting up STAR_DATA = ${STAR_DATA}" fi
fi
if (( -e $STAR_PATH/repository )) ; then
export CVSROOT=  $STAR_PATH/repository; if (($ECHO)) ; then echo   "Setting up CVSROOT   = ${CVSROOT}" fi
fi


# The block below will be enabled only if there is botha ROOT_LEVEL
# and a CERN_LEVEL file in $STAR/mgr/. If so, ROOT and CERN levels
# will be set to the explicit version. Otherwise, some historical
# deefault will be assumed.
if (( $?DECHO )) ; then   echo "$self :: ROOT_LEVEL and CERN_LEVEL" fi
if (( -f $STAR/mgr/ROOT_LEVEL && -f $STAR/mgr/CERN_LEVEL )) ; then
  export ROOT_LEVEL=`cat $STAR/mgr/ROOT_LEVEL`
  export CERN_LEVEL=`cat $STAR/mgr/CERN_LEVEL`

  # try with post-fix
  if (( -f $STAR/mgr/CERN_LEVEL.${STAR_SYS} )) ; then
    # Overwrite
    export CERN_LEVEL=`cat $STAR/mgr/CERN_LEVEL.${STAR_SYS}`
  fi
  if (( -f $STAR/mgr/CERN_LEVEL.${STAR_HOST_SYS} )) ; then
    # Overwrite
    export CERN_LEVEL=`cat $STAR/mgr/CERN_LEVEL.${STAR_HOST_SYS}`
  fi

  # try with post-fix
  if (( -f $STAR/mgr/ROOT_LEVEL.${STAR_SYS} )) ; then
    # Overwrite
    export ROOT_LEVEL=`cat $STAR/mgr/ROOT_LEVEL.${STAR_SYS}`
  fi
  if (( -f $STAR/mgr/ROOT_LEVEL.${STAR_HOST_SYS} )) ; then
    # Overwrite
    export ROOT_LEVEL=`cat $STAR/mgr/ROOT_LEVEL.${STAR_HOST_SYS}`
  fi

  # now check if CERN exists
  if (( $?CERN )) ; then
    if (( ! -e $CERN/$CERN_LEVEL )) ; then
	if (( $?DECHO)) ; tehn  echo "$self :: Caught $CERN_LEVEL from config in $STAR/mgr/ but not found - reverting to pro" fi
	export CERN_LEVEL=pro
    fi
  fi

else
 # this block should really not be expanded - use the
 # method above instead to change version so we do not
 # have to maintain this long list of switch statements  
 if (( $?DECHO)) ; then  echo "$self :: We will use old logic of hard-coding LEVEL to a specific SL" fi
 switch ( $STAR_VERSION )

  case SL98l:
    export ROOT_LEVEL=2.20
    breaksw

  case SL99a:
  case SL99b:
  case SL99c:
    export ROOT_LEVEL=2.21
    export CERN_LEVEL=99
    breaksw

  case SL99d:
  case SL99e:
    export ROOT_LEVEL=2.21.08
    export CERN_LEVEL=99
    breaksw

  case SL99f:
  case SL99g:
    export ROOT_LEVEL=2.22
    export CERN_LEVEL=99
    breaksw

  default:
    export ROOT_LEVEL=5.12.00

  endsw
fi

if (($ECHO)) ; then echo   "Setting up ROOT_LEVEL= ${ROOT_LEVEL}" fi


# At this point, CERN_LEVEL should be defined but if not,
# the global setup will define it to a default
if (( $?CERN_LEVEL )) ; then
    source $GROUP_DIR/setup CERN ${CERN_LEVEL}
else
    source $GROUP_DIR/setup CERN
fi





if (( $?DECHO )) ; then  echo "$self :: Paths alteration for STAR_MGR, STAR_SCRIPTS STAR_CGI etc ... begins" fi
if (( -x ${GROUP_DIR}/dropit)) ; then
    export GROUPPATH=`${GROUP_DIR}/dropit -p ${GROUP_DIR} -p mgr -p ${STAR_MGR}  -p mgr/bin -p ${STAR_MGR}/bin -p ${STAR_SCRIPTS} -p ${STAR_CGI} -p ${MY_BIN} -p ${STAR_BIN} -p ${STAF}/mgr -p ${STAF_BIN}`
    export PATH=`${GROUP_DIR}/dropit -p ${OPTSTAR}/bin -p $PATH`
else
    export GROUPPATH=${GROUP_DIR}:mgr:${STAR_MGR}:mgr/bin:${STAR_MGR}/bin:${STAR_SCRIPTS}:${STAR_CGI}:${MY_BIN}:${STAR_BIN}:${STAF}/mgr:${STAF_BIN}
    export PATH= ${OPTSTAR}/bin:$PATH
fi

# test return value of PTEST from dropit
if (( $?DECHO && $?DUMPENV )) ; then
    if (( -e /tmp/dropit.$USER )) ; then
	tmp=`cat /tmp/dropit.$USER`
	echo "$self :: $tmp"
	unset tmp
	rm -f /tmp/dropit.$USER
	if (( -e /tmp/dropit.ENV.$USER )) ; then
	    echo "$self :: ENV dump now --->"
	    cat /tmp/dropit.ENV.$USER
	    rm -f /tmp/dropit.ENV.$USER
	    echo "$self :: <-- END of ENV dump"
	fi
    fi
fi


# ROOT
if (( $?DECHO )) ; then  echo "$self :: Conditional exec of rootenv.csh" fi
if (( -f $GROUP_DIR/rootenv.csh)) ; then
  source $GROUP_DIR/rootenv.csh
fi

if (( $?DECHO )) ; then   echo "$self :: Re-adjusting xxPATH for OPTSTAR and STAR_PATH" fi
if (( $?DECHO )) ; then  echo "$self :: PATH is now $PATH" fi
if (( -x ${GROUP_DIR}/dropit)) ; then
  # clean-up PATH
  export MANPATH=`${GROUP_DIR}/dropit -p ${OPTSTAR}/man -p ${MANPATH}`
  export PATH=   `${GROUP_DIR}/dropit -p ${PATH} GROUPPATH`
  export PATH=   `${GROUP_DIR}/dropit -p ${PATH} $STAR_PATH`
  export LD_LIBRARY_PATH=`${GROUP_DIR}/dropit -p "${LD_LIBRARY_PATH}" $STAR_PATH`
  if (($?SHLIB_PATH == 1)); then      export SHLIB_PATH=     `${GROUP_DIR}/dropit -p ${SHLIB_PATH} $STAR_PATH` fi

  export PATH=`${GROUP_DIR}/dropit -p ${GROUPPATH} -p /usr/afsws/bin -p /usr/afsws/etc -p ${OPTSTAR}/bin -p /usr/sue/bin -p /usr/local/bin -p ${PATH}`
else
  if (( $?DECHO )); then  echo "$self ::  ${GROUP_DIR}/dropit is not -x" fi
fi


## Put mysql on path if available
if (( -d /usr/local/mysql/bin)) ; then
  if (( -x ${GROUP_DIR}/dropit)) ; then  export PATH=`${GROUP_DIR}/dropit -p ${PATH} -p /usr/local/mysql/bin` fi
fi

if (($?MANPATH == 1)) ; then
  ##VP   export MANPATH=${MANPATH}:${STAR_PATH}/man
  export MANPATH=`${GROUP_DIR}/dropit -p ${MANPATH} -p ${STAR_PATH}/man`
else
  export MANPATH=${STAR_PATH}/man
fi


if (( $?DECHO)) ; then  echo "$self :: OS Specific tasks. Our OS=$STAR_SYS" fi
switch ($STAR_SYS)
    case "rs_aix*":
        if (( -x ${GROUP_DIR}/dropit)) ; then  export MANPATH=`${GROUP_DIR}/dropit -p {$MANPATH} -p /usr/share/man` fi
        breaksw
    case "alpha_osf32c":
	breaksw
    case "hp700_ux90":
	breaksw

    case "hp_ux102":
      if (($?SHLIB_PATH == 0)) ; then export SHLIB_PATH=; if (( -x ${GROUP_DIR}/dropit)) ; then export SHLIB_PATH `${GROUP_DIR}/dropit -p ${SHLIB_PATH} $STAR_PATH` fi fi
      if (($?MINE_lib == 1 && $?STAR_lib == 1)) ; then
        export SHLIB_PATH=`${GROUP_DIR}/dropit -p ${MINE_lib} -p ${MINE_LIB} -p ${STAR_lib} -p ${STAR_LIB} -p ${STAF_LIB} -p ${SHLIB_PATH}`
      else
	if (( -x ${GROUP_DIR}/dropit)) ; then  export SHLIB_PATH=`${GROUP_DIR}/dropit -p ${SHLIB_PATH} .${STAR_HOST_SYS}/LIB` fi
##VP         export SHLIB_PATH=${MINE_LIB}:${STAR_LIB}:${STAF_LIB}:${SHLIB_PATH}
        export SHLIB_PATH=`${GROUP_DIR}/dropit -p ${MINE_LIB} -p ${STAR_LIB} -p ${STAF_LIB} -p ${SHLIB_PATH}`
      fi
      export LD_LIBRARY_PATH=${SHLIB_PATH}
      export BFARCH=hp_ux102
      limit  coredumpsize 0
      breaksw


    case "sun4*":
      #  ====================
      # Sun/Solaris version 4
      #  ====================
      if (( ! $?SUNWS )) ; then
	if (( -r $STAR_MGR/sunWS )) ; then
	    export SUNWS=`cat $STAR_MGR/sunWS`
	    if (( ! -d /opt/$SUNWS )) ; then
		if (($ECHO)) ; then echo "$SUNWS Workshop not found. Reverting to SUNWspro"
		export SUNWS="SUNWspro"
	    fi
        else
	    # default packages distribution directory
	    export SUNWS="SUNWspro"
	fi
      fi

      if ((! $?SUNOPT)) ; then  export SUNOPT=/opt fi

      WSVERS=`echo $SUNWS  | sed "s/WS//"`   # full version number
      WSMVER=`echo $WSVERS | sed "s/\..*//"` # major version number

      if (($?LD_LIBRARY_PATH == 0)) ; then export LD_LIBRARY_PATH=      export LD_LIBRARY_PATH `${GROUP_DIR}/dropit -p /usr/openwin/lib -p /usr/dt/lib -p /usr/local/lib -p ${LD_LIBRARY_PATH}` fi


      # Rebuild path - Basic
      if (( -x ${GROUP_DIR}/dropit)) ; then
	export LD_LIBRARY_PATH=`${GROUP_DIR}/dropit -p ${LD_LIBRARY_PATH} -p $SUNOPT/$SUNWS/lib -p $SUNOPT/$SUNWS/SC$WSVERS/lib -p $SUNOPT/$SUNWS/WS$WSMVER/lib`
	export PATH=`${GROUP_DIR}/dropit -p $SUNOPT/$SUNWS/bin -p ${PATH}`
	export MANPATH=`${GROUP_DIR}/dropit -p $SUNOPT/$SUNWS/man -p ${MANPATH}`

	if (($?MINE_lib == 1 && $?STAR_lib == 1 )) ; then
	    export LD_LIBRARY_PATH=`${GROUP_DIR}/dropit -p ${MINE_lib} -p ${MINE_LIB} -p ${STAR_lib} -p ${STAR_LIB} -p ${STAF_LIB} -p ${LD_LIBRARY_PATH}`
        else
	    export LD_LIBRARY_PATH=`${GROUP_DIR}/dropit -p ${MINE_LIB} -p ${STAR_LIB} -p ${STAF_LIB} -p ${LD_LIBRARY_PATH}`
        fi
      fi

      export  BFARCH=SunOS5
      if (("${STAR_HOST_SYS}" == "sun4x_56_CC5")) ; then  export BFARCH=SunOS5_CC5 fi
      limit   coredump 0
      unlimit descriptors
      breaksw


    case "alpha_dux*":
      limit datasize unlimited
      limit stacksize unlimited
      breaksw

    case "*linux26":   # amd64_linux26 would be valid
    case "x8664_*":
    case "i386_*":
      #  ====================
      # make sure that afws is in the path
      if ((! -d /usr/afsws/bin)) ; then export PATH=`${GROUP_DIR}/dropit -p $PATH -p ${AFS_RHIC}/i386_redhat50/usr/afsws/bin` fi


      # PGI
      if (( $?redhat )) ; then
	# from SL5 onward, stop loading PGI automatically
        if (( -x "/usr/bin/bc")) ; then 
	    # not tha bc may not be installed
	    loadPGI=`echo "$redhat < 50" | /usr/bin/bc`
	    if (( $loadPGI  )) ; then
		if (( $?DECHO )) ; then  echo "$self :: RH/SL < 5.0 - will attempt to load PGI" fi

		source $GROUP_DIR/setup PGI
		if (( $?DECHO )) ; then
		    echo "$self :: PGI = $PGI"
		fi
	    fi
	    unset loadPGI
	fi
      fi
    case "x86*":

      export PATH= `${GROUP_DIR}/dropit -p $PATH  -p /usr/local/bin/ddd`
      if (($?LD_LIBRARY_PATH == 0)) ; then export LD_LIBRARY_PATH= fi

      # Final path adjustement
      if (($?MINE_lib == 1 && $?STAR_lib == 1)) ; then
        if (( -x ${GROUP_DIR}/dropit)) ; then
          export LD_LIBRARY_PATH=`${GROUP_DIR}/dropit -p ${MINE_lib} -p ${MINE_LIB} -p ${STAR_lib} -p ${STAR_LIB} -p ${STAF_LIB} -p ${LD_LIBRARY_PATH}`
        fi
      else
       if (( -x ${GROUP_DIR}/dropit)) ; then 
         export LD_LIBRARY_PATH=`${GROUP_DIR}/dropit -p ${LD_LIBRARY_PATH} -p .${STAR_HOST_SYS}/LIB`
         export LD_LIBRARY_PATH=`${GROUP_DIR}/dropit -p ${MINE_LIB} -p ${STAR_LIB} -p ${STAF_LIB} -p ${LD_LIBRARY_PATH}`
       fi
      fi
      if (( $?DECHO )) ; then  echo "LD_LIBRARY_PATH = $LD_LIBRARY_PATH" fi
      #  cygwin tcsh has no 'limit' command embedded
      if (( `echo $STAR_SYS | $GREP _nt` == "")) ; then
	limit  coredump 0
	export BFARCH=Linux2
      fi
      breaksw


    default:
	#  ====================
        if (( $?DECHO )) ; then echo "$self :: The OS was not recognized - entire setup was skipped" fi
	breaksw
endsw

if (( $?DECHO )) ; then  echo "$self :: PATH is now $PATH" fi

# ==================================================================
# Extra package support
# ==================================================================
if (( $?DECHO )) ; then echo "$self :: Extraneous packages check" fi



# Support for JAVA/JDK
if (( ! $?JAVA_ROOT )) ; then
    # Search for a default path
    foreach p ( /usr/java /usr/lib/jvm )
	if (( -d $p )) ; then
	    a = `/bin/ls $p | /usr/bin/tail -1`
	    if [[ "$a" != ""]] ; then
		if (( -d $p/$a/jre )) ; then
		    export JAVA_ROOT=$p/$a/jre
		else 
		    export JAVA_ROOT=$p/$a
		fi
		break
	    fi
	fi
    end   
    if (( ! $?JAVA_ROOT )) ; then
	if (( -d /opt/VDT )) ; then
	    a = `ls /opt/VDT | $GREP -e jdk -e j2sdk | tail -1`
	    if [[ "$a" != ""]] ; then
		export JAVA_ROOT=/opt/VDT/$a
	    fi
	fi
    fi
fi
if (( $?JAVA_ROOT )) ; then
    if (( -d $JAVA_ROOT/ )) ; then
	if (( `echo $PATH | $GREP kerberos` != "")) ; then
	    # Will need to find a better way ... java has
	    # a 'kinit'
	    path=(/usr/kerberos/bin $JAVA_ROOT/bin $path)
	else
	    path=($JAVA_ROOT/bin $path)
	fi
	if (( -d $JAVA_ROOT/man )) ; then
	    export MANPATH=${MANPATH}:$JAVA_ROOT/man
	fi
	#CLASSPATH anyone ??
    fi
fi


# Support for GraXML
#if (( ! $?GRAXML_HOME && -d ${STAR_PATH}/GeoM )) ; then
#    if (( -d ${STAR_PATH}/GeoM/${STAR_LEVEL}/GraXML )) ; then
#	export GRAXML_HOME=${STAR_PATH}/GeoM/${STAR_LEVEL}/GraXML
#    else
#	# revert to a default if exists
#	if (( -e ${STAR_PATH}/GeoM/dev/GraXML )) ; then
#	    export GRAXML_HOME=${STAR_PATH}/GeoM/dev/GraXML
#	fi
#    fi
#fi
#if (( $?GRAXML_HOME )) ; then
#    path=($path $GRAXML_HOME/bin)
#fi


# Support for subversion if installed in a sub-directory
# Will start with simple one location
if (( -d /usr/local/subversion )) ; then
    export SVNDIR=/usr/local/subversion
    path=($path $SVNDIR/bin )
    export MANPATH=${MANPATH}:$SVNDIR/man
    export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:$SVNDIR/lib
fi


# Support for Qt
#if ((! $?QTDIR )) ; then
    if (( -d $OPTSTAR/qt )) ; then
	if (( $?DECHO )) ; then echo "$self :: Defining QTDIR as OPTSTAR/qt" fi
	if (( -l $OPTSTAR/qt )) ; then
	    b = `/bin/ls -ld $OPTSTAR/qt | /usr/bin/cut -f2 -d">" | sed 's/ //g'`
	    export QTDIR=$OPTSTAR/$b
        else
	    export QTDIR=$OPTSTAR/qt
	fi

	# path=($path $QTDIR/bin)
	export MANPATH=${MANPATH}:$QTDIR/man
	export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:$QTDIR/lib
    else
	# make some more conditional logic - we assume
	# those will be soft-links form example. Qt4 will
	# take precedence over Qt3 in this scheme
	if (( $?DECHO )) ; then  echo "$self :: Checking QTDIR as OPTSTAR/qt{3|4}" fi
	if (( ! $?QTDIR && -d $OPTSTAR/qt4 )) ; then
	    export QTDIR=$OPTSTAR/qt4
	fi
	if (( ! $?QTDIR && -d $OPTSTAR/qt3 )) ; then
	    export QTDIR=$OPTSTAR/qt3
	fi
    fi
#fi
if (( $?QTDIR )) ; then
    export MANPATH=${MANPATH}:$QTDIR/man
    export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${QTDIR}/lib
    export PATH=${PATH}:${QTDIR}/bin
fi



# ==================================================================
# END
# The above setups may mess path and append without checking
# if already defined. dropit will "fix" duplicates
if (( -x ${GROUP_DIR}/dropit )) ; then
    export LD_LIBRARY_PATH=`${GROUP_DIR}/dropit -p "${LD_LIBRARY_PATH}"`
    export PATH= `${GROUP_DIR}/dropit -p "${PATH}"`
fi
# ==================================================================


if (( $?DECHO )) ; then
    echo "$self :: Final touch ..."
    echo "$self :: LD_LIBRARY_PATH -> $LD_LIBRARY_PATH"
    echo "$self :: PATH            -> $PATH"
fi

# We need this aliases even during BATCH
if ((-r $GROUP_DIR/group_aliases.csh)) ; then . $GROUP_DIR/group_aliases.csh fi

# Scratch space ... Also in star_login but defined here in case
# undefined
if (($?SCRATCH == 0)) ; then
    export SCRATCH=/tmp/$LOGNAME
fi


# User Scratch directory
if (( ! -d $SCRATCH )) ; then
    mkdir -p $SCRATCH && chmod 755 $SCRATCH
fi
if (($ECHO)) ; then echo   "Setting up SCRATCH   = $SCRATCH" fi


# Echo CERN level information
if (($?CERN_ROOT == 0 )) ; then export CERN_ROOT= $CERN/$CERN_LEVEL fi

if (($?CERN_ROOT == 1 )) ; then
    if (($ECHO)) ; then echo   "CERNLIB version "$CERN_LEVEL" has been initiated with CERN_ROOT="${CERN_ROOT} fi
fi

# CLHEP library support
if ((! $?CLHEP_BASE_DIR )) ; then
    export CLHEP_BASE_DIR=${OPTSTAR}
fi


# HP Jetprint
if ((-x ${GROUP_DIR}/dropit )) ; then
if (( -d /opt/hpnp)) ; then
  if (($ECHO)) ; then echo   "Paths set up for HP Jetprint" fi
  export MANPATH=`${GROUP_DIR}/dropit -p $MANPATH -p /opt/hpnp/man`
  export PATH=   `${GROUP_DIR}/dropit -p $PATH  -p /opt/hpnp/bin -p /opt/hpnp/admin`
fi
export PATH=`${GROUP_DIR}/dropit -p $HOME/bin -p $HOME/bin/.$STAR_HOST_SYS -p $PATH -p $CERN_ROOT/bin -p $CERN_ROOT/mgr .`
fi

# clean-up PATH
if (( $?DECHO )) ; then  echo "$self :: Paths cleanup ..." fi
#if (( -d /cern/../usr.local/lib) export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/cern/../usr.local/lib
if (( -x ${GROUP_DIR}/dropit)) ; then
    if [["$CERN_LEVEL" != "pro"]] ; then
	export PATH= `${GROUP_DIR}/dropit cern`
	export PATH=`${GROUP_DIR}/dropit -p ${PATH} -p ${CERN_ROOT}/bin`
    fi
    export PATH=`${GROUP_DIR}/dropit -p ${OPTSTAR}/bin -p ${PATH}`
    switch ($STAR_SYS)
	case "hp_ux102":
	#  ====================
	export SHLIB_PATH=`${GROUP_DIR}/dropit -p ${SHLIB_PATH} -p ${OPTSTAR}/lib`
	if (( -d ${OPTSTAR}/lib/mysql )) ; then
	    export SHLIB_PATH=`${GROUP_DIR}/dropit -p ${SHLIB_PATH} -p ${OPTSTAR}/lib/mysql`
	fi
	export SHLIB_PATH=`${GROUP_DIR}/dropit -p "$SHLIB_PATH"`
	breaksw

    default:
	#  ====================
	export LD_LIBRARY_PATH=`${GROUP_DIR}/dropit -p ${LD_LIBRARY_PATH} -p ${OPTSTAR}/lib`
	# Note from 2011/10 - Unofrtunately, MySQL has not been there for a while
	if (( -d ${OPTSTAR}/lib/mysql )) ; then
	    export LD_LIBRARY_PATH=`${GROUP_DIR}/dropit -p ${LD_LIBRARY_PATH} -p ${OPTSTAR}/lib/mysql`

	# ... but in the default system path - changing this may chang previous 
	# behavior however
	#else if (( -d /usr/lib64/mysql && $USE_64BITS == 1 )) ; then
	#    export LD_LIBRARY_PATH=`${GROUP_DIR}/dropit -p ${LD_LIBRARY_PATH} -p /usr/lib64/mysql`
	#else if (( -d /usr/lib/mysql && $USE_64BITS == 0 )) ; then
	#    export LD_LIBRARY_PATH=`${GROUP_DIR}/dropit -p ${LD_LIBRARY_PATH} -p /usr/lib/mysql `
	fi
	export LD_LIBRARY_PATH=`${GROUP_DIR}/dropit -p "$LD_LIBRARY_PATH"`
	breaksw
    endsw
    export LD_LIBRARY_PATH=`${GROUP_DIR}/dropit -p "$LD_LIBRARY_PATH /usr/lib"`
    if (($USE_64BITS)) ; then 
	export LD_LIBRARY_PATH= ${LD_LIBRARY_PATH}:/usr/lib64
    else                  
	export LD_LIBRARY_PATH= ${LD_LIBRARY_PATH}:/usr/lib
    fi
    export MANPATH=`${GROUP_DIR}/dropit -p ${MANPATH}`
    export PATH=`${GROUP_DIR}/dropit -p ${PATH} GROUPPATH`
fi












#
# Display this message as it is likely the environment is
# screwed up if this happens.
#
if (( "$OPTSTAR" == "")) ; then
    if (($ECHO)) ; then) ; then
	    echo ""
	    echo "          ########################################"
	    echo "          ########################################"
	    echo "          ##                                    ##"
	    echo "          ## /!\  OPTSTAR is undefined  /!\     ##"
	    echo "          ##                                    ##"
	    echo "          ## NO local or AFS based installation ##"
	    echo "          ##                                    ##"
	    echo "          ## You have ONLY a PARTIALLY working  ##"
	    echo "          ## STAR environment                   ##"
	    echo "          ##                                    ##"
	    echo "          ########################################"
	    echo "          ########################################"
	    echo ""

	    # turn some echo OFF now so this message is
	    # not cluttered
	    export SILENT=1
    fi
fi




if (($ECHO)) ; then
    echo "STAR setup on" `hostname` "by" `date` " has been completed"
    echo   "LD_LIBRARY_PATH = $LD_LIBRARY_PATH"
    unset ECHO
fi

# restore if previously defined
if (( $?GRPE_pself )) ; then
    self=$GRPE_pself
    unset GRPE_pself
fi
if ((-r ${HOME}/bin)) ; then  export PATH=${HOME}/bin:${PATH} fi
if ((-r ${HOME}/bin/.${STAR_HOST_SYS})) ; then  export PATH=${HOME}/bin/.${STAR_HOST_SYS}:${PATH} fi
if ((-x ${GROUP_DIR}/dropit)) ; then  export PATH=`${GROUP_DIR}/dropit` fi

#
# Uncomment to get statistics on version used at
# login level.
#
#set date="`date`"
#cat >> $GROUP_DIR/statistics/star${STAR_VERSION} <<\lndir $ROO EOD
#$USER from $HOST asked for STAR_LEVEL=$STAR_LEVEL / STAR_VERSION=$STAR_VERSION  $date
#EOD
#END


#echo "$STAR"
#echo "$LD_LIBRARY_PATH"
