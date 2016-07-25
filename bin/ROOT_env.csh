#! /usr/local/bin/tcsh -f
setenv No_Copyright ""
setenv ThisCell "Unknown"
if (-r /usr/vice/etc/ThisCell) setenv ThisCell `cat /usr/vice/etc/ThisCell`
if  ($?ATLAS_ROOT == 0) then
  if ($ThisCell == "cern.ch")         setenv ATLAS_ROOT  "/afs/$ThisCell/atlas"
  if ($ThisCell == "usatlas.bnl.gov") setenv ATLAS_ROOT  "/afs/$ThisCell"
  if ($ThisCell == "rhic.bnl.gov")    setenv ATLAS_ROOT  "/afs/usatlas.bnl.gov"
  if ($ThisCell == "psc.edu")         setenv ATLAS_ROOT  "/afs/psc.edu/usr/9/"
  if  ($?ATLAS_ROOT == 0) then 
    echo "Unknown afs cell --- exit"
    exit 1;
  endif
endif
if ( $#argv > 0 )          setenv ROOT_LEVEL $1
if ( $?ROOT_LEVEL == 0)    setenv ROOT_LEVEL 3.04.02
if ( $?ATLAS_SCRIPTS == 0) setenv ATLAS_SCRIPTS $ATLAS_ROOT/scripts
if ( $?ROOT == 0)          setenv ROOT $ATLAS_ROOT/offline/external/ROOT
if ( $?SystemName == 0) then 
setenv SystemName "Unknown"
which fs >& /dev/null
if ( $? == 0 ) then
     fs sysname | grep ':' > /dev/null
     if ($?) then
	setenv SystemName `fs sysname | awk -F\' '{print $2}'`
     else
	setenv SystemName `fs sysname | awk -F\: '{print $2}'`
     endif
else   
    set mach_os = ` uname -sr | sed -e 's/ //g' -e 's/\.//g' `
    #
    switch ($mach_os)
    case "sn6*":
	    setenv SystemName cray
	    breaksw
    case "AIX1":
	    setenv SystemName rs_aix31
	    breaksw
    case "AIX2":
	    setenv SystemName rs_aix32
	    breaksw
    case "HP-UXB1020":
	    setenv SystemName hp_ux102
	    breaksw
    case "IRIX4*":
    case "IRIX52":
	    setenv SystemName sgi_52
	    breaksw
    case "IRIX53:"
	    setenv SystemName sgi_53
	    breaksw
    case "IRIX62":
	    setenv SystemName sgi_62
	    breaksw
    case "IRIX63":
	    setenv SystemName sgi_63
	    breaksw
    case "IRIX6464":
	    setenv SystemName sgi_64
	    breaksw
    case "Linux222":
	    setenv SystemName i386_redhat51
	    breaksw
    case "Linux*":
	    setenv SystemName i386_redhat61
	    breaksw
    case "OSF1V32":
    case "OSF1V40":
	    setenv SystemName alpha_osf32c
	    breaksw
    case "OSF1*":
	    setenv SystemName osf1
	    breaksw
    case "SunOS54"
	    setenv SystemName sun4x_54
	    breaksw
    case "SunOS551"
	    setenv SystemName sun4x_55
	    breaksw
    case "SunOS5*":
	    switch ("`uname -p`")
	    case "*86":
		    setenv SystemName sunx86_55
		    breaksw
	    default:
                setenv SystemName sun4x_55
                breaksw
	    endsw
        breaksw
    default:
	    setenv SystemName unknown
	    breaksw
    endsw
endif
switch ($SystemName)
    case "i386_*":
	set RedhatRelease = `awk '{print $5}' /etc/redhat-release | awk -F. '{print $1$2;}'`
	set gccv = `gcc -dumpversion | awk -F. '{print $1$2$3}'`
	setenv SystemName rh${RedhatRelease}_gcc${gccv}
	breaksw
    default:
	breaksw
endsw
#	echo "setenv SystemName $SystemName"
endif
if (-e ${ATLAS_SCRIPTS}/dropit) then
    setenv PATH `${ATLAS_SCRIPTS}/dropit ROOT`
    if ($?LD_LIBRARY_PATH == 1) then
    setenv LD_LIBRARY_PATH `${ATLAS_SCRIPTS}/dropit -p ${LD_LIBRARY_PATH} ROOT`
    endif
endif
setenv ROOTROOT             ${ROOT}/${ROOT_LEVEL}
setenv ROOTSYS              ${ROOTROOT}/${SystemName}/root
setenv PATH                "${ROOTSYS}/bin:${PATH}"
if ($?LD_LIBRARY_PATH == 0) then
  setenv LD_LIBRARY_PATH     "${ROOTSYS}/lib"
else
  setenv LD_LIBRARY_PATH     "${ROOTSYS}/lib:${LD_LIBRARY_PATH}"
endif
if ($?NODEBUG) then
  setenv PATH                "${ROOTSYS}/BIN:${PATH}"
  setenv LD_LIBRARY_PATH     "${ROOTSYS}/LIB:${LD_LIBRARY_PATH}"
endif
