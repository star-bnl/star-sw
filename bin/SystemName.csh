#! /usr/local/bin/tcsh -f
if ( -e `which fs` ) then
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
#	echo $RedhatRelease
#	echo `which gcc`
	switch ($RedhatRelease) 
	    case "7*":
	    case "8*":
	    case "9*":
	    case "30":
		set gccv = `gcc -dumpversion | awk -F. '{print $1$2$3}'`
		setenv SystemName rh${RedhatRelease}_gcc${gccv}
		breaksw;
	    default:
	    set gccv = `gcc --version | awk -F. '{print $1$2$3}'`     
	    switch ($gccv)
		case   "egcs-29166"
	#	    echo $SystemName
		    setenv SystemName i386_redhat61
		    breaksw
		default:
		    setenv SystemName rh${RedhatRelease}_gcc${gccv}
		    breaksw
		endsw   
		default:
		breaksw
	    endsw
	endsw
endsw
