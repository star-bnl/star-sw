#  $Log: star_init.csh,v $
#  Revision 1.1.1.1  1997/12/31 14:35:23  fisyak
#
#             Last modification $Date: 1997/12/31 14:35:23 $ 
#! /bin/csh -f
#. default setings
echo $STAR_SYS
switch ($STAR_SYS)
    case "rs_aix31":
#  ====================
#   breaksw
    case "rs_aix32":
#  ====================
#   breaksw
    case "rs_aix41":
#  ====================
#	setenv CXX         xlC
	setenv FFLAGS     "-O -qextname    -qrndsngl"
	setenv F_EXTENDED  -e
	setenv LDFLAGS    "-bnoentry -bE:$*.exp import.map -bh:8 -T512 -H512"
	setenv LD_LIBS    "-lld -lxlf90 -lxlf -lm -lc"
	    setenv OPSYS AIX
    breaksw
    case "linux":
#  ====================
	setenv FFLAGS     "-w -O2 -export-dynamic -fno-second-underscore"
	setenv F_EXTENDED  -e 
	setenv LDFLAGS     -shared
	setenv LD_LIBS    "-ldl -L/usr/X11R6/lib/ -lX11 -lXm -lXt"
	    setenv OPSYS Linux
    breaksw
    case "alpha_osf32c":
#  ====================
	setenv CXX        cxx
	setenv CFLAGS     -w
	setenv FFLAGS    "-pic  -static -fpe2"
	setenv F_EXTENDED -extend_source
	setenv LDFLAGS   '-shared -expect_unresolved \"*\"'
	setenv LD_LIBS    "lUfor -lfor -lFutil -lm -lm_4sqrt -lots -lc"
	    setenv OPSYS OSF1V32
    breaksw
    case "hp700_ux90":
#  ====================
	setenv CXX        CC
	setenv CXXFLAGS  "+a1  -z +Z -g -Wl,-E"
	setenv CFLAGS    "+z  -Aa -D_HPUX_SOURCE"
	setenv FFLAGS    "+ppu +z +O2"
	setenv LDFLAGS   "-b +a1 -z"
	setenv LD_LIBS    /opt/fortran/lib/libU77.a
	setenv CC_LIBS   "-L/opt/CC/lib -lC.ansi -lcxx -lcl -lc"
    breaksw
    case "sgi_52":
#  ====================
#   breaksw
    case "sgi_53":
#  ====================
	setenv FFLAGS     "-Nn20000   -O2"
	setenv LDFLAGS     -shared
	    setenv OPSYS IRIX53
    breaksw
    case "sgi_63":
#  ====================
#   breaksw
    case "sgi_64":
#  ====================
	setenv CXX          CC
	setenv CXXFLAGS    -32
	setenv CFLAGS      -32
	setenv FFLAGS     "-32 -Nn20000 -O2"
	setenv LDFLAGS    "-32      -shared"
	setenv LD_LIBS    "-lftn -lm -lc"
	    setenv OPSYS IRIX64
    breaksw
    case "linux":
#  ====================
	setenv CXX          CC
	setenv CXXFLAGS   "-pic -D_cplusplus -t -z muldefs"
	setenv FFLAGS     "-w -pic -Nq1500 -Nl100"
	setenv F_EXTENDED  -e
	setenv LDFLAGS     -G
	setenv LD_LIBS    "-L/opt/SUNWspro/SC4.2/lib/libp -lM77 -lF77 -lsunmath"
	    setenv OPSYS Linux
    breaksw
    case "alpha_osf32c":
#  ====================
	setenv CC         cc
	setenv CFLAGS     -w
	setenv FFLAGS    "-pic  -static -fpe2"
	setenv F_EXTENDED -extend_source
	setenv LDFLAGS   '-shared -expect_unresolved \"*\"'
	setenv LD_LIBS    "lUfor -lfor -lFutil -lm -lm_4sqrt -lots -lc"
	    setenv OPSYS OSF1V32
    breaksw
    case "sun4m_54":
#  ====================
#   breaksw
    case "sun4m_55":
#  ====================
	setenv FFLAGS     "-PIC -Nl100 -Nx1000 -Nq1500"
	setenv LDFLAGS     -G
	setenv LD_LIBS     -ldl
    breaksw
    case "sunx86_55":
#  ====================
	setenv CXX          CC
	setenv CXXFLAGS   "-pic -D_cplusplus"
	setenv FFLAGS     "-w -pic -Nq1500 -Nl100"
	setenv F_EXTENDED  -e
	setenv LDFLAGS     -G
	setenv LD_LIBS    "-L/opt/SUNWspro/SC4.2/lib/libp -lM77 -lF77 -lsunmath"
	    setenv OPSYS sun4os5pc
    breaksw
    default:
#  ====================
	    setenv OPSYS UNKNOWN
    breaksw
endsw
