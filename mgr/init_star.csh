#  $Log: init_star.csh,v $
#  Revision 1.5  1998/02/17 18:06:48  fisyak
#  Add dropit for PATH
#
#  Revision 1.4  1998/02/13 14:18:20  fisyak
#  Simplify Makefile, reduce SLibrary
#
#  Revision 1.3  1998/02/12 13:35:09  fisyak
#  Add versioning, new Makefile with domain/package libraries
#
#  Revision 1.2  1998/02/10 00:06:10  fisyak
#  SL98a second version
#
#  Revision 1.1  1998/01/31 23:32:52  fisyak
#  New Environment variables
#
#  Revision 1.2  1998/01/30 12:42:16  fisyak
#  Save changes before moving to SL97b
#
#  Revision 1.1.1.1  1997/12/31 14:35:23  fisyak
#
#             Last modification $Date: 1998/02/17 18:06:48 $ 
#! /bin/csh -f
#. default setings
	setenv CC        gcc
	setenv CFLAGS   "-fpic -w"
	setenv CXX       gcc
	setenv CXXFLAGS "-fpic -w"
        setenv STAR_LD_LIBRARY_PATH ""
        setenv ARFLAGS rvu
switch ($SYS_HOST_STAR)
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
	setenv CXXFLAGS  "-w -O +a1"
	setenv CFLAGS    "+z  -Aa -D_HPUX_SOURCE"
	setenv FFLAGS    "+ppu +z +O2"
	setenv LDFLAGS   "-b +a1 -z"
	setenv LD_LIBS    /opt/fortran/lib/libU77.a
	setenv CC_LIBS   "-L/opt/CC/lib -lC.ansi -lcxx -lcl -lc"
    breaksw
    case "hp_ux102":
#  ====================
	setenv CXX        CC
	setenv CXXFLAGS  "-w +a1 -z +Z"
#       setenv CC         cc
#	setenv CFLAGS    "-w +a1 -z +z"
	setenv FFLAGS    "+ppu -K +z -w"
	setenv F_EXTENDED +es
	setenv CPPFLAGS   -Dextname
	setenv ARFLAGS     slrvu
	setenv RANLIB      /bin/true
	setenv LD         CC
	setenv LDFLAGS   "-b -z"
	setenv LD_LIBS   "/opt/fortran/lib/libU77.a  -lf"
	setenv CC_LIBS   "-L/usr/lib -L/lib -L/opt/CC/lib -lC.ansi -lcxx -lcl -lisamstub -lc"
	setenv CERN_LIBS " "
#	setenv LIBRARIES " "
    breaksw
    case "hp_ux102_aCC":
#  ====================
	setenv CXX         aCC
	setenv CXXFLAGS  "-w -z +Z"
	setenv CC         cc
	setenv CFLAGS    "-Ae -w -z +z"
	setenv FFLAGS    "+ppu +z +O2"
	setenv LDFLAGS   "-b -z"
        setenv LD         aCC
	setenv LD_LIBS    /opt/fortran/lib/libU77.a
	setenv CC_LIBS   "-lm"
	setenv CERN_LIBS " "
#	setenv LIBRARIES " "
    breaksw
    case "hp_ux102_gcc":
#  ====================
	setenv FFLAGS    "+ppu +z +O2"
	setenv LDFLAGS   "-b"
	setenv LD_LIBS    /opt/fortran/lib/libU77.a
	setenv CC_LIBS   "-L/opt/CC/lib -lC.ansi -lcxx -lcl -lc"
    breaksw
    case "sgi_52":
#  ====================
#   breaksw
    case "sgi_53":
#  ====================
	setenv FFLAGS     "-Nn20000 -static -trapuv -KPIC"
#                                                  -u
	setenv F_EXTENDED  -extend_source
	setenv CC           cc
	setenv CFLAGS     "-ansi -KPIC -kpicopt -w"
#	setenv CFLAGS     "-fpic -fPIC -w"
	setenv CXX         CC
	setenv CXXFLAGS   "-32 ${CFLAGS} -xansi -w"
#                                               -use_cfront 
	setenv ARFLAGS     slrvu
#	setenv LD          ld
	setenv LD          CC
	setenv LDFLAGS    "-32 -shared"
        setenv LD_LIBS    "-lsun -lftn -lm -lc -lPW -lmalloc"
	    setenv OPSYS IRIX53
    breaksw
    case "sgi_63":
#  ====================
#   breaksw
    case "sgi_64":
#  =============-32-=======
	setenv FFLAGS     "-32 -Nn20000 -static -trapuv -KPIC"
#                                                       -u
	setenv F_EXTENDED  -extend_source
	setenv CC           cc
	setenv CFLAGS     "-32 -ansi -KPIC -kpicopt -w"	
	setenv CXX          CC
#	setenv CXXFLAGS   "${CXXFLAGS} -32"
	setenv CXXFLAGS   "-32 ${CFLAGS} -xansi -use_cfront -w"
#	setenv CFLAGS     "${CFLAGS} -32"
	setenv LD          CC
	setenv LDFLAGS    "-32 -shared"
	setenv LD_LIBS    "-lXext -lm"
#	setenv LIBRARIES  " "
	    setenv OPSYS IRIX64_32
    breaksw
    case "sgi_64_n32":
#  =============-32-=======
	setenv CXX          CC
	setenv CXXFLAGS    -n32
	setenv CFLAGS      -n32
	setenv FFLAGS     "-n32 -Nn20000 -O2"
	setenv F_EXTENDED  -extend_source
	setenv LDFLAGS    "-n32 -shared"
	setenv LD_LIBS    "-lXext -lm"
	    setenv OPSYS IRIX64_n32
    breaksw
    case "sgi_64_64":
#  =============-32-=======
	setenv CXX          CC
	setenv CXXFLAGS    -64
	setenv CFLAGS      -64
	setenv FFLAGS     "-64 -Nn20000 -O2"
	setenv F_EXTENDED  -extend_source
	setenv LDFLAGS    "-64 -shared"
	setenv LD_LIBS    "-lXext -lm"
	    setenv OPSYS IRIX64_64
    breaksw
    case "linux":
#  ====================
	setenv CXX          CC
	setenv CXXFLAGS   "-pic -D_cplusplus -t -z muldefs"
	setenv FFLAGS     "-w -pic -Nq1500 -Nl100"
	setenv F_EXTENDED  -e
	setenv LDFLAGS    "
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
#	setenv FFLAGS     "-PIC -Nl100 -Nx1000 -Nq1500"
#	setenv LDFLAGS     -G
#	setenv LD_LIBS     -ldl
#    breaksw
    case "sun4x_55":
#  ====================
    case "sun4x_56":
#  ====================
	setenv CPPFLAGS   "-DSUN -DSOLARIS -Dsun"
	setenv FFLAGS     "-xl -PIC -w"
	setenv F_EXTENDED  -e
	setenv CC           gcc
	setenv CFLAGS     "-ansi -fpic -fPIC -w"
	setenv CXX         g++
	setenv CXXFLAGS    -w
	setenv LD          $CXX
	setenv LDFLAGS     -G
	setenv LD_LIBS    "-L/opt/SUNWspro/lib -L/opt/SUNWspro/SC4.2/lib -lM77 -lF77 -lm -lc -L/usr/ucblib -R/usr/ucblib -lucb"
#                                                                                    -lsunmath 
	setenv CC_LIBS    " -L/usr/ucblib -R/usr/ucblib -lm -ldl -lform -lmalloc"
	setenv STAR_LD_LIBRARY_PATH "/usr/openwin/lib:/vol/pub/netscape-4.0/lib:/usr/dt/lib:/opt/SUNWspro/lib"
    breaksw
    case "sunx86_55":
#  ====================
	setenv CXX          CC
	setenv CXXFLAGS   "-w -pic -D_cplusplus"
	setenv FFLAGS     "-w -pic -Nq1500 -Nl100"
	setenv F_EXTENDED  -e
	setenv LDFLAGS    "-G -t -z muldefs"
	setenv LD_LIBS    "-L/opt/SUNWspro/SC4.2/lib/libp -lM77 -lF77 -lsunmath"
	    setenv OPSYS sun4os5pc
    breaksw
    default:
#  ====================
	    setenv OPSYS UNKNOWN
    breaksw
endsw
setenv LD_LIBRARY_PATH "$STAR_LD_LIBRARY_PATH":"$LD_LIBRARY_PATH"
if ( -e $STAR/mgr/dropit ) setenv LD_LIBRARY_PATH `$STAR/mgr/dropit $LD_LIBRARY_PATH ""`
unset STAR_LD_LIBRARY_PATH




