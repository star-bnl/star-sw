#! /usr/bin/env tcsh 
#setenv STAR  STAR
setenv TABLE  TABLE
if (! $?NODEBUG) then
    set ROOTBUILD="debug"
else
    set ROOTBUILD="opt"
endif
#setenv PYTHIA /afs/rhic.bnl.gov/star/ROOT/XNew/.$STAR_HOST_SYS/root
#setenv PYTHIA6 /afs/rhic.bnl.gov/star/ROOT/XNew/.$STAR_HOST_SYS/root
#setenv VENUS /afs/rhic.bnl.gov/star/ROOT/XNew/.$STAR_HOST_SYS/root
#setenv PYTHIA8 $STAR_LIB
#setenv PYTHIA6 $STAR_LIB
#setenv VENUS $XOPTSTAR
#if (-r $ROOTSYS/bin/setxrd.csh && -d /opt/xrootd/xrootd-3.2.7) source $ROOTSYS/bin/setxrd.csh  /opt/xrootd/xrootd-3.2.7
#if (-r $ROOTSYS/bin/setxrd.csh && -d /opt/xrootd) source $ROOTSYS/bin/setxrd.csh  /opt/xrootd
#if (-r $ROOTSYS/bin/setxrd.csh && -d /opt/xrootd) source $ROOTSYS/bin/setxrd.csh  $ROOTSYS
#if (-r $XOPTSTAR/bin/setxrd.csh && -d /opt/xrootd) source $ROOTSYS/bin/setxrd.csh  $XOPTSTAR
if (-r ./bin/setxrd.csh && -d $XOPTSTAR) source ./bin/setxrd.csh  $XOPTSTAR
setenv CERN_ROOT_Local $CERN_ROOT
setenv SHIFTLIB "" #"-lshift"
setenv SHIFTLIBDIR ""#$CERN/../usr.local/lib
setenv SHIFTINCDIR ""#$CERN_ROOT/../../usr.local/include/shift
setenv x11libdir /usr/X11R6/lib
setenv xpmlibdir /usr/X11R6/lib
setenv xftlibdir /usr/X11R6/lib
setenv DISABLE "--disable-python --disable-qt --disable-qtgsi --disable-vc"
#setenv MYSQL /opt/star
#setenv MYSQLINCDIR $MYSQL/include/mysql	
#setenv MYSQLCLILIB $MYSQL/lib/mysql
	setenv MYSQLINCDIR /usr/include/mysql
	setenv MYSQLCLILIB /usr/lib/mysql
#setenv GSL_DIR $ROOTSYS
setenv GSL_DIR $XOPTSTAR # $ROOTSYS
setenv FFTW3   $ROOTSYS # $XOPTSTAR 
setenv F77 gfortran
switch ( $STAR_HOST_SYS )  
    case *x8664*gcc*:
#	setenv CERN_ROOT_Local /afs/rhic.bnl.gov/.asis/$STAR_HOST_SYS/cern/2004
	setenv ARCH linuxx8664gcc
#	setenv MYSQLINCDIR /usr/include/mysql
	setenv MYSQLCLILIB /usr/lib64/mysql
#	setenv SHIFTLIBDIR $CERN_ROOT/lib
#	setenv SHIFTINCDIR  /afs/rhic.bnl.gov/.asis/share/usr.local/include
#	setenv x11libdir /usr/X11R6/lib64
#	setenv xpmlibdir /usr/X11R6/lib64
	setenv x11libdir /usr/lib64
	setenv xpmlibdir /usr/lib64
	setenv xftlibdir /usr/lib64
    breaksw
    case *x8664*icc*:
#	setenv CERN_ROOT_Local /afs/rhic.bnl.gov/.asis/$STAR_HOST_SYS/cern/2004
	setenv ARCH linuxx8664icc
#	setenv MYSQLINCDIR /usr/include/mysql
	setenv MYSQLCLILIB /usr/lib64/mysql
#	setenv SHIFTLIBDIR $CERN_ROOT/lib
#	setenv SHIFTINCDIR  /afs/rhic.bnl.gov/.asis/share/usr.local/include
#	setenv x11libdir /usr/X11R6/lib64
#	setenv xpmlibdir /usr/X11R6/lib64
	setenv x11libdir /usr/lib64
	setenv xpmlibdir /usr/lib64
	setenv xftlibdir /usr/lib64
	setenv F77 ifort
	setenv DISABLE "$DISABLE --disable-vc --enable-cxx14"
    breaksw
    case i386*:
    case rh*gcc*:
    case sl5*gcc*:
    case sl6*gcc*:
	setenv ARCH  linux
#	setenv SHIFTLIBDIR $CERN_ROOT/lib
#	setenv SHIFTINCDIR  /afs/rhic.bnl.gov/.asis/share/usr.local/include
	setenv x11libdir /usr/lib
	setenv xpmlibdir /usr/lib
	setenv xftlibdir /usr/lib
    breaksw
    case sl*gcc*:
	setenv ARCH  linux
#	setenv MYSQLINCDIR /usr/include/mysql
#	setenv MYSQLCLILIB /usr/lib
#	setenv MYSQLINCDIR /afs/rhic/star/users/fisyak/public/sources/mysql-max-4.1.20-pc-linux-gnu-i686-icc-glibc23/include
#	setenv MYSQLCLILIB  /afs/rhic/star/users/fisyak/public/sources/mysql-max-4.1.20-pc-linux-gnu-i686-icc-glibc23/lib
# 	setenv MYSQLINCDIR  ${ROOTSYS}/mysql-4.1.20/include
#	setenv MYSQLCLILIB  ${ROOTSYS}/mysql-4.1.20/lib
#	setenv PATH         ${ROOTSYS}/mysql-4.1.20/bin:${PATH}
    breaksw
    case sl*icc*:
    case rh*icc*:
	setenv ARCH  linuxicc
# 	setenv MYSQLINCDIR /usr/include/mysql
#	setenv MYSQLCLILIB /usr/lib
#	setenv MYSQLINCDIR /afs/rhic/star/users/fisyak/public/sources/mysql-max-4.1.20-pc-linux-gnu-i686-icc-glibc23/include
#	setenv MYSQLCLILIB  /afs/rhic/star/users/fisyak/public/sources/mysql-max-4.1.20-pc-linux-gnu-i686-icc-glibc23/lib
#	setenv MYSQLINCDIR  ${ROOTSYS}/mysql-4.1.20/include
#	setenv MYSQLCLILIB  ${ROOTSYS}/mysql-4.1.20/lib
#	setenv PATH         ${ROOTSYS}/mysql-4.1.20/bin:${PATH}
#	setenv PYTHIA /afs/rhic.bnl.gov/star/ROOT/XNew/.i386_redhat61/root
#	setenv PYTHIA6 /afs/rhic.bnl.gov/star/ROOT/XNew/.i386_redhat61/root
#	setenv VENUS /afs/rhic.bnl.gov/star/ROOT/XNew/.i386_redhat61/root
#	setenv QTDIR ""
	setenv F77 ifort
	setenv x11libdir /usr/lib
	setenv xpmlibdir /usr/lib
	setenv xftlibdir /usr/lib
#	setenv GSL_DIR /home/pinkenbu/new/install.1
	setenv DISABLE "$DISABLE --disable-vc --enable-cxx14"
    breaksw
    case alpha_dux*:
	setenv ARCH  alphacxx6
	setenv XPM $XOPTSTAR
    breaksw
    case sun4x_5*:
	setenv ARCH solarisCC5
    breaksw
    case hp_ux102:
	setenv ARCH hpuxacc
	setenv XPM $ROOTSYS/lib
    breaksw
    case *darwin*:
	setenv ARCH macosx64
 	setenv MYSQLINCDIR /sw/include/mysql
	setenv MYSQLCLILIB /sw/lib
	setenv GSLDIR /sw
    breaksw
    case *x8664*:
#	setenv CERN_ROOT_Local /afs/rhic.bnl.gov/.asis/$STAR_HOST_SYS/cern/2004
	setenv ARCH linuxx8664gcc
#	setenv MYSQLINCDIR /usr/include/mysql
#	setenv MYSQLCLILIB /usr/lib64/mysql
	setenv SHIFTLIBDIR $CERN_ROOT/lib
	setenv SHIFTINCDIR  /afs/rhic.bnl.gov/.asis/share/usr.local/include
	setenv x11libdir /usr/X11R6/lib64
	setenv xpmlibdir /usr/X11R6/lib64
	setenv xftlibdir /usr/X11R6/lib64
    breaksw
    default:
     exit 1
endsw
    setenv ENABLE_CXX11 ""
    setenv EXTRA_FLAGS ""
switch ( $STAR_HOST_SYS )  
    case *gcc6*:
    case *gcc5*:
    case *gcc492*:
     setenv ENABLE_CXX11 "--enable-cxx14"
     setenv EXTRA_FLAGS " --cflags=-fdiagnostics-color=always --cxxflags=-fdiagnostics-color=always"
     setenv EXTRA_FLAGS " $EXTRA_FLAGS --cxxflags=-msse --cxxflags=-msse2 --cxxflags=-msse3 --cxxflags=-msse4.1 --cxxflags=-mssse3"
     setenv EXTRA_FLAGS " $EXTRA_FLAGS --cflags=-msse --cflags=-msse2 --cflags=-msse3 --cflags=-msse4.1 --cflags=-mssse3"
     breaksw
    case *gcc482*:
     setenv ENABLE_CXX11 "--enable-cxx11"
     setenv EXTRA_FLAGS " --cflags=-fdiagnostics-color=always --cxxflags=-fdiagnostics-color=always"
     setenv EXTRA_FLAGS " $EXTRA_FLAGS --cxxflags=-msse --cxxflags=-msse2 --cxxflags=-msse3 --cxxflags=-msse4.1 --cxxflags=-mssse3"
     setenv EXTRA_FLAGS " $EXTRA_FLAGS --cflags=-msse --cflags=-msse2 --cflags=-msse3 --cflags=-msse4.1 --cflags=-mssse3"
     breaksw
     case *x8664*icc*:
     case *icc*:
     setenv EXTRA_FLAGS " $EXTRA_FLAGS --cxxflags=-axAVX  --cflags=-axAVX" # May generate AVX,SSE4.2, SSE4.1, SSSE3, SSE3, SSE2, and SSE 
     breaksw
    case *darwin*:
      setenv ENABLE_CXX11 "--enable-cxx11"
      setenv EXTRA_FLAGS  "--cflags=-fcolor-diagnostics --cxxflags=-fcolor-diagnostics"
    breaksw
    default:
#     exit 1
    setenv EXTRA_FLAGS " --cflags=-std=c++0x"
 endsw  
echo "EXTRA_FLAGS = $EXTRA_FLAGS"
#setenv libdir \$\(LD_LIBRARY_PATH\)
#setenv macrodir \$HOME/macros:./StRoot/macros:./StRoot/macros/graphics:./StRoot/macros/analysis:./StRoot/macros/test:./StRoot/macros/examples:./StRoot/macros/html:./StRoot/macros/qa:./StRoot/macros/embedding:\$\(STAR\)/StRoot/macros:\$\(STAR\)/StRoot/macros/graphics:\$\(STAR\)/StRoot/macros/analysis:\$\(STAR\)/StRoot/macros/test:\$\(STAR\)/StRoot/macros/examples:\$\(STAR\)/StRoot/macros/html:\$\(STAR\)/StRoot/macros/qa:\$\(STAR\)/StRoot/macros/embedding:\$\(ROOTSYS\)/macros:\$\(ROOTSYS\)/tutorials
#setenv plugindir \$STAR/plugins
./configure $ARCH --build=debug\
    --build=$ROOTBUILD \
    --with-thread-libdir=/usr/lib \
    --with-afs=/usr/awsfs/lib \
    --with-x11-libdir=$x11libdir \
    --with-xpm-libdir=$xpmlibdir \
    --with-gsl-incdir=$GSL_DIR/include \
    --with-gsl-libdir=$GSL_DIR/lib \
    --with-pythia6-libdir=$STAR_LIB \
    --with-pythia8-libdir=$STAR_LIB \
    --with-pythia8-incdir=$STAR/pams/gen/pythia8/include \
    --with-llvm-config=$ROOTSYS/../../../.sl64_gcc447/tools \
    --with-f77=$F77 \
    --with-mysql-incdir=$MYSQLINCDIR --with-mysql-libdir=$MYSQLCLILIB \
    --with-fftw3-incdir=$XOPTSTAR/include --with-fftw3-libdir=$XOPTSTAR/lib \
    --all \
    --disable-python \
    $DISABLE \
    $ENABLE_CXX11 $EXTRA_FLAGS
#    --with-xrootd=$XOPTSTAR \
#unsetenv libdir 
#unsetenv macrodir
#unsetenv plugindir
