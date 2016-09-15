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
#setenv VENUS $OPTSTAR
#if (-r $ROOTSYS/bin/setxrd.csh && -d /opt/xrootd/xrootd-3.2.7) source $ROOTSYS/bin/setxrd.csh  /opt/xrootd/xrootd-3.2.7
#if (-r $ROOTSYS/bin/setxrd.csh && -d /opt/xrootd) source $ROOTSYS/bin/setxrd.csh  /opt/xrootd
if (-r $ROOTSYS/bin/setxrd.csh && -d /opt/xrootd) source $ROOTSYS/bin/setxrd.csh  $ROOTSYS
setenv CERN_ROOT_Local $CERN_ROOT
setenv SHIFTLIB "" #"-lshift"
setenv SHIFTLIBDIR ""#$CERN/../usr.local/lib
setenv SHIFTINCDIR ""#$CERN_ROOT/../../usr.local/include/shift
setenv x11libdir /usr/X11R6/lib
setenv xpmlibdir /usr/X11R6/lib
setenv xftlibdir /usr/X11R6/lib
#setenv MYSQL /opt/star
#setenv MYSQLINCDIR $MYSQL/include/mysql	
#setenv MYSQLCLILIB $MYSQL/lib/mysql
#setenv GSL_DIR $ROOTSYS
setenv GSL_DIR $OPTSTAR # $ROOTSYS
setenv FFTW3   $ROOTSYS # $OPTSTAR 
setenv F77 gfortran
switch ( $STAR_HOST_SYS )  
    case *x8664*:
#	setenv CERN_ROOT_Local /afs/rhic.bnl.gov/.asis/$STAR_HOST_SYS/cern/2004
	setenv ARCH linuxx8664gcc
	setenv MYSQLINCDIR /usr/include/mysql
	setenv MYSQLCLILIB /usr/lib64/mysql
	setenv SHIFTLIBDIR $CERN_ROOT/lib
	setenv SHIFTINCDIR  /afs/rhic.bnl.gov/.asis/share/usr.local/include
#	setenv x11libdir /usr/X11R6/lib64
#	setenv xpmlibdir /usr/X11R6/lib64
	setenv x11libdir /usr/lib64
	setenv xpmlibdir /usr/lib64
	setenv xftlibdir /usr/lib64
    breaksw
    case i386*:
    case rh*gcc*:
    case sl5*gcc*:
    case sl6*gcc*:
	setenv ARCH  linux
	setenv MYSQLINCDIR /usr/include/mysql
	setenv MYSQLCLILIB /usr/lib/mysql
	setenv SHIFTLIBDIR $CERN_ROOT/lib
	setenv SHIFTINCDIR  /afs/rhic.bnl.gov/.asis/share/usr.local/include
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
	setenv F77 g77
    breaksw
    case sl*icc*:
    case rh*icc*:
	setenv ARCH  linuxicc
 	setenv MYSQLINCDIR /usr/include/mysql
	setenv MYSQLCLILIB /usr/lib
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
	setenv GSL_DIR /home/pinkenbu/new/install.1
    breaksw
    case alpha_dux*:
	setenv ARCH  alphacxx6
	setenv XPM $OPTSTAR
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
    case *gcc492*:
    case *gcc482*:
     setenv ENABLE_CXX11 "--enable-cxx11"
     setenv EXTRA_FLAGS " --cflags=-fdiagnostics-color=always --cxxflags=-fdiagnostics-color=always"
     breaksw
    case *darwin*:
      setenv ENABLE_CXX11 "--enable-cxx11"
      setenv EXTRA_FLAGS  "--cflags=-fcolor-diagnostics"
    breaksw
    default:
#     exit 1
    setenv EXTRA_FLAGS " --cflags=-std=c++0x"
 endsw  
./configure $ARCH --build=debug\
    --build=$ROOTBUILD \
    --with-thread-libdir=/usr/lib \
    --with-afs=/usr/awsfs/lib \
    --with-x11-libdir=$x11libdir \
    --with-xpm-libdir=$xpmlibdir \
    --with-gsl-incdir=$GSL_DIR/include \
    --with-gsl-libdir=$GSL_DIR/lib \
    --with-pythia6-libdir=$STAR_LIB \
    --with-llvm-config=$ROOTSYS/../../../.sl64_gcc447/tools \
    --with-f77=$F77 \
    --with-mysql-incdir=$MYSQLINCDIR --with-mysql-libdir=$MYSQLCLILIB \
    --all \
    --disable-python \
    --disable-qt --disable-qtgsi \
    $ENABLE_CXX11 $EXTRA_FLAGS
#    --with-xrootd=$OPTSTAR \
