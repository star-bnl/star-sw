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
#setenv DISABLE "--disable-qt --disable-qtgsi --disable-vc"
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
    case sl7*gcc*:
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
    setenv EXTRA_FLAGS " --cflags=-fdiagnostics-color=always --cxxflags=-fdiagnostics-color=always"
switch ( $STAR_HOST_SYS )  
    case *AVX*:
	setenv EXTRA_FLAGS " $EXTRA_FLAGS --cflags=-mavx";
	breaksw
    default:
	setenv EXTRA_FLAGS " $EXTRA_FLAGS --cflags=-msse4.2";
        breaksw
endsw
switch ( $STAR_HOST_SYS )  
    case *gcc9*:
    case *gcc8*:
    case *gcc7*:
     setenv ENABLE_CXX11 "--enable-cxx17"
     setenv EXTRA_FLAGS " $EXTRA_FLAGS" # --cflags=--std=c++17"
     breaksw
    case *_x8664_gcc6*:
     setenv ENABLE_CXX11 "--enable-cxx11"
     setenv EXTRA_FLAGS " $EXTRA_FLAGS --cflags=--std=c++14"
     breaksw
    case *gcc6*:
     setenv ENABLE_CXX11 "--enable-cxx11"
     setenv EXTRA_FLAGS " $EXTRA_FLAGS --cflags=--std=c++14"
     breaksw
    case *_x8664_gcc5*:
    case *_x8664_gcc492*:
     setenv ENABLE_CXX11 "--enable-cxx11"
     setenv EXTRA_FLAGS " $EXTRA_FLAGS --cflags=--std=c++14"
     breaksw
    case *gcc492*:
     setenv ENABLE_CXX11 "--enable-cxx11"
    case *gcc5*:
     setenv EXTRA_FLAGS " $EXTRA_FLAGS --cflags=--std=c++14"
     breaksw
    case *_x8664_gcc48*:
     setenv ENABLE_CXX11 "--enable-cxx11"
     setenv EXTRA_FLAGS " $EXTRA_FLAGS --cflags=--std=c++11"
     breaksw
    case *gcc48*:
     setenv ENABLE_CXX11 "--enable-cxx11"
     setenv EXTRA_FLAGS " $EXTRA_FLAGS --cflags=--std=c++11"
     breaksw
     case *gcc447*:
     setenv ENABLE_CXX11 ""
     setenv EXTRA_FLAGS ""
     setenv DISABLE "--disable-python --disable-qt --disable-qtgsi --enable-vc"
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
#./configure $ARCH --build=debug\
#    --build=$ROOTBUILD \
#    --with-thread-libdir=/usr/lib \
#    --with-afs=/usr/awsfs/lib \
#    --with-x11-libdir=$x11libdir \
#    --with-xpm-libdir=$xpmlibdir \
#    --with-gsl-incdir=$GSL_DIR/include \
#    --with-gsl-libdir=$GSL_DIR/lib \
#    --with-pythia6-libdir=$XOPTSTAR/lib \
#    --with-llvm-config=$ROOTSYS/../../../.sl64_gcc447/tools \
#    --with-f77=$F77 \
#    --with-mysql-incdir=$MYSQLINCDIR --with-mysql-libdir=$MYSQLCLILIB \
#    --with-fftw3-incdir=$XOPTSTAR/include --with-fftw3-libdir=$XOPTSTAR/lib \
#    --enable-builtin_ftgl       \
#    --enable-builtin_freetype   \
#    --enable-builtin_glew       \
#    --enable-builtin_pcre       \
#    --enable-builtin_zlib       \
#    --enable-builtin_lzma       \
#    --all \
#    --disable-python \
#    $DISABLE \
#    $ENABLE_CXX11 $EXTRA_FLAGS
#    --with-xrootd=$XOPTSTAR \
#setenv plugindir \$STAR/plugins
./configure $ARCH \
    --build=$ROOTBUILD \
    --enable-builtin_ftgl       \
    --enable-builtin_freetype   \
    --enable-builtin_glew       \
    --enable-builtin_pcre       \
    --enable-builtin_zlib       \
    --enable-builtin_lzma       \
    --with-gsl-incdir=$GSL_DIR/include \
    --with-gsl-libdir=$GSL_DIR/lib \
    --with-pythia6-libdir=$XOPTSTAR/lib \
    --with-pythia8-libdir=$XOPTSTAR/lib \
    --with-pythia8-incdir=$XOPTSTAR/include \
    --with-mysql-incdir=$MYSQLINCDIR --with-mysql-libdir=$MYSQLCLILIB \
    --with-fftw3-incdir=$XOPTSTAR/include --with-fftw3-libdir=$XOPTSTAR/lib \
    --with-xrootd=$XOPTSTAR \
    --with-f77=$F77 \
    --all \
    $DISABLE \
    $ENABLE_CXX11 $EXTRA_FLAGS
#unsetenv libdir 
#unsetenv macrodir
#unsetenv plugindir
#    --enable-builtin_afterimage \
#    --enable-builtin_lz4        \
