if ("${OPTSTAR}" == "${XOPTSTAR}") then
  setenv XOPTSTAR ${OPTSTAR}/${STAR_HOST_SYS}
  if (! -d ${XOPTSTAR}) mkdir -p ${XOPTSTAR}
  if (! -d ${XOPTSTAR}/qt4) mkdir -p ${XOPTSTAR}/qt4
  setenv QTDIR  ${XOPTSTAR}/qt4
  starver .DEV2
endif
setenv FORCE_32BITS FALSE
setenv CC  "gcc"
setenv CXX "g++"
setenv F77 "gfortran"
switch (${STAR_HOST_SYS})
  case "*x8664*":
        setenv CC       "gcc -m64"
        setenv CXX      "g++ -m64"
        setenv F77      "gfortran -m64"
	setenv cflags   "-m64 -fPIC"
	setenv cxxflags "-m64 -fPIC"
	setenv LDFLAGS  "-m64"
	setenv FCFLAGS  "-m64 -fPIC"
        setenv arch     "x86_64"
	setenv bits     "64b"
     breaksw
  default:
        setenv FORCE_32BITS TRUE
        setenv CC       "gcc -m32"
        setenv CXX      "g++ -m32"
        setenv F77      "gfortran -m32"
	setenv cflags   "-m32 -fPIC"
	setenv cxxflags "-m32 -fPIC"
	setenv LDFLAGS  "-m32"
	setenv FCFLAGS  "-m32 -fPIC"
        setenv arch     "i386"
	setenv bits     "32b"
     breaksw
endsw
setenv CXXFLAGSD "$cxxflags"
setenv CFLAGSD   "$cflags"
setenv LDFLAGS `root-config --ldflags` 
#setenv CFLAGS  "" #"`root-config --auxcflags` -fPIC -pthread"
#setenv cflags "`root-config --auxcflags` -fPIC -pthread"
setenv cflags "`root-config --cflags` -fPIC -pthread"
setenv cxxflags "$cflags"
setenv FCFLAGS  "$cflags"
setenv CC  "`root-config --cc`"
setenv CXX "`root-config --cxx` $LDFLAGS -fPIC "
setenv F77 "`root-config --f77` $LDFLAGS -fPIC"
setenv PERL_EXT_CFLAGS   "$cflags"
setenv PERL_EXT_CPPFLAGS "$cxxflags"
setenv ARCH "`root-config --arch`"
switch (${STAR_HOST_SYS})
  case "*icc*"
        setenv CC  icc
        setenv CXX icpc
        setenv F77 ifort
        setenv FC  ifort
     breaksw
  default:
        setenv F77 gfortran
        setenv FC gfortran
endsw
# 
#set list = "libtool cmake-3.10.0-rc1 apr-1.5.2 apr-util-1.5.4 apache-log4cxx-0.10.0.CVS  fastjet-3.0.3 fftw-3.3.5  texinfo-6.3  gsl   Python-2.7.12 pyparsing-1.5.7 xrootd-4.6.1 Coin-3.1.3 qt-everywhere-opensource-src-4.8.7 pythia6 pythia8230 eigen3 mercurial-4.4-rc coin soqt Coin3D-simage-cf953eacd849 Coin3D-soqt-483ecb26b30c boost_1_66_0";   Python-2.7.12 
#set list = "cmake-3.11.4 apr-1.5.2 apr-util-1.5.4 apache-log4cxx-0.10.0.CVS  fastjet-3.0.3 fftw-3.3.5  texinfo-6.3  gsl-2.1 pyparsing-1.5.7 xrootd-4.6.1 Coin-3.1.3 qt-everywhere-opensource-src-4.8.7 pythia6 pythia8230 eigen3 mercurial-4.4-rc coin soqt Coin3D-simage-cf953eacd849 Coin3D-soqt-483ecb26b30c boost_1_66_0";
#set list = "cmake-3.11.4 apr-1.5.2 apr-util-1.5.4 apache-log4cxx-0.10.0.CVS  fastjet-3.0.3 fftw-3.3.5  texinfo-6.3  gsl-2.1 pyparsing-1.5.7 xrootd-4.6.1 Coin-3.1.3 qt-everywhere-opensource-src-4.8.7 pythia6 pythia8235 eigen3 mercurial-4.4-rc coin soqt Coin3D-simage-cf953eacd849 Coin3D-soqt-483ecb26b30c boost_1_66_0";
#set list = "cmake-3.13.4 apr-1.5.2 apr-util-1.5.4 apache-log4cxx-0.10.0.CVS  fastjet-3.0.3 fftw-3.3.5  texinfo-6.3  gsl-2.1 Python-2.7.12 pyparsing-1.5.7 xrootd-4.6.1 Coin-3.1.3 qt-everywhere-opensource-src-4.8.7 pythia6 pythia8235 eigen3 mercurial-4.4-rc coin soqt Coin3D-simage-cf953eacd849 Coin3D-soqt-483ecb26b30c boost_1_66_0";
#set list = "cmake-3.14.5 apr-1.5.2 apr-util-1.5.4 apache-log4cxx-0.10.0.CVS  fastjet-3.0.3 fftw-3.3.5  texinfo-6.3  gsl-2.1 Python-2.7.12 pyparsing-1.5.7 xrootd-4.9.0-rc3 Coin-3.1.3 qt-everywhere-opensource-src-4.8.7 pythia6 pythia8235 eigen3 mercurial-4.4-rc coin soqt Coin3D-simage-cf953eacd849 Coin3D-soqt-483ecb26b30c boost_1_66_0 VecGeom";
#set list = "cmake-3.14.5 apr-1.5.2 apr-util-1.5.4 apache-log4cxx-0.10.0.CVS  fastjet-3.0.3 fftw-3.3.5  texinfo-6.3  gsl-2.1 pyparsing-1.5.7 xrootd-4.9.0-rc3 Coin-3.1.3 qt-everywhere-opensource-src-4.8.7 pythia6 pythia8235 eigen3 mercurial-4.4-rc coin soqt Coin3D-simage-cf953eacd849 Coin3D-soqt-483ecb26b30c boost_1_66_0 VecGeom";
#set list = "cmake-3.14.5 apr-1.5.2 apr-util-1.5.4 apache-log4cxx-0.10.0.CVS  fastjet-3.0.3 fftw-3.3.5  texinfo-6.3  gsl-2.1 pyparsing-1.5.7 xrootd-4.9.0-rc3 Coin-3.1.3 qt-everywhere-opensource-src-4.8.7 pythia6 pythia8243  eigen3 mercurial-4.4-rc coin soqt Coin3D-simage-cf953eacd849 Coin3D-soqt-483ecb26b30c boost_1_66_0 VecGeom node-v10.16.0";
#set list = "Python-2.7.12 cmake-3.14.5 apr-1.5.2 apr-util-1.5.4 apache-log4cxx-0.10.0.CVS  fastjet-3.0.3 fftw-3.3.5  texinfo-6.3  gsl-2.1 pyparsing-1.5.7  xrootd-4.10.0-rc5 Coin-3.1.3 qt-everywhere-opensource-src-4.8.7 pythia6 pythia8243  eigen3 mercurial-4.4-rc coin soqt Coin3D-simage-cf953eacd849 Coin3D-soqt-483ecb26b30c boost_1_66_0 VecGeom node-v10.16.0";
#set list = "cmake-3.14.5 apr-1.5.2 apr-util-1.5.4 apache-log4cxx-0.10.0.CVS  fastjet-3.0.3 fftw-3.3.5  texinfo-6.3  gsl pyparsing-1.5.7 xrootd-4.10.0-rc5  Coin-3.1.3 qt-everywhere-opensource-src-4.8.7 pythia6 pythia8243  eigen3 mercurial-4.4-rc coin soqt Coin3D-simage-cf953eacd849 Coin3D-soqt-483ecb26b30c boost_1_66_0 VecGeom node-v10.16.0";
#set list = "cmake-3.14.5 apr-1.5.2 apr-util-1.5.4 apache-log4cxx-0.10.0.CVS  fastjet-3.0.3 fftw-3.3.5  texinfo-6.3  gsl  Python-2.7.12 pyparsing-1.5.7 xrootd-4.10.0-rc5  Coin-3.1.3 qt-everywhere-opensource-src-4.8.7 pythia6 pythia8243  eigen3 mercurial-4.4-rc coin soqt Coin3D-simage-cf953eacd849 Coin3D-soqt-483ecb26b30c boost_1_66_0 veccore VecGeom node-v10.16.0 ";
#set list = "cmake-3.14.5 apr-1.5.2 apr-util-1.5.4 apache-log4cxx-0.10.0.CVS  fastjet-3.0.3 fftw-3.3.5  texinfo-6.3  gsl pyparsing-1.5.7 xrootd-4.10.0-rc5  Coin-3.1.3 qt-everywhere-opensource-src-4.8.7 pythia6 pythia8301  eigen3 mercurial-4.4-rc coin soqt Coin3D-simage-cf953eacd849 Coin3D-soqt-483ecb26b30c boost_1_66_0 veccore VecGeom node-v10.16.0 "; # pythia8301 cannot be done with cint
set list = "cmake-3.14.5 apr-1.5.2 apr-util-1.5.4 apache-log4cxx-0.10.0.CVS  fastjet-3.0.3 fftw-3.3.5  texinfo-6.3  gsl  Python-2.7.12 pyparsing-1.5.7 xrootd-4.10.0-rc5  Coin-3.1.3 qt-everywhere-opensource-src-4.8.7 pythia6 pythia8243  eigen3 mercurial-5.2 coin soqt Coin3D-simage-cf953eacd849 Coin3D-soqt-483ecb26b30c boost_1_66_0 veccore VecGeom node-v10.16.0 tbb cfitsio-3.49";

#set list = "gsl-2.1";
#set list = "boost_1_66_0";
#set list = "Coin3D-simage-cf953eacd849";
#set list = "coin"
#eigen-eigen-10219c95fe65";
#set list = "pythia8226"
#set list = "pythia6"
#set list = "eigen3"
#set list = "qt-everywhere-opensource-src-4.8.6"
#if ($#argv != 0) set list = $argv[1];
setenv DIR ~/sources/.${STAR_HOST_SYS}
if ($?NODEBUG) setenv DIR ~/sources/.${STAR_HOST_SYS}_opt
if (! -d ${DIR}) mkdir ${DIR}
foreach pkg ($list) 
    rehash
    setenv CXXFLAGS "${cxxflags}"
    setenv CFLAGS   "${cflags}"
    cd $DIR
    rehash 
setenv CXXFLAGSd "$cxxflags"
setenv CFLAGSd   "$cflags"
#    source ${GROUP_DIR}/.starver ${STAR_LEVEL}
    if ( -r ${pkg}.Done || -r ${pkg}.Failed) continue
#    if (! -r ${pkg}) then
      if ($pkg != "xrootd-4.4.1" && $pkg != "xrootd-4.5.0-rc1" && $pkg != "Coin-3.1.3" && $pkg != "eigen3" && $pkg != "VecGeom" && pkg != "tbb") then
        if (-r ~/sources/${pkg}) then
          dirsync  ~/sources/${pkg} ${pkg}
        else 
          if (-r ~/sources/${pkg}.tar.gz) then
            gunzip ~/sources/${pkg}.tar.gz
            tar xf ~/sources/${pkg}.tar
	    mv ${pkg} ../
          else
            if (-r ~/sources/${pkg}.tar) then
              tar xf ~/sources/${pkg}.tar
	      mv ${pkg} ../
            else
              if (-r ~/sources/${pkg}.tz) then
                tar xfz ~/sources/${pkg}.tz
         	mv ${pkg} ../
              endif
            endif
          endif
	  dirsync  ../${pkg} ${pkg}
        endif
      else 
        mkdir ${pkg}
      endif
#    endif
    cd ${pkg}
    switch ($pkg)
      case "libtools*":
      case "cfitsio*":
       ./bootstrap
       ./configure --prefix=${XOPTSTAR}
       make install
       if ( $?) break;
        touch ../${pkg}.Done
	breaksw
      case "VecGeom*":
      case "veccore":
      case "tbb":
#      case "pythia83*":
	cmake -DCMAKE_INSTALL_PREFIX=${XOPTSTAR} ../../${pkg}
	if ( $?) break;
	make install
        if ( $?) break;
        touch ../${pkg}.Done
	breaksw
      case "eigen*": 
	cmake -DCMAKE_INSTALL_PREFIX=${XOPTSTAR} ../../${pkg}
	if ( $?) break;
	make install
        if ( $?) break;
#	make blas
#        if ( $?) break;
#	make check
#        if ( $?) break;
        touch ../${pkg}.Done
	breaksw
      case "xrootd*":
# has problem with gcc 4.8.2
          cmake ../../${pkg} -DCMAKE_FORCE_32BITS=${FORCE_32BITS} -DCMAKE_INSTALL_PREFIX=${XOPTSTAR} -DENABLE_PERL=FALSE -DCMAKE_INSTALL_LIBDIR=lib -DCMAKE_SHARED_LINKER_FLAGS=$LDFLAGS
	  make 
          if ( $?) break;
	  make install
          if ( $?) break;
          touch ../${pkg}.Done
	  breaksw
      case "cmake*":
	./bootstrap
          ./configure --prefix=${XOPTSTAR}
          make install
          if ( $?) break;
          touch ../${pkg}.Done
          breaksw
      case "apr-util*":
          ./configure --prefix=${XOPTSTAR} --with-apr=${XOPTSTAR}
          make install
          if ( $?) break;
          touch ../${pkg}.Done
          breaksw
      case "node*":
          ./configure --prefix=$XOPTSTAR 
          make install
          if ( $?) break;
          touch ../${pkg}.Done
          breaksw
      case "apache-log4cxx-0.10.0.CVS":
          dirsync  ~/sources/${pkg} .
#	  unsetenv CXXFLAGS  # ${CXXFLAGS} -Wnonarrowing"
#	  unsetenv CFLAGS    # ${CFLAGS} -Wnonarrowing"
#          ./configure --prefix=$XOPTSTAR --with-apr=$XOPTSTAR --with-apr-util=$XOPTSTAR --disable-libtool --with-tags=$LDFLAGS
          ./configure --prefix=$XOPTSTAR --with-apr=$XOPTSTAR --with-apr-util=$XOPTSTAR 
##	  make clean
          make
	  if ( $?) break;
          make install
          if ( $?) break;
          touch ../${pkg}.Done
          breaksw
      case "pyparsing-1.5.7":
          ${XOPTSTAR}/bin/python setup.py install
	  if ( $?) break;
          touch ../${pkg}.Done
          breaksw
     case "gsl":
#	setenv PREFIX $XOPTSTAR
#	libtoolize --force
#	aclocal
#	autoheader
#	automake --force-missing --add-missing
#	autoconf
      sh -x ./autogen.sh
#      if (! -r ./doc/version.texi) then
#    echo "set UPDATED $(date +'%d %B %Y')" > doc/version.texi 
#    echo "set UPDATED-MONTH $(date +'%B %Y')" >> doc/version.texi 
#    echo "set EDITION $(PACKAGE_VERSION)" >> doc/version.texi 
#    echo "set VERSION $(PACKAGE_VERSION)" >> doc/version.texi       
#      endif
      case "gsl*":
#          ./configure -arch "$arch" --prefix=PREFIX=$XOPTSTAR
          ./configure --prefix=$XOPTSTAR --enable-maintainer-mode
          make
	  if ( $?) break;
          make install
	  if ( $?) break;
          touch ../${pkg}.Done
          breaksw
      case "texinfo*":
          ./configure --prefix=$XOPTSTAR --disable-rpath --enable-maintainer-mode
           make
	  if ( $?) break;
           make install
	  if ( $?) break;
          touch ../${pkg}.Done
           breaksw
      case "Coin-3.1.3":
#           ../../${pkg}/${bits}/configure --prefix=$XOPTSTAR
           ../../${pkg}/configure --prefix=$XOPTSTAR
	   make
	   make install
           touch ../${pkg}.Done
           breaksw
      case "Python*":
	setenv CXXFLAGS "${CXXFLAGSD}"
	setenv CFLAGS   "${CFLAGSD}"
          ./configure --prefix=$XOPTSTAR --with-libs='-lpthread'
# --with-pth  --enable-shared
	  make -j 4
          make install
          touch ../${pkg}.Done
          breaksw
      case "qt*":
      setenv CXXFLAGS "${CXXFLAGSD}"
      setenv CFLAGS   "${CFLAGSD}"
#          if (! -d ${XOPTSTAR}/qt4) mkdir -p ${XOPTSTAR}/qt4
          ./configure --prefix=$QTDIR -no-glib -no-qt3support -no-cups <<EOF
o
yes
EOF
#          ./configure --prefix=$XOPTSTAR/qt4 -no-glib -opensource <<EOF
#          ./configure -prefix $XOPTSTAR/qt4 -no-glib -opensource -continue  <<EOF
#yes
#EOF
          setenv PATH ./bin:$PATH
	  qmake
          cp -rp bin $QTDIR
          cp -rp mkspecs $QTDIR
          make install
          cp -rp bin $QTDIR
          cp -rp mkspecs $QTDIR
          touch ../${pkg}.Done
	  breaksw
      case "pythia6":
          csh -x makePythia6.${ARCH}
          if ( $?) break;
	  cp libPythia6* ${XOPTSTAR}/lib
          if ( $?) break;
          touch ../${pkg}.Done
          breaksw
      case "pythia8*":
          ./configure --prefix=$XOPTSTAR --enable-64bit  --enable-shared #--cxx-common=\'$CFLAGS\' 
	  make install
          if ( $?) break;
          touch ../${pkg}.Done
	  breaksw
      case "fftw*":
          ./configure --prefix=$XOPTSTAR --with-pic
          make install
          if ( $?) break;
          touch ../${pkg}.Done
	  breaksw
      case "mercurial*":
          make PREFIX=$XOPTSTAR install
          if ( $?) break;
          touch ../${pkg}.Done
	  breaksw
      case "boost*":
	  bootstrap.sh --prefix=${XOPTSTAR}
          if ( $?) break;
	  ./b2 install
          if ( $?) break;
          touch ../${pkg}.Done
	  breaksw
      case "Coin3D-soqt*":
          cd src/Inventor/Qt
	  ln -s  ../../../../soqt/src/Inventor/Qt/common .
          cd -
      case "apr-1.5.1":
      default:
          ./configure --prefix=$XOPTSTAR
          make install
          if ( $?) break;
          touch ../${pkg}.Done
          breaksw
     endsw
  endif 
end
