if ("$OPTSTAR" == "$XOPTSTAR") then
  setenv XOPTSTAR ${OPTSTAR}/${STAR_HOST_SYS}
  if (! -d $XOPTSTAR) mkdir -p $XOPTSTAR
endif
setenv FORCE_32BITS FALSE
setenv CC  "gcc"
setenv CXX "g++"
setenv F77 "gfortran"
switch (${STAR_HOST_SYS})
  case "*x8664*":
#        setenv CC       "gcc -m64"
#        setenv CXX      "g++ -m64"
#        setenv F77      "gfortran -m64"
#	setenv CFLAGS   "-m64 -fPIC"
#	setenv CXXFLAGS "-m64 -fPIC"
#	setenv LDFLAGS  "-m64"
#	setenv FCFLAGS  "-m64 -fPIC"
        setenv arch     "x86_64"
	setenv bits     "64b"
     breaksw
  default:
        setenv FORCE_32BITS TRUE
#        setenv CC       "gcc -m32"
#        setenv CXX      "g++ -m32"
#        setenv F77      "gfortran -m32"
#	setenv CFLAGS   "-m32 -fPIC"
#	setenv CXXFLAGS "-m32 -fPIC"
#	setenv LDFLAGS  "-m32"
#	setenv FCFLAGS  "-m32 -fPIC"
        setenv arch     "i386"
	setenv bits     "32b"
     breaksw
endsw
setenv LDFLAGS `root-config --ldflags` 
setenv CFLAGS  "`root-config --auxcflags` -fPIC -pthread"
setenv CXXFLAGS "$CFLAGS"
setenv FCFLAGS  "$CFLAGS"
setenv CC  "`root-config --cc`"
setenv CXX "`root-config --cxx` $LDFLAGS -fPIC "
setenv F77 "`root-config --f77` $LDFLAGS -fPIC"
setenv PERL_EXT_CFLAGS   "$CFLAGS"
setenv PERL_EXT_CPPFLAGS "$CXXFLAGS"

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
set list = "apr-1.5.2 apr-util-1.5.4 apache-log4cxx-0.10.0.CVS  fastjet-3.0.3 fftw-3.3.5  texinfo-6.3  gsl   Python-2.7.12 pyparsing-1.5.7 xrootd-4.4.1 Coin-3.1.3 qt-everywhere-opensource-src-4.8.6 pythia8226 ";
#if ($#argv != 0) set list = $argv[1];
foreach pkg ($list) 
    cd ~/sources/.${STAR_HOST_SYS}
#    source ${GROUP_DIR}/.starver ${STAR_LEVEL}
    if ( -r ${pkg}.Done) continue
    if (! -r ${pkg}) then
      if ($pkg != "xrootd-4.4.1" && $pkg != "xrootd-4.5.0-rc1" && $pkg != "Coin-3.1.3") then
        if (-r ~/sources/${pkg}) then
          dirsync  ~/sources/${pkg} ${pkg}
        else 
          if (-r ~/sources/${pkg}.tar.gz) then
            gunzip ~/sources/${pkg}.tar.gz
            tar xf ~/sources/${pkg}.tar
          else
            if (-r ~/sources/${pkg}.tar) then
              tar xf ~/sources/${pkg}.tar
            else
              if (-r ~/sources/${pkg}.tz) then
                tar xfz ~/sources/${pkg}.tz
              else
                break;
              endif
            endif
          endif
        endif
      else 
        mkdir ${pkg}
      endif
    endif
    cd ${pkg}
    switch ($pkg)
      case "xrootd*":
# has problem with gcc 4.8.2
          cmake ../../${pkg} -DCMAKE_FORCE_32BITS=${FORCE_32BITS} -DCMAKE_INSTALL_PREFIX=${XOPTSTAR} -DENABLE_PERL=FALSE -DCMAKE_INSTALL_LIBDIR=lib -DCMAKE_SHARED_LINKER_FLAGS=$LDFLAGS
	  make 
          if ( $?) break;
	  make install
          if ( $?) break;
          touch ../${pkg}.Done
      case "apr-util*":
          ./configure --prefix=$XOPTSTAR --with-apr=$XOPTSTAR
          make install
          if ( $?) break;
          touch ../${pkg}.Done
          breaksw
      case "apache-log4cxx-0.10.0.CVS":
           ./configure --prefix=$XOPTSTAR --with-apr=$XOPTSTAR --with-apr-util=$XOPTSTAR --disable-libtool
	   make clean
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
          ./configure --prefix=$XOPTSTAR --with-libs='-lpthread'
# --with-pth  --enable-shared
	  make -j 4
          make install
          touch ../${pkg}.Done
          breaksw
      case "qt*":
          ./configure --prefix=$XOPTSTAR -no-glib <<EOF
o
yes
EOF
          make install
          touch ../${pkg}.Done
	  breaksw
      case "pythia8*":
          ./configure --prefix=$XOPTSTAR --cxx-common=\'$CFLAGS\' 
	  breaksw
      case "apr-1.5.1":
      default:
          ./configure --prefix=$XOPTSTAR
          make install
          touch ../${pkg}.Done
          breaksw
     endsw
  endif 
end
