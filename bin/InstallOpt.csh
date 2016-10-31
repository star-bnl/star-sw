if ("$OPTSTAR" == "$XOPTSTAR") then
  setenv XOPTSTAR ${OPTSTAR}/${STAR_HOST_SYS}
  if (! -d $XOPTSTAR) mkdir -p $XOPTSTAR
endif
setenv FORCE_32BITS FALSE
switch (${STAR_HOST_SYS})
  case "*x8664*":
	setenv CFLAGS   "-m64 -fPIC"
	setenv CXXFLAGS "-m64 -fPIC"
	setenv LDFLAGS  "-m64"
	setenv FCFLAGS  "-m64 -fPIC"
        setenv arch     "x86_64"
     breaksw
  default:
        setenv FORCE_32BITS TRUE
	setenv CFLAGS   "-m32 -fPIC"
	setenv CXXFLAGS "-m32 -fPIC"
	setenv LDFLAGS  "-m32"
	setenv FCFLAGS  "-m32 -fPIC"
        setenv arch     "i386"
     breaksw
endsw
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
# texinfo-6.3
foreach pkg ( apr-1.5.2 apr-util-1.5.4 apache-log4cxx-0.10.0.CVS  fastjet-3.0.3 fftw-3.3.5   gsl) #  xrootd-4.4.1  Python-2.7.12 pyparsing-1.5.7 gsl  gsl-2.1 
    cd ~/sources/.${STAR_HOST_SYS}
    if ( -r ${pkg}.Done) continue
    if (! -r ${pkg}) then
      if ($pkg != "xrootd-4.4.1") then
        if (-r ~/sources/${pkg}) then
          dirsync  ~/sources/${pkg} ${pkg}
        else 
          if (-r ~/sources/${pkg}.tar) then
            tar xf ~/sources/${pkg}.tar
          else
            break;
          endif
        endif
      endif
    else 
      mkdir ${pkg}
    endif
    echo "cd $pkg"
    cd ${pkg}
    switch ($pkg)
      case "xrootd-4.4.1":
          cmake ../../${pkg} -DCMAKE_FORCE_32BITS=${FORCE_32BITS} -DCMAKE_INSTALL_PREFIX=${XOPTSTAR} -DENABLE_PERL=FALSE
	  make install
      if (! $?) then 
          touch ../${pkg}.Done
      else
          exit 1;
      endif
      case "apr-util*":
          ./configure --prefix=$XOPTSTAR --with-apr=$XOPTSTAR
          make install
     if (! $?) then 
          touch ../${pkg}.Done
     else
          exit 1;
     endif
          breaksw
      case "apache-log4cxx-0.10.0.CVS":
           ./configure --prefix=$XOPTSTAR --with-apr=$XOPTSTAR --with-apr-util=$XOPTSTAR
	   make clean
           make
           make install
     if (! $?) then 
          touch ../${pkg}.Done
     else
          exit 1;
     endif
          breaksw
      case "pyparsing-1.5.7":
          ${XOPTSTAR}/bin/python setup.py install
     if (! $? ) then 
          touch ../${pkg}.Done
     else
          exit 1;
     endif
          breaksw
     case "gsl":
	setenv PREFIX $XOPTSTAR
	libtoolize --force
	aclocal
	autoheader
	automake --force-missing --add-missing
	autoconf
      case "gsl*":
#          ./configure -arch "$arch" --prefix=PREFIX=$XOPTSTAR
          ./configure --prefix=$XOPTSTAR
          make
          make install
     if (! $?) then 
          touch ../${pkg}.Done
     else
          exit 1;
     endif
          breaksw
      case "apr-1.5.1":
          breaksw
      default:
          ./configure --prefix=$XOPTSTAR
          make install
     if (! $?) then 
          touch ../${pkg}.Done
     else
          exit 1;
     endif
          breaksw
     endsw
  endif 
end
