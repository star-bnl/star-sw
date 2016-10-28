if ("$OPTSTAR" == "$XOPTSTAR") then
  setenv XOPTSTAR ${OPTSTAR}/${STAR_HOST_SYS}
  if (! -d $XOPTSTAR) mkdir -p $XOPTSTAR
endif
switch (${STAR_HOST_SYS})
  case "*x8664*":
	setenv CFLAGS   "-m64 -fPIC"
	setenv CXXFLAGS "-m64 -fPIC"
	setenv LDFLAGS  "-m64 -fPIC"
	setenv FCFLAGS  "-m64 -fPIC"
     breaksw
  default:
	setenv CFLAGS   "-m32 -fPIC"
	setenv CXXFLAGS "-m32 -fPIC"
	setenv LDFLAGS  "-m32 -fPIC"
	setenv FCFLAGS  "-m32 -fPIC"
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

foreach pkg ( apr-1.5.1 apr-util-1.5.3 apache-log4cxx-0.10.0.CVS fastjet-3.0.3 fftw-3.3.5 gsl-2.1) #   Python-2.7.12 pyparsing-1.5.7 gsl  gsl-2.1 
    cd ~/sources/.${STAR_HOST_SYS}
    if (! -r ${pkg}.Done) then
       if (! -r ${pkg}) then
         if (-r ~/sources/${pkg}.tar) then
           tar xf ~/sources/${pkg}.tar
         else
           if (-r ~/sources/${pkg}) then
	     dirsync  ~/sources/${pkg} ${pkg}
           else 
             exit 1;
           endif
         endif
       endif
       cd ${pkg}
       switch ($pkg)
	case "apr-util-1.5.3":
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
	case "apr-1.5.1":
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
