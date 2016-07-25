#!/bin/tcsh -f
if (-d "/star/institutions/bnl/fine/qt-x11-free-3.3.1" && $STAR_HOST_SYS == "sl305_icc81") then
   setenv QTDIR "/star/institutions/bnl/fine/qt-x11-free-3.3.1"
else 
   if (-d "/star/institutions/bnl/fine/qt-x11-free-3.3.1.sl305_gcc323" && $STAR_HOST_SYS == "sl305_gcc323") then
    setenv QTDIR  "/star/institutions/bnl/fine/qt-x11-free-3.3.1.sl305_gcc323"
   endif
endif
echo "set QTDIR = $QTDIR"
