#! /usr/local/bin/tcsh -f
set P=/star/data07/calib/fisyak/local
setenv PATH ${P}/bin:${PATH}
setenv LD_LIBRARY_PATH ${P}/lib:${LD_LIBRARY_PATH}
setenv PATH `dropit`
setenv LD_LIBRARY_PATH `dropit -p $LD_LIBRARY_PATH`
