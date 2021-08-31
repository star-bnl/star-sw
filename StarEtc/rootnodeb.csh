#
# Initialisation of Root 6.16.00


setenv myDEV2 /gpfs01/star/subsys-tpc/fisyak/STAR/packages/.DEV2
setup 64b
set myROOT = 6.16.00_vp
##set myROOT = 6.20.08
##set myROOT = 5.34.38

setup ROOT $myROOT
###star.dev
stardev
setup ROOT $myROOT
if (-e ${ROOTSYS}_novc) then
  setenv ROOTSYS  		${ROOTSYS}_novc
  setenv PATH 			${PATH:s/rootdeb/rootdeb_novc/}	
  setenv MANPATH		${MANPATH:s/rootdeb/rootdeb_novc/}
  setenv PYTHONPATH		${PYTHONPATH:s/rootdeb/rootdeb_novc/}
  setenv CINTSUSDIR		${CINTSYSDIR:s/rootdeb/rootdeb_novc/}
  setenv LD_LIBRARY_PATH	${LD_LIBRARY_PATH:s/rootdeb/rootdeb_novc/}
endif
##setenv LD_LIBRARY_PATH ${ROOTLIB}:.sl73_x8664_gcc485/lib:/afs/rhic.bnl.gov/star/packages/.DEV/.sl73_x8664_gcc485/lib:/opt/star/sl73_x8664_gcc485/qt4/lib:/opt/star/sl73_x8664_gcc485/lib:/afs/rhic.bnl.gov/star/ROOT/Xrootd/prod/.sl73_x8664_gcc485/lib

setenv STARSYS $STAR/.$STAR_HOST_SYS
setenv STAR_OBJ $STARSYS/obj
setenv STAR_BIN $STARSYS/bin
unsetenv STAR_PAMS
