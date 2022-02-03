#
setenv myDEV2 /gpfs01/star/subsys-tpc/fisyak/STAR/packages/.DEV2
setup 64b
if (1) then
  set myROOT = 6.24.00
  setenv ROOT_LEVEL $myROOT
# module avail
  module use /cvmfs/star.sdcc.bnl.gov/star-spack/spack/share/spack/modules/linux-rhel7-x86_64
  module load star-env-root-6.24.00
endif
if (0) then
  set myROOT = 6.24.07
  setenv ROOT_LEVEL $myROOT
  setenv ROOTSYS /gpfs01/star/subsys-tpc/fisyak/STAR/ROOT/6.99.99/.sl73_x8664_gcc485/rootdeb
endif

setenv PATH $ROOTSYS/bin:/opt/star/sl73_x8664_gcc485/bin:/star/u/perev/bin:/usr/lib/jvm/jre-openjdk/bin:/afs/rhic/rhstar/group:mgr:$STAR/mgr:/afs/rhic.bnl.gov/star/packages/scripts:/afs/rhic.bnl.gov/star/packages/cgi:.sl73_x8664_gcc485/bin:/usr/afsws/bin:/usr/afsws/etc:/usr/local/bin:/opt/star/sl73_x8664_gcc485/qt4/bin:/usr/bin:/bin:/usr/sbin:.:/usr/local/sbin:/usr/lpp/mmfs/bin/:/cern64/pro/bin:/afs/rhic.bnl.gov/star/ROOT/Xrootd/prod/.sl73_x8664_gcc485/bin
setenv XROOTDSYS /afs/rhic.bnl.gov/star/ROOT/Xrootd/prod
setenv MANPATH $ROOTSYS/man:/opt/star/sl73_x8664_gcc485/man:/usr/share/man:/afs/rhic.bnl.gov/star/packages/man
setenv STAR_ROOT /afs/rhic.bnl.gov/star
setenv CVSROOT /afs/rhic.bnl.gov/star/packages/repository
setenv PYTHONPATH $ROOTSYS/lib
##setenv CINTSYSDIR /afs/rhic.bnl.gov/star/ROOT/6.24.00/.sl73_x8664_gcc485/rootdeb/cint
setenv STARSYS ${STAR}/.${STAR_HOST_SYS}
setenv STAR_OBJ $STARSYS/obj
setenv STAR_BIN $STARSYS/bin
setenv LD_LIBRARY_PATH $STARSYS/lib:$ROOTSYS/lib:/opt/star/sl73_x8664_gcc485/qt4/lib:/opt/star/sl73_x8664_gcc485/lib:/cvmfs/star.sdcc.bnl.gov/star-spack/spack/opt/spack/linux-rhel7-x86_64/gcc-4.8.5/zlib-1.2.11-vhzh5cfaki5lx5sjuth5iuojq5azdkbd/lib:/cvmfs/star.sdcc.bnl.gov/star-spack/spack/opt/spack/linux-rhel7-x86_64/gcc-4.8.5/libiconv-1.16-xqfklyc2vor3e36qddhohdjn23oizvzp/lib:/cvmfs/star.sdcc.bnl.gov/star-spack/spack/opt/spack/linux-rhel7-x86_64/gcc-4.8.5/libbsd-0.11.3-rtahy5ln7qia6gkaqld5zkqwhwr4fbcn/lib:/cvmfs/star.sdcc.bnl.gov/star-spack/spack/opt/spack/linux-rhel7-x86_64/gcc-4.8.5/libmd-1.0.3-27t6n5ivk3c2bshfhge3cpmfndimvuj3/lib:/afs/rhic.bnl.gov/star/ROOT/Xrootd/prod/.sl73_x8664_gcc485/lib

unsetenv STAR_PAMS

