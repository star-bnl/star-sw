#! /bin/csh

# This file can be modified by user to provide local path or afs path to
# different platofrm or version of HepMC, Pythia8 and ROOT.

# Safeguard test if running with /afs paths.
if (! -d "/afs/cern.ch/sw/lcg") then
	echo ""
	echo "This script is for use on CERN server only."
	echo ""
	exit 0
endif


# Prevents user to modify these paths by using e.g.
# ./configure --with-HepMC=<path>
setenv AFS_PATHS 'yes'

setenv HEPMCLOCATION '/afs/cern.ch/sw/lcg/external/HepMC/2.03.09/slc4_amd64_gcc34'
setenv PYTHIALOCATION '/afs/cern.ch/sw/lcg/external/MCGenerators/pythia8/135/slc4_amd64_gcc34'
setenv MCTESTERLOCATION '/afs/cern.ch/sw/lcg/external/MCGenerators/mctester/1.23.1/slc4_amd64_gcc34'
setenv PYTHIA8DATA "${PYTHIALOCATION}/xmldoc"

setenv PATH "/afs/cern.ch/sw/lcg/app/releases/ROOT/5.22.00/slc4_amd64_gcc34/root/bin:${PATH}"

set ROOTLIBPATH=`root-config --libdir`
set HERE=`(cd .. && pwd)`

if (! $?LD_LIBRARY_PATH) setenv LD_LIBRARY_PATH ""

setenv LD_LIBRARY_PATH "${HERE}/lib:${ROOTLIBPATH}:${HEPMCLOCATION}/lib:${PYTHIALOCATION}/lib:${MCTESTERLOCATION}/lib:${LD_LIBRARY_PATH}"

echo ""
echo "Paths for HepMC, Pythia8, MC-Tester and ROOT set to AFS software"
echo "WARNING: As of today (28 Jan 2010) there is only MC-TESTER 1.23.1"
echo "         available on AFS. Scripts located in examples/testing/H"
echo "         won't work with this version. In addition, AFS"
echo "         path does not contain files needed for analysis,"
echo "         therefore only generation step can be done."
echo ""

