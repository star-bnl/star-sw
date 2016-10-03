#!/bin/sh

echo ""
echo "-------------------------------------------------------"
echo "This script will prepare/restrict 'tauola-fortran'     "
echo "directory for use only by its  C++/HepMC interface."
echo ""
echo "It does not need to be executed, however it clears out"
echo "2/3 of the tar ball removing files which may be useful"
echo "for phenomenologist of low energies but for other users"
echo "(e.g. LHC/TEVATRON users) may be only confusing."
echo ""
echo "Configuration in main directory must be done"
echo "prior to running this script"
echo "-------------------------------------------------------"
echo ""
echo "-------------------------------------"
echo "WARNING: This action is irreversible."
echo "-------------------------------------"
echo ""
echo "Proceed? (yes/no)"
read ANSWER

ANSWER=`echo $ANSWER | tr "[:upper:]" "[:lower:]"`

if test "$ANSWER" != "yes" && test "$ANSWER" != "y"; then
	echo "Aborted."
	exit
fi

echo "Proceeding..."
(cd photos-F ; make 10kD-all)
(cd tauola-F ; make CLEO)

if [ ! -d "tauola" ]; then
	echo "Something went wrong with MAKE."
	echo "Celanup aborted."
	exit
fi

echo "Deleting..."
rm -rf doc
rm -rf eli
rm -rf glibk
rm -rf demo-factory
rm -rf include
rm -rf photos*
rm -rf jetset*
rm -rf tauola-F
rm -rf tauola-BBB
rm -rf tauola-factory
rm -rf randg
rm -rf platform

rm -rf tauola/demo-standalone
rm -rf tauola/demo-jetset
rm -rf tauola/demo-KK-face

rm -rf README*
rm -rf makefile

echo ""
echo "Directory cleaning finished."
echo "Script 'onlyLHC.sh' can be removed now."
echo ""

