#!/bin/sh
# $Id: makedist.sh,v 1.1 2004/09/02 23:50:50 potekhin Exp $
# -------------------------------------------------------------------------
# Script to produce source and optionally binary distribution of geant4_vmc.
# Called by main Makefile.
#
# According to: 
# $ROOTSYS/build/unix/makedist.sh
# Author: Fons Rademakers, 29/2/2000
#
# Usage: makedist.sh [lib]
#
# By I.Hrivnacova, 7/10/2002

CURDIR=`pwd`

# gmake is called from geant4_vmc/source
cd ../..

MAKELIB=
VERSION=`cat geant4_vmc/config/version_number`
MACHINE=`uname`
OSREL=`uname -r`
if [ "$1" = "lib" ] ; then  
  TYPE=$MACHINE.$OSREL.
  MAKELIB=geant4_vmc/$1
else   
  TYPE=""
fi  
TARFILE=geant4_vmc.$VERSION.$TYPE"tar"

TAR=`which gtar`
dum=`echo $TAR | grep "no gtar"`
stat=$?
if [ "$TAR" = '' ] || [ $stat = 0 ]; then
   TAR="tar cvf"
   rm -f $TARFILE.gz
   EXCLUDE=
else 
   TAR=$TAR" zcvf"
   rm -f $TARFILE.gz
   TARFILE=$TARFILE".gz"
   EXCLUDE="--exclude CVS"
fi

$TAR $TARFILE $EXCLUDE geant4_vmc/README geant4_vmc/"history" geant4_vmc/config  \
   geant4_vmc/"source" geant4_vmc/convertors geant4_vmc/examples $MAKELIB
cd $CURDIR

exit 0
