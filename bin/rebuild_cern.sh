#!/bin/sh

# Establish the environment variables for the build procedures
# Depending on the system, other directories may need to be added to the PATH
# e.g. for the build tools and alternative compilers.

##CERN_LEVEL=`gunzip -c src_Imakefile.tar.gz | tar tf - | awk -F/ '{print $1}'`
CERN_LEVEL=2006
CERN=`pwd`
CERN_ROOT=$CERN/$CERN_LEVEL
CVSCOSRC=$CERN/$CERN_LEVEL/src
PATH=$CERN_ROOT/bin:$PATH

export CERN
export CERN_LEVEL
export CERN_ROOT 
export CVSCOSRC
export PATH

# Delete old Makefiles and create the top level Makefile with imake

cd $CERN_ROOT/build
find . -type f -name Makefile -exec rm {} \;
$CVSCOSRC/config/imake_boot

# Install kuipc and the scripts (cernlib, paw and gxint) in $CERN_ROOT/bin

gmake bin/kuipc > log/kuipc 2>&1
gmake scripts/Makefile
cd scripts
gmake install.bin > ../log/scripts 2>&1

# Install the libraries

cd $CERN_ROOT/build
gmake > log/make.`date +%m%d` 2>&1

# test packlib and mathlib

cd $CERN_ROOT/build/packlib
gmake test > ../log/t_packlib.`date +%m%d` 2>&1
cd $CERN_ROOT/build/mathlib
gmake test > ../log/t_mathlib.`date +%m%d` 2>&1




