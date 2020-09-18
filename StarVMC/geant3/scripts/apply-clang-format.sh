#!/bin/bash

#------------------------------------------------
# The Virtual Monte Carlo packages
# Copyright (C) 2015 Ivana Hrivnacova
# All rights reserved.
#
# For the licensing terms see geant4_vmc/LICENSE.
# Contact: root-vmc@cern.ch
#-------------------------------------------------

# Process the TGeant3 source code files with clang-format.
# The script should be run from the TGeant3 directory.
#
# To apply clang-format on a single file:
# clang-format -style=file -i path/filename
#
# I. Hrivnacova 17/04/2019
#

if [ ! -f TGeant3.h ]; then
  echo "The script should be run from the TGeant3 directory."
  exit 1;
fi

for FILE in `find . -iname "*.h" -o -iname "*.cxx"`
do 
  echo "... processing file $FILE"
  clang-format -style=file -i $FILE
done
