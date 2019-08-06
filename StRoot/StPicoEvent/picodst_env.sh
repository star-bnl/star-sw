#!/bin/bash

# Assuming that this macro is sitting at its original location,
# its path can be used to set the PicoDst enviroment

filnam=$BASH_SOURCE            # filename of this (sourced) macro
absnam=`readlink -f $filnam`   # absolute filename
pather=`dirname $absnam`       # path to the config directory

export PICODST=$pather

export PATH=${PICODST}':'${PATH}
export LD_LIBRARY_PATH=${PICODST}':'${LD_LIBRARY_PATH}

echo PICODST = $PICODST
