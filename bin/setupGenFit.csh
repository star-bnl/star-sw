#! /usr/bin/env tcsh 
if (-d $STAR_OBJ/GenFit) mkdir  $STAR_OBJ/GenFit
cd  $STAR_OBJ/GenFit
cmake $STAR/GenFit -DCMAKE_INSTALL_PREFIX=$STAR_OBJ/..
make -j 8 install
