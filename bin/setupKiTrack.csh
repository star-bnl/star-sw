#! /usr/bin/env tcsh 
if (! -d $STAR_OBJ/KiTrack) mkdir  $STAR_OBJ/KiTrack
cd  $STAR_OBJ/KiTrack
cmake $STAR/KiTrack -DCMAKE_INSTALL_PREFIX=$STAR_OBJ/..
make -j 8 install
