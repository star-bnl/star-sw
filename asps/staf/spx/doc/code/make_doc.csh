#!/bin/csh
ln -s ../../../asu .
ctoohtml `ls asu/src/*.c asu/src/*.cc asu/inc/*.h asu/inc/*.hh | egrep -v _def.c`
