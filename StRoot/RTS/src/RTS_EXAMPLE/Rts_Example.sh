#!/bin/bash
#######################################################
# $Id: Rts_Example.sh,v 1.6 2009/07/09 16:17:26 fine Exp $
# Author: v.Fine [ fine@bnl.gov ]
#######################################################
# The simple script to compile and build the main RTS application
# against of the RTS system from the STAR offline software release
#######################################################

# stop at the first error.
trap  "echo ; echo SOME FATAL ERROR DURING EXECUTION, SORRY... ; echo ; exit;" ERR

source_application=$1
if [ "$1" == "" ]; then
   echo ""
   echo  Usage:  Rts_Example.sh [source_file_name]
   echo  ------
   echo No source file name has been provided.
   echo The default application \"rts_example\" will be built.
   echo ""
   source_application=$STAR/StRoot/RTS/src/RTS_EXAMPLE/rts_example.C
fi
base_name=$(basename $source_application)
executable_name=${base_name%%.*}
echo This script will build \"$executable_name\" from \"$(basename $source_application)\" against 
echo \"libRTS.so\" from the STAR offline release.
 g++ -g -o $executable_name         \
     -IStRoot/RTS/trg/include       \
     -IStRoot/RTS/include           \
     -IStRoot/RTS/src               \
     -I$STAR/StRoot/RTS/trg/include \
     -I$STAR/StRoot/RTS/include     \
     -I$STAR/StRoot/RTS/src         \
     -DINSIST_ON_EMC_PSEUDO         \
      $source_application -L.$STAR_HOST_SYS/lib -L$STAR_LIB -lRTS
if [ -f $executable_name  ] ; then
  echo ""
  echo Your \"$executable_name\" application is ready to be used:
  echo ""
  ls -l $executable_name
  echo ""
else
  echo ""
  echo Sorry, there was a compilation problem. 
  echo Please  review the error messages and try again
  echo ""
fi
