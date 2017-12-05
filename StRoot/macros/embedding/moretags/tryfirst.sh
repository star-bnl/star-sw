#!/bin/sh

if [ $# -ne 2 ] ; then
   echo ""
   echo " Usage : $0 [MuDstList] [StarLibrary] "
   echo ""
   exit
fi

if [ ! -d "test" ] ; then
   mkdir test
fi

./runmakeMuDstQA.csh $1 test/ $2
