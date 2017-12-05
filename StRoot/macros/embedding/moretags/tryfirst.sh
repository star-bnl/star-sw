#!/bin/sh

if [ ! -d "test" ] ; then
   mkdir test
fi

./runmakeMuDstQA.csh $1 test/ $2
