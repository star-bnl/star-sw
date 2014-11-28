#!/bin/sh
inpPath=./

for directory in $( ls $inpPath ); do
  if [ $( ls $inpPath/$directory/*.ushist.root | wc -l ) -gt 0 ] ; then
    ls $inpPath/$directory/*.ushist.root 
    if [ -d $directory ] ; then
      hadd $directory.hist.root $directory/*ushist.root
    fi
  fi
done
