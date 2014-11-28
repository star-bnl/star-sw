#! /usr/local/bin/bash
############################################################################
#
# $Id: runall.sh,v 1.1 1999/05/19 22:23:49 ullrich Exp $
#
# Author: Thomas Ullrich, May 1999
# --------------------------------------------------------------------------
#
# Shell script to run all examples.
#
# --------------------------------------------------------------------------
#
# $Log: runall.sh,v $
# Revision 1.1  1999/05/19 22:23:49  ullrich
# Initial Revision
#
############################################################################
for F in *.cc
do
    FF=`basename $F .cc`

    if [ $FF = "hbookTest2" ]
    then
	rm -f hbook.ntp
    fi
    
    $FF

    if [ $? -ne 0 ]
    then
      echo "Interrupt script, error in $FF"
      exit 1
    fi
done
exit 0    
