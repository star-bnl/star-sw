#! /usr/local/bin/bash
############################################################################
#
# $Id: makeall.sh,v 1.1 1999/05/19 22:23:51 ullrich Exp $
#
# Author: Thomas Ullrich, May 1999
# --------------------------------------------------------------------------
#
# Shell script to compile all examples.
#
# --------------------------------------------------------------------------
#
# $Log: makeall.sh,v $
# Revision 1.1  1999/05/19 22:23:51  ullrich
# Initial Revision
#
############################################################################
for F in *.cc
do
    FF=`basename $F .cc`
    gmake $FF
done
exit 0    
