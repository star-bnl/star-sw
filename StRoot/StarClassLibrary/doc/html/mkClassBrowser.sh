#!/usr/bin/ksh
############################################################################
#
# $Id: mkClassBrowser.sh,v 1.1 1999/02/17 12:38:47 ullrich Exp $
#
# Author: Thomas Ullrich, August 1998
# --------------------------------------------------------------------------
# Create the StarClassLibrary Class Browser
# This shell script generates all HTML files.
# --------------------------------------------------------------------------
#
# $Log: mkClassBrowser.sh,v $
# Revision 1.1  1999/02/17 12:38:47  ullrich
# Initial Revision
#
############################################################################

# Check command line arguments
if [ $# -le 0 ]
then
    echo "Usage: mkClassBrowser.sh headerFiles...";
    exit 2;
fi

# Call the utility programs
rm -f *.html
prepareFile $@
writeHtml *.out
writeIndex *.html
rm *.out *.keywords

# That's it
exit 0

