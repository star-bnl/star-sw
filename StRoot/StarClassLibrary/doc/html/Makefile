############################################################################
#
# $Id: Makefile,v 1.1 1999/02/17 12:38:47 ullrich Exp $
#
# Author: Thomas Ullrich, August 1998
# --------------------------------------------------------------------------
#
# --------------------------------------------------------------------------
#
# $Log: Makefile,v $
# Revision 1.1  1999/02/17 12:38:47  ullrich
# Initial Revision
#
############################################################################
CXX 	 = g++ 
CXXFLAGS = 
CPPFLAGS = 
LDFLAGS  =

all:		utilities
		@chmod +x mkClassBrowser.sh
		@mkClassBrowser.sh ../../*.h*

utilities:	prepareFile writeHtml writeIndex

%:		%.cc
		$(CXX) $(CXXFLAGS) $(CPPFLAGS) $< -o $@ $(LDFLAGS)

clean:
		@rm -f prepareFile writeHtml writeIndex *.html
