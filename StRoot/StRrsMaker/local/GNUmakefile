############################################################################
#
# $Id: GNUmakefile,v 1.1 2000/01/27 17:13:10 lasiuk Exp $
#
# Author: brian
# --------------------------------------------------------------------------
#
# "gmake" makes sub-libraries for each subdomain.
# (The .o files(s) are made by implicit rules.)
#
# --------------------------------------------------------------------------
#
# $Log: GNUmakefile,v $
# Revision 1.1  2000/01/27 17:13:10  lasiuk
# Initial Revisions
#
############################################################################

SUBDIRS = ..

all:
	@for dir in $(SUBDIRS); do (cd $$dir; $(MAKE)); done

clean:
	@for dir in $(SUBDIRS); do (cd $$dir; $(MAKE) clean); done


