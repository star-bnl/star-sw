############################################################################
#
# $Id: GNUmakefile,v 2.0 2000/08/09 16:17:29 gans Exp $
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
# Revision 2.0  2000/08/09 16:17:29  gans
# Readded Files That were not added in last CVS. Cosmetic Changes, naming convention
# for StRichDrawableT(foo)
#
# Revision 1.1  2000/01/27 17:13:10  lasiuk
# Initial Revisions
#
############################################################################

SUBDIRS = ..

all:
	@for dir in $(SUBDIRS); do (cd $$dir; $(MAKE)); done

clean:
	@for dir in $(SUBDIRS); do (cd $$dir; $(MAKE) clean); done


