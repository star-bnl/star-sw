############################################################################
#
# $Id: GNUmakefile,v 1.1 1998/11/10 17:11:56 fisyak Exp $
#
# Author: Thomas Ullrich, Mar 25 1998
# --------------------------------------------------------------------------
#
# "gmake" makes sub-libraries for each subdomain.
# (The .o files(s) are made by implicit rules.)
#
# --------------------------------------------------------------------------
#
# $Log: GNUmakefile,v $
# Revision 1.1  1998/11/10 17:11:56  fisyak
# Put Brian trs versin into StRoot
#
# Revision 1.1.1.1  1998/05/19 22:33:43  lasiuk
# Initial Revision
#
############################################################################

SUBDIRS = prototype run prototype/examples

all:
	@for dir in $(SUBDIRS); do (cd $$dir; $(MAKE)); done

clean:
	@for dir in $(SUBDIRS); do (cd $$dir; $(MAKE) clean); done


