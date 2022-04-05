############################################################################
#
# $Id: GNUmakefile,v 1.2 1998/11/12 23:04:54 lasiuk Exp $
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
# Revision 1.2  1998/11/12 23:04:54  lasiuk
# automate the makefiles
#
# Revision 1.1  1998/11/12 22:48:08  lasiuk
# Standalone Makefiles
#
# Revision 1.1  1998/11/10 17:11:56  fisyak
# Put Brian trs versin into StRoot
#
#
############################################################################

SUBDIRS = ..

all:
	@for dir in $(SUBDIRS); do (cd $$dir; $(MAKE)); done

clean:
	@for dir in $(SUBDIRS); do (cd $$dir; $(MAKE) clean); done


