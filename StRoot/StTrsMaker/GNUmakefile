############################################################################
#
# $Id: GNUmakefile,v 1.3 1998/11/12 23:22:12 lasiuk Exp $
#
# Author: Thomas Ullrich, Mar 25 1998
# --------------------------------------------------------------------------
#
# --------------------------------------------------------------------------
#
# $Log: GNUmakefile,v $
# Revision 1.3  1998/11/12 23:22:12  lasiuk
# make compatible with new local/ Makefiles
#
# Revision 1.2  1998/11/12 22:40:33  lasiuk
# Yale/BNL friendly structure
#
# Revision 1.6  1998/10/31 14:13:24  lasiuk
# SCL_HOME (add underscore) to make consistent with SCL!
#
# Revision 1.5  1998/09/14 10:29:57  lasiuk
# directory structure of scl
#
# Revision 1.4  1998/06/04 23:25:11  lasiuk
# link with tracks from SCL
#
# Revision 1.3  1998/05/21 21:26:46  lasiuk
# simple
#
# Revision 1.2  1998/05/20 15:04:08  ullrich
# Added SCL category 'utilities' to include path.
#
# Revision 1.1.1.1  1998/05/19 22:33:44  lasiuk
# Initial Revision
#
############################################################################

name := StTrs

include local/GNUmakefile.architecture
CPPFLAGS += -I$(SCL_HOME)/include \
	    -I./include

include local/GNUmakefile.common
