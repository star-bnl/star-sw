############################################################################
#
# $Id: GNUmakefile,v 1.1 1998/11/10 17:11:59 fisyak Exp $
#
# Author: Brian, October 1998
# --------------------------------------------------------------------------
#
# --------------------------------------------------------------------------
#
# $Log: GNUmakefile,v $
# Revision 1.1  1998/11/10 17:11:59  fisyak
# Put Brian trs versin into StRoot
#
# Revision 1.1  1998/11/10 17:11:59  fisyak
# Put Brian trs versin into StRoot
#
# Revision 1.2  1998/11/16 14:46:26  lasiuk
# do not use architecture for exectuable
#
# Revision 1.1  1998/11/08 17:44:55  lasiuk
# Initial Revision
include ../GNUmakefile.architecture
############################################################################
CPPFLAGS +=     -I$(SCL_HOME)/include \
		-I../prototype/include
aT_BNL  = 1
LDFLAGS   =     -L$(SCL_HOME)/$(STAR_SYS)/lib \
		-L$(shell pwd)/../prototype/$(STAR_SYS) \
		-L//cern/pro/lib \
LIBS      =     -L$(SCL_HOME)/$(STAR_SYS)/lib \
		-L$(shell pwd)/$(TRS_HOME)/$(STAR_SYS) \
		LIB_PATH    += -L/cern/pro/lib
LDFLAGS   +=    -L$(STDHOME)/lib -lstd15d
endif
	CXX       =      CC
	LIB_PATH  += -L/cern/pro/lib -L$(STDHOME)/lib -L/afs/rhic/star/packages/ObjectSpace/2.0m/lib
LDFLAGS   +=    -L$(STDHOME)/lib -lospace -lmx

endif
	LIBS       +=    -L$(STDHOME)/lib -lospace -lmx
all:		$(STAR_SYS)/trsLandau $(STAR_SYS)/trsIonization
	LIB_PATH  +=    -L$(STDHOME)/lib
$(STAR_SYS)/%:	%.cc
		@if [ ! -d $(STAR_SYS) ]; then mkdir $(STAR_SYS); fi
		$(CXX) $(CXXFLAGS) $(CPPFLAGS) $< -o $@ $(LDFLAGS)
#all:	trsExample
all:	trsDataBase
		@rm -rf $(STAR_SYS) core $(SCL_TEMPLATE_REPOSITORY)
	@echo Compliling...
	$(CXX) $(CXXFLAGS)  $(CPPFLAGS) $< -o $@ $(LIB_PATH) $(ROOTLIBS) $(LIBS)
	@rm -f trsSector core *.o $(SCL_TEMPLATE_REPOSITORY)

clean:
	@rm -rf trsSector trsExample trsDataBase core *.o
