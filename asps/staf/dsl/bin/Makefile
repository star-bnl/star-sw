#cet(3/22/94)try at rsgi01.rhic.bnl.gov
#cet(3/8/94)#BASE_PATH=../13aug93
#whg(2/28/95)removed vxworks and lib stuff
#bem(4/29/96)add sun4os5 architecture
BASE_PATH:=$(shell cd ..;pwd)
ifndef OSARCH
OSARCH := $(STAR_ARCH)
endif
BPATH=$(BASE_PATH)/bin/$(OSARCH)
HPATH=$(BASE_PATH)/inc
LPATH=$(BASE_PATH)/lib/$(OSARCH)
SPATH=$(BASE_PATH)/src
VPATH=$(BASE_PATH)/bin/vxw
export
#
all:		dstest

$(BPATH):
	mkdir -p $@

$(VPATH):
	mkdir -p $@

dstest: $(BPATH)
	cd $(BPATH); $(MAKE) -f ../Makefile.bin dstest

clean:	
	cd $(BPATH); $(MAKE) -f ../Makefile.bin clean

realclean:
	cd aix; $(MAKE) -f ../Makefile.bin clean
	cd hpux; $(MAKE) -f ../Makefile.bin clean
	cd irix; $(MAKE) -f ../Makefile.bin clean
	cd linux; $(MAKE) -f ../Makefile.bin clean
	cd nt4; $(MAKE) -f ../Makefile.bin vwclean
	cd osf1; $(MAKE) -f ../Makefile.bin clean
	cd sun4; $(MAKE) -f ../Makefile.bin clean
	cd sun4os5; $(MAKE) -f ../Makefile.bin clean
	cd sun4os5pc; $(MAKE) -f ../Makefile.bin clean
	cd t3e; $(MAKE) -f ../Makefile.bin clean
	cd vxw; $(MAKE) -f ../Makefile.bin vwclean

test: $(BPATH)
	cd $(BPATH); $(MAKE) -f ../Makefile.bin test

vwtest: $(VPATH)
	cd $(VPATH); $(MAKE) -f ../Makefile.bin vwtest

vwclean:
	cd $(VPATH); $(MAKE) -f ../Makefile.bin vwclean
