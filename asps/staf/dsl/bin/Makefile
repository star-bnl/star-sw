#cet(3/22/94)try at rsgi01.rhic.bnl.gov
#cet(3/8/94)#BASE_PATH=../13aug93
#whg(2/28/95)removed vxworks and lib stuff
#bem(4/29/96)add sun4os5 architecture
BASE_PATH:=$(shell cd ..;pwd)
BPATH=$(BASE_PATH)/bin/$(STAR_ARCH)
HPATH=$(BASE_PATH)/inc
LPATH=$(BASE_PATH)/lib/$(STAR_ARCH)
SPATH=$(BASE_PATH)/src
VPATH=$(BASE_PATH)/bin/vxw
export
#
all:		dstest

dstest:
	cd $(BPATH); $(MAKE) -f ../Makefile.bin dstest

clean:	
	cd $(BPATH); $(MAKE) -f ../Makefile.bin clean

realclean:
	cd aix; $(MAKE) -f ../Makefile.bin clean
	cd hpux; $(MAKE) -f ../Makefile.bin clean
	cd irix; $(MAKE) -f ../Makefile.bin clean
	cd sun4; $(MAKE) -f ../Makefile.bin clean
        cd sun4os5; $(MAKE) -f ../Makefile.bin clean
	cd vxw; $(MAKE) -f ../Makefile.bin vwclean

vwtest:
	cd $(VPATH); $(MAKE) -f ../Makefile.bin vwtest

vwclean:
	cd $(VPATH); $(MAKE) -f ../Makefile.bin vwclean
