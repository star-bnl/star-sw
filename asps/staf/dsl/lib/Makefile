#cet(3/22/94)try at rsgi01.rhic.bnl.gov
#cet(3/8/94)#BASE_PATH=../13aug93
#cet(17aug94)-rsgi02:/usr/local/bin/gcc
#whg(28aug95)removed vxworks and bin stuff
#whg(28aug95)disabled vpath for %.o - does not work right ??
#whg(01mar95)split into Makefile and Makefile.lib
#bem(4/29/96)add sun4os5 architecture
#cet(30sep98)small rework
#
BASE_PATH:=$(shell cd ..;pwd)
ifndef OSARCH
OSARCH := $(STAR_ARCH)
endif
HPATH=$(BASE_PATH)/inc
SPATH=$(BASE_PATH)/src
LPATH=$(BASE_PATH)/lib/$(OSARCH)
VPATH=$(BASE_PATH)/lib/vxw
export 

all: lib

test:
	cd $(LPATH); $(MAKE) -f ../Makefile.lib test

$(LPATH) : 
	mkdir -p $@

lib: $(LPATH)
	cd $(LPATH); $(MAKE) -f ../Makefile.lib lib

clean:
	cd $(LPATH); $(MAKE) -f ../Makefile.lib clean

realclean:
	cd aix; $(MAKE) -f ../Makefile.lib clean
	cd hpux; $(MAKE) -f ../Makefile.lib clean
	cd irix; $(MAKE) -f ../Makefile.lib clean
	cd linux; $(MAKE) -f ../Makefile.lib clean
	cd nt4; $(MAKE) -f ../Makefile.lib clean
	cd osf1; $(MAKE) -f ../Makefile.lib clean
	cd sun4; $(MAKE) -f ../Makefile.lib clean
	cd sun4os5; $(MAKE) -f ../Makefile.lib clean
	cd sun4os5pc; $(MAKE) -f ../Makefile.lib clean
	cd t3e; $(MAKE) -f ../Makefile.lib clean
	cd vxw; $(MAKE) -f ../Makefile.lib vwclean

vwlib:
	cd $(VPATH); $(MAKE) -f ../Makefile.lib vwlib

vwclean:
	cd $(VPATH); $(MAKE) -f ../Makefile.lib vwclean


