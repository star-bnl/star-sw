BASE_DIR := $(shell pwd)
ifndef OSARCH
OSARCH := $(STAR_ARCH)
endif
HPATH := $(BASE_DIR)/inc
LPATH := $(BASE_DIR)/lib/$(OSARCH)
INSTALLROOT := $(STAR_LIB)/$(STAR_SYS_LEVEL)/sys/

all: dslib dsbin

most: dslib dsbin

dslib:
	cd lib; $(MAKE) all

dsbin:
	cd bin; $(MAKE) all

clean:
	cd lib; $(MAKE) clean
	cd bin; $(MAKE) clean


realclean:
	cd bin; $(MAKE) realclean
	cd lib; $(MAKE) realclean
	cd tcp; $(MAKE) clean

tcptest:
	cd tcp; $(MAKE) all

install:
	cd $(LPATH); \
	cp libdsl*.a $(INSTALLROOT)/lib
	cd $(HPATH); \
	cp ds*.h $(INSTALLROOT)/inc

vwall: vwlib vwtest

vwlib:
	cd lib; $(MAKE) vwlib

vwtest:
	cd bin; $(MAKE) vwtest

vwclean:
	cd bin; $(MAKE) vwclean
	cd lib; $(MAKE) vwclean
