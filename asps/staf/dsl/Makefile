BASE_DIR := $(shell pwd)
HPATH := $(BASE_DIR)/inc
LPATH := $(BASE_DIR)/lib/$(STAR_ARCH)
INSTALL := -ln -s
INSTALL := cp

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
tcp:
	cd tcp; $(MAKE) all

install:
	$(INSTALL) $(LPATH)/libdsl*.a \
		$(STAR_LIB)/$(STAR_SYS_LEVEL)/sys/lib
	$(INSTALL) $(HPATH)/ds*.h \
		$(STAR_LIB)/$(STAR_SYS_LEVEL)/sys/inc

vwall: vwlib vwtest

vwlib:
	cd lib; $(MAKE) vwlib

vwtest:
	cd bin; $(MAKE) vwtest

vwclean:
	cd bin; $(MAKE) vwclean
	cd lib; $(MAKE) vwclean
