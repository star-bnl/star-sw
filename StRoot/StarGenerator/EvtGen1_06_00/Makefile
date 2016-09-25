include ./config.mk

targets=lib_shared
targets+=lib_archive
ifeq ($(EVTGEN_EXTERNAL),1)
	targets+=libext_shared
	targets+=libext_archive
endif

default: all

all: $(targets)

lib_shared:  
	$(MAKE) -C $(SRCDIR) lib_shared

lib_archive: 
	$(MAKE) -C $(SRCDIR) lib_archive

libext_shared:
	$(MAKE) -C $(SRCDIR) libext_shared

libext_archive:
	$(MAKE) -C $(SRCDIR) libext_archive

install:
	if test "${PREFIX}" != "." ; then \
	  mkdir -p ${PREFIX}/lib ${PREFIX}/include ${PREFIX}/share && \
	  cp -rf lib/* ${PREFIX}/lib/ && \
	  cp -rf EvtGen EvtGenBase EvtGenModels EvtGenExternal ${PREFIX}/include/ && \
	  cp -rf *.DEC *.XML evt.pdl ${PREFIX}/share/ ; \
        fi

distclean: clean
	rm -rf tmp/ bin/ lib/ tmp_exp/
	rm -f config.mk fort.*

clean: 
	$(MAKE) -C $(SRCDIR) clean

help:
	@echo "A list of targets to make:"
	@echo ""
	@echo "lib_shared:  compiles EvtGenBase and EvtGenModels as a shared library and puts it into ./lib"
	@echo "lib_archive: compiles EvtGenBase and EvtGenModels as an archive library and puts it into ./lib/archive"
	@echo "libext_shared:  compiles EvtGenExternal as a shared library and puts it into ./lib"
	@echo "libext_archive: compiles EvtGenExternal as an archive library and puts it into ./lib/archive"
	@echo "default:     lib_shared lib_archive libext_shared libext_archive"
        @echo "install:     install generator to location specified by --prefix option"
	@echo "clean:       removes all libraries, executables and objects"
	@echo "distclean:   same as 'clean' plus removing ./config.mk, tmp/ and lib/ directories"
	@echo ""

.PHONY: distclean clean	
