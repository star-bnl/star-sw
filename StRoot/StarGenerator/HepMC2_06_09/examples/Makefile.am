# This makefile is used to copy files during the "make install" step

INSTALLDIR = $(pkgdatadir)/examples

# files to distribute
EXTRA_DIST = \
    example_BuildEventFromScratch.cc \
    example_EventSelection.cc \
    example_UsingIterators.cc \
    example_VectorConversion.cc \
    VectorConversion.h \
    example_UsingIterators.txt

install-data-local:
	$(mkinstalldirs) $(DESTDIR)$(INSTALLDIR)
	$(INSTALL_DATA) GNUmakefile.example $(DESTDIR)$(INSTALLDIR)/GNUmakefile
	for file in $(EXTRA_DIST); do \
	  $(INSTALL_DATA) $(srcdir)/$$file $(DESTDIR)$(INSTALLDIR)/$$file; done
