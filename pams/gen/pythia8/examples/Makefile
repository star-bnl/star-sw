#
# Examples Makefile.
#
#                  M. Kirsanov 07.04.2006
#                     Modified 18.11.2006
#                     26.03.2008 CLHEP dependency removed

SHELL = /bin/sh

-include ../config.mk
-include config.mk

# Location of directories.
TMPDIR=tmp
TOPDIR=$(shell \pwd)
INCDIR=include
SRCDIR=src
LIBDIR=lib
LIBDIRARCH=lib/archive
BINDIR=bin

# There is no default behaviour, so remind user.
all:
	@echo "Usage: for NN = example number: make mainNN"

# Create an executable for one of the normal test programs
main00  main01 main02 main03 main04 main05 main06 main07 main08 main09 main10 \
	main11 main12 main13 main14 main15 main16 main17 main18 main19 main20 \
        main21 main22 main23 main24 main25 main26 main27: \
	../$(LIBDIRARCH)/libpythia8.a
	@mkdir -p ../$(BINDIR)
	$(CXX) $(CXXFLAGS) -I../$(INCDIR) $@.cc -o ../$(BINDIR)/$@.exe \
	-L../$(LIBDIRARCH) -lpythia8 -llhapdfdummy
	@ln -fs ../$(BINDIR)/$@.exe $@.exe

# Create an executable linked to HepMC (if all goes well).
ifneq (x$(HEPMCLOCATION),x)
  # without pythia6:
  main31 main32: ../$(LIBDIRARCH)/libpythia8.a ../$(LIBDIRARCH)/libhepmcinterface.a
	@mkdir -p ../$(BINDIR)
	$(CXX) $(CXXFLAGS) -I../$(INCDIR) -I$(HEPMCLOCATION)/include \
	$@.cc -o ../$(BINDIR)/$@.exe \
	-L../$(LIBDIRARCH) -lpythia8 -llhapdfdummy \
	-lhepmcinterface \
	-L$(HEPMCLOCATION)/lib -lHepMC
	@ln -fs ../$(BINDIR)/$@.exe $@.exe
  # with pythia6:
  main54: ../$(LIBDIRARCH)/libpythia8.a ../$(LIBDIRARCH)/libhepmcinterface.a
	@mkdir -p ../$(BINDIR)
	$(CXX) $(CXXFLAGS) -I../$(INCDIR) -I$(HEPMCLOCATION)/include \
	$@.cc -o ../$(BINDIR)/$@.exe \
	-L../$(LIBDIRARCH) -lpythia8 -llhapdfdummy \
	-L$(PYTHIA6LOCATION) $(PYTHIA6LIBNAME) \
	-lhepmcinterface \
	-L$(HEPMCLOCATION)/lib -lHepMC \
	$(FLIBS)
	@ln -fs ../$(BINDIR)/$@.exe $@.exe
else
  main31 main32 main54:
	@echo ERROR, this target needs HepMC, variable HEPMCLOCATION
endif

# Create an executable that links to LHAPDF
main41 main42: ../$(LIBDIRARCH)/libpythia8.a
	@mkdir -p ../$(BINDIR)
	$(CXX) $(CXXFLAGS) -I../$(INCDIR) $@.cc -o ../$(BINDIR)/$@.exe \
	-L../$(LIBDIRARCH) -lpythia8 \
	-L$(LHAPDFLOCATION) $(LHAPDFLIBNAME) \
	$(FLIBS)
	@ln -fs ../$(BINDIR)/$@.exe $@.exe

# Create an executable that links to Pythia6 
main51 main52: ../$(LIBDIRARCH)/libpythia8.a
	@mkdir -p ../$(BINDIR)
	$(CXX) $(CXXFLAGS) -I../$(INCDIR) $@.cc -o ../$(BINDIR)/$@.exe \
	-L../$(LIBDIRARCH) -lpythia8 -llhapdfdummy \
	-L$(PYTHIA6LOCATION) $(PYTHIA6LIBNAME) \
	$(FLIBS)
	@ln -fs ../$(BINDIR)/$@.exe $@.exe

# Create an executable for the Fortran-only test program 
main53: ../$(LIBDIRARCH)/libpythia8.a
	@mkdir -p ../$(BINDIR)
	$(FC) $(FFLAGS) $@.f -o ../$(BINDIR)/$@.exe \
	-L$(PYTHIA6LOCATION) $(PYTHIA6LIBNAME) $(FLIBS)
	@ln -fs ../$(BINDIR)/$@.exe $@.exe

# Clean up: remove executables and outdated files.
.PHONY: clean
clean:
	rm -rf ../$(BINDIR)
	rm -rf *.exe
	rm -f *~; rm -f \#*; rm -f core*
