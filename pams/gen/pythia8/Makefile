#
# Libraries Makefile. Some ideas from Geant4 Makefiles
#
#                  M. Kirsanov 07.04.2006
#                     Modified 18.11.2006
#                     26.03.2008 CLHEP dependency removed

SHELL = /bin/sh

-include config.mk

# flags:
#
#FFLAGSSHARED = -fPIC
CFLAGSSHARED = -fPIC
CXXFLAGSSHARED = -fPIC
#
LDFLAGSSHARED = $(CXXFLAGS) -pthread -fPIC


HEPMCERROR=
ifneq (x$(HEPMCLOCATION),x)
 ifeq ($(wildcard $(HEPMCLOCATION)/include/HepMC/*.h),)
  HEPMCERROR= HepMC interface: ERROR, no HepMC headers found in ${HEPMCLOCATION}/include/HepMC
 endif
endif

# Location of directories.
TMPDIR=tmp
TOPDIR=$(shell \pwd)
INCDIR=include
SRCDIR=src
LIBDIR=lib
LIBDIRARCH=lib/archive
BINDIR=bin

# Location of libraries to be built.
ifeq ($(SHAREDLIBS),yes)
  targets=$(LIBDIRARCH)/libpythia8.a
  targets+=$(LIBDIR)/libpythia8.so
  targets+=$(LIBDIRARCH)/liblhapdfdummy.a
  targets+=$(LIBDIR)/liblhapdfdummy.so
else
  targets=$(LIBDIRARCH)/libpythia8.a
  targets+=$(LIBDIRARCH)/liblhapdfdummy.a
endif

ifneq (x$(HEPMCLOCATION),x)
 targets+=$(LIBDIRARCH)/libhepmcinterface.a
 ifeq ($(SHAREDLIBS),yes)
  targets+=$(LIBDIR)/libhepmcinterface.so
 endif
endif


all: $(targets) config.mk

config.mk: ./configure
	./configure

# Main part: build Pythia8 library. 

$(TMPDIR)/%.o : $(SRCDIR)/%.cc
	@mkdir -p $(TMPDIR)
	$(CXX) $(CXXFLAGS) $(CXXFLAGSSHARED) -c -I$(INCDIR) $< -o $@

$(TMPDIR)/archive/%.o : $(SRCDIR)/%.cc
	@mkdir -p $(TMPDIR)/archive
	$(CXX) $(CXXFLAGS) -c -I$(INCDIR) $< -o $@

$(TMPDIR)/%.o : lhapdfdummy/%.cc
	@mkdir -p $(TMPDIR)
	$(CXX) $(CXXFLAGS) $(CXXFLAGSSHARED) -c -I$(INCDIR) $< -o $@

$(TMPDIR)/archive/%.o : lhapdfdummy/%.cc
	@mkdir -p $(TMPDIR)/archive
	$(CXX) $(CXXFLAGS) -c -I$(INCDIR) $< -o $@

# Creating the dependency files *.d
# The compiler with option -M is used to build the dependency strings. They
# are further edited with sed (stream editor). The first sed command adds the
# dependency for the *.d files themselves, the second one is needed because
# object files are put in the directory different from src. The last line
# removes empty *.d files produced in case of error.

ifeq ($(SHAREDLIBS),yes)
  $(TMPDIR)/%.d : $(SRCDIR)/%.cc
	@echo Making dependency for file $<; \
	mkdir -p $(TMPDIR); \
	$(CC) -M -I$(INCDIR) $< | \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' | \
	sed 's/$*.o/$(TMPDIR)\/$*.o/' > $@; \
	[ -s $@ ] || rm -f $@
endif

$(TMPDIR)/archive/%.d : $(SRCDIR)/%.cc
	@echo Making dependency for file $<; \
	mkdir -p $(TMPDIR)/archive; \
	$(CC) -M -I$(INCDIR) $< | \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' | \
	sed 's/$*.o/$(TMPDIR)\/archive\/$*.o/' > $@; \
	[ -s $@ ] || rm -f $@

objects := $(patsubst $(SRCDIR)/%.cc,$(TMPDIR)/%.o,$(wildcard $(SRCDIR)/*.cc))
objectsarch := $(patsubst $(SRCDIR)/%.cc,$(TMPDIR)/archive/%.o,$(wildcard $(SRCDIR)/*.cc))

$(LIBDIR)/libpythia8.so: $(objects)
	@mkdir -p $(LIBDIR)
	$(CXX) $(LDFLAGSSHARED) $(objects) -o $@ -shared -Wl,-soname,$(notdir $@)

$(LIBDIRARCH)/libpythia8.a: $(objectsarch)
	@mkdir -p $(LIBDIRARCH)
	ar cru $@ $(objectsarch)

objdum := $(patsubst lhapdfdummy/%.cc,$(TMPDIR)/%.o,$(wildcard lhapdfdummy/*.cc))
objdumarch := $(patsubst lhapdfdummy/%.cc,$(TMPDIR)/archive/%.o,$(wildcard lhapdfdummy/*.cc))

$(LIBDIR)/liblhapdfdummy.so: $(objdum)
	@mkdir -p $(LIBDIR)
	$(CXX) $(LDFLAGSSHARED) $(objdum) -o $@ -shared -Wl,-soname,$(notdir $@)

$(LIBDIRARCH)/liblhapdfdummy.a: $(objdumarch)
	@mkdir -p $(LIBDIRARCH)
	ar cru $@ $(objdumarch)

deps := $(patsubst $(SRCDIR)/%.cc,$(TMPDIR)/%.d,$(wildcard $(SRCDIR)/*.cc))
depsarch := $(patsubst $(SRCDIR)/%.cc,$(TMPDIR)/archive/%.d,$(wildcard $(SRCDIR)/*.cc))


# The "if" below is needed in order to avoid producing the dependency files
# when you want to just clean

ifneq ($(MAKECMDGOALS),clean)
-include $(deps)
-include $(depsarch)
endif

# Build HepMC interface part if HepMC location is set.

ifneq (x$(HEPMCLOCATION),x)
 HEPMCINCLUDE=-I$(HEPMCLOCATION)/include

 ifeq (x$(HEPMCERROR),x)

   $(TMPDIR)/%.o : hepmcinterface/%.cc config.mk
	@mkdir -p $(TMPDIR)
	$(CXX) $(CXXFLAGS) $(CXXFLAGSSHARED) $(HEPMCVFLAG) -c -I$(INCDIR) $(HEPMCINCLUDE) $< -o $@

   $(TMPDIR)/archive/%.o : hepmcinterface/%.cc config.mk
	@mkdir -p $(TMPDIR)/archive
	$(CXX) $(CXXFLAGS) $(HEPMCVFLAG) -c -I$(INCDIR) $(HEPMCINCLUDE) $< -o $@

   $(TMPDIR)/%.d : hepmcinterface/%.cc
	@echo Making dependency for file $<; \
	mkdir -p $(TMPDIR); \
	$(CC) -M -I$(INCDIR) $(HEPMCINCLUDE) $< | \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' | \
	sed 's/$*.o/$(TMPDIR)\/$*.o/' > $@; \
	[ -s $@ ] || rm -f $@

   $(TMPDIR)/archive/%.d : hepmcinterface/%.cc
	@echo Making dependency for file $<; \
	mkdir -p $(TMPDIR)/archive; \
	$(CC) -M -I$(INCDIR) $(HEPMCINCLUDE) $< | \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' | \
	sed 's/$*.o/$(TMPDIR)\/archive\/$*.o/' > $@; \
	[ -s $@ ] || rm -f $@

   objectsI := $(patsubst hepmcinterface/%.cc,$(TMPDIR)/%.o,$(wildcard hepmcinterface/*.cc))
   objectsIarch := $(patsubst hepmcinterface/%.cc,$(TMPDIR)/archive/%.o,$(wildcard hepmcinterface/*.cc))

   $(LIBDIR)/libhepmcinterface.so : $(objectsI)
	@mkdir -p $(LIBDIR)
	$(CXX) $(LDFLAGSSHARED) $(objectsI) -o $@ -shared -Wl,-soname,$(notdir $@)

   $(LIBDIRARCH)/libhepmcinterface.a : $(objectsIarch)
	@mkdir -p $(LIBDIRARCH)
	ar cru $(LIBDIRARCH)/libhepmcinterface.a $(objectsIarch)

   depsI := $(patsubst hepmcinterface/%.cc,$(TMPDIR)/%.d,$(wildcard hepmcinterface/*.cc))
   depsIarch := $(patsubst hepmcinterface/%.cc,$(TMPDIR)/archive/%.d,$(wildcard hepmcinterface/*.cc))

   ifneq ($(MAKECMDGOALS),clean)
   -include $(depsI)
   -include $(depsIarch)
   endif

 else

   $(LIBDIRARCH)/libhepmcinterface.a $(LIBDIR)/libhepmcinterface.so :
	@echo $(HEPMCERROR)



 endif

endif

# Clean up: remove (almost?) everything that cannot be recreated.

.PHONY: clean
clean:
	rm -f *~; rm -f \#*;
	rm -rf $(TMPDIR)
	rm -rf $(LIBDIR)
	rm -rf $(BINDIR)
	rm -f config.mk
	cd $(SRCDIR); rm -f *~; rm -f \#*; cd -
	cd $(INCDIR); rm -f *~; rm -f \#*; cd -
	cd xmldoc; rm -f *~; rm -f \#*; cd -
	cd htmldoc; rm -f *~; rm -f \#*; cd -
	cd phpdoc; rm -f *~; rm -f \#*; cd -
	cd hepmcinterface; rm -f *~; rm -f \#*; cd -
	cd lhapdfdummy; rm -f *~; rm -f \#*; cd -
	cd examples; rm -rf *.exe; rm -f *~; rm -f \#*; rm -f core*; rm -f config.*; cd -

