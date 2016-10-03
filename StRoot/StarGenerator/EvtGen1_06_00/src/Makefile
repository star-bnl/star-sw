include ../config.mk

TMPDIR=../tmp
EXTTMPDIR=../tmp_ext

sources := './EvtGen.cpp'
sources += $(shell find './EvtGenBase' -type 'f' -name '*.cpp')
sources += $(shell find './EvtGenModels' -type 'f' -name '*.cpp')
sources += $(shell find './EvtGenBase' -type 'f' -name '*.[Ff]')
sources += $(shell find './EvtGenModels' -type 'f' -name '*.[Ff]')
objects := $(shell echo $(sources) | sed 's,\./,$(TMPDIR)/,g;s,\.cpp,\.o,g;s,\.F,\.o,g;')
mkfiles := $(shell echo $(sources) | sed 's,\./,$(TMPDIR)/,g;s,\.cpp,\.mk,g;s,\.F,\.mk,g;s,\.f,\.mk,g;')

extsources := $(shell find './EvtGenExternal' -type 'f' -name '*.cpp')
extsources += $(shell find './EvtGenExternal' -type 'f' -name '*.[Ff]')
extobjects := $(shell echo $(extsources) | sed 's,\./,$(EXTTMPDIR)/,g;s,\.cpp,\.o,g;s,\.F,\.o,g;')
extmkfiles := $(shell echo $(extsources) | sed 's,\./,$(EXTTMPDIR)/,g;s,\.cpp,\.mk,g;s,\.F,\.mk,g;s,\.f,\.mk,g;')

lib_shared: $(LIB_SHARED)

lib_archive: $(LIB_ARCHIVE)

libext_shared: $(LIBEXT_SHARED)

libext_archive: $(LIBEXT_ARCHIVE)

coreShLibs=-L$(HEPMCLIBDIR) $(HEPMCLIBLIST)

extShLibs=-L$(HEPMCLIBDIR) $(HEPMCLIBLIST)

coreIncDir=-I$(INCLUDEDIR)
coreIncDir+=-I$(HEPMCINCDIR)

extIncDir=-I$(INCLUDEDIR)
extIncDir+=-I$(HEPMCINCDIR)

ifeq ($(EVTGEN_PHOTOS),1)
	extShLibs+=-L$(PHOTOSLIBDIR) $(PHOTOSLIBLIST)
	extIncDir+=-I$(PHOTOSINCDIR)
endif
ifeq ($(EVTGEN_TAUOLA),1)
	extShLibs+=-L$(TAUOLALIBDIR) $(TAUOLALIBLIST)
	extIncDir+=-I$(TAUOLAINCDIR)
endif
ifeq ($(EVTGEN_PYTHIA),1)
	extShLibs+=-L$(PYTHIALIBDIR) $(PYTHIALIBLIST)
	extIncDir+=-I$(PYTHIAINCDIR)
endif

extShLibs+=-L$(LIBDIR_SHARED) $(LIBDIRLIST)

# Core
$(LIB_SHARED) : $(objects)
	mkdir -p $(LIBDIR_SHARED)
	@echo creating the shared library $@
	$(CXX) $(LDFLAGSSHARED) $(objects) -o $@ -shared -Wl,$(SOFLAGS),$(notdir $@) $(FLIBS) \
	$(coreShLibs)

$(LIB_ARCHIVE) : $(objects)
	ar cru $(LIB_ARCHIVE) $(objects)

# External
$(LIBEXT_SHARED) : $(extobjects)
	mkdir -p $(LIBDIR_SHARED)
	@echo creating the shared library $@
	$(CXX) $(LDFLAGSSHARED) $(extobjects) -o $@ -shared -Wl,$(SOFLAGS),$(notdir $@) $(FLIBS) \
	$(extShLibs)

$(LIBEXT_ARCHIVE) : $(extobjects)
	ar cru $(LIBEXT_ARCHIVE) $(extobjects)

#---- The following is for dependency checking:

$(TMPDIR)/%.mk: %.cpp
	echo $(coreIncDir)
	$(CXX) -MM $(CXXFLAGS) $< $(coreIncDir) | sed 's,[^ ]*\.o:,$(TMPDIR)/&,' > $@

$(TMPDIR)/%.mk: %.F
	$(FC) $(FFLAGS) -c $< -I$(INCLUDEDIR) | sed 's,[^ ]*\.o:,$(TMPDIR)/&,' > $@


$(EXTTMPDIR)/%.mk: %.cpp
	echo $(extIncDir)
	$(CXX) -MM $(CXXFLAGS) $(EXTRAFLAGS) $< $(extIncDir) | sed 's,[^ ]*\.o:,$(EXTTMPDIR)/&,' > $@

$(EXTTMPDIR)/%.mk: %.F
	$(FC) $(FFLAGS) -c $< -I$(INCLUDEDIR) | sed 's,[^ ]*\.o:,$(EXTTMPDIR)/&,' > $@


ifneq ($(MAKECMDGOALS),clean)
 ifneq ($(mkfiles),)
  -include $(mkfiles)
 endif
endif

#----

$(TMPDIR)/%.o : %.cpp $(TMPDIR)/%.mk
	$(CXX) $(CXXFLAGS) $(CXXFLAGSSHARED) -c -o $@ $< $(coreIncDir)

$(TMPDIR)/%.o: %.F $(TMPDIR)/%.mk
	$(FC) $(FFLAGS) $(FFLAGSSHARED) -c -o $@ $< -I$(INCLUDEDIR)

$(TMPDIR)/%.o: %.f $(TMPDIR)/%.mk
	$(FC) $(FFLAGS) $(FFLAGSSHARED) -c -o $@ $< -I$(INCLUDEDIR)


$(EXTTMPDIR)/%.o : %.cpp $(EXTTMPDIR)/%.mk
	$(CXX) $(CXXFLAGS) $(CXXFLAGSSHARED) $(EXTRAFLAGS) -c -o $@ $< $(extIncDir)

$(EXTTMPDIR)/%.o: %.F $(EXTTMPDIR)/%.mk
	$(FC) $(FFLAGS) $(FFLAGSSHARED) -c -o $@ $< -I$(INCLUDEDIR)

$(EXTTMPDIR)/%.o: %.f $(EXTTMPDIR)/%.mk
	$(FC) $(FFLAGS) $(FFLAGSSHARED) -c -o $@ $< -I$(INCLUDEDIR)

.PHONY: clean

clean:
	rm -f *~
	rm -f $(objects) $(mkfiles) $(extobjects) $(extmkfiles)
	rm -f $(LIB_SHARED) 
	rm -f $(LIB_ARCHIVE)
	rm -f $(LIBEXT_SHARED)
	rm -f $(LIBEXT_ARCHIVE)
