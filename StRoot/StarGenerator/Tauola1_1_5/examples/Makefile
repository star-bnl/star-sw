include make.inc

ifneq ($(TAUOLALOCATION), )
include $(TAUOLALOCATION)/make.inc
endif

ROOTLIB       :=  $(shell root-config --glibs)
ROOTINC       :=  $(shell root-config --cflags)
ROOTLIBPATH   :=  $(shell root-config --libdir)
PYTHIA_LIBS    = -L$(PYTHIALOCATION)/lib/archive -lpythia8 -llhapdfdummy -lhepmcinterface
MC_TESTER_LIBS = -L$(MCTESTERLOCATION)/lib -lHEPEvent -lHepMCEvent -lMCTester
TAUOLA_LIBS    = -L$(TAUOLALOCATION)/lib -lTauolaCxxInterface -lTauolaFortran
HEPMC_LIBS     =

ifneq ($(HEPMCLOCATION), )
  override HEPMC_LIBS += -L$(HEPMCLOCATION)/lib -lHepMC
endif

# Check for newer versions of pythia8
ifneq ($(PYTHIA8180_OR_LATER), )
  override CFLAGS      += -DPYTHIA8180_OR_LATER
  override PYTHIA_LIBS  = -L$(PYTHIALOCATION)/lib/archive -lpythia8 -llhapdfdummy -lpythia8tohepmc
endif


all:
	@make taumain_hepevt_example.exe
ifneq ($(HEPMCLOCATION), )
	@make taumain_stand_alone_example.exe
ifneq ($(PYTHIALOCATION), )
	@make single_tau_gun_example.exe
	@make taummk_pythia_example.exe
ifneq ($(MCTESTERLOCATION), )
	@make taumain_pythia_example.exe
endif
endif
endif
	@echo "##################################################################"
	@echo " Examples compiled successfully. Available examples:"
	@if test -e taumain_hepevt_example.exe; then \
	echo " ./taumain_hepevt_example.exe"; fi
	@if test -e taumain_stand_alone_example.exe; then \
	echo " ./taumain_stand_alone_example.exe"; fi
	@if test -e single_tau_gun_example.exe; then \
	echo " ./single_tau_gun_example.exe"; fi
	@if test -e taummk_pythia_example.exe; then \
	echo " ./taummk_pythia_example.exe"; fi
	@if test -e taumain_pythia_example.exe; then \
	echo " ./taumain_pythia_example.exe"; fi
	@echo "##################################################################"


# Universal example (all paths presents)
%.o: %.c
	$(CC) $(CFLAGS) -I$(TAUOLALOCATION)/include -I$(PYTHIALOCATION)/include -I$(MCTESTERLOCATION)/include \
	-I$(HEPMCLOCATION)/include $(ROOTINC) -c $< -o $@

# Universal example (all paths presents) - linking procedure
%.exe: %.o
	$(CC) -o $@ $(LDFLAGS) $< $(ROOTLIB) \
	$(PYTHIA_LIBS) $(MC_TESTER_LIBS) $(HEPMC_LIBS) $(TAUOLA_LIBS) \
	-Wl,-rpath,$(TAUOLALOCATION)/lib \
	-Wl,-rpath,$(ROOTLIBPATH) \
	-Wl,-rpath,$(HEPMCLOCATION)/lib \
	-Wl,-rpath,$(PYTHIALOCATION)/lib \
	-Wl,-rpath,$(MCTESTERLOCATION)/lib

# Version for tau gun example - HepMC & Pythia8 required
single_tau_gun_example.exe: single_tau_gun_example.c
	$(CC) $(CFLAGS) -I$(TAUOLALOCATION)/include -I$(HEPMCLOCATION)/include \
	-I$(PYTHIALOCATION)/include -c $<
	$(CC) -o $@ $(LDFLAGS) $(<:.c=.o) $(HEPMC_LIBS) $(TAUOLA_LIBS) $(PYTHIA_LIBS) \
	-Wl,-rpath,$(TAUOLALOCATION)/lib \
	-Wl,-rpath,$(HEPMCLOCATION)/lib \
	-Wl,-rpath,$(PYTHIALOCATION)/lib

# Version for taummk example - HepMC & Pythia8 required
taummk_pythia_example.exe: taummk_pythia_example.c
	$(CC) $(CFLAGS) -I$(TAUOLALOCATION)/include -I$(HEPMCLOCATION)/include \
	-I$(PYTHIALOCATION)/include -c $<
	$(CC) -o $@ $(LDFLAGS) $(<:.c=.o) $(HEPMC_LIBS) $(TAUOLA_LIBS) $(PYTHIA_LIBS) \
	-Wl,-rpath,$(TAUOLALOCATION)/lib \
	-Wl,-rpath,$(HEPMCLOCATION)/lib \
	-Wl,-rpath,$(PYTHIALOCATION)/lib

# Version for standalone example - only HepMC required
taumain_stand_alone_example.exe: taumain_stand_alone_example.c
	$(CC) $(CFLAGS) -I$(TAUOLALOCATION)/include -I$(HEPMCLOCATION)/include -c $<
	$(CC) -o $@ $(LDFLAGS) $(<:.c=.o) $(HEPMC_LIBS) $(TAUOLA_LIBS) \
	-Wl,-rpath,$(TAUOLALOCATION)/lib \
	-Wl,-rpath,$(HEPMCLOCATION)/lib

# Version vithout HePMC
taumain_hepevt_example.exe: taumain_hepevt_example.c
	$(CC) $(CFLAGS) -I$(TAUOLALOCATION)/include -c $<
	$(CC) -o $@ $(LDFLAGS) $(<:.c=.o) $(TAUOLA_LIBS) $(HEPMC_LIBS) \
	-Wl,-rpath,$(TAUOLALOCATION)/lib \
	-Wl,-rpath,$(HEPMCLOCATION)/lib


clean:
	rm -f *.o *.a *~ *.exe
	rm -f testing/*.o testing/*.exe

Clean: clean
	rm -rf config.log config.status autom4te.cache configure.paths.sh
	rm -f make.inc

make.inc:
	@echo ""
	@echo "Please execute ./configure first!"
	@echo ""
	@false

$(TAUOLALOCATION)/make.inc:
	@echo ""
	@echo "Please compile the interface first!"
	@echo ""
	@false
