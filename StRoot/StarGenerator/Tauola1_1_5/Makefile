include make.inc

LIB_VER = 1.0.0

#Name of libraries to create
LIB_TAUOLA_CXX_INT_SO = libTauolaCxxInterface.so
LIB_TAUOLA_CXX_INT_A  = libTauolaCxxInterface.a
LIB_TAUOLA_FORTRAN_SO = libTauolaFortran.so
LIB_TAUOLA_FORTRAN_A  = libTauolaFortran.a

TAUOLA_CXX_INT_OBJECTS = src/$(FORTRAN_TAUOLA_INTERFACE_DIR)/*.o \
                         src/$(EVENT_RECORD_INTERFACE_DIR)/*.o \
                         src/$(C_TAUOLA_INTERFACE_DIR)/*.o \
                         src/$(UTILITIES_DIR)/*.o

# If RChL currents exist - add additional objects to the library
ifneq "$(wildcard tauola-fortran/tauola-modified/new-currents/ )" ""

TAUOLA_RCHL_OBJECTS = tauola-fortran/tauola-modified/new-currents/RChL-currents/*/*.o \
                      tauola-fortran/tauola-modified/new-currents/other-currents/*.o

TAUOLA_FORTRAN_DIR = tauola-fortran/tauola-modified
else
TAUOLA_RCHL_OBJECTS =
TAUOLA_FORTRAN_DIR = tauola-fortran/tauola
endif

TAUOLA_FORTRAN_OBJECTS = $(TAUOLA_FORTRAN_DIR)/*.o $(TAUOLA_RCHL_OBJECTS)

#directories containing source code
EVENT_RECORD_INTERFACE_DIR   = eventRecordInterfaces
FORTRAN_TAUOLA_INTERFACE_DIR = tauolaFortranInterfaces
C_TAUOLA_INTERFACE_DIR       = tauolaCInterfaces
UTILITIES_DIR                = utilities

##### Link objects to make library ######
all: tauola_fortran src tau_spinner

install:
	mkdir -p $(PREFIX)/include
	mkdir -p $(PREFIX)/include/Tauola
	cp include/Tauola/* $(PREFIX)/include/Tauola/.
	mkdir -p $(PREFIX)/lib
	cp lib/* $(PREFIX)/lib/.

src: include_dir $(EVENT_RECORD_INTERFACE_DIR) $(FORTRAN_TAUOLA_INTERFACE_DIR) $(C_TAUOLA_INTERFACE_DIR) $(UTILITIES_DIR)
	ar cr lib/$(LIB_TAUOLA_CXX_INT_A) $(TAUOLA_CXX_INT_OBJECTS)
	ar cr lib/$(LIB_TAUOLA_FORTRAN_A) $(TAUOLA_FORTRAN_OBJECTS)
	$(LD) $(LDFLAGS) $(SOFLAGS) -o lib/$(LIB_TAUOLA_CXX_INT_SO).$(LIB_VER) $(TAUOLA_CXX_INT_OBJECTS)
	$(LD) $(LDFLAGS) $(SOFLAGS) -o lib/$(LIB_TAUOLA_FORTRAN_SO).$(LIB_VER) $(TAUOLA_FORTRAN_OBJECTS)
	ln -sf $(LIB_TAUOLA_CXX_INT_SO).$(LIB_VER) lib/$(LIB_TAUOLA_CXX_INT_SO)
	ln -sf $(LIB_TAUOLA_FORTRAN_SO).$(LIB_VER) lib/$(LIB_TAUOLA_FORTRAN_SO)
	@echo "##################################################################"	
	@echo " Tauola C++ Interface library created and moved to lib/ directory "
	@echo "##################################################################"
	@echo ""
	@echo "##################################################################"	
	@echo " To run examples, cd examples/ directory and there './configure'  "
	@echo " and 'make' again. Examples require Pythia8, ROOT and MC-Tester   "
	@echo "  installed. For details see examples/README.                     "
	@echo "##################################################################"


####### Make object files ########
include_dir:
	mkdir -p include
	mkdir -p include/Tauola

$(FORTRAN_TAUOLA_INTERFACE_DIR):
	make -C src/$(FORTRAN_TAUOLA_INTERFACE_DIR)
	cp src/$(FORTRAN_TAUOLA_INTERFACE_DIR)/*.h include/Tauola

$(EVENT_RECORD_INTERFACE_DIR):
	make -C src/$(EVENT_RECORD_INTERFACE_DIR)
	cp src/$(EVENT_RECORD_INTERFACE_DIR)/*.h include/Tauola

$(C_TAUOLA_INTERFACE_DIR):
	make -C src/$(C_TAUOLA_INTERFACE_DIR)
	cp src/$(C_TAUOLA_INTERFACE_DIR)/*.h include/Tauola

$(UTILITIES_DIR):
	make -C src/$(UTILITIES_DIR)
	cp src/$(UTILITIES_DIR)/*.h include/Tauola

tau_spinner: src
ifeq ($(MAKE_TAU_SPINNER),yes)
	make libTauSpinner -C TauSpinner
	make move -C TauSpinner
endif

tauola_fortran:
ifneq "$(wildcard tauola-fortran/tauola-F )" ""
	make 10kD-all -C tauola-fortran/photos-F
	make CLEO -C tauola-fortran/tauola-F
else
	echo "\nUsing pre-constructed 'tauola-fortran/tauola'\n"
endif
	make -C $(TAUOLA_FORTRAN_DIR)
	@echo "#################################################"
	@echo "      tauola-fortran compiled successfully.      "
	@echo "#################################################"
	@true

athena: clean_src #modify for athena environment
	cp -rf src src-BCP
	cp -rf tauola-fortran/tauola-F tauola-fortran/tauola-F-BCP
	./platform/to-Athena.sh

athena_revert: #revert from athena environment modification
	rm -rf src
	mv src-BCP src
	rm -rf tauola-fortran/tauola-F
	mv tauola-fortran/tauola-F-BCP tauola-fortran/tauola-F
	rm -f platform/to-Athena.dependencies
	make clean

clean_src:
	make clean -C src/$(EVENT_RECORD_INTERFACE_DIR)
	make clean -C src/$(FORTRAN_TAUOLA_INTERFACE_DIR)
	make clean -C src/$(C_TAUOLA_INTERFACE_DIR)
	make clean -C src/$(UTILITIES_DIR)

clean: clean_src
ifneq "$(wildcard tauola-fortran/tauola-F )" ""
	make Clean -C tauola-fortran
endif
	make clean -C $(TAUOLA_FORTRAN_DIR)
	make clean -C TauSpinner
	rm -f *~

Clean: clean
	rm -f lib/* include/Tauola/*
	rmdir -p include/Tauola
	rm -rf config.log config.status autom4te.cache configure.paths.sh
	rm -f platform/make.inc make.inc
	rm -f examples/make.inc

make.inc:
	@echo ""
	@echo "Please execute ./configure first!"
	@echo "(Consider using 'source platform/afs.paths.sh' [or .csh] first)"
	@echo ""
	@false

always:
	@true
