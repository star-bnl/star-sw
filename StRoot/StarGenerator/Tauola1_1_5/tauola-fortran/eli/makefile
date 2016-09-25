include ../../make.inc
COMMAND = demo.exe
COMMAND_OBJECTS = demo.o tauface-jetset.o tauola_photos_ini.o
EXTER_LIB4  = ../glib.a
CERNLIB = ../..
JETSET =  ${CERNLIB}/jetset2/libpythia.a
PHOTOS =  ${CERNLIB}/photos/glib.a 
CLIBS = $(JETSET) $(PHOTOS)
############
all:	$(COMMAND)
	@echo "==================================================="
	@echo "Warning: HEPEVT must be 4000 double precision      "
	@echo "         for this non-complete demo                "
	@echo "Warning: If your run stops with: INITWK: WRONG IDFX"
	@echo "         check ..include/README-phyfix             "
	@echo "==================================================="

.f.o:
	$(F77) $(FFLAGS) -c $<
$(EXTER_LIB4):
	make -C ..
$(JETSET):
	make -C ${CERNLIB}/jetset2
$(PHOTOS):
	make -C $(CERNLIB)/photos	
# Link procedure
$(COMMAND): $(COMMAND_OBJECTS) $(EXTER_LIB4) $(CLIBS)
	$(LD) -o  $@ $(LDFLAGS) $(COMMAND_OBJECTS) $(EXTER_LIB3) $(EXTER_LIB4) $(CLIBS) 
run:
	./$(COMMAND)
	mv demo.out prod/
	@echo " make a choice:           "
	@echo " diff prod/demo-pythia.cpc   prod/demo.out "
	@echo " diff prod/demo-pythia.aleph prod/demo.out "
	@echo " diff prod/demo-pythia.cleo  prod/demo.out "
	
clean:                                                     
	rm -f *.o *.a  *~ *.exe
Clean:	clean
	cd prod; rm -f  *~ *.out *.ntup



	 

