# modified on 6 Dec 99 by Piotr Golonka
include ../make.inc
all:
	@echo
	@echo "==================================================="
	@echo "                PHOTOS-F"
	@echo
	@echo "You need to specify one of targets to make:"
	@echo "   KK-all   ( or KK   for preprocess stage only"
	@echo "   4kD-all  ( or 4kD  for preprocess stage only"
	@echo "   2kR-all  ( or 2kR  for preprocess stage only"
	@echo "   2kD-all  ( or 2kD  for preprocess stage only"
	@echo "   10kD-all ( or 10kD for preprocess stage only"
	@echo
	@echo "==================================================="             
clean:
	@echo Cleaning ...	
	@rm -f *.o; rm -f *.a
	@rm -f photos.f
	@rm -f *~ 
	@echo "   Removing old instalation..."   
	@rm -rf ../photos
	@rm -rf ../include/HEPEVT.h
	@(cd ../jetset;	make clean)
KK:
	@echo Preprocessing photos.F for KK ...
	@echo Creating symbolic link for HEPEVT include file:
	@ln -s ../include/HEPEVT-KKMC.h ../include/HEPEVT.h
	@$(CPP) $(CPPFLAGS) -DKORALK  photos.F photos.f
	@$(CPP) $(CPPFLAGS) -DKORALK  demo/phodem.F demo/phodem.f
4kD:
	@echo Preprocessing photos.F for HEPEVT: 4000 DOUBLE PRECISSION ...
	@echo Creating symbolic link for HEPEVT include file:
	@ln -s ../include/HEPEVT-FOURD.h ../include/HEPEVT.h
	@$(CPP) $(CPPFLAGS) -DCZTERYD photos.F photos.f
	@$(CPP) $(CPPFLAGS) -DCZTERYD  demo/phodem.F demo/phodem.f
2kR:
	@echo Preprocessing photos.F for HEPEVT: 2000 REAL*4 ...
	@echo Creating symbolic link for HEPEVT include file:
	@ln -s ../include/HEPEVT-TWOR.h ../include/HEPEVT.h
	@$(CPP) $(CPPFLAGS) -DDWAR    photos.F photos.f
	@$(CPP) $(CPPFLAGS) -DDWAR  demo/phodem.F demo/phodem.f
2kD:
	@echo Preprocessing photos.F for HEPEVT: 2000 REAL*8 ...
	@echo Creating symbolic link for HEPEVT include file:
	@ln -s ../include/HEPEVT-TWOD.h ../include/HEPEVT.h
	@$(CPP) $(CPPFLAGS) -DDWAD    photos.F photos.f
	@$(CPP) $(CPPFLAGS) -DDWAD  demo/phodem.F demo/phodem.f
10kD:
	@echo Preprocessing photos.F for HEPEVT: 10000 DOUBLE PRECISSION ...
	@echo Creating symbolic link for HEPEVT include file:
	@ln -s ../include/HEPEVT-TEND.h ../include/HEPEVT.h
	@$(CPP) $(CPPFLAGS) -DDZIESD  photos.F photos.f
	@$(CPP) $(CPPFLAGS) -DDZIESD  demo/phodem.F demo/phodem.f

KK-all:   clean KK   install
4kD-all:  clean 4kD  install
2kR-all:  clean 2kR  install
2kD-all:  clean 2kD  install
10kD-all: clean 10kD install 

photos-fill: ../photos ../photos/demo ../photos/demo/prod
	@echo "Installing sources in ../photos"
	@(cp makefile-F ../photos/makefile)
	@(cp makefile.templ ../photos/makefile.templ)
	@(cp photos.f ../photos/photos.f)
	@(cp demo/makefile ../photos/demo/makefile)
	@(cp demo/prod/go ../photos/demo/prod/go)
	@(cp demo/prod/baza.output ../photos/demo/prod/baza.output)
	@(cp demo/prod/nowy.output ../photos/demo/prod/nowy.output)

demo-fill: demo/phodem.f ../photos/demo
	@echo "Installing demo in ../photos/demo"
	@(cp demo/phodem.f ../photos/demo/phodem.f)

photos-dir: ../photos ../photos/demo ../photos/demo/prod  ../photos/demo/tex
	@echo "Creating directories ..."
../photos:
	@mkdir -p ../photos
../photos/demo: ../photos
	@mkdir -p ../photos/demo
../photos/demo/prod: ../photos/demo
	@mkdir -p ../photos/demo/prod
../photos/demo/tex: ../photos/demo
	@mkdir -p ../photos/demo/tex

install:photos-dir photos-fill demo-fill lund-common

lund-common: 
	@echo " "
	@echo "    /---------------------------------------------------\ "
	@echo "    |                    WARNING  !                     | "
	@echo "    |   Before TAUOLA precompilation (in tauola-F dir)  | "
	@echo "    |   you may need change/replace                     | "
	@echo "    |   /LUJETS/  <==>  /PYJETS/                        | "
	@echo "    |   or their dimensionality. See README-phyfix      | "
	@echo "    |   in dir. TAUOLA/include. This affects universal  | "
	@echo "    |   interface only.                                 | "
	@echo "    \---------------------------------------------------/ " 
	@echo







