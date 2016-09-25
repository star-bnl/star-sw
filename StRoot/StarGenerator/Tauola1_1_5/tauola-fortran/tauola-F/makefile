# platform - dependant files
include ../make.inc


all:CePeCe

#######################
clean:  
	@echo "Cleanup... "
	@rm -f *.o
	@rm -f *.a
	@rm -f *~
	@rm -f prod/*.f 
	@rm -f *.diff 
	@rm -rf ../tauola 

#######################

######################################
###Check if link to HEPEVT.h exist...

HEPEVT:
	@if test ! -r ../include/HEPEVT.h; \
	then echo "You must execute make xxx-all in PHOTOS-F directory first!!!";\
	false; fi

#######################################
CPC: CePeCe
cpc: CePeCe
cleo:CLEO
aleph:ALEPH

CePeCe: HEPEVT clean tauola-dir
	@echo Preparing CPC version of tauola in ../tauola
	@( rm -f standalone-F/*.f )
#     fill directory prod of CPC version
	@( $(CPP) $(CPPFLAGS) -DCePeCe  formf.F prod/formf.f )
	@( $(CPP) $(CPPFLAGS) -DCePeCe  tauola.F prod/tauola.f)       
	@( cp  makefile.cpc prod/makefile) 
	@( cp  makefile.templ.cpc prod/makefile.templ)
#     create interfaces 
	@( $(CPP) $(CPPFLAGS) -DCePeCe  tauface-KK-F/Tauface.F tauface-KK-F/Tauface.f)
	@( $(CPP) $(CPPFLAGS) -DCePeCe  standalone-F/taumain.F standalone-F/taumain.f) 
	@( $(CPP) $(CPPFLAGS) -DCePeCe  jetset-F/tauola_photos_ini.F jetset-F/tauola_photos_ini.f) 
	@( $(CPP) $(CPPFLAGS) -DCePeCe  jetset-F/tauface-jetset.F jetset-F/tauface-jetset.f) 
#     fill true (external) ../tauola directory 
	@( cp prod/*.f ../tauola/.)
	@( cp  makefile.cpc ../tauola/makefile) 
	@( cp  makefile.templ.cpc ../tauola/makefile.templ) 
	@( cp  standalone-F/taumain.f ../tauola/demo-standalone/taumain.f ) 
	@( cp  tauface-KK-F/Tauface.f ../tauola/demo-KK-face )
	@(cp jetset-F/tauola_photos_ini.f ../tauola/demo-jetset/tauola_photos_ini.f)
	@(cp jetset-F/tauface-jetset.f ../tauola/demo-jetset/tauface-jetset.f)
	@echo "CPC">../tauola/version    
	@echo "        **************************************************"
	@echo "        *                                                *"
	@echo "        *        This is 'standard TAUOLA' version       *"
	@echo "        *                                                *"
	@echo "        *        You can build another one.              *"
	@echo "        *        For details execute info-script         *"  
	@echo "        *        ../tauola-factory/glue                  *"
	@echo "        *                                                *"
	@echo "        **************************************************"
#-----
CLEO: HEPEVT clean  tauola-dir
	@echo Preparing CLEO version of tauola in ../tauola
	@( rm -f standalone-F/*.f )
#     fill directory prod of CLEO version
	@( $(CPP) $(CPPFLAGS) -DCLEO    formf.F prod/formf.f)
	@( $(CPP) $(CPPFLAGS) -DCLEO    tauola.F prod/tauola.f)
	@( $(CPP) $(CPPFLAGS) -DCLEO    curr_cleo.F prod/curr_cleo.f)
	@( $(CPP) $(CPPFLAGS) -DCLEO    f3pi.F prod/f3pi.f)
	@( $(CPP) $(CPPFLAGS) -DCLEO    pkorb.F prod/pkorb.f)
	@( cp  makefile.cleo prod/makefile) 
	@( cp  makefile.templ.cleo prod/makefile.templ) 
#     create interfaces 
	@( $(CPP) $(CPPFLAGS) -DCLEO    tauface-KK-F/Tauface.F tauface-KK-F/Tauface.f)
	@( $(CPP) $(CPPFLAGS) -DCLEO  standalone-F/taumain.F standalone-F/taumain.f)  
	@( $(CPP) $(CPPFLAGS) -DCLEO  jetset-F/tauola_photos_ini.F jetset-F/tauola_photos_ini.f)
	@( $(CPP) $(CPPFLAGS) -DCLEO  jetset-F/tauface-jetset.F jetset-F/tauface-jetset.f)  
#     fill true (external) ../tauola directory 
	@( cp prod/*.f ../tauola/.)
	@( cp  makefile.cleo ../tauola/makefile) 
	@( cp  makefile.templ.cleo ../tauola/makefile.templ) 
	@( cp  standalone-F/taumain.f ../tauola/demo-standalone/taumain.f )    
	@( cp  tauface-KK-F/Tauface.f ../tauola/demo-KK-face )
	@(cp jetset-F/tauola_photos_ini.f ../tauola/demo-jetset/tauola_photos_ini.f)
	@(cp jetset-F/tauface-jetset.f ../tauola/demo-jetset/tauface-jetset.f)
	@echo "CLEO" > ../tauola/version
	@echo "        **************************************************"
	@echo "        *                                                *"
	@echo "        *        This is 'standard TAUOLA' version       *"
	@echo "        *                                                *"
	@echo "        *        You can build another one.              *"
	@echo "        *        For details execute info-script         *"  
	@echo "        *        ../tauola-factory/glue                  *"
	@echo "        *                                                *"
	@echo "        **************************************************"
#----
ALEPH: HEPEVT  clean  tauola-dir
	@echo Preparing ALEPH version of tauola in ../tauola
	@( rm -f standalone-F/*.f )
#     fill directory prod of ALEPH version
	@$(CPP) $(CPPFLAGS) -DALEPH   formf.F prod/formf.f
	@$(CPP) $(CPPFLAGS) -DALEPH   tauola.F prod/tauola.f
	@( cp  makefile.cpc prod/makefile) 
	@( cp  makefile.templ.cpc prod/makefile.templ) 
#     create interfaces 
	@( $(CPP) $(CPPFLAGS) -DALEPH    tauface-KK-F/Tauface.F tauface-KK-F/Tauface.f)
	@( $(CPP) $(CPPFLAGS) -DALEPH  standalone-F/taumain.F standalone-F/taumain.f)
	@( $(CPP) $(CPPFLAGS) -DALEPH  jetset-F/tauola_photos_ini.F jetset-F/tauola_photos_ini.f)
	@( $(CPP) $(CPPFLAGS) -DALEPH  jetset-F/tauface-jetset.F jetset-F/tauface-jetset.f) 
#     fill true (external) ../tauola directory 
	@( cp prod/*.f ../tauola/.)
	@( cp  makefile.cpc ../tauola/makefile) 
	@( cp  makefile.templ.cpc ../tauola/makefile.templ) 
	@( cp  standalone-F/taumain.f ../tauola/demo-standalone/taumain.f )    
	@( cp  tauface-KK-F/Tauface.f ../tauola/demo-KK-face )
	@(cp jetset-F/tauola_photos_ini.f ../tauola/demo-jetset/tauola_photos_ini.f)
	@(cp jetset-F/tauface-jetset.f ../tauola/demo-jetset/tauface-jetset.f)
	@echo "ALEPH" > ../tauola/version
	@echo "        **************************************************"
	@echo "        *                                                *"
	@echo "        *        This is 'standard TAUOLA' version       *"
	@echo "        *                                                *"
	@echo "        *        You can build another one.              *"
	@echo "        *        For details execute info-script         *"  
	@echo "        *        ../tauola-factory/glue                  *"
	@echo "        *                                                *"
	@echo "        **************************************************"
#----
#----
Tauface:
	@echo "Copying Tauface to ../KK2f"
	@(cp tauface-KK-F/Tauface.f ../KK2f/Tauface.f)

tauola-dir: ../tauola ../tauola/demo-standalone ../tauola/demo-standalone/prod ../tauola/demo-jetset ../tauola/demo-jetset/prod ../tauola/demo-KK-face
	@echo "Preparing distribution directory ../tauola"
	@(cp standalone-F/makefile ../tauola/demo-standalone/makefile)
	@(cp standalone-F/README ../tauola/demo-standalone/README)
	@(cp standalone-F/prod/go ../tauola/demo-standalone/prod/go)
	@(cp standalone-F/prod/dane.dat ../tauola/demo-standalone/prod/dane.dat)
	@(cp standalone-F/prod/tauola.cpc ../tauola/demo-standalone/prod/tauola.cpc)
	@(cp standalone-F/prod/tauola.aleph ../tauola/demo-standalone/prod/tauola.aleph)
	@(cp standalone-F/prod/tauola.cleo ../tauola/demo-standalone/prod/tauola.cleo)
	@(cp jetset-F/demo.f ../tauola/demo-jetset/demo.f)
	@(cp jetset-F/makefile-F ../tauola/demo-jetset/makefile)
	@(cp jetset-F/README ../tauola/demo-jetset/README)
	@(cp jetset-F/prod/go ../tauola/demo-jetset/prod/go)
	@(cp jetset-F/prod/demo.cpc ../tauola/demo-jetset/prod/demo.cpc)
	@(cp jetset-F/prod/demo.aleph ../tauola/demo-jetset/prod/demo.aleph)
	@(cp jetset-F/prod/demo.cleo ../tauola/demo-jetset/prod/demo.cleo)
	@(cp tauface-KK-F/README ../tauola/demo-KK-face/README)
../tauola:
	@mkdir -p ../tauola
../tauola/demo-standalone : ../tauola
	@mkdir -p ../tauola/demo-standalone
../tauola/demo-standalone/prod: ../tauola/demo-standalone
	@mkdir -p ../tauola/demo-standalone/prod
../tauola/demo-jetset: ../tauola/
	@mkdir -p ../tauola/demo-jetset	
../tauola/demo-jetset/prod: ../tauola/
	@mkdir -p ../tauola/demo-jetset/prod	
../tauola/demo-KK-face: ../tauola
	@mkdir -p ../tauola/demo-KK-face
pythia:
	@(cp -rf ../tauola/demo-jetset  ../tauola/demo-pythia)
	@(cp     ../eli/README ../tauola/demo-pythia/.)
	@(cp     ../eli/demo.f ../tauola/demo-pythia/.)
	@(cp     ../eli/makefile ../tauola/demo-pythia/.)
	@(cp  -rf ../eli/demo-pythia.* ../tauola/demo-pythia/prod/.)
