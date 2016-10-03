# platform - dependant files
include ../make.inc


all:authors

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
CPC: authors
cpc: authors
babar:BaBar
belle:Belle

#-----
authors: HEPEVT clean  tauola-dir
	@echo Preparing CLEO version of tauola in ../tauola
	@( rm -f standalone-F/*.f )
#     fill directory prod of CLEO version
	@( $(CPP) $(CPPFLAGS) -DAUTHORS    formf.F prod/formf.f)
	@( $(CPP) $(CPPFLAGS) -DAUTHORS    tauola.F prod/tauola.f)
	@( $(CPP) $(CPPFLAGS) -DAUTHORS    curr_cleo.F prod/curr_cleo.f)
	@( $(CPP) $(CPPFLAGS) -DAUTHORS    f3pi.F prod/f3pi.f)
	@( $(CPP) $(CPPFLAGS) -DAUTHORS    pkorb.F prod/pkorb.f)
	@( cp  makefile.cleo prod/makefile) 
	@( cp  makefile.templ.cleo prod/makefile.templ) 
#     create interfaces 
	@( $(CPP) $(CPPFLAGS) -DAUTHORS    tauface-KK-F/Tauface.F tauface-KK-F/Tauface.f)
	@( $(CPP) $(CPPFLAGS) -DAUTHORS  standalone-F/taumain.F standalone-F/taumain.f)  
	@( $(CPP) $(CPPFLAGS) -DAUTHORS  jetset-F/tauola_photos_ini.F jetset-F/tauola_photos_ini.f)
	@( $(CPP) $(CPPFLAGS) -DCLEO  jetset-F/tauface-jetset.F jetset-F/tauface-jetset.f)  
#     fill true (external) ../tauola directory 
	@( cp prod/*.f ../tauola/.)
	@( cp  makefile.cleo ../tauola/makefile) 
	@( cp  makefile.templ.cleo ../tauola/makefile.templ) 
	@( cp  standalone-F/taumain.f ../tauola/demo-standalone/taumain.f )    
	@( cp  tauface-KK-F/Tauface.f ../tauola/demo-KK-face )
	@(cp jetset-F/tauola_photos_ini.f ../tauola/demo-jetset/tauola_photos_ini.f)
	@(cp jetset-F/tauface-jetset.f ../tauola/demo-jetset/tauface-jetset.f)
	@echo "AUTHORS" > ../tauola/version
	@echo "        **************************************************"
	@echo "        *                                                *"
	@echo "        *        This is 'prototype TAUOLA'              *"
	@echo "        *                                                *"
	@echo "        *         version for future developments        *"
	@echo "        *        It is oriented for fits with precision  *"
	@echo "        *        data. For version >cleo< is supplemented*"  
	@echo "        *        with many empty slots in particular 5pi *"
	@echo "        *        options available are: authors          *"
	@echo "        *        other: BaBar Belle                      *"
	@echo "        *        are prepared but empty                  *"
	@echo "        *        WARNING: demos will take much longer    *"
	@echo "        *        to run than in standard versions        *"
	@echo "        **************************************************"
#----
#-----
BaBar: HEPEVT clean  tauola-dir
	@echo Preparing CLEO version of tauola in ../tauola
	@( rm -f standalone-F/*.f )
#     fill directory prod of CLEO version
	@( $(CPP) $(CPPFLAGS) -DBABAR    formf.F prod/formf.f)
	@( $(CPP) $(CPPFLAGS) -DBABAR    tauola.F prod/tauola.f)
	@( $(CPP) $(CPPFLAGS) -DBABAR    curr_cleo.F prod/curr_cleo.f)
	@( $(CPP) $(CPPFLAGS) -DBABAR    f3pi.F prod/f3pi.f)
	@( $(CPP) $(CPPFLAGS) -DBABAR   pkorb.F prod/pkorb.f)
	@( cp  makefile.cleo prod/makefile) 
	@( cp  makefile.templ.cleo prod/makefile.templ) 
#     create interfaces 
	@( $(CPP) $(CPPFLAGS) -DBABAR    tauface-KK-F/Tauface.F tauface-KK-F/Tauface.f)
	@( $(CPP) $(CPPFLAGS) -DBABAR standalone-F/taumain.F standalone-F/taumain.f)  
	@( $(CPP) $(CPPFLAGS) -DBABAR  jetset-F/tauola_photos_ini.F jetset-F/tauola_photos_ini.f)
	@( $(CPP) $(CPPFLAGS) -DBABAR  jetset-F/tauface-jetset.F jetset-F/tauface-jetset.f)  
#     fill true (external) ../tauola directory 
	@( cp prod/*.f ../tauola/.)
	@( cp  makefile.cleo ../tauola/makefile) 
	@( cp  makefile.templ.cleo ../tauola/makefile.templ) 
	@( cp  standalone-F/taumain.f ../tauola/demo-standalone/taumain.f )    
	@( cp  tauface-KK-F/Tauface.f ../tauola/demo-KK-face )
	@(cp jetset-F/tauola_photos_ini.f ../tauola/demo-jetset/tauola_photos_ini.f)
	@(cp jetset-F/tauface-jetset.f ../tauola/demo-jetset/tauface-jetset.f)
	@echo "BABAR" > ../tauola/version
	@echo "        **************************************************"
	@echo "        *                                                *"
	@echo "        *        This is 'standard TAUOLA' version       *"
	@echo "        *                                                *"
	@echo "        *        for future developments                 *"
	@echo "        *        starting pont is old version cleo       *"  
	@echo "        *        called now authors                      *"
	@echo "        *        other options: authors Belle            *"
	@echo "        **************************************************"
#----
#-----
Belle: HEPEVT clean  tauola-dir
	@echo Preparing CLEO version of tauola in ../tauola
	@( rm -f standalone-F/*.f )
#     fill directory prod of CLEO version
	@( $(CPP) $(CPPFLAGS) -DBELLE    formf.F prod/formf.f)
	@( $(CPP) $(CPPFLAGS) -DBELLE    tauola.F prod/tauola.f)
	@( $(CPP) $(CPPFLAGS) -DBELLE    curr_cleo.F prod/curr_cleo.f)
	@( $(CPP) $(CPPFLAGS) -DBELLE   f3pi.F prod/f3pi.f)
	@( $(CPP) $(CPPFLAGS) -DBELLE   pkorb.F prod/pkorb.f)
	@( cp  makefile.cleo prod/makefile) 
	@( cp  makefile.templ.cleo prod/makefile.templ) 
#     create interfaces 
	@( $(CPP) $(CPPFLAGS) -DBELLE    tauface-KK-F/Tauface.F tauface-KK-F/Tauface.f)
	@( $(CPP) $(CPPFLAGS) -DBELLE standalone-F/taumain.F standalone-F/taumain.f)  
	@( $(CPP) $(CPPFLAGS) -DBELLE  jetset-F/tauola_photos_ini.F jetset-F/tauola_photos_ini.f)
	@( $(CPP) $(CPPFLAGS) -DBELLE  jetset-F/tauface-jetset.F jetset-F/tauface-jetset.f)  
#     fill true (external) ../tauola directory 
	@( cp prod/*.f ../tauola/.)
	@( cp  makefile.cleo ../tauola/makefile) 
	@( cp  makefile.templ.cleo ../tauola/makefile.templ) 
	@( cp  standalone-F/taumain.f ../tauola/demo-standalone/taumain.f )    
	@( cp  tauface-KK-F/Tauface.f ../tauola/demo-KK-face )
	@(cp jetset-F/tauola_photos_ini.f ../tauola/demo-jetset/tauola_photos_ini.f)
	@(cp jetset-F/tauface-jetset.f ../tauola/demo-jetset/tauface-jetset.f)
	@echo "BELLE" > ../tauola/version
	@echo "        **************************************************"
	@echo "        *                                                *"
	@echo "        *        This is 'standard TAUOLA' version       *"
	@echo "        *                                                *"
	@echo "        *        for future developments                 *"
	@echo "        *        starting pont is old version cleo       *"  
	@echo "        *        called now Belle                        *"
	@echo "        *        other options: BaBar authors            *"
	@echo "        **************************************************"
#----
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
	@(cp standalone-F/prod/tauola.authors ../tauola/demo-standalone/prod/tauola.authors)
	@(cp standalone-F/prod/tauola.belle ../tauola/demo-standalone/prod/tauola.belle)
	@(cp standalone-F/prod/tauola.babar ../tauola/demo-standalone/prod/tauola.babar)
	@(cp jetset-F/demo.f ../tauola/demo-jetset/demo.f)
	@(cp jetset-F/makefile-F ../tauola/demo-jetset/makefile)
	@(cp jetset-F/README ../tauola/demo-jetset/README)
	@(cp jetset-F/prod/go ../tauola/demo-jetset/prod/go)
	@(cp jetset-F/prod/demo.authors ../tauola/demo-jetset/prod/demo.authors)
	@(cp jetset-F/prod/demo.belle ../tauola/demo-jetset/prod/demo.belle)
	@(cp jetset-F/prod/demo.babar ../tauola/demo-jetset/prod/demo.babar)
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

