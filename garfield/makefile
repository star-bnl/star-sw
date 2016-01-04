#OBJDIR = $(GARFIELD_HOME)/.$(STAR_HOST_SYS)/Object
OBJDIR = $(STAR_OBJ)/garfield
SRCDIR = $(GARFIELD_HOME)/Source
INCDIR = $(GARFIELD_HOME)/Include
#LIBDIR = $(GARFIELD_HOME)/.$(STAR_HOST_SYS)/Library
LIBDIR = .
HEEDDIR = $(GARFIELD_HOME)/Heed

HEADERS = $(wildcard $(INCDIR)/*.hh)

SOURCES = $(wildcard $(SRCDIR)/*.cc)

OBJECTS = $(subst $(SRCDIR),$(OBJDIR),$(SOURCES:.cc=.o))
OBJECTS += $(OBJDIR)/magboltz.o 
OBJECTS += $(OBJDIR)/GarfieldDict.o

TARGETS = $(OBJECTS)
TARGETS += heed

# Fortran compiler
FC = `root-config --f77`

# Compilation flags
CPPFLAGS = -I$(INCDIR) -I$(HEEDDIR)
CFLAGS = -Wall -Wextra -pedantic -ansi -Wabi -Wno-long-long -Woverloaded-virtual \
	 `root-config --cflags` \
        -fpic -fno-common -c \
	$(CPPFLAGS) -DINS_CRETURN 

FFLAGS = -fpic -c

# Optimization flags
# CFLAGS += -Os
# FFLAGS += -Os
#CFLAGS += -O2
#FFLAGS += -O2
CFLAGS += -fPIC
FFLAGS += `root-config --ldflags` -fPIC
# Debug flags

CFLAGS += -g
FFLAGS += -g
ifneq ($(NODEBUG),) 
CFLAGS += -O
FFLAGS += -O
endif 
# Profiling flag
# CFLAGS += -pg

# Linking flags
LDFLAGS = `root-config --glibs` `root-config --ldflags` -lGeom
ifeq (,$(findstring darwin,$(STAR_HOST_SYS)))
LDFLAGS += -shared -Wl,-Bdynamic -lgfortran -lm
#lgfortran = $(shell gfortran `root-config --ldflags` -print-file-name=libgfortran.a)
#LDFLAGS += -shared -Wl,-Bdynamic $(lgfortran) -lm
#LDFLAGS += -shared  -Wl,--whole-archive -Wl,-Bstatic -Wl,-z -Wl,muldefs $(lgfortran)  -Wl,--no-whole-archive -Wl,-Bdynamic -lm
else
lgfortran = $(shell gfortran -print-file-name=libgfortran.dylib)
LDFLAGS += $(lgfortran) -lm
endif
.PHONY:	all  showbuild 
all: heed lib
lib:	$(LIBDIR)/libGarfield.so 
$(LIBDIR)/libGarfield.so:	$(OBJECTS) $(wildcard $(OBJDIR)/Heed/*.o)
	@echo Creating library libGarfield...
#	ar rcs $(LIBDIR)/libGarfield.a $(OBJECTS) \
#	$(wildcard $(OBJDIR)/Heed/*.o)
#	ranlib $(LIBDIR)/libGarfield.a
	`root-config --ld` -o $(LIBDIR)/libGarfield.so $(LDFLAGS)  $(OBJECTS) \
	$(wildcard $(OBJDIR)/Heed/*.o)
#	`root-config --ld` -o $(LIBDIR)/libGarfield.so `root-config --ldflags` -shared -Wl,-Bdynamic -lgfortran $<
	touch $(OBJDIR)/last_updated_on
	@echo Finished.


installdirs : 
	@if [ ! -d $(LIBDIR) ] ; then mkdir -p $(LIBDIR); \
	    echo "   >>>> Create $(LIBDIR)"; fi
	@if [ ! -d $(OBJDIR) ]; then mkdir -p $(OBJDIR); \
	    echo "   >>>> Create $(OBJDIR)"; \
        else echo " $(OBJDIR) already exists"; fi
	@if [ ! -d $(OBJDIR)/Heed/ ]; then mkdir -p $(OBJDIR)/Heed; \
	    echo "   >>>> Create $(OBJDIR)/Heed"; \
        else echo " $(OBJDIR)/Heed already exists"; fi
        
heed:	
	@echo Compiling Heed...
	cd Heed; make; cd -
	@touch $(OBJDIR)/last_updated_on

clean:
	@echo Removing object files...
	$(RM) $(OBJDIR)/*.o
	@echo Removing libraries...
	$(RM) $(LIBDIR)/*.a
	cd $(HEEDDIR); make clean
	@echo Removing dictionary...
	$(RM) $(OBJDIR)/GarfieldDict.C 

$(OBJDIR)/AvalancheMicroscopic.o: \
	$(SRCDIR)/AvalancheMicroscopic.cc \
	$(INCDIR)/AvalancheMicroscopic.hh \
	$(INCDIR)/FundamentalConstants.hh $(INCDIR)/GarfieldConstants.hh \
	$(INCDIR)/Random.hh \
	$(INCDIR)/Sensor.hh $(INCDIR)/Medium.hh $(INCDIR)/ViewDrift.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/AvalancheMC.o: \
	$(SRCDIR)/AvalancheMC.cc $(INCDIR)/AvalancheMC.hh \
	$(INCDIR)/FundamentalConstants.hh $(INCDIR)/GarfieldConstants.hh \
	$(INCDIR)/Random.hh \
	$(INCDIR)/Sensor.hh $(INCDIR)/Medium.hh $(INCDIR)/ViewDrift.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@      
$(OBJDIR)/DriftLineRKF.o: \
	$(SRCDIR)/DriftLineRKF.cc $(INCDIR)/DriftLineRKF.hh \
	$(INCDIR)/FundamentalConstants.hh \
	$(INCDIR)/Sensor.hh $(INCDIR)/Medium.hh $(INCDIR)/ViewDrift.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@
 
$(OBJDIR)/Track.o: \
	$(SRCDIR)/Track.cc $(INCDIR)/Track.hh \
	$(INCDIR)/ViewDrift.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@        
$(OBJDIR)/TrackBichsel.o: \
	$(SRCDIR)/TrackBichsel.cc $(INCDIR)/TrackBichsel.hh \
	$(INCDIR)/Track.hh $(SRCDIR)/Track.cc
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@       
$(OBJDIR)/TrackPAI.o: \
	$(SRCDIR)/TrackPAI.cc $(INCDIR)/TrackPAI.hh \
	$(INCDIR)/Track.hh $(SRCDIR)/Track.cc
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/TrackSimple.o: \
	$(SRCDIR)/TrackSimple.cc $(INCDIR)/TrackSimple.hh \
	$(INCDIR)/Track.hh $(SRCDIR)/Track.cc
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@        
$(OBJDIR)/TrackHeed.o: \
	$(SRCDIR)/TrackHeed.cc $(INCDIR)/TrackHeed.hh \
	$(INCDIR)/Track.hh $(SRCDIR)/Track.cc \
	$(HEEDDIR)/HeedChamber.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/TrackElectron.o: \
	$(SRCDIR)/TrackElectron.cc $(INCDIR)/TrackElectron.hh \
	$(INCDIR)/Track.hh $(SRCDIR)/Track.cc
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/ComponentBase.o: \
	$(SRCDIR)/ComponentBase.cc $(INCDIR)/ComponentBase.hh \
	$(INCDIR)/Medium.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/ComponentConstant.o: \
	$(SRCDIR)/ComponentConstant.cc $(INCDIR)/ComponentConstant.hh \
	$(SRCDIR)/ComponentBase.cc $(INCDIR)/ComponentBase.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/ComponentUser.o: \
	$(SRCDIR)/ComponentUser.cc $(INCDIR)/ComponentUser.hh \
	$(SRCDIR)/ComponentBase.cc $(INCDIR)/ComponentBase.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@       
$(OBJDIR)/ComponentAnalyticField.o: \
	$(SRCDIR)/ComponentAnalyticField.cc \
	$(INCDIR)/ComponentAnalyticField.hh \
	$(SRCDIR)/ComponentBase.cc $(INCDIR)/ComponentBase.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/ComponentNeBem2d.o: \
	$(SRCDIR)/ComponentNeBem2d.cc $(INCDIR)/ComponentNeBem2d.hh \
	$(SRCDIR)/ComponentBase.cc $(INCDIR)/ComponentBase.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@        
$(OBJDIR)/ComponentFieldMap.o: \
	$(SRCDIR)/ComponentFieldMap.cc $(INCDIR)/ComponentFieldMap.hh \
	$(SRCDIR)/ComponentBase.cc $(INCDIR)/ComponentBase.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/ComponentAnsys121.o: \
	$(SRCDIR)/ComponentAnsys121.cc $(INCDIR)/ComponentAnsys121.hh \
	$(SRCDIR)/ComponentFieldMap.cc $(INCDIR)/ComponentFieldMap.hh 
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/ComponentAnsys123.o: \
	$(SRCDIR)/ComponentAnsys123.cc $(INCDIR)/ComponentAnsys123.hh \
	$(SRCDIR)/ComponentFieldMap.cc $(INCDIR)/ComponentFieldMap.hh 
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/ComponentCST.o: \
	$(SRCDIR)/ComponentCST.cc $(INCDIR)/ComponentCST.hh \
	$(SRCDIR)/ComponentFieldMap.cc $(INCDIR)/ComponentFieldMap.hh 
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/ComponentElmer.o: \
	$(SRCDIR)/ComponentElmer.cc $(INCDIR)/ComponentElmer.hh \
	$(SRCDIR)/ComponentFieldMap.cc $(INCDIR)/ComponentFieldMap.hh 
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/ComponentTcad2d.o: \
	$(SRCDIR)/ComponentTcad2d.cc $(INCDIR)/ComponentTcad2d.hh \
	$(SRCDIR)/ComponentBase.cc $(INCDIR)/ComponentBase.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@   
$(OBJDIR)/ComponentTcad3d.o: \
	$(SRCDIR)/ComponentTcad3d.cc $(INCDIR)/ComponentTcad3d.hh \
	$(SRCDIR)/ComponentBase.cc $(INCDIR)/ComponentBase.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@   
$(OBJDIR)/ComponentVoxel.o: \
	$(SRCDIR)/ComponentVoxel.cc $(INCDIR)/ComponentVoxel.hh \
	$(SRCDIR)/ComponentBase.cc $(INCDIR)/ComponentBase.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@   
$(OBJDIR)/GeometrySimple.o: \
	$(SRCDIR)/GeometrySimple.cc $(INCDIR)/GeometrySimple.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@   
$(OBJDIR)/GeometryRoot.o: \
	$(SRCDIR)/GeometryRoot.cc $(INCDIR)/GeometryRoot.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@    

$(OBJDIR)/ViewFEMesh.o: \
	$(SRCDIR)/ViewFEMesh.cc $(INCDIR)/ViewFEMesh.hh \
	$(SRCDIR)/ViewDrift.cc $(INCDIR)/ViewDrift.hh \
	$(INCDIR)/ComponentFieldMap.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/ViewField.o: \
	$(SRCDIR)/ViewField.cc $(INCDIR)/ViewField.hh \
	$(INCDIR)/Sensor.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/ViewDrift.o: \
	$(SRCDIR)/ViewDrift.cc $(INCDIR)/ViewDrift.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/ViewMedium.o: \
	$(SRCDIR)/ViewMedium.cc $(INCDIR)/ViewMedium.hh \
	$(INCDIR)/Medium.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/ViewSignal.o: \
	$(SRCDIR)/ViewSignal.cc $(INCDIR)/ViewSignal.hh \
	$(INCDIR)/Sensor.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/ViewCell.o: \
	$(SRCDIR)/ViewCell.cc $(INCDIR)/ViewCell.hh \
	$(INCDIR)/ComponentAnalyticField.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/ViewGeometry.o: \
	$(SRCDIR)/ViewGeometry.cc $(INCDIR)/ViewGeometry.hh \
	$(INCDIR)/GeometrySimple.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Medium.o: \
	$(SRCDIR)/Medium.cc $(INCDIR)/Medium.hh \
	$(INCDIR)/FundamentalConstants.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/MediumGas.o: \
	$(SRCDIR)/MediumGas.cc $(INCDIR)/MediumGas.hh \
	$(SRCDIR)/Medium.cc $(INCDIR)/Medium.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/MediumMagboltz.o: \
	$(SRCDIR)/MediumMagboltz.cc $(INCDIR)/MediumMagboltz.hh \
	$(SRCDIR)/Medium.cc $(INCDIR)/Medium.hh $(SRCDIR)/OpticalData.cc \
	$(INCDIR)/FundamentalConstants.hh $(INCDIR)/Random.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/magboltz.o: \
	$(SRCDIR)/magboltz-9.01.f
	@echo $@
	$(FC) $(FFLAGS) $< -o $@
$(OBJDIR)/MediumSilicon.o: \
	$(SRCDIR)/MediumSilicon.cc $(INCDIR)/MediumSilicon.hh \
	$(SRCDIR)/Medium.cc $(INCDIR)/Medium.hh \
	$(INCDIR)/FundamentalConstants.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/MediumCdTe.o: \
	$(SRCDIR)/MediumCdTe.cc $(INCDIR)/MediumCdTe.hh \
	$(SRCDIR)/Medium.cc $(INCDIR)/Medium.hh \
	$(INCDIR)/FundamentalConstants.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/MediumGaAs.o: \
	$(SRCDIR)/MediumGaAs.cc $(INCDIR)/MediumGaAs.hh \
	$(SRCDIR)/Medium.cc $(INCDIR)/Medium.hh \
	$(INCDIR)/FundamentalConstants.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/OpticalData.o: \
	$(SRCDIR)/OpticalData.cc $(INCDIR)/OpticalData.hh \
	$(INCDIR)/FundamentalConstants.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/SolidBox.o: \
	$(SRCDIR)/SolidBox.cc $(INCDIR)/SolidBox.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/SolidTube.o: \
	$(SRCDIR)/SolidTube.cc $(INCDIR)/SolidTube.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/SolidSphere.o: \
	$(SRCDIR)/SolidSphere.cc $(INCDIR)/SolidSphere.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/RandomEngineGSL.o: \
	$(SRCDIR)/RandomEngineGSL.cc $(INCDIR)/RandomEngineGSL.hh
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/RandomEngineRoot.o: \
	$(SRCDIR)/RandomEngineRoot.cc $(INCDIR)/RandomEngineRoot.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/PlottingEngineRoot.o: \
	$(SRCDIR)/PlottingEngineRoot.cc $(INCDIR)/PlottingEngineRoot.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@        

$(OBJDIR)/Numerics.o: \
	$(SRCDIR)/Numerics.cc $(INCDIR)/Numerics.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@  

$(OBJDIR)/Sensor.o: \
	$(SRCDIR)/Sensor.cc $(INCDIR)/Sensor.hh \
	$(INCDIR)/ComponentBase.hh $(INCDIR)/FundamentalConstants.hh
	@echo $@
	$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/TetrahedralTree.o: \
	$(SRCDIR)/TetrahedralTree.cc $(INCDIR)/TetrahedralTree.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/GarfieldDict.o: \
	$(OBJDIR)/GarfieldDict.C
	@echo $@
	$(CXX) $(CFLAGS) -DDICT_SKIP_HEED $< -o $@

$(OBJDIR)/GarfieldDict.C: $(HEADERS) $(INCDIR)/LinkDef.h
	@echo Creating dictionary...
	rootcint -f $@ -c $(CPPFLAGS) -p $^ 
showbuild:
	@echo "ROOTSYS            = $(ROOTSYS)"
	@echo "STAR_HOST_SYS      = $(STAR_HOST_SYS)"
	@echo "GARFIELD_HOME      = $(GARFIELD_HOME)"
	@echo "CFLAGS             = $(CFLAGS)"
	@echo "FFLAGS             = $(FFLAGS)"
	@echo "NODEBUG            = $(NODEBUG)"
	@echo "DYNAMIC            = $(DYNAMIC)"
