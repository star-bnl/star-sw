OBJDIR = $(GARFIELD_HOME)/Object
SRCDIR = $(GARFIELD_HOME)/Source
INCDIR = $(GARFIELD_HOME)/Include
LIBDIR = $(GARFIELD_HOME)/Library
HEEDDIR = $(GARFIELD_HOME)/Heed

HEADERS = $(wildcard $(INCDIR)/*.hh)

SOURCES = $(wildcard $(SRCDIR)/*.cc)

OBJECTS = $(subst $(SRCDIR),$(OBJDIR),$(SOURCES:.cc=.o))
OBJECTS += \
	$(OBJDIR)/magboltz.o \
	$(OBJDIR)/efieldCalc.o
OBJECTS += $(OBJDIR)/GarfieldDict.o

TARGETS = $(OBJECTS)
TARGETS += heed

# Fortran compiler
FC = gfortran

# Compilation flags
CFLAGS = -Wall -Wextra -pedantic -Wabi -Wno-long-long \
	 `root-config --cflags` \
        -fpic -fno-common -Os -c \
	-I$(INCDIR) -I$(HEEDDIR) -DINS_CRETURN 

FFLAGS = -fpic -Os -c

# Debug flags
# CFLAGS += -g
# FFLAGS += -g

# Linking flags
LDFLAGS = `root-config --glibs` `root-config --ldflags`-lGeom \
	-lgfortran -lm

all:	$(TARGETS)
	@echo Creating library libGarfield...
	@ar rcs $(LIBDIR)/libGarfield.a $(OBJECTS) \
	$(wildcard $(OBJDIR)/Heed/*.o)
	@ranlib $(LIBDIR)/libGarfield.a
	@touch $(OBJDIR)/last_updated_on
	@echo Finished.

.PHONY:	heed

heed:	
	@echo Compiling Heed...
	@cd $(HEEDDIR); make; cd $(GARFIELD_HOME)
	@touch $(OBJDIR)/last_updated_on

clean:
	@echo Removing object files...
	@$(RM) $(OBJDIR)/*.o
	@echo Removing libraries...
	@$(RM) $(LIBDIR)/*.a
	@cd $(HEEDDIR); make clean

$(OBJDIR)/AvalancheMicroscopic.o: \
	$(SRCDIR)/AvalancheMicroscopic.cc \
	$(INCDIR)/AvalancheMicroscopic.hh \
	$(INCDIR)/FundamentalConstants.hh $(INCDIR)/Random.hh \
	$(INCDIR)/Sensor.hh $(INCDIR)/Medium.hh $(INCDIR)/ViewDrift.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/AvalancheMC.o: \
	$(SRCDIR)/AvalancheMC.cc $(INCDIR)/AvalancheMC.hh \
	$(INCDIR)/FundamentalConstants.hh $(INCDIR)/Random.hh \
	$(INCDIR)/Sensor.hh $(INCDIR)/Medium.hh $(INCDIR)/ViewDrift.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@      
$(OBJDIR)/DriftLineRKF.o: \
	$(SRCDIR)/DriftLineRKF.cc $(INCDIR)/DriftLineRKF.hh \
	$(INCDIR)/FundamentalConstants.hh \
	$(INCDIR)/Sensor.hh $(INCDIR)/Medium.hh $(INCDIR)/ViewDrift.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@
 
$(OBJDIR)/Track.o: $(SRCDIR)/Track.cc $(INCDIR)/Track.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@        
$(OBJDIR)/TrackBichsel.o: \
	$(SRCDIR)/TrackBichsel.cc $(INCDIR)/TrackBichsel.hh \
 	$(INCDIR)/Track.hh $(SRCDIR)/Track.cc
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@       
$(OBJDIR)/TrackPAI.o: \
	$(SRCDIR)/TrackPAI.cc $(INCDIR)/TrackPAI.hh \
	$(INCDIR)/Track.hh $(SRCDIR)/Track.cc
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/TrackSimple.o: \
	$(SRCDIR)/TrackSimple.cc $(INCDIR)/TrackSimple.hh \
	$(INCDIR)/Track.hh $(SRCDIR)/Track.cc
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@        
$(OBJDIR)/TrackHeed.o: \
	$(SRCDIR)/TrackHeed.cc $(INCDIR)/TrackHeed.hh \
	$(INCDIR)/Track.hh $(SRCDIR)/Track.cc \
	$(HEEDDIR)/HeedChamber.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/ComponentBase.o: \
	$(SRCDIR)/ComponentBase.cc $(INCDIR)/ComponentBase.hh \
	$(INCDIR)/Medium.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/ComponentConstant.o: \
	$(SRCDIR)/ComponentConstant.cc $(INCDIR)/ComponentConstant.hh \
	$(SRCDIR)/ComponentBase.cc $(INCDIR)/ComponentBase.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/ComponentUser.o: \
	$(SRCDIR)/ComponentUser.cc $(INCDIR)/ComponentUser.hh \
	$(SRCDIR)/ComponentBase.cc $(INCDIR)/ComponentBase.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@       
$(OBJDIR)/ComponentAnalyticField.o: \
	$(SRCDIR)/ComponentAnalyticField.cc \
	$(INCDIR)/ComponentAnalyticField.hh \
	$(SRCDIR)/ComponentBase.cc $(INCDIR)/ComponentBase.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/ComponentAnalyticFieldWrap.o: \
	$(SRCDIR)/ComponentAnalyticFieldWrap.cc \
	$(INCDIR)/ComponentAnalyticFieldWrap.hh \
	$(SRCDIR)/efieldCalc.f \
	$(SRCDIR)/ComponentBase.cc $(INCDIR)/ComponentBase.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/efieldCalc.o: \
	$(SRCDIR)/efieldCalc.f
	@echo $@
	@$(FC) $(FFLAGS) $< -o $@
$(OBJDIR)/ComponentNeBem2d.o: \
	$(SRCDIR)/ComponentNeBem2d.cc $(INCDIR)/ComponentNeBem2d.hh \
	$(SRCDIR)/ComponentBase.cc $(INCDIR)/ComponentBase.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@        
$(OBJDIR)/ComponentFieldMap.o: \
	$(SRCDIR)/ComponentFieldMap.cc $(INCDIR)/ComponentFieldMap.hh \
	$(SRCDIR)/ComponentBase.cc $(INCDIR)/ComponentBase.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/ComponentAnsys121.o: \
	$(SRCDIR)/ComponentAnsys121.cc $(INCDIR)/ComponentAnsys121.hh \
	$(SRCDIR)/ComponentFieldMap.cc $(INCDIR)/ComponentFieldMap.hh 
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/ComponentAnsys123.o: \
	$(SRCDIR)/ComponentAnsys123.cc $(INCDIR)/ComponentAnsys123.hh \
	$(SRCDIR)/ComponentFieldMap.cc $(INCDIR)/ComponentFieldMap.hh 
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/ComponentTcad2d.o: \
	$(SRCDIR)/ComponentTcad2d.cc $(INCDIR)/ComponentTcad2d.hh \
	$(SRCDIR)/ComponentBase.cc $(INCDIR)/ComponentBase.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@   

$(OBJDIR)/GeometrySimple.o: \
	$(SRCDIR)/GeometrySimple.cc $(INCDIR)/GeometrySimple.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@   
$(OBJDIR)/GeometryRoot.o: \
	$(SRCDIR)/GeometryRoot.cc $(INCDIR)/GeometryRoot.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@    

$(OBJDIR)/ViewField.o: \
	$(SRCDIR)/ViewField.cc $(INCDIR)/ViewField.hh \
	$(INCDIR)/Sensor.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/ViewDrift.o: \
	$(SRCDIR)/ViewDrift.cc $(INCDIR)/ViewDrift.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/ViewMedium.o: \
	$(SRCDIR)/ViewMedium.cc $(INCDIR)/ViewMedium.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/ViewSignal.o: \
	$(SRCDIR)/ViewSignal.cc $(INCDIR)/ViewSignal.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Medium.o: \
	$(SRCDIR)/Medium.cc $(INCDIR)/Medium.hh \
	$(INCDIR)/FundamentalConstants.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/MediumMagboltz86.o: \
	$(SRCDIR)/MediumMagboltz86.cc $(INCDIR)/MediumMagboltz86.hh \
	$(SRCDIR)/Medium.cc $(INCDIR)/Medium.hh $(SRCDIR)/OpticalData.cc \
	$(INCDIR)/FundamentalConstants.hh $(INCDIR)/Random.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/magboltz.o: \
	$(SRCDIR)/magboltz-8.9.f
	@echo $@
	@$(FC) $(FFLAGS) $< -o $@
$(OBJDIR)/MediumSilicon.o: \
	$(SRCDIR)/MediumSilicon.cc $(INCDIR)/MediumSilicon.hh \
	$(SRCDIR)/Medium.cc $(INCDIR)/Medium.hh \
	$(INCDIR)/FundamentalConstants.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/OpticalData.o: \
	$(SRCDIR)/OpticalData.cc $(INCDIR)/OpticalData.hh \
	$(INCDIR)/FundamentalConstants.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/SolidBox.o: \
	$(SRCDIR)/SolidBox.cc $(INCDIR)/SolidBox.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/SolidTube.o: \
	$(SRCDIR)/SolidTube.cc $(INCDIR)/SolidTube.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/RandomEngineGSL.o: \
	$(SRCDIR)/RandomEngineGSL.cc $(INCDIR)/RandomEngineGSL.hh
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/RandomEngineRoot.o: \
	$(SRCDIR)/RandomEngineRoot.cc $(INCDIR)/RandomEngineRoot.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/PlottingEngineRoot.o: \
	$(SRCDIR)/PlottingEngineRoot.cc $(INCDIR)/PlottingEngineRoot.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@        

$(OBJDIR)/Numerics.o: \
	$(SRCDIR)/Numerics.cc $(INCDIR)/Numerics.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@  

$(OBJDIR)/Sensor.o: \
	$(SRCDIR)/Sensor.cc $(INCDIR)/Sensor.hh \
	$(INCDIR)/ComponentBase.hh $(INCDIR)/FundamentalConstants.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/GarfieldDict.o: \
	$(SRCDIR)/GarfieldDict.C
	@echo $@
	@$(CXX) $(CFLAGS) -DDICT_SKIP_HEED $< -o $@

$(SRCDIR)/GarfieldDict.C: $(HEADERS) $(INCDIR)/LinkDef.h
	@echo Creating dictionary...
	@rootcint -f $@ -c $(CFLAGS) -p $^ 
