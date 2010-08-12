OBJDIR = $(GARFIELD_HOME)/Object
SRCDIR = $(GARFIELD_HOME)/Source
INCDIR = $(GARFIELD_HOME)/Include

HEADERS = $(wildcard $(INCDIR)/*.hh)
SOURCES = $(wildcard $(SRCDIR)/*.cc)
OBJECTS = $(subst $(SRCDIR),$(OBJDIR),$(SOURCES:.cc=.o))
OBJECTS += \
	$(OBJDIR)/magboltz.o \
	$(OBJDIR)/efieldCalc.o
OBJECTS += $(OBJDIR)/GarfieldDict.o

# Fortran compiler
FC = gfortran

# Compilation flags
CFLAGS = -Wall -Wextra -pedantic -Wabi -Wno-long-long \
        `root-config --cflags` \
        -g -fpic -O3 -c \
	-I$(INCDIR) -I$(ROOTSYS)/include
FFLAGS = -g -fpic -O3 -c

# Linking flags
LDFLAGS = `root-config --glibs` -lGeom -lgfortran -lm

all:	$(OBJECTS)
	touch $(OBJDIR)/last_updated_on

lib:	all
	$(CXX) $(LC) -shared -o libGarfield.so $(OBJECTS)

clean:
	rm -f $(OBJDIR)/*.o

$(OBJDIR)/AvalancheMicroscopic.o: \
	$(SRCDIR)/AvalancheMicroscopic.cc \
	$(INCDIR)/AvalancheMicroscopic.hh \
	$(INCDIR)/FundamentalConstants.hh $(INCDIR)/Random.hh \
	$(INCDIR)/Sensor.hh $(INCDIR)/Medium.hh $(INCDIR)/ViewDrift.hh
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/AvalancheMC.o: \
	$(SRCDIR)/AvalancheMC.cc $(INCDIR)/AvalancheMC.hh \
	$(INCDIR)/FundamentalConstants.hh $(INCDIR)/Random.hh \
	$(INCDIR)/Sensor.hh $(INCDIR)/Medium.hh $(INCDIR)/ViewDrift.hh
	$(CXX) $(CFLAGS) $< -o $@      
$(OBJDIR)/DriftLineRKF.o: \
	$(SRCDIR)/DriftLineRKF.cc $(INCDIR)/DriftLineRKF.hh \
	$(INCDIR)/FundamentalConstants.hh \
	$(INCDIR)/Sensor.hh $(INCDIR)/Medium.hh $(INCDIR)/ViewDrift.hh
	$(CXX) $(CFLAGS) $< -o $@
 
$(OBJDIR)/Track.o: $(SRCDIR)/Track.cc $(INCDIR)/Track.hh
	$(CXX) $(CFLAGS) $< -o $@        
$(OBJDIR)/TrackHeed.o: \
	$(SRCDIR)/TrackHeed.cc $(INCDIR)/TrackHeed.hh \
	$(INCDIR)/Track.hh $(SRCDIR)/Track.cc
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/TrackBichsel.o: \
	$(SRCDIR)/TrackBichsel.cc $(INCDIR)/TrackBichsel.hh \
 	$(INCDIR)/Track.hh $(SRCDIR)/Track.cc
	$(CXX) $(CFLAGS) $< -o $@       
$(OBJDIR)/TrackSimple.o: \
	$(SRCDIR)/TrackSimple.cc $(INCDIR)/TrackSimple.hh \
	$(INCDIR)/Track.hh $(SRCDIR)/Track.cc
	$(CXX) $(CFLAGS) $< -o $@        

$(OBJDIR)/ComponentBase.o: \
	$(SRCDIR)/ComponentBase.cc $(INCDIR)/ComponentBase.hh \
	$(INCDIR)/Medium.hh
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/ComponentConstant.o: \
	$(SRCDIR)/ComponentConstant.cc $(INCDIR)/ComponentConstant.hh \
	$(SRCDIR)/ComponentBase.cc $(INCDIR)/ComponentBase.hh
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/ComponentUser.o: \
	$(SRCDIR)/ComponentUser.cc $(INCDIR)/ComponentUser.hh \
	$(SRCDIR)/ComponentBase.cc $(INCDIR)/ComponentBase.hh
	$(CXX) $(CFLAGS) $< -o $@       
$(OBJDIR)/ComponentAnalyticField.o: \
	$(SRCDIR)/ComponentAnalyticField.cc \
	$(INCDIR)/ComponentAnalyticField.hh \
	$(SRCDIR)/ComponentBase.cc $(INCDIR)/ComponentBase.hh
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/ComponentAnalyticFieldWrap.o: \
	$(SRCDIR)/ComponentAnalyticFieldWrap.cc \
	$(INCDIR)/ComponentAnalyticFieldWrap.hh \
	$(SRCDIR)/efieldCalc.f \
	$(SRCDIR)/ComponentBase.cc $(INCDIR)/ComponentBase.hh
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/efieldCalc.o: \
	$(SRCDIR)/efieldCalc.f
	$(FC) $(FFLAGS) $< -o $@
$(OBJDIR)/ComponentNeBem2d.o: \
	$(SRCDIR)/ComponentNeBem2d.cc $(INCDIR)/ComponentNeBem2d.hh \
	$(SRCDIR)/ComponentBase.cc $(INCDIR)/ComponentBase.hh
	$(CXX) $(CFLAGS) $< -o $@        
$(OBJDIR)/ComponentFieldMap.o: \
	$(SRCDIR)/ComponentFieldMap.cc $(INCDIR)/ComponentFieldMap.hh \
	$(SRCDIR)/ComponentBase.cc $(INCDIR)/ComponentBase.hh
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/ComponentAnsys121.o: \
	$(SRCDIR)/ComponentAnsys121.cc $(INCDIR)/ComponentAnsys121.hh \
	$(SRCDIR)/ComponentFieldMap.cc $(INCDIR)/ComponentFieldMap.hh 
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/ComponentAnsys123.o: \
	$(SRCDIR)/ComponentAnsys123.cc $(INCDIR)/ComponentAnsys123.hh \
	$(SRCDIR)/ComponentFieldMap.cc $(INCDIR)/ComponentFieldMap.hh 
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/ComponentTcad2d.o: \
	$(SRCDIR)/ComponentTcad2d.cc $(INCDIR)/ComponentTcad2d.hh \
	$(SRCDIR)/ComponentBase.cc $(INCDIR)/ComponentBase.hh
	$(CXX) $(CFLAGS) $< -o $@   

$(OBJDIR)/GeometrySimple.o: \
	$(SRCDIR)/GeometrySimple.cc $(INCDIR)/GeometrySimple.hh
	$(CXX) $(CFLAGS) $< -o $@   
$(OBJDIR)/GeometryRoot.o: \
	$(SRCDIR)/GeometryRoot.cc $(INCDIR)/GeometryRoot.hh
	$(CXX) $(CFLAGS) $< -o $@    

$(OBJDIR)/ViewField.o: \
	$(SRCDIR)/ViewField.cc $(INCDIR)/ViewField.hh \
	$(INCDIR)/Sensor.hh
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/ViewDrift.o: \
	$(SRCDIR)/ViewDrift.cc $(INCDIR)/ViewDrift.hh
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/ViewMedium.o: \
	$(SRCDIR)/ViewMedium.cc $(INCDIR)/ViewMedium.hh
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/ViewSignal.o: \
	$(SRCDIR)/ViewSignal.cc $(INCDIR)/ViewSignal.hh
	$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Medium.o: \
	$(SRCDIR)/Medium.cc $(INCDIR)/Medium.hh \
	$(INCDIR)/FundamentalConstants.hh
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/MediumMagboltz86.o: \
	$(SRCDIR)/MediumMagboltz86.cc $(INCDIR)/MediumMagboltz86.hh \
	$(SRCDIR)/Medium.cc $(INCDIR)/Medium.hh $(SRCDIR)/OpticalData.cc \
	$(INCDIR)/FundamentalConstants.hh $(INCDIR)/Random.hh
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/magboltz.o: \
	$(SRCDIR)/magboltz-8.9.f
	$(FC) $(FFLAGS) $< -o $@
$(OBJDIR)/MediumSilicon.o: \
	$(SRCDIR)/MediumSilicon.cc $(INCDIR)/MediumSilicon.hh \
	$(SRCDIR)/Medium.cc $(INCDIR)/Medium.hh \
	$(INCDIR)/FundamentalConstants.hh
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/OpticalData.o: \
	$(SRCDIR)/OpticalData.cc $(INCDIR)/OpticalData.hh \
	$(INCDIR)/FundamentalConstants.hh
	$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/SolidBox.o: \
	$(SRCDIR)/SolidBox.cc $(INCDIR)/SolidBox.hh
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/SolidTube.o: \
	$(SRCDIR)/SolidTube.cc $(INCDIR)/SolidTube.hh
	$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/RandomEngineGSL.o: \
	$(SRCDIR)/RandomEngineGSL.cc $(INCDIR)/RandomEngineGSL.hh
	$(CXX) $(CFLAGS) $< -o $@
$(OBJDIR)/RandomEngineRoot.o: \
	$(SRCDIR)/RandomEngineRoot.cc $(INCDIR)/RandomEngineRoot.hh
	$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/PlottingEngineRoot.o: \
	$(SRCDIR)/PlottingEngineRoot.cc $(INCDIR)/PlottingEngineRoot.hh
	$(CXX) $(CFLAGS) $< -o $@        

$(OBJDIR)/Numerics.o: \
	$(SRCDIR)/Numerics.cc $(INCDIR)/Numerics.hh
	$(CXX) $(CFLAGS) $< -o $@  

$(OBJDIR)/Sensor.o: \
	$(SRCDIR)/Sensor.cc $(INCDIR)/Sensor.hh \
	$(INCDIR)/ComponentBase.hh $(INCDIR)/FundamentalConstants.hh
	$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/GarfieldDict.o: \
	GarfieldDict.cc
	$(CXX) $(CFLAGS) $< -o $@

GarfieldDict.cc: $(HEADERS) LinkDef.h
	rootcint -f $@ -c $(CFLAGS) -p $^
