HOME = .
OBJECT = $(HOME)/Object
SOURCE = $(HOME)/Source
INCLUDE = $(HOME)/Include

CC = g++ -c -O3 `root-config --cflags` -I$(INCLUDE)
LC = g++ `root-config --glibs` -lMinuit -lgsl -lgslcblas -lg2c -lm 
FF = f77 -c -O3
# Compiler flags
CFLAGS = -Wall -Wextra -pedantic -Wabi -Wno-long-long -g `root-config --cflags`

OBJS = \
	$(OBJECT)/AvalancheMicroscopic.o \
	$(OBJECT)/AvalancheMC.o \
	$(OBJECT)/Track.o \
	$(OBJECT)/TrackHeed.o \
	$(OBJECT)/TrackBichsel.o \
	$(OBJECT)/ComponentBase.o \
	$(OBJECT)/ComponentConstant.o \
	$(OBJECT)/ComponentUser.o \
	$(OBJECT)/ComponentFieldMap.o \
	$(OBJECT)/ComponentAnsys121.o \
	$(OBJECT)/ComponentAnsys123.o \
	$(OBJECT)/ComponentTcad2d.o \
	$(OBJECT)/ComponentNeBem2d.o \
	$(OBJECT)/GeometrySimple.o \
	$(OBJECT)/GeometryRoot.o \
	$(OBJECT)/FieldView.o \
	$(OBJECT)/DriftView.o \
	$(OBJECT)/Input.o \
	$(OBJECT)/Medium.o \
	$(OBJECT)/MediumMagboltz86.o \
	$(OBJECT)/magboltz.o \
	$(OBJECT)/MediumSilicon.o \
	$(OBJECT)/OpticalData.o \
	$(OBJECT)/SolidBox.o \
	$(OBJECT)/SolidTube.o \
	$(OBJECT)/RandomEngineGSL.o \
	$(OBJECT)/PlottingEngineRoot.o \
	$(OBJECT)/Sensor.o

all:	$(OBJS)
	touch $(OBJECT)/last_updated_on
clean:
	rm -f $(OBJECT)/*.o

$(OBJECT)/AvalancheMicroscopic.o: $(SOURCE)/AvalancheMicroscopic.cc $(INCLUDE)/AvalancheMicroscopic.hh $(INCLUDE)/FundamentalConstants.hh $(INCLUDE)/Random.hh $(INCLUDE)/Sensor.hh $(INCLUDE)/Medium.hh
	$(CC) $(CFLAGS) $< -o $@
$(OBJECT)/AvalancheMC.o: $(SOURCE)/AvalancheMC.cc $(INCLUDE)/AvalancheMC.hh $(INCLUDE)/FundamentalConstants.hh $(INCLUDE)/Random.hh $(INCLUDE)/Sensor.hh $(INCLUDE)/Medium.hh
	$(CC) $(CFLAGS) $< -o $@      

$(OBJECT)/Track.o: $(SOURCE)/Track.cc $(INCLUDE)/Track.hh
	$(CC) $(CFLAGS) $< -o $@        
$(OBJECT)/TrackHeed.o: $(SOURCE)/TrackHeed.cc $(INCLUDE)/TrackHeed.hh $(INCLUDE)/Track.hh $(SOURCE)/Track.cc
	$(CC) $(CFLAGS) $< -o $@       
$(OBJECT)/TrackBichsel.o: $(SOURCE)/TrackBichsel.cc $(INCLUDE)/TrackBichsel.hh $(INCLUDE)/Track.hh $(SOURCE)/Track.cc
	$(CC) $(CFLAGS) $< -o $@        
$(OBJECT)/ComponentBase.o: $(SOURCE)/ComponentBase.cc $(INCLUDE)/ComponentBase.hh $(INCLUDE)/Medium.hh
	$(CC) $(CFLAGS) $< -o $@
$(OBJECT)/ComponentConstant.o: $(SOURCE)/ComponentConstant.cc $(INCLUDE)/ComponentConstant.hh $(INCLUDE)/ComponentBase.hh
	$(CC) $(CFLAGS) $< -o $@
$(OBJECT)/ComponentUser.o: $(SOURCE)/ComponentUser.cc $(INCLUDE)/ComponentUser.hh $(INCLUDE)/ComponentBase.hh
	$(CC) $(CFLAGS) $< -o $@       
$(OBJECT)/ComponentNeBem2d.o: $(SOURCE)/ComponentNeBem2d.cc $(INCLUDE)/ComponentNeBem2d.hh $(INCLUDE)/ComponentBase.hh
	$(CC) $(CFLAGS) $< -o $@        
$(OBJECT)/ComponentFieldMap.o: $(SOURCE)/ComponentFieldMap.cc $(INCLUDE)/ComponentFieldMap.hh $(INCLUDE)/ComponentBase.hh
	$(CC) $(CFLAGS) $< -o $@
$(OBJECT)/ComponentAnsys121.o: $(SOURCE)/ComponentAnsys121.cc $(INCLUDE)/ComponentAnsys121.hh $(INCLUDE)/ComponentFieldMap.hh $(INCLUDE)/Input.hh
	$(CC) $(CFLAGS) $< -o $@
$(OBJECT)/ComponentAnsys123.o: $(SOURCE)/ComponentAnsys123.cc $(INCLUDE)/ComponentAnsys123.hh $(INCLUDE)/ComponentFieldMap.hh $(INCLUDE)/Input.hh
	$(CC) $(CFLAGS) $< -o $@
$(OBJECT)/ComponentTcad2d.o: $(SOURCE)/ComponentTcad2d.cc $(INCLUDE)/ComponentTcad2d.hh $(INCLUDE)/ComponentBase.hh
	$(CC) $(CFLAGS) $< -o $@   
$(OBJECT)/GeometrySimple.o: $(SOURCE)/GeometrySimple.cc $(INCLUDE)/GeometrySimple.hh
	$(CC) $(CFLAGS) $< -o $@   
$(OBJECT)/GeometryRoot.o: $(SOURCE)/GeometryRoot.cc $(INCLUDE)/GeometryRoot.hh
	$(CC) $(CFLAGS) $< -o $@    
$(OBJECT)/FieldView.o: $(SOURCE)/FieldView.cc $(INCLUDE)/FieldView.hh $(INCLUDE)/Sensor.hh
	$(CC) $(CFLAGS) $< -o $@
$(OBJECT)/DriftView.o: $(SOURCE)/DriftView.cc $(INCLUDE)/DriftView.hh
	$(CC) $(CFLAGS) $< -o $@
$(OBJECT)/Input.o: $(SOURCE)/Input.cc $(INCLUDE)/Input.hh
	$(CC) $(CFLAGS) $< -o $@
$(OBJECT)/Medium.o: $(SOURCE)/Medium.cc $(INCLUDE)/Medium.hh $(INCLUDE)/FundamentalConstants.hh
	$(CC) $(CFLAGS) $< -o $@
$(OBJECT)/MediumMagboltz86.o: $(SOURCE)/MediumMagboltz86.cc $(INCLUDE)/MediumMagboltz86.hh $(SOURCE)/OpticalData.cc $(INCLUDE)/FundamentalConstants.hh $(INCLUDE)/Random.hh
	$(CC) $(CFLAGS) $< -o $@
$(OBJECT)/magboltz.o: $(SOURCE)/magboltz-8.9.f
	$(FF) $< -o $@
$(OBJECT)/MediumSilicon.o: $(SOURCE)/MediumSilicon.cc $(INCLUDE)/MediumSilicon.hh $(INCLUDE)/FundamentalConstants.hh $(INCLUDE)/Medium.hh
	$(CC) $(CFLAGS) $< -o $@
$(OBJECT)/OpticalData.o: $(SOURCE)/OpticalData.cc $(INCLUDE)/OpticalData.hh $(INCLUDE)/FundamentalConstants.hh
	$(CC) $(CFLAGS) $< -o $@
$(OBJECT)/SolidBox.o: $(SOURCE)/SolidBox.cc $(INCLUDE)/SolidBox.hh
	$(CC) $(CFLAGS) $< -o $@
$(OBJECT)/SolidTube.o: $(SOURCE)/SolidTube.cc $(INCLUDE)/SolidTube.hh
	$(CC) $(CFLAGS) $< -o $@
$(OBJECT)/RandomEngineGSL.o: $(SOURCE)/RandomEngineGSL.cc $(INCLUDE)/RandomEngineGSL.hh
	$(CC) $(CFLAGS) $< -o $@
$(OBJECT)/PlottingEngineRoot.o: $(SOURCE)/PlottingEngineRoot.cc $(INCLUDE)/PlottingEngineRoot.hh
	$(CC) $(CFLAGS) $< -o $@        
$(OBJECT)/Sensor.o: $(SOURCE)/Sensor.cc $(INCLUDE)/Sensor.hh $(INCLUDE)/ComponentBase.hh $(INCLUDE)/FundamentalConstants.hh
	$(CC) $(CFLAGS) $< -o $@
