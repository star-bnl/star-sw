#
#-----------------------------------
CC      = c++
#-----------------------------------

INC      = ../inc
SRC      = ../ftf
GRAPHSRC = graphic

#----------------------------------- 

#CFLAGS    = -g -D ALPHA_500MHZ
CFLAGS	  = -g
CFLAGS	  = -O2
CFLAGS    = -g -D DEBUG -D SL3ROOT -I$(ROOTSYS)/include
LIBS      = -lm

#-----------------------------------

.SUFFIXES: .cc .o

#===================================

TRACKER 	= FtfFinder.o FtfTrack.o FtfHit.o FtfPara.o \
	  	  FtfBaseHit.o FtfBaseTrack.o \
	  	  FtfUtilities.o FtfVolume.o

GRAPHTRACK      = FtfGraphic.o FtfBaseHitDict.o FtfHitDict.o FtfBaseTrackDict.o \
		  FtfTrackDict.o FtfFinderDict.o FtfParaDict.o FtfSl3Dict.o \
		  FtfGraphicDict.o

SL3TRACKER     	= $(TRACKER) FtfSl3.o sl3Swap.o  sl3CoordinateTransform.o


#===================================

.cc.o:
	$(CC) $(CFLAGS) -I $(INC) -I $(SRC) -c $*.cc

sl3A:$(SL3TRACKER) sl3StandAlone.o
	$(CC) sl3StandAlone.o $(SL3TRACKER) $(CFLAGS) $(LIBS) -I $(INC) -I $(SRC) -o sl3A

sl3Daq: $(SL3TRACKER) sl3Daq.o
	$(CC) sl3Daq.o $(SL3TRACKER) $(CFLAGS) $(LIBS) -I $(INC) -I $(SRC) -o sl3Daq

FtfSl3.so:$(SL3TRACKER) $(GRAPHTRACK) 
	$(CC) -shared -o FtfSl3.so -I$(ROOTSYS)/include \
	       $(SL3TRACKER) $(GRAPHTRACK) -lm


#+++++++++++++++++++++++++++++++++++++++++++++++++++++
#   FTF TRACKER
#+++++++++++++++++++++++++++++++++++++++++++++++++++++
FtfFinder.o: $(SRC)/FtfFinder.h
	$(CC) $(CFLAGS) -I $(SRC) -c $(SRC)/FtfFinder.cc
FtfTrack.o: $(SRC)/FtfTrack.h
	$(CC) $(CFLAGS) -I $(SRC) -c $(SRC)/FtfTrack.cc
FtfHit.o: $(SRC)/FtfHit.h 
	$(CC) $(CFLAGS) -I $(SRC) -c $(SRC)/FtfHit.cc
FtfPara.o: $(SRC)/FtfPara.h 
	$(CC) $(CFLAGS) -I $(SRC) -c $(SRC)/FtfPara.cc
FtfBaseHit.o: $(SRC)/FtfBaseHit.h 
	$(CC) $(CFLAGS) -I $(SRC) -c $(SRC)/FtfBaseHit.cc
FtfBaseTrack.o: $(SRC)/FtfBaseTrack.h 
	$(CC) $(CFLAGS) -I $(SRC) -c $(SRC)/FtfBaseTrack.cc
FtfUtilities.o: 
	$(CC) $(CFLAGS) -I $(SRC) -c $(SRC)/FtfUtilities.cc
FtfVolume.o: $(SRC)/FtfVolume.h 
	$(CC) $(CFLAGS) -I $(SRC) -c $(SRC)/FtfVolume.cc
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   SL3 Tracker
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
FtfSl3.o: $(SRC)/FtfSl3.h 
	$(CC) $(CFLAGS) -I $(SRC) -I $(INC) -c $(SRC)/FtfSl3.cc
sl3Swap.o: 
	$(CC) $(CFLAGS) -I $(INC) -c $(SRC)/sl3Swap.cc
sl3CoordinateTransform.o: $(INC)/sl3GeoConstants.h 
	$(CC) $(CFLAGS) -I $(INC) -c $(SRC)/sl3CoordinateTransform.cc
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   FTF Graphic
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
FtfGraphic.o: $(GRAPHSRC)/FtfGraphic.h 
	$(CC) $(CFLAGS) -I$(INC) -I $(SRC) -I $(GRAPHSRC) -c $(GRAPHSRC)/FtfGraphic.cc
FtfBaseHitDict.o: $(GRAPHSRC)/FtfBaseHitDict.h 
	$(CC) $(CFLAGS) -I$(INC) -I $(SRC) -I $(GRAPHSRC) -c $(GRAPHSRC)/FtfBaseHitDict.cxx
FtfHitDict.o: $(GRAPHSRC)/FtfHitDict.h 
	$(CC) $(CFLAGS) -I$(INC) -I $(SRC) -I $(GRAPHSRC) -c $(GRAPHSRC)/FtfHitDict.cxx
FtfBaseTrackDict.o: $(GRAPHSRC)/FtfBaseTrackDict.h 
	$(CC) $(CFLAGS) -I$(INC) -I $(SRC) -I $(GRAPHSRC) -c $(GRAPHSRC)/FtfBaseTrackDict.cxx
FtfTrackDict.o: $(GRAPHSRC)/FtfTrackDict.h 
	$(CC) $(CFLAGS) -I$(INC) -I $(SRC) -I $(GRAPHSRC) -c $(GRAPHSRC)/FtfTrackDict.cxx
FtfFinderDict.o: $(GRAPHSRC)/FtfFinderDict.h 
	$(CC) $(CFLAGS) -I$(INC) -I $(SRC) -I $(GRAPHSRC) -c $(GRAPHSRC)/FtfFinderDict.cxx
FtfParaDict.o: $(GRAPHSRC)/FtfParaDict.h 
	$(CC) $(CFLAGS) -I$(INC) -I $(SRC) -I $(GRAPHSRC) -c $(GRAPHSRC)/FtfParaDict.cxx
FtfSl3Dict.o: $(GRAPHSRC)/FtfSl3Dict.h 
	$(CC) $(CFLAGS) -I$(INC) -I $(SRC) -I $(GRAPHSRC) -c $(GRAPHSRC)/FtfSl3Dict.cxx
FtfGraphicDict.o: $(GRAPHSRC)/FtfGraphicDict.h 
	$(CC) $(CFLAGS) -I$(INC) -I $(SRC) -I $(GRAPHSRC) -c $(GRAPHSRC)/FtfGraphicDict.cxx

#==================================
clean:
	rm -rf *.o
	rm FtfSl3.so
	rm sl3Daq
	rm sl3A

