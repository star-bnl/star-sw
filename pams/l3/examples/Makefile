#
#-----------------------------------
CC      = g++
#-----------------------------------

INC      = ../inc
SRC      = ../ftf
GRAPHSRC = graphic

#----------------------------------- 

#CFLAGS    = -g -D ALPHA_500MHZ
#CFLAGS	  = -g
#
#    Option for graphic, debug version
#
CFLAGS    = -g -D DEBUG -D SL3ROOT -I$(ROOTSYS)/include -I$(ROOTSYS)/src    
#
#   Option for stand alone versions
# for alphas -mcpu=ev6 should be added, much faster!!! :)
CFLAGS	  = -O2  -I/DAQ/include 
LIBS      = -lm

#-----------------------------------

.SUFFIXES: .cc .cxx .o

#===================================

TRACKER 	= FtfFinder.o FtfTrack.o FtfHit.o FtfPara.o \
	  	  FtfBaseHit.o FtfBaseTrack.o \
	  	  FtfUtilities.o 

GRAPHTRACK      = FtfGraphic.o FtfBaseHitDict.o FtfHitDict.o FtfBaseTrackDict.o \
		  FtfTrackDict.o FtfFinderDict.o FtfParaDict.o FtfSl3Dict.o \
		  FtfGraphicDict.o

SL3TRACKER     	= $(TRACKER) FtfSl3.o FtfDedx.o sl3Swap.o 


.cxx.o:
	$(CC) $(CFLAGS) -I $(INC) -I $(SRC) -c $*.cxx
.cc.o:
	$(CC) $(CFLAGS) -I $(INC) -I $(SRC) -c $*.cc

#===================================


sl3A:$(SL3TRACKER) sl3StandAlone.o $(TRACKER)
	$(CC) sl3StandAlone.o $(SL3TRACKER) $(CFLAGS) $(LIBS) -I $(INC) -I $(SRC) -o sl3A

sl3D:$(SL3TRACKER) sl3DumpTracks.o
	$(CC) sl3DumpTracks.o $(SL3TRACKER) $(CFLAGS) $(LIBS) -I $(INC) -I $(SRC) -o sl3D

sl3Daq: $(SL3TRACKER) sl3Daq.o
	$(CC) sl3Daq.o $(SL3TRACKER) $(CFLAGS) $(LIBS) -I $(INC) -I $(SRC) -o sl3Daq

Ftf.so:$(SL3TRACKER) $(GRAPHTRACK)
	$(CC) -shared -o Ftf.so -I$(ROOTSYS)/include $(SL3TRACKER) $(GRAPHTRACK) -lm


#+++++++++++++++++++++++++++++++++++++++++++++++++++++
#   FTF TRACKER
#+++++++++++++++++++++++++++++++++++++++++++++++++++++
FtfFinder.o: $(INC)/FtfFinder.h $(SRC)/FtfFinder.cxx
	$(CC) $(CFLAGS) -I $(INC) -c $(SRC)/FtfFinder.cxx
FtfTrack.o: $(INC)/FtfTrack.h
	$(CC) $(CFLAGS) -I $(INC) -c $(SRC)/FtfTrack.cxx
FtfHit.o:  $(INC)/FtfHit.h $(SRC)/FtfHit.cxx
	$(CC) $(CFLAGS) -I $(INC) -c $(SRC)/FtfHit.cxx
FtfPara.o: $(INC)/FtfPara.h $(SRC)/FtfPara.cxx 
	$(CC) $(CFLAGS) -I $(INC) -c $(SRC)/FtfPara.cxx
FtfBaseHit.o: $(INC)/FtfBaseHit.h 
	$(CC) $(CFLAGS) -I $(INC) -c $(SRC)/FtfBaseHit.cxx
FtfBaseTrack.o: $(INC)/FtfBaseTrack.h 
	$(CC) $(CFLAGS) -I $(INC) -c $(SRC)/FtfBaseTrack.cxx
FtfUtilities.o: 
	$(CC) $(CFLAGS) -I $(INC) -c $(SRC)/FtfUtilities.cxx
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   SL3 Tracker
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
FtfSl3.o: $(INC)/FtfSl3.h $(SRC)/FtfSl3.cxx
	$(CC) $(CFLAGS) -I $(SRC) -I $(INC) -c $(SRC)/FtfSl3.cxx
FtfDedx.o: $(INC)/FtfDedx.h $(SRC)/FtfDedx.cxx
	$(CC) $(CFLAGS) -I $(SRC) -I $(INC) -c $(SRC)/FtfDedx.cxx
sl3Swap.o: 
	$(CC) $(CFLAGS) -I $(INC) -c $(SRC)/sl3Swap.cxx
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   FTF Graphic
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
FtfGraphic.o: $(GRAPHSRC)/FtfGraphic.h 
	$(CC) $(CFLAGS) -I$(INC) -I $(SRC) -I $(GRAPHSRC) -c $(GRAPHSRC)/FtfGraphic.cxx
FtfBaseHitDict.o: $(GRAPHSRC)/FtfBaseHitDict.h 
	$(CC) $(CFLAGS) -I$(INC) -I $(SRC) -I $(GRAPHSRC) -c $(GRAPHSRC)/FtfBaseHitDict.cxx
FtfHitDict.o: $(GRAPHSRC)/FtfHitDict.cxx
	$(CC) $(CFLAGS) -I$(INC) -I $(SRC) -I $(GRAPHSRC) -c $(GRAPHSRC)/FtfHitDict.cxx
FtfBaseTrackDict.o: $(GRAPHSRC)/FtfBaseTrackDict.cxx
	$(CC) $(CFLAGS) -I$(INC) -I $(SRC) -I $(GRAPHSRC) -c $(GRAPHSRC)/FtfBaseTrackDict.cxx
FtfTrackDict.o: $(GRAPHSRC)/FtfTrackDict.cxx
	$(CC) $(CFLAGS) -I$(INC) -I $(SRC) -I $(GRAPHSRC) -c $(GRAPHSRC)/FtfTrackDict.cxx
FtfFinderDict.o: $(GRAPHSRC)/FtfFinderDict.cxx
	$(CC) $(CFLAGS) -I$(INC) -I $(SRC) -I $(GRAPHSRC) -c $(GRAPHSRC)/FtfFinderDict.cxx
FtfParaDict.o: $(GRAPHSRC)/FtfParaDict.cxx
	$(CC) $(CFLAGS) -I$(INC) -I $(SRC) -I $(GRAPHSRC) -c $(GRAPHSRC)/FtfParaDict.cxx
FtfSl3Dict.o: $(GRAPHSRC)/FtfSl3Dict.cxx
	$(CC) $(CFLAGS) -I$(INC) -I $(SRC) -I $(GRAPHSRC) -c $(GRAPHSRC)/FtfSl3Dict.cxx
FtfGraphicDict.o:  $(GRAPHSRC)/FtfGraphicDict.cxx
	$(CC) $(CFLAGS) -I$(INC) -I $(SRC) -I $(GRAPHSRC) -c $(GRAPHSRC)/FtfGraphicDict.cxx

$(GRAPHSRC)/FtfSl3Dict.cxx: $(INC)/FtfSl3.h
	cd $(GRAPHSRC)
	rm FtfSl3Dict.*
	rootcint FtfSl3Dict.cxx -c FtfSl3.h
	cd ..
$(GRAPHSRC)/FtfBaseTrackDict.cxx: $(INC)/FtfBaseTrack.h
	rm $(GRAPHSRC)/FtfBaseTrackDict.*
	rootcint $(GRAPHSRC)/FtfBaseTrackDict.cxx -c $(INC)/FtfBaseTrack.h
$(GRAPHSRC)/FtfGraphicDict.cxx: $(GRAPHSRC)/FtfGraphic.h
	rm $(GRAPHSRC)/FtfGraphicDict.*
	rootcint $(GRAPHSRC)/FtfGraphicDict.cxx -c $(GRAPHSRC)/FtfGraphic.h

#==================================
clean:
	rm -rf *.o
	rm Ftf.so
	rm sl3Daq
	rm sl3A

