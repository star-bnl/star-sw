#
#-----------------------------------
CC      = g++
#-----------------------------------

DIR      = /u/yepes/wrk/pams/l3
INC      = ../inc
SRC      = ../ftf
GRAPHSRC = graphic
GEN      = ../gen
GL3DIR   = ../gl3

#----------------------------------- 

#CFLAGS    = -g -D ALPHA_500MHZ
#CFLAGS	  = -g
#
#    Option for graphic, debug version
#
CFLAGS    = -g -DDEBUG -D SL3ROOT -I$(ROOTSYS)/include 
#
#   Option for stand alone versions
# for alphas -mcpu=ev6 should be added, much faster!!! :)
CFLAGS	  = -O2  -I/DAQ/include 
LIBS      = -lm

#-----------------------------------

.SUFFIXES: .cc .cxx .o

#===================================

TRACKER 	= FtfFinder.o FtfHit.o FtfTrack.o FtfPara.o \
	  	  FtfBaseHit.o FtfBaseTrack.o \
	  	  FtfUtilities.o 

GRAPHTRACK      = FtfGraphic.o FtfBaseHitDict.o FtfHitDict.o FtfBaseTrackDict.o \
		  FtfFinderDict.o FtfTrackDict.o FtfParaDict.o FtfSl3Dict.o \
		  FtfGraphicDict.o FtfDedxDict.o

SL3TRACKER     	= $(TRACKER) FtfSl3.o FtfDedx.o sl3Swap.o 

TRACKMATCHER    = TrackMatcher.o TrackMatcherDict.o TrackEvaluation.o \
		  FtfMcTrack.o TrackEvaluationDict.o FtfMcTrackDict.o

GL3     	= gl3Conductor.o gl3Event.o gl3Analysis.o gl3GeneralHistos.o \
		  gl3JPsi.o gl3GammaGamma.o gl3dEdx.o gl3HighPt.o gl3Histo.o \
		  gl3Svt.o gl3Residuals.o gl3Track.o gl3Hit.o 

GL3             = gl3Track.o gl3TrackDict.o


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

Ftf.so:$(SL3TRACKER) $(GRAPHTRACK) $(TRACKMATCHER) $(GL3)
	$(CC) -shared -o Ftf.so -I$(ROOTSYS)/include $(SL3TRACKER) $(GL3) \
	$(GRAPHTRACK) $(TRACKMATCHER) -lm
mFtf.so:FtfBaseHit.o FtfBaseHitDict.o
	$(CC) -shared $(CFLAGS) -o mFtf.so FtfBaseHit.o FtfBaseHitDict.o -lm


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
#   Track Matcher
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
TrackMatcher.o: $(INC)/TrackMatcher.h $(GEN)/TrackMatcher.cxx
	$(CC) $(CFLAGS) -I $(INC) -c $(GEN)/TrackMatcher.cxx
TrackMatcherDict.o:  $(GRAPHSRC)/TrackMatcherDict.cxx
	$(CC) $(CFLAGS) -I$(INC) -I $(GRAPHSRC) -c $(GRAPHSRC)/TrackMatcherDict.cxx
$(GRAPHSRC)/TrackMatcherDict.cxx: $(INC)/TrackMatcher.h
	rm -f TrackMatcherDict.*
	rootcint TrackMatcherDict.cxx -c $(INC)/TrackMatcher.h 
	mv TrackMatcherDict.* $(GRAPHSRC)/.

TrackEvaluation.o: $(INC)/TrackEvaluation.h $(GEN)/TrackEvaluation.cxx
	$(CC) $(CFLAGS) -I $(INC) -c $(GEN)/TrackEvaluation.cxx
TrackEvaluationDict.o:  $(GRAPHSRC)/TrackEvaluationDict.cxx
	$(CC) $(CFLAGS) -I$(INC) -I $(GRAPHSRC) -c $(GRAPHSRC)/TrackEvaluationDict.cxx
$(GRAPHSRC)/TrackEvaluationDict.cxx: $(INC)/TrackEvaluation.h
	rm -f TrackEvaluationDict.*
	rootcint TrackEvaluationDict.cxx -c $(INC)/TrackEvaluation.h 
	mv TrackEvaluationDict.* $(GRAPHSRC)/.

FtfMcTrack.o: $(INC)/FtfMcTrack.h $(SRC)/FtfMcTrack.cxx
	$(CC) $(CFLAGS) -I $(INC) -c $(SRC)/FtfMcTrack.cxx
FtfMcTrackDict.o:  $(GRAPHSRC)/FtfMcTrackDict.cxx
	$(CC) $(CFLAGS) -I$(INC) -I $(GRAPHSRC) -c $(GRAPHSRC)/FtfMcTrackDict.cxx
$(GRAPHSRC)/FtfMcTrackDict.cxx: $(INC)/FtfMcTrack.h
	rm -f FtfMcTrackDict.*
	rootcint FtfMcTrackDict.cxx -c $(INC)/FtfMcTrack.h 
	mv FtfMcTrackDict.* $(GRAPHSRC)/.



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   FTF Graphic
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
FtfGraphic.o: $(GRAPHSRC)/FtfGraphic.h 
	$(CC) $(CFLAGS) -I$(INC) -I $(SRC) -I $(GRAPHSRC) -c $(GRAPHSRC)/FtfGraphic.cxx
FtfBaseHitDict.o: $(GRAPHSRC)/FtfBaseHitDict.cxx
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
FtfDedxDict.o: $(GRAPHSRC)/FtfDedxDict.cxx
	$(CC) $(CFLAGS) -I$(INC) -I $(SRC) -I $(GRAPHSRC) -c $(GRAPHSRC)/FtfDedxDict.cxx
FtfGraphicDict.o:  $(GRAPHSRC)/FtfGraphicDict.cxx
	$(CC) $(CFLAGS) -I$(INC) -I $(SRC) -I $(GRAPHSRC) -c $(GRAPHSRC)/FtfGraphicDict.cxx

#+++++++++++++++++++++++++++++++++++++++++++++++++++++
#   GL3         
#+++++++++++++++++++++++++++++++++++++++++++++++++++++
gl3Conductor.o: $(INC)/gl3Conductor.h $(GL3DIR)/gl3Conductor.cxx
	$(CC) $(CFLAGS) -I $(INC) -c $(GL3DIR)/gl3Conductor.cxx
gl3Event.o: $(INC)/gl3Event.h $(GL3DIR)/gl3Event.cxx
	$(CC) $(CFLAGS) -I $(INC) -c $(GL3DIR)/gl3Event.cxx
gl3Analysis.o: $(INC)/gl3Analysis.h $(GL3DIR)/gl3Analysis.cxx
	$(CC) $(CFLAGS) -I $(INC) -c $(GL3DIR)/gl3Analysis.cxx
gl3GeneralHistos.o: $(INC)/gl3GeneralHistos.h $(GL3DIR)/gl3GeneralHistos.cxx
	$(CC) $(CFLAGS) -I $(INC) -c $(GL3DIR)/gl3GeneralHistos.cxx
gl3JPsi.o: $(INC)/gl3JPsi.h $(GL3DIR)/gl3JPsi.cxx
	$(CC) $(CFLAGS) -I $(INC) -c $(GL3DIR)/gl3JPsi.cxx
gl3GammaGamma.o: $(INC)/gl3GammaGamma.h $(GL3DIR)/gl3GammaGamma.cxx
	$(CC) $(CFLAGS) -I $(INC) -c $(GL3DIR)/gl3GammaGamma.cxx
gl3dEdx.o: $(INC)/gl3dEdx.h $(GL3DIR)/gl3dEdx.cxx
	$(CC) $(CFLAGS) -I $(INC) -c $(GL3DIR)/gl3dEdx.cxx
gl3HighPt.o: $(INC)/gl3HighPt.h $(GL3DIR)/gl3HighPt.cxx
	$(CC) $(CFLAGS) -I $(INC) -c $(GL3DIR)/gl3HighPt.cxx
gl3Svt.o: $(INC)/gl3Svt.h $(GL3DIR)/gl3Svt.cxx
	$(CC) $(CFLAGS) -I $(INC) -c $(GL3DIR)/gl3Svt.cxx
gl3Histo.o: $(INC)/gl3Histo.h $(GL3DIR)/gl3Histo.cxx
	$(CC) $(CFLAGS) -I $(INC) -c $(GL3DIR)/gl3Histo.cxx
gl3Track.o: $(INC)/gl3Track.h $(GL3DIR)/gl3Track.cxx
	$(CC) $(CFLAGS) -I $(INC) -c $(GL3DIR)/gl3Track.cxx
gl3Hit.o: $(INC)/gl3Hit.h $(GL3DIR)../../gl3/gl3Hit.cxx
	$(CC) $(CFLAGS) -I $(INC) -c $(GL3DIR)/gl3Hit.cxx
gl3Residuals.o: $(INC)/gl3Residuals.h $(GL3DIR)/gl3Residuals.cxx
	$(CC) $(CFLAGS) -I $(INC) -c $(GL3DIR)/gl3Residuals.cxx


gl3TrackDict.o:  $(GRAPHSRC)/gl3TrackDict.cxx
	$(CC) $(CFLAGS) -I$(INC) -I $(SRC) -I $(GRAPHSRC) -c $(GRAPHSRC)/gl3TrackDict.cxx

$(GRAPHSRC)/gl3TrackDict.cxx: $(INC)/gl3Track.h
	rootcint gl3TrackDict.cxx -c $(INC)/gl3Track.h
	mv *Dict.cxx $(GRAPHSRC)/.
	mv *Dict.h   $(GRAPHSRC)/.

#================================================================


$(GRAPHSRC)/FtfSl3Dict.cxx: $(INC)/FtfSl3.h
	rootcint FtfSl3Dict.cxx -c $(INC)/FtfSl3.h
	mv *Dict.cxx $(GRAPHSRC)/.
	mv *Dict.h   $(GRAPHSRC)/.
$(GRAPHSRC)/FtfFinderDict.cxx: $(INC)/FtfFinder.h
	rootcint FtfFinderDict.cxx -c $(INC)/FtfFinder.h
	mv *Dict.cxx $(GRAPHSRC)/.
	mv *Dict.h   $(GRAPHSRC)/.
$(GRAPHSRC)/FtfDedxDict.cxx: $(INC)/FtfDedx.h
	rootcint FtfDedxDict.cxx -c $(INC)/FtfDedx.h
	mv *Dict.cxx $(GRAPHSRC)/.
	mv *Dict.h   $(GRAPHSRC)/.
$(GRAPHSRC)/FtfHitDict.cxx: $(INC)/FtfHit.h
	rootcint FtfHitDict.cxx -c $(INC)/FtfHit.h
	mv *Dict.cxx $(GRAPHSRC)/.
	mv *Dict.h   $(GRAPHSRC)/.
$(GRAPHSRC)/FtfTrackDict.cxx: $(INC)/FtfTrack.h
	rootcint FtfTrackDict.cxx -c $(INC)/FtfTrack.h
	mv *Dict.cxx $(GRAPHSRC)/.
	mv *Dict.h   $(GRAPHSRC)/.
$(GRAPHSRC)/FtfBaseHitDict.cxx: $(INC)/FtfBaseHit.h
	rootcint FtfBaseHitDict.cxx -c $(INC)/FtfBaseHit.h
	mv *Dict.cxx $(GRAPHSRC)/.
	mv *Dict.h   $(GRAPHSRC)/.
$(GRAPHSRC)/FtfBaseTrackDict.cxx: $(INC)/FtfBaseTrack.h
	rootcint FtfBaseTrackDict.cxx -c $(INC)/FtfBaseTrack.h
	mv *Dict.cxx $(GRAPHSRC)/.
	mv *Dict.h   $(GRAPHSRC)/.
$(GRAPHSRC)/FtfParaDict.cxx: $(INC)/FtfPara.h
	rootcint FtfParaDict.cxx -c $(INC)/FtfPara.h
	mv *Dict.cxx $(GRAPHSRC)/.
	mv *Dict.h   $(GRAPHSRC)/.
$(GRAPHSRC)/FtfGraphicDict.cxx: $(GRAPHSRC)/FtfGraphic.h
	rootcint FtfGraphicDict.cxx -c $(GRAPHSRC)/FtfGraphic.h
	mv *Dict.cxx $(GRAPHSRC)/.
	mv *Dict.h   $(GRAPHSRC)/.

#==================================
clean:
	rm -rf *.o
	rm Ftf.so
	rm sl3Daq
	rm sl3A

