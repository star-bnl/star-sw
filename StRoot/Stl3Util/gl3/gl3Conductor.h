//:>------------------------------------------------------------------
//: FILE:       gl3Conductor.h
//: HISTORY:
//:              1feb2000 version 1.00
//:              6jul2000 add St_l3_Coordinate_Transformer
//:             16jul2000 add runStart and runEnd
//:             17jul2000 add runNumber            
//:             18jul2000 add timing information   
//:             27jul2000 add print timing 
//:             13aug2000 replace trackMerging with maxSectorNForTrackMerging     
//:             24aug2000 move functionality of the constructor into setup 
//:                       function. this has to be called directly now
//:<------------------------------------------------------------------
#include "Stl3Util/gl3/gl3Event.h"
#include "Stl3Util/gl3/gl3Algorithm.h"

#include "Stl3Util/base/St_l3_Coordinate_Transformer.h"

#include "Stl3Util/foreign/RC_Config.h"

#include <stdio.h>
#include <math.h>
#include <list>
#include <vector>

#ifndef GL3CONDUCTOR
#define GL3CONDUCTOR

#define NTOKENS 4096

typedef gl3Algorithm* pGl3Algorithm ;

class gl3EventDecision {
public:
   short accept ; 
   short build  ;
   int   status[4];
   gl3EventDecision ( ):accept(0),build(0) 
     { 
       for(int i = 0 ; i < 4 ; i++ ) 
	 status[i] = 0 ; 
     } ;
};


class gl3HistoContainer {
public:
   int nBytes;
   int nHistos;
   int runNumber ;
   int buffer;
};

class gl3Conductor {


public:
   short         maxEvents ;
   short         maxAlgorithms ;
   short         communicationsFlag ;
   int           nEvents ;
   int           nAlgorithms ;
   int           maxTokens ;
   int           runNumber ;
   pGl3Algorithm* algorithm ;
   int*          tokenIndex ;
   //  unsigned int mask[32];

   //
   //  Options
   //
   short         hitProcessing ; // 0 = does read hits
                                 // 1 = reassigns trackId in hits to
				 //     pass that info downstream
				 // 2 = full hit unpacking for module use
   int           maxSectorNForTrackMerging ;  // Max multipliticy for track merging

 public:
   gl3Conductor (void ) ; // empty constructor, functionality moved to setup
   ~gl3Conductor ( ) ;

   //int  configure (char *file);
   int  configure (L3_CFG *cfg);
   int  add ( gl3Algorithm* algorithm ) ;


   int fillSummary(int token, struct L3_summary*, struct L3_SUMD *);
   int collectSummary(int token);
   int fillTracks(int token);

   void clearAlgorithms();
   int  checkHistoRequest() ;
   int  init ( ) ;

   int  processEvent(EventDescriptor *desc, L3_P* l3data,
		     TrgSumData* = NULL, RawTrgDet* = NULL);

   //    int  processEvent(int token, L3_P* l3data,
   // 		     TrgSumData* = NULL, RawTrgDet* = NULL);

   gl3Event* getEvent     ( int token ) ;
   int       releaseToken ( int token ) ;
   int       resetHistos  (  ) ;
   int       runStart     ( int _runNumber ) ;
   int       runEnd       (  ) ;
   int       end          (  ) ;


   void setBField         ( float bField ) ; 
   void setHitProcessing  ( int hitPro ) ; 
   void setMaxSectorNForTrackMerging   ( int _trackMerging ) ; 
   int  setup  ( St_l3_Coordinate_Transformer* _trans,
                 int maxEvents=1, int maxAlgorithm=10 ) ;
   int  setCommunications (  ) ; 
   int  writeHistos ( int maxBytes, char *buffer ) ;

 private:
   int getFreeEventIndex();
   int getTokenIndex(int);

   gl3Event*    event ;
   L3_summary*  summary;
   L3_SUMD*     summaryData;
   
   int nReco;


   // Histogramming/histo communication stuff
   list<gl3Histo *> histoList ;
   gl3Histo* l3Rates;


   int socketFd, remoteSocket ;
   int gl3Port ;

   // Timing
   void resetTimer();
   void timingMark();

   void allocateTimingHistos();
   void fillTimingHistos();

   void printTiming();
   
   int lastTimeEntry;
   vector<unsigned long> cpuTimes, realTimes;
   vector<gl3Histo*>  timingHistos;

   double ccPerMs;
};
#endif
