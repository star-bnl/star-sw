//:>------------------------------------------------------------------
//: FILE:       gl3Conductor.h
//: HISTORY:
//:              1feb2000 version 1.00
//:              6jul2000 add St_l3_Coordinate_Transformer
//:             16jul2000 add runStart and runEnd
//:             17jul2000 add runNumber            
//:             18jul2000 add timing information   
//:<------------------------------------------------------------------
#include <stdio.h>
#include <math.h>
#include "l3List.h"
#include "gl3Event.h"
#include "gl3Analysis.h"
#include "St_l3_Coordinate_Transformer.h"

#ifndef GL3CONDUCTOR
#define GL3CONDUCTOR

#define NTOKENS 4096

typedef gl3Analysis* pGl3Analysis ;

class gl3EventDecision {
public:
   short accept ; 
   short build  ;
   int   status[4];
   gl3EventDecision ( ):accept(0),build(0) { for(int i = 0 ; i < 4 ; i++ ) status[i] = 0 ; } ;
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
   short         maxAnalyses ;
   short         communicationsFlag ;
   int           nEvents ;
   int           nAnalyses ;
   long          maxTokens ;
   long          runNumber ;
   gl3Event*     event ;
   pGl3Analysis* analysis ;
   l3List        histoList ;
   int*          tokenIndex ;
   unsigned long mask[32];

   double        totalCpuTime ;
   double        totalRealTime ;
   double        readCpuTime ;
   double        readRealTime ;
   double*       analysisCpuTime ;
   double*       analysisRealTime ;
   //
   //  Options
   //
   short         hitProcessing ; // 0=does read hits
                                 // 1=reassigns trackId in hits to
				 // pass that info downstream
				 // 2=full hit unpacking for module use
   // 
   //  Communications
   //
   int socketFd, remoteSocket ;
   int gl3Port ;
//
//   Methods
//
   gl3Conductor  ( St_l3_Coordinate_Transformer* _trans,
                   int maxEventsIn=1, int maxAnalysisIn=10 ) ;
   ~gl3Conductor ( ) ;

   int  add ( gl3Analysis* analysis ) ;
   int  checkHistoRequest() ;
   int  init ( ) ;
   gl3EventDecision processEvent  ( int maxLength, char* buffer ) ;

   gl3Event* getEvent     ( int token ) ;
   int       releaseToken ( int token ) ;
   int       resetHistos  (  ) ;
   int       runStart     ( long _runNumber ) ;
   int       runEnd       (  ) ;
   double    cpuTime      ( ) ;
   double    realTime     ( ) ;


   void setHitProcessing  ( int hitPro ) ; 
   int  setCommunications (  ) ; 
   int  writeHistos ( int maxBytes, char *buffer ) ;
   int  setup  ( St_l3_Coordinate_Transformer* _trans,
                 int maxEvents=1, int maxAnalysis=10 ) ;


};
#endif
