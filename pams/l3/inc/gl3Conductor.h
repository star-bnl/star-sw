//:>------------------------------------------------------------------
//: FILE:       gl3Conductor.h
//: HISTORY:
//:              1feb2000 version 1.00
//:<------------------------------------------------------------------
#include <stdio.h>
#include <math.h>
#include "l3List.h"
#include "gl3Event.h"
#include "gl3Analysis.h"

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


class gl3Conductor {
public:

   short         maxEvents ;
   short         maxAnalyses ;
   short         communicationsFlag ;
   int           nEvents ;
   int           nAnalyses ;
   long          maxTokens ;
   gl3Event*     event ;
   pGl3Analysis* analysis ;
   l3List        histoList ;
   int*          tokenIndex ;
   unsigned long mask[32];
   // 
   //  Communications
   //
   int socketFd, remoteSocket ;
   int gl3Port ;
//
//   Methods
//
   gl3Conductor  ( int maxEventsIn=1, int maxAnalysisIn=10 ) ;
   ~gl3Conductor ( ) ;

   int  add ( gl3Analysis* analysis ) ;
   int  checkHistoRequest() ;
   int  init ( ) ;
   gl3EventDecision processEvent  ( int maxLength, char* buffer ) ;

   gl3Event* getEvent     ( int token ) ;
   int       releaseToken ( int token ) ;

   int  setCommunications (  ) ; 
   int  writeHistos ( int maxBytes, char *buffer ) ;
   int  setup  ( int maxEvents=1, int maxAnalysis=10 ) ;


};
#endif
