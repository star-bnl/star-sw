//:>------------------------------------------------------------------
//: FILE:       FtfFinder.h
//: HISTORY:
//:             28oct1996 version 1.00
//:             23aug1999 ppy printVols and printRows deleted
//:<------------------------------------------------------------------

#ifndef FTFFINDER
#define FTFFINDER
#include <memory.h>

#include "FtfGeneral.h"
#include "FtfPara.h"
#include "FtfHit.h"
#include "FtfTrack.h"
#include "FtfMcTrack.h"
#include "FtfVolume.h"

#ifdef SL3ROOT
#include "Rtypes.h"
#else
#define ClassDef(a,b)
#endif


class FtfFinder {

public:
   FtfFinder( ) ;
   friend class FtfTrack ;
	
   void    dEdx                    ( ) ;
   int     getTracks               ( ) ;
   void    mergePrimaryTracks      ( ) ;
   double  process ( ) ;
   int     reset                   ( ) ;
   int     setConformalCoordinates ( ) ;
   int     setPointers             ( ) ;
   double  CpuTime                 ( ) ;
   double  RealTime                ( ) ;
//
//
   int           nHits      ;  
   int           nHitsOutOfRange ;
   int           maxHits    ;  
   FtfHit        *hit       ;  
   int           nTracks    ; 
   FtfTrack      *track     ;  
   FtfPara       para       ;
   int           maxTracks  ;
   int           nMcTracks  ;
   FtfMcTrack    *mcTrack    ;
   VOLUME        *volume     ;
   ROW           *rowk       ;
   AREA          *trackArea ;
   double        cpuTime ;
   double        realTime ;
private: 

   FtfTrack      *currentTrack ;

   ClassDef(FtfFinder,1)

    
} ;
#endif

