//:>------------------------------------------------------------------
//: FILE:       FtfFinder.h
//: HISTORY:
//:             28oct1996 version 1.00
//:             23aug1999 ppy printVols and printRows deleted
//:             26jan2000 ppy destructor added
//:             27jan2000 ppy VOLUME, ROW and AREA classes replaced by
//:                           FtfContainer
//:             11feb2000 ppy timeout added, variables initialCpuTime and
//:                           initialRealTime added
//:<------------------------------------------------------------------

#ifndef FTFFINDER
#define FTFFINDER
#include <string.h>

#include "Stl3Util/ftf/FtfGeneral.h"
#include "Stl3Util/ftf/FtfPara.h"
#include "Stl3Util/ftf/FtfHit.h"
#include "Stl3Util/ftf/FtfTrack.h"
#include "Stl3Util/ftf/FtfMcTrack.h"


class FtfFinder {

 public:
   FtfFinder( ) ;
   ~FtfFinder( ) ;
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
   FtfContainer  *volumeC ;
   FtfContainer  *rowC    ;
   FtfContainer  *trackC  ;
   double        initialCpuTime ;
   double        initialRealTime ;
   double        cpuTime ;
   double        realTime ;
private: 

   FtfTrack      *currentTrack ;
} ;
#endif

