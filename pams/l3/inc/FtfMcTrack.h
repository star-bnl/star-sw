/*****************************************************************************
*
*        Generated tracks                
*/
#ifndef FTFMCTRACK
#define FTFMCTRACK
#include "FtfGeneral.h"
#include "FtfBaseTrack.h"
#include "FtfHit.h"

//
//   Define class with space points
//
class FtfMcSpacePoint {
public:
   float x ;
   float y ;
   float z ;
} ;

class FtfMcTrack : public FtfBaseTrack {
   
public:

   short     pid ;        // Particle ID 
   short     parentPid ; // Parent Particle ID
   float     p[3];        // 3-momentum 
   float     vertex[3] ;  // track origin 

   long      nMcHits ;  // # space points in hit array

   inline virtual void nextHit (){ currentHit = currentHit->nextMcTrackHit ; } ;

} ;
#endif

