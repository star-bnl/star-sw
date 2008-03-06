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

   short     pid ;       // Particle ID 
   short     parentPid ; // Parent Particle ID
   float     p[3];       // 3-momentum 
   float     vertex[3] ; // track origin 
   void*     nextTrack ; // pointer to next track in track grid

   long      nMcHits ;  // # space points in hit array

   inline virtual void nextHit (){ currentHit = ((FtfBaseHit *)currentHit)->nextMcTrackHit ; } ;

   void setPrimary                ( short qIn, float ptIn, float eta, float psiIn ) ;

   void setRandomPrimary ( float ptMin,  float ptMax, float etaMin, float etaMax,
			   float psiMin, float psiMax, float zVert ) ;


   void set ( int _id, float _r0, float _z0, float _phi0,
              float _pt, float _tanl, float _psi, int _q, int _nHits ) ;

} ;
#endif

