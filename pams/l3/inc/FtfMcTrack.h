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
       short     parent_pid ; // Parent Particle ID
       float     p[3];        // 3-momentum 
       float     vertex[3] ;  // track origin 

	   long      n_mc_hits ;  // # space points in hit array
	   FtfMcSpacePoint* hit ;

	   FtfMcTrack  ( ) ;
	   ~FtfMcTrack ( ) ;
	   inline virtual   void next_hit (){ current_hit = current_hit->nxmhit ; } ;

   } ;
#endif

