/*****************************************************************************
*
*        Generated tracks                
*/
#ifndef TRGGRAPHICMCTRACK
#define TRGGRAPHICMCTRACK
#include "TrgGraphicTrack.h"
#include "TrgGraphicHit.h"

class TrgGraphic ;
//
//   Define class with space points
//
class TrgGraphicPoint {
public:
	float x ;
	float y ;
	float z ;
} ;

class TrgGraphicMcTrack : public TrgGraphicTrack {
   
   public:

   short     pid ;        // Particle ID 
   short     parent_pid ; // Parent Particle ID
   float     p[3];        // 3-momentum 
   float     vertex[3] ;  // track origin 

   long      nMcHits ;  // # space points in hit array
   TrgGraphicPoint* hit ;

   void plotMc ( TrgGraphic* gr, int lpid, int ppid ) ;

} ;
#endif

