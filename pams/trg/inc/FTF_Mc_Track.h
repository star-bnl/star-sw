/*****************************************************************************
*
*        Generated tracks                
*/
#ifndef FTFMCTRACK
#define FTFMCTRACK
#include "FTF_general.h"
#include "BaseTrack.h"
#include "FTF_Hit.h"

//
//   Define class with space points
//
class FTF_Mc_Space_Point {
public:
	float x ;
	float y ;
	float z ;
} ;

class FTF_Mc_Track : public BaseTrack {
   
      public:

       short     pid ;        // Particle ID 
       short     parent_pid ; // Parent Particle ID
       float     p[3];        // 3-momentum 
       float     vertex[3] ;  // track origin 

	   long      n_mc_hits ;  // # space points in hit array
	   FTF_Mc_Space_Point* hit ;

	   FTF_Mc_Track  ( ) ;
	   ~FTF_Mc_Track ( ) ;
	   inline virtual   void next_hit (){ current_hit = current_hit->nxmhit ; } ;

   } ;
#endif

