/*  StFgtCosmicRayTestStandGeom.h
 *
 *  FGT geometry class declaration.
 *  ONLY USE FOR COSMIC RAY TEST STAND
 *
 *  \author W. Witzke (wowitz0@uky.edu)
 *
 */

#ifndef _ST_FGT_CS_GEOM_H_
#define _ST_FGT_CS_GEOM_H_

#include <TObject.h>
#include <TVector3.h>
#include <string>
#include <sstream>
#include <cstdlib>
#include <cmath>

#include "StFgtGeom.h"
#include "StFgtGeomDefs.h"

class StFgtCosmicTestStandGeom:public StFgtGeom
{
    public:

	    
  
         static Int_t getNaiveGeoIdFromElecCoordin
	   (
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	    )
	   {
	     
	     Short_t disc = int(arm/2) + (rdo-1)*3;
	     
	     Short_t quadrant = (arm & 1)*2 + int( apv/12 );
      
	     if ((arm==0)&&(apv<12)&&(apv>=0)) 
	       {
		 disc=0;
		 quadrant=0;
	       }
	     
	     if ((arm==1)&&(apv<12)&&(apv>=0)) 
	       {
		 disc=1;
		 quadrant=0;
	       }
	     
	     if ((arm ==1)&&(apv<24)&&(apv>=12))
	       {
		 disc=2;
		 quadrant=0;
	       }
	     
	     if ( apv >= 12 )
	       return
		 (
		  disc*kNumFgtQuadrants + quadrant
		  ) * kNumFgtLayers + mNaiveMapping[ (apv-12)*128+channel ];
	     else
	       return
		 (
		  disc*kNumFgtQuadrants + quadrant
		  ) * kNumFgtLayers + mNaiveMapping[ apv*128+channel ];
	   }
	   
	 

};

#endif

/*
 *  $ Id: $
 *  $ Log: $
 *
 */
