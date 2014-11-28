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

#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"

class StFgtCosmicTestStandGeom:public StFgtGeom
{
    public:

	    
  
         static Int_t getNaiveGeoIdFromElecCoord
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
                apv -= 12;

             return
                ( disc*kFgtNumQuads + quadrant ) * kFgtNumLayers * kFgtNumStrips
                + mNaiveMapping[ apv*128+channel ];
	   }
	   
	static void getNaiveElecCoordFromGeoId(
            Int_t geoId, Int_t& rdo, Int_t& arm, Int_t& apv, Int_t& channel
	)
	{
           // start with doing the same as for FGT-in-star 
           Short_t disc, quadrant, strip;
           Char_t layer;

           decodeGeoId( geoId, disc, quadrant, layer, strip );

           if( !mReverseNaiveMappingValid )
              makeReverseNaiveMappingValid();

           Int_t key = ( (layer=='P')*kFgtNumStrips + strip );
           channel = mReverseNaiveMapping[ key ];
           apv = channel / 128;
           channel %= 128;

           if( quadrant % 2 )
              apv += 12;

           rdo = disc/3+1;
           arm = (disc % 3)*2 + (quadrant>1);

           // set apv into range 0-11
           apv %= 12;

           // adjust values
           if( quadrant == 0 ){
              if( disc == 0 ){
                 arm = 0; 
              } else if( disc == 1 ){
                 arm = 1;
              } else if( disc == 2 ){
                 arm = 1;
                 apv += 12;
              };
           };
        };
	 

};

#endif

/*
 *  $Id: StFgtCosmicTestStandGeom.h,v 1.1 2012/01/31 15:50:08 wwitzke Exp $
 *  $Log: StFgtCosmicTestStandGeom.h,v $
 *  Revision 1.1  2012/01/31 15:50:08  wwitzke
 *  Added StFgtCosmicTestStandGeom in StFgtPool.
 *
 *  Revision 1.5  2012/01/26 13:13:12  sgliske
 *  Updated to use StFgtConsts, which
 *  replaces StFgtEnums and StFgtGeomDefs
 *
 *  Revision 1.4  2011/10/06 15:17:27  sgliske
 *  CVS says it updated the file, but I don't see a change
 *
 *  Revision 1.3  2011/09/29 18:34:53  sgliske
 *  Fixed phiQuadXaxis, added asserts to getQuad,and added reverse lookup: elec. coord. from geoId
 *
 *
 */
