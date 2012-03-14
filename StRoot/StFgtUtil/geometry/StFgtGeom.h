/*  StFgtGeom.h
 *
 *  FGT geometry class declaration.
 *
 *  \author W. Witzke (wowitz0@uky.edu)
 *
 */

#ifndef _ST_FGT_GEOM_H_
#define _ST_FGT_GEOM_H_

//#include <assert.h>
#include <TVector3.h>
#include <string>
#include <sstream>
#include <cstdlib>
#include <cmath>
#include <iostream>
#include <algorithm>
#include "StRoot/StFgtUtil/StFgtConsts.h"


//  StFgtGeom is a "singleton" class. Only one of it needs to exist in any
//  program. However, because the data contained in this class is entirely
//  static, the class itself is also entirely static.
class StFgtGeom
{
    public:

	//  For all functions where they appear: Disc can be >= 0 (in theory,
	//  although only values 0-5 work at the moment, I believe). Quadrant
	//  is 0-3.  Layer is 'P' or 'R'. Strip is 0-720

        ///Location of disks in Z. these values for now are copied from FgtdGeo3.xml, 
        //should come from vmc eventually
	static Double_t getDiscZ(int iDisc) { 
	  switch(iDisc)
	    {
	    case 0:
	      return 67.399;
	    case 1:
	      return 77.8765;
	    case 2:
	      return 87.084;
	    case 3:
	      return 97.4821;
	    case 4:
	      return 108.9121;
	    case 5:
	      return 118.9927;
	    default:
	      return -1;
	    }
	}

	
	//geoId is a unique number used to identify a specific strip 
	//on a specific disk/quadrant/layer/strip.  Please NOTE:
	//The set of geoIds IS NOT CONTINUOUS simply becuase strip 
	//number is not continuous. On the R plane strips 280-399 
	//are not implemented.
	static Int_t encodeGeoId( Int_t disc, Int_t quadrant,
				  Char_t layer, Int_t strip );
	static void decodeGeoId( Int_t geoId, Short_t & disc,
				 Short_t & quadrant, Char_t & layer,
				 Short_t & strip );


	//Geoname is human readable form of geoId
	static std::string encodeGeoName( Int_t disc, Int_t quadrant,
					  Char_t layer, Int_t strip );
	static void decodeGeoName( const std::string & geoName, Short_t & disc,
				   Short_t & quadrant, Char_t & layer,
				   Short_t & strip );
	static std::string translateGeoIdToGeoName( Int_t geoId );
	static Int_t translateGeoNameToGeoId( const std::string & geoName );
	static void getPhysicalCoordinate( Int_t geoId, Short_t & disc,
					   Short_t & quadrant, Char_t & layer,
					   Double_t & ordinate, 
					   Double_t & lowerSpan, 
					   Double_t & upperSpan );
      
	//  The ordinate, lowerSpan and upperSpan are all in centimeters or
	//  radians, depending on the layer.
	static void getPhysicalCoordinate( const std::string & geoName,
					   Short_t & disc, Short_t & quadrant,
					   Char_t & layer, Double_t & ordinate,
					   Double_t & lowerSpan,
					   Double_t & upperSpan );
  
	//  Please note that the following functions do NOT access the STAR
	//  database to find mapping information. They assume the most
	//  straight-forward mapping scheme and use that.
	//  For those functions that have them, currently rdo can be 1-2, arm
	//  can be 0-5, apv can be 0-23 (although 10, 11, 22, and 23 are not
	//  technically valid) and channel is 0-127. To access database functions
	//  please use functions in StFgtDb.h

	//  Electronic Id is determined from the electronic devices rdo/arm/apv/channel
	//  and does form a continuous set of integers.  The mapping from geoId to electronicId 
	//  is accessible via the database or from "naive"  functions below

	static Int_t encodeElectronicId( Int_t rdo, Int_t arm,
					 Int_t apv, Int_t channel );

	static void decodeElectronicId( Int_t elecId, Int_t &rdo, Int_t &arm,
					Int_t &apv, Int_t &channel );
	static Int_t getElectIdFromElecCoord( Int_t rdo, Int_t arm,
					      Int_t apv, Int_t ch );
	static void getElecCoordFromElectId( Int_t eID, Int_t& rdo, Int_t& arm, 
					     Int_t& apv, Int_t& ch );

        // get the octant for a given layer and strip
        static Char_t getOctant( Char_t layer, Int_t strip );

        // get the octant given the APV number
        static Char_t getOctant( Int_t apv );

	static Int_t getNaiveGeoIdFromElecCoord( Int_t rdo, Int_t arm,
						 Int_t apv, Int_t channel );
	static void getNaiveElecCoordFromGeoId( Int_t geoId, Int_t& rdo,
						Int_t& arm, Int_t& apv,
						Int_t& channel );
	static std::string getNaiveGeoNameFromElecCoord( Int_t rdo, Int_t arm,
							 Int_t apv,
							 Int_t channel );
	static void getNaivePhysCoordFromElecCoord( Int_t rdo, Int_t arm,
						    Int_t apv, Int_t channel,
						    Short_t & disc,
						    Short_t & quadrant,
						    Char_t & layer,
						    Double_t & ordinate,
						    Double_t & lowerSpan,
						    Double_t & upperSpan );

	//  This is similar to the above functions, but it takes electronic
	//  coordinates and only returns the final ordinate. This is here
	//  primarily so that it can be used as a drop in replacement for
	//  older code that has similar functionality.
	static Double_t getNaiveMapping( Int_t rdo, Int_t arm,
					 Int_t apv, Int_t channel );
	static bool isNaiveR( Int_t rdo, Int_t arm, Int_t apv, Int_t channel );

	//  Jan's necessary functions start here.  These were written by Jan,
	//  modified slightly by me.
	//  Jan: I have adjusted the dimensions to match FGT as build, September, 2011
	static double rIn()	{ return kFgtRin; }
	static double rMid()	{ return kFgtRmid; }
	static double rOut()	{ return kFgtRout; }
	static double rFirst()  { return kFgtRfirst; }
	static double rLast()   { return kFgtRlast;}
	static double pFirst()  { return kFgtPfirst; }
	static double pLast()   { return kFgtPlast;}

	static double radStrip_pitch() { return kFgtRadPitch; }		//  cm
	static double phiStrip_pitch() { return  kFgtPhiAnglePitch; }	//  rad

	static double yLimit() { return kFgtRout; }

	//  deadQuadEdge is in cm, local ref frame
	static double deadQuadEdge()	{ return kFgtDeadQuadEdge; }

	static double radStripOff() { return mRadStripOff; }
	static double phiStripOff() { return mPhiStripOff; }

	static  double phiQuadXaxis(int iquad);
	static  bool inDisc( TVector3 rLab );	
	static  bool belowFlat( TVector3 rLoc );
	static  int  getQuad( double phiLab );
	
	//  What follows are some functions to help with the
	//  localXYtoStripID function.  These are also written by Jan, modified
	//  slightly by me.

	//  These next two return -1 on error.
	static  int rad2LocalStripId( double rad, double phi,
				      double *binFrac=0 );
	static  int phi2LocalStripId( double rad, double phi,
				      double *binFrac=0 );

	static double rStrip_Phi_High(int rindex);//return upper phi range for an r strip
	static double rStrip_Phi_Low(int rindex);//return lower phi range for an r strip
	static double pHistrip_R_Low(int pindex);//return lower r range for a phi strip
	static double pHistrip_R_High(int pindex);//return upper r range for a phi strip

    protected:
	//  StFgtGeomData stores data on each ordinate associated with each
	//  global ID used to index individual geometry elements.
	struct StFgtGeomData
	{
	    Bool_t isPhi;
	    Double_t ordinate;
	    Double_t lowerSpan;
	    Double_t upperSpan;
	};

	friend class StFgtDbFileMaker;


	//  Various constants used in Jan's conversion functions.
	static double mPi;
	static double mHalfPi;
	static double mRadStripOff;
	static double mPhiStripOff;
	static int mRadStripLOCId_number;
	static int mPhiStripLOCId_number;


	//  ---Private member variables---
	static StFgtGeomData mStrips[ 2*kFgtNumStrips ];

        // maps from (apv*128 + channel) to ((layer=='P')*kFgtNumStrips + stripID)
	static Int_t mNaiveMapping[  kFgtNumChannels*kFgtApvsPerQuad ];

        // reverse mapping: ((layer=='P')*kFgtNumStrips + stripID) to (apv*128 + channel)
        static Bool_t mReverseNaiveMappingValid;
	static Int_t mReverseNaiveMapping[ 2*kFgtNumStrips ];
        static void makeReverseNaiveMappingValid();
};

inline Int_t StFgtGeom::encodeGeoId
(
    Int_t disc, Int_t quadrant, Char_t layer, Int_t strip
)
{
    return
	(
	    ( disc*kFgtNumQuads + quadrant ) * kFgtNumLayers + ( layer == 'P' )
	) * kFgtNumStrips + strip;
}

inline void StFgtGeom::decodeGeoId
(
    Int_t geoId,
    Short_t & disc, Short_t & quadrant, Char_t & layer, Short_t & strip
)
{
    strip = geoId % kFgtNumStrips;
    geoId /= kFgtNumStrips;
  
    layer = ( geoId % kFgtNumLayers ) ? 'P' : 'R';
    geoId /= kFgtNumLayers;
  
    quadrant = geoId % kFgtNumQuads;
    disc = geoId / kFgtNumQuads;
}

inline std::string StFgtGeom::encodeGeoName
(
    Int_t disc, Int_t quadrant, Char_t layer, Int_t strip
)
{
    std::stringstream buff;
    buff << disc+1 << (Char_t)(quadrant+'A') << layer;

    if ( strip < 10 )
	buff << "00";
    else if ( strip < 100 )
	buff << "0";

    buff << strip;
    return buff.str();
}

inline void StFgtGeom::decodeGeoName
(
    const std::string & geoName,
    Short_t & disc, Short_t & quadrant, Char_t & layer, Short_t & strip
)
{
    //assert( geoName.size() == 6 );
    disc = geoName[0] - '1';
    quadrant = geoName[1] - 'A';
    layer = geoName[2];
    strip = std::atoi( (geoName.substr(3)).c_str() );
}

inline std::string StFgtGeom::translateGeoIdToGeoName( Int_t geoId )
{
    Short_t disc, quadrant, strip;
    Char_t layer;
  
    decodeGeoId( geoId, disc, quadrant, layer, strip );
    return encodeGeoName( disc, quadrant, layer, strip );
}

inline Int_t StFgtGeom::translateGeoNameToGeoId( const std::string & geoName )
{
    Short_t disc, quadrant, strip;
    Char_t layer;
  
    decodeGeoName( geoName, disc, quadrant, layer, strip );
    return encodeGeoId( disc, quadrant, layer, strip );
}

//  The ordinate, lowerSpan and upperSpan are all in centimeters or
//  radians, depending on the layer.
inline void StFgtGeom::getPhysicalCoordinate
(
    Int_t geoId,
    Short_t & disc, Short_t & quadrant, Char_t & layer,
    Double_t & ordinate, Double_t & lowerSpan, Double_t & upperSpan
)
{
    Short_t strip;
  
    decodeGeoId( geoId, disc, quadrant, layer, strip );
    ordinate =
	mStrips[
	    (layer == 'P') * kFgtNumStrips + strip
	    ].ordinate;
    lowerSpan =
    mStrips[
	    (layer == 'P') * kFgtNumStrips + strip
	    ].lowerSpan;
    upperSpan =
	mStrips[
	    (layer == 'P') * kFgtNumStrips + strip
	    ].upperSpan;
}

//  The ordinate, lowerSpan and upperSpan are all in centimeters or
//  radians, depending on the layer.
inline void StFgtGeom::getPhysicalCoordinate
(
    const std::string & geoName,
    Short_t & disc, Short_t & quadrant, Char_t & layer,
    Double_t & ordinate, Double_t & lowerSpan, Double_t & upperSpan
)
{
    Short_t strip;
  
    decodeGeoName( geoName, disc, quadrant, layer, strip );
    ordinate =
	mStrips[
	    (layer == 'P') * kFgtNumStrips + strip
	    ].ordinate;
    lowerSpan =
	mStrips[
	    (layer == 'P') * kFgtNumStrips + strip
	    ].lowerSpan;
    upperSpan =
	mStrips[
	    (layer == 'P') * kFgtNumStrips + strip
	    ].upperSpan;
}

//  Please note that the following functions do NOT access the STAR
//  database to find mapping information. They assume the most
//  straight-forward mapping scheme and use that.
//  For those functions that have them, currently rdo can be 1-2, arm
//  can be 0-5, apv can be 0-23 (although 10, 11, 22, and 23 are not
//  technically valid) and channel is 0-127.
inline Int_t StFgtGeom::encodeElectronicId
(
    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
)
{
    if ( apv >= 12 )
    {
	apv -= 2;
    }
    return channel+128*(apv+20*(arm+6*(rdo-1)));
}

inline void StFgtGeom::decodeElectronicId
(
    Int_t elecId,
    Int_t &rdo, Int_t &arm, Int_t &apv, Int_t &channel
)
{
    channel = elecId % 128;
    elecId /= 128;

    apv = elecId % 20;
    elecId /= 20;

    arm = elecId % 6;
    rdo = 1 + elecId / 6;

    if ( apv > 9 )
    {
	apv += 2;
    }
    return;
}

inline Int_t StFgtGeom::getElectIdFromElecCoord
(
    Int_t rdo, Int_t arm, Int_t apv, Int_t ch
)
{
    Int_t eID = -1;
    if ((rdo > 0)&&( rdo < 3))
	if ((arm >= 0)&&(arm < 6))
	    if ((apv >= 0)&&(apv< 24))
		if ((ch >= 0)&&(ch < 128))
		    eID = encodeElectronicId(rdo,arm,apv,ch); 
  
    return eID; 
}

inline void StFgtGeom::getElecCoordFromElectId
(
   Int_t eID,
   Int_t& rdo, Int_t& arm, Int_t& apv, Int_t& ch
)
{
  decodeElectronicId(eID,rdo,arm,apv,ch);
}

// get the octant for a given layer and strip
inline Char_t StFgtGeom::getOctant( Char_t layer, Int_t strip )
{
    return
	( strip < ( layer == 'R' ? kFgtNumRstripsPerOctant : kFgtNumPstripsPerOctant )
	    ? kFgtLowerStripOctant 
	    : kFgtHigherStripOctant );
}

// get the octant given the APV number
inline Char_t StFgtGeom::getOctant( Int_t apv )
{
   return ( (apv%kFgtApvsPerAssembly) < kFgtApvsPerOct
	    ? kFgtLowerStripOctant 
	    : kFgtHigherStripOctant );
}

inline Int_t StFgtGeom::getNaiveGeoIdFromElecCoord
(
    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
)
{
    Short_t disc = int(arm/2) + (rdo-1)*3;
    Short_t quadrant = (arm & 1)*2 + int( apv/12 );

    if ( apv >= 12 )
	return
	(
	    disc*kFgtNumQuads + quadrant
	) * kFgtNumLayers * kFgtNumStrips
	    + mNaiveMapping[ (apv-12)*128+channel ];
    else
	return
	(
	    disc*kFgtNumQuads + quadrant
	) * kFgtNumLayers * kFgtNumStrips
	    + mNaiveMapping[ apv*128+channel ];

}

inline std::string StFgtGeom::getNaiveGeoNameFromElecCoord
(
    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
)
{
    return
	translateGeoIdToGeoName(
	    getNaiveGeoIdFromElecCoord( rdo, arm, apv, channel )
	);
}

inline void StFgtGeom::getNaivePhysCoordFromElecCoord
(
    Int_t rdo, Int_t arm, Int_t apv, Int_t channel,
    Short_t & disc, Short_t & quadrant, Char_t & layer,
    Double_t & ordinate, Double_t & lowerSpan, Double_t & upperSpan
)
{
    getPhysicalCoordinate(
	getNaiveGeoIdFromElecCoord( rdo, arm, apv, channel ),
	disc, quadrant, layer, ordinate, lowerSpan, upperSpan
    );
}

//  This is similar to the above functions, but it takes electronic
//  coordinates and only returns the final ordinate. This is here
//  primarily so that it can be used as a drop in replacement for
//  older code that has similar functionality.
inline Double_t StFgtGeom::getNaiveMapping(
    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
)
{
    if ( apv >= 12 )
	return
	    mStrips[ mNaiveMapping[ (apv-12)*128+channel ] ].ordinate;
    else
	return
	    mStrips[ mNaiveMapping[ apv*128+channel ] ].ordinate;
}

inline bool StFgtGeom::isNaiveR(
    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
)
{
    if ( apv >= 12 )
	return
	    mNaiveMapping[ (apv-12)*128+channel ] < 720;
    else
	return
	    mNaiveMapping[ apv*128+channel ] < 720;
}
#endif


/*
 *  $Id: StFgtGeom.h,v 1.40 2012/03/14 00:58:56 rfatemi Exp $
 *  $Log: StFgtGeom.h,v $
 *  Revision 1.40  2012/03/14 00:58:56  rfatemi
 *  added documentation
 *
 *  Revision 1.39  2012/03/07 20:31:43  avossen
 *  corrected z disc z position
 *
 *  Revision 1.38  2012/02/09 18:23:24  wwitzke
 *  Fixed various minor issues, including nesting the StFgtGeomData, making the
 *  various "pi" variables use the TMath definions of pi, and removing the various
 *  asserts from the code.
 *
 *  Revision 1.37  2012/02/09 17:52:03  wwitzke
 *  Changed organization of StFgtGeom to put inline functions after class body.
 *  Also moved getNaiveElecCoordFromGeoId to .cxx as a non-inline function.
 *
 *  Revision 1.36  2012/02/09 17:05:58  wwitzke
 *  Fixed public/protected/private problems (moved some members from public to
 *  protected, consolidated public methods).
 *
 *  Revision 1.35  2012/02/09 17:00:10  wwitzke
 *  Modified some variable names to conform to standard naming conventions.
 *
 *  Revision 1.33  2012/01/31 15:34:23  rfatemi
 *  make it a friend of StFgtDbMaker which lives in StFgtPool
 *
 *  Revision 1.32  2012/01/28 10:46:37  sgliske
 *  Forgot to include assert, and removing unneeded TObject include
 *
 *  Revision 1.31  2012/01/28 10:29:47  sgliske
 *  static const doubles moved from StFgtGeom to StFgtConsts
 *  Also, geoName updated to more recent convention
 *  disc in 1-6, quad in A-D, strip in 0-719
 *
 *  Revision 1.30  2012/01/27 13:21:25  rfatemi
 *  used only constants declared in enums in StFgtConsts.h
 *
 *  Revision 1.29  2012/01/26 18:41:49  balewski
 *  fixing , new constants
 *
 *  Revision 1.28  2012/01/26 13:13:12  sgliske
 *  Updated to use StFgtConsts, which
 *  replaces StFgtEnums and StFgtGeomDefs
 *
 *  Revision 1.27  2012/01/19 16:27:27  rfatemi
 *  Move encode(decode) ElectronicId from StFgtDbImp.h
 *
 *  Revision 1.26  2012/01/18 18:22:31  sgliske
 *  added function getElecCoordFromElectId
 *
 *  Revision 1.25  2012/01/13 19:10:41  rfatemi
 *  included getElectIdfromElecCoord to calculate electronic id from RDO,ARM,APVmod,CH
 *
 *  Revision 1.24  2011/11/06 22:34:34  rfatemi
 *  implement phi2LocalStripId
 *
 *  Revision 1.23  2011/11/05 02:24:56  rfatemi
 *  add phi2LocalStripId
 *
 *  Revision 1.22  2011/11/03 20:04:41  avossen
 *  added simple function to get disc z
 *
 *  Revision 1.21  2011/10/13 21:02:15  balewski
 *  cleanup of not needed intermediate methods
 *
 *  Revision 1.20  2011/10/11 15:56:15  rfatemi
 *  add in new access functions
 *
 *  Revision 1.19  2011/10/09 13:36:44  rfatemi
 *  Update with Rlast and Rfirst for location of R strips
 *
 *  Revision 1.18  2011/10/07 19:43:32  balewski
 *  make method public
 *
 *  Revision 1.17  2011/10/07 03:42:38  rfatemi
 *  Updates to get strip Id from radius r
 *
 *  Revision 1.16  2011/10/06 15:16:15  sgliske
 *  fixed spaces before Log: and Id:
 *
 *
 */
