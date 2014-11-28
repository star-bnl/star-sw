/*  StGmtGeom.h
 *
 *  GMT geometry class declaration.
 *
 *  \authors K.S. Engle and Richard Witt (witt@usna.edu)
 *  based on StFgtGeom
 */

#ifndef _ST_GMT_GEOM_H_
#define _ST_GMT_GEOM_H_

//#include <assert.h>
#include <TVector3.h>
#include <string>
#include <sstream>
#include <cstdlib>
#include <cmath>
#include <iostream>
#include <algorithm>
#include "StRoot/StGmtUtil/StGmtConsts.h"


//  StGmtGeom is a "singleton" class. Only one of it needs to exist in any
//  program. However, because the data contained in this class is entirely
//  static, the class itself is also entirely static.
class StGmtGeom
{
    public:

	//  For all functions where they appear: Disc can be >= 0 (in theory,
	//  although only values 0-5 work at the moment, I believe). Quadrant
	//  is 0-3.  Layer is 'P' or 'R'. Strip is 0-720

        //  Location of modules in Z. 
	static Double_t getModuleZ(int iModule);

	//  Location of modules in phi. 
	static Double_t getModulePhi(int iModule);


	static Int_t getNaiveMapping( Int_t idx );
	
	//geoId is a unique number used to identify a specific strip 
	//on a specific disk/quadrant/layer/strip.  Please NOTE:
	//The set of geoIds IS NOT CONTINUOUS simply becuase strip 
	//number is not continuous. On the R plane strips 280-399 
	//are not implemented.
// 	static Int_t encodeGeoId( Int_t module,
// 				  Char_t layer, Int_t strip );
	static Int_t encodeGeoId( Int_t rdo, Int_t arm, Int_t apv, Int_t channel );
// 	static Int_t decodeGeoId( Int_t geoId, Short_t & module,
// 				 Char_t & layer,
// 				 Short_t & strip );
	static Int_t decodeGeoId( Int_t geoId, Short_t & module, Int_t & layer, Short_t & strip );


	//Geoname is human readable form of geoId
	static std::string encodeGeoName( Int_t module, 
					  Char_t layer, Int_t strip );
// 	static Int_t decodeGeoName( const std::string & geoName, Short_t & module,
// 				   Char_t & layer,
// 				   Short_t & strip );
	static Int_t decodeGeoName( const std::string & geoName, Short_t & module,
				   Int_t & layer,
				   Short_t & strip );
	static std::string translateGeoIdToGeoName( Int_t geoId );
	static Int_t translateGeoNameToGeoId( const std::string & geoName );

	//Returns range upper and lower range of R or Phi valus depending on geoId.  
	//NOTE phi values are only local - that is they are the same for each quadrant
	//The ordinate, lowerSpan and upperSpan are all in centimeters or radians
// 	static Int_t getPhysicalCoordinate( Int_t geoId, Short_t & module,
// 					   Char_t & layer);
	static Int_t getPhysicalCoordinate( Int_t geoId, Short_t & module,
					   Int_t & layer);


      
	//Returns range upper and lower range of R or Phi valus depending on geoName.  
	//NOTE phi values are only local - that is they are the same for each quadrant
	//The ordinate, lowerSpan and upperSpan are all in centimeters or radians
// 	static Int_t getPhysicalCoordinate( const std::string & geoName,
// 					    Short_t & module,
// 					    Char_t & layer);
	static Int_t getPhysicalCoordinate( const std::string & geoName,
					    Short_t & module,
					    Int_t & layer);
	

	//Similar to getPhysicalCoordinate but returns phi in STAR coordinate system
// 	static Int_t getGlobalPhysicalCoordinate( Int_t geoId, Short_t & module,
// 					    Char_t & layer);
	static Int_t getGlobalPhysicalCoordinate( Int_t geoId, Short_t & module,
					    Int_t & layer);

	//Similar to getPhysicalCoordinate but returns phi in STAR coordinate system
// 	static Int_t getGlobalPhysicalCoordinate( const std::string & geoName,
// 					    Short_t & module,
// 					    Char_t & layer);
	static Int_t getGlobalPhysicalCoordinate( const std::string & geoName,
					    Short_t & module,
					    Int_t & layer);

	static Int_t getCoordNumFromElecCoord( Int_t rdo, Int_t arm, Int_t apv, Int_t channel);
	static Double_t getPositionFromElecCoord( Int_t rdo, Int_t arm, Int_t apv, Int_t channel);
  
	//  Please note that the following functions do NOT access the STAR
	//  database to find mapping information. They assume the most
	//  straight-forward mapping scheme and use that.
	//  For those functions that have them, currently rdo can be 1-2, arm
	//  can be 0-5, apv can be 0-23 (although 10, 11, 22, and 23 are not
	//  technically valid) and channel is 0-127. To access database
	//  functions please use functions in StGmtDb.h

	//  Electronic Id is determined from the electronic devices
	//  rdo/arm/apv/channel and does form a continuous set of integers.
	//  The mapping from geoId to electronicId is accessible via the
	//  database or from "naive"  functions below

	static Int_t encodeElectronicId( Int_t rdo, Int_t arm,
					 Int_t apv, Int_t channel );

	static Int_t decodeElectronicId( Int_t elecId, Int_t &rdo, Int_t &arm,
					Int_t &apv, Int_t &channel );
	static Int_t getElectIdFromElecCoord( Int_t rdo, Int_t arm,
					      Int_t apv, Int_t ch );
	static Int_t getElecCoordFromElectId( Int_t eID, Int_t& rdo, Int_t& arm, 
					     Int_t& apv, Int_t& ch );

        static Short_t getModuleIdFromElecCoord(Int_t rdo, Int_t arm, Int_t apv);
	
        // get the octant for a given layer and strip
        static Char_t getOctant( Char_t layer, Int_t strip );

        // get the octant given the APV number
        static Char_t getOctant( Int_t apv );

	// get the octant given a phi in radians
	// maps to i8: 0=A.L, 1=A.S, 2=B.L, .... 7=D.S
	static Int_t getOctant( Double_t phi);

	static Int_t getNaiveGeoIdFromElecCoord( Int_t rdo, Int_t arm,
						 Int_t apv, Int_t channel );
	static Int_t getNaiveElecCoordFromGeoId( Int_t geoId, Int_t& rdo,
						Int_t& arm, Int_t& apv,
						Int_t& channel );
	static std::string getNaiveGeoNameFromElecCoord( Int_t rdo, Int_t arm,
							 Int_t apv,
							 Int_t channel );
	static Int_t getNaivePhysCoordFromElecCoord( Int_t rdo, Int_t arm,
						    Int_t apv, Int_t channel,
						    Short_t & module,
						    Char_t & layer
						    );

	//  This is similar to the above functions, but it takes electronic
	//  coordinates and only returns the final ordinate. This is here
	//  primarily so that it can be used as a drop in replacement for
	//  older code that has similar functionality.
	static Double_t getNaiveMapping( Int_t rdo, Int_t arm,
					 Int_t apv, Int_t channel );
	static bool isNaiveR( Int_t rdo, Int_t arm, Int_t apv, Int_t channel );

	//  Jan's necessary functions start here.  These were written by Jan,
	//  modified slightly by me.
	//  Jan: I have adjusted the dimensions to match GMT as build, September, 2011
// 	static double rIn()	{ return kGmtRin; }
// 	static double rMid()	{ return kGmtRmid; }
// 	static double rOut()	{ return kGmtRout; }
	static double sFirst()  { return kGmtSfirst; }
	static double sLast()   { return kGmtSlast;}
	static double pFirst()  { return kGmtPfirst; }
	static double pLast()   { return kGmtPlast;}

// 	static double radStrip_pitch() { return kGmtRadPitch; }		//  cm
// 	static double phiStrip_pitch() { return  kGmtPhiAnglePitch; }	//  rad
	static double xStrip_pitch() { return kGmtXPitch; }		//  cm
	static double yStrip_pitch() { return  kGmtYPitch; }	//  cm

// 	static double yLimit() { return kGmtRout; }

	//  deadQuadEdge is in cm, local ref frame
// 	static double deadQuadEdge()	{ return kGmtDeadQuadEdge; }

// 	static double radStripOff() { return mRadStripOff; }
// 	static double phiStripOff() { return mPhiStripOff; }
// 	static double StripOff() { return mStripOff; }
// 	static double PadOff() { return mPadOff; }

// 	static  double phiQuadXaxis(int iquad);
// 	static  bool inDisc( TVector3 rLab );	
// 	static  bool belowFlat( TVector3 rLoc );
// 	static  int  getQuad( double phiLab );
	static  bool inModule( TVector3 rLab );	
	
	//  What follows are some functions to help with the
	//  localXYtoStripID function.  These are also written by Jan, modified
	//  slightly by me.

	//  These next two return -1 on error.
	static  int rad2LocalStripId( double rad, double phi,
				      double *binFrac=0 );
	static  int phi2LocalStripId( double rad, double phi,
				      double *binFrac=0 );

// 	static double rStrip_Phi_High(int rindex);//return upper phi range for an r strip
// 	static double rStrip_Phi_Low(int rindex);//return lower phi range for an r strip
// 	static double pHistrip_R_Low(int pindex);//return lower r range for a phi strip
// 	static double pHistrip_R_High(int pindex);//return upper r range for a phi strip

    protected:
	//  StGmtGeomData stores data on each ordinate associated with each
	//  global ID used to index individual geometry elements.
// 	struct StGmtGeomData
// 	{
// // 	    Bool_t isPhi;
// // 	    Double_t ordinate;
// // 	    Double_t lowerSpan;
// // 	    Double_t upperSpan;
// 	    Bool_t isX;
// 	    Double_t localX;
// 	    Double_t localY;
// 	};
	struct StGmtGeomData
	{
	    Int_t apv;         // apv number (arm channel 0-3 or 12-15)
	    Int_t channel;     // apv channel (0-127)
	    Bool_t isY;        // is it a pad (true) or strip (false)
	    Int_t coordinate;  // coordinate number
	    Double_t location; // in cm
	    TString signal;   // label string
	    Int_t readout;  // number in readout order
	};

	friend class StGmtDbFileMaker;


	//  Various constants used in Jan's conversion functions.
	static double mPi;
	static double mHalfPi;
// 	static double mRadStripOff;
// 	static double mPhiStripOff;
// 	static int mRadStripLOCId_number;
// 	static int mPhiStripLOCId_number;
// 	static double mStripOff;
// 	static double mPadOff;


	//  ---Private member variables---
// 	static StGmtGeomData mStrips[ 2*kGmtNumStrips ];
	static StGmtGeomData mStrips[ kGmtNumStripsPerModule ]; // 128 X strip and 128 Y strips

        // maps from (apv*128 + channel) to ((layer=='P')*kGmtNumStrips + stripID)
// 	static Int_t mNaiveMapping[  kGmtNumChannels*kGmtApvsPerQuad ];
	static Int_t mNaiveMapping[ kGmtNumStripsPerModule ];

        // reverse mapping: ((layer=='P')*kGmtNumStrips + stripID) to (apv*128 + channel)
        static Bool_t mReverseNaiveMappingValid;
	static Int_t mReverseNaiveMapping[ kGmtNumStripsPerModule ];
        static void makeReverseNaiveMappingValid();

	
     private: 

	//  Calculates coordinates of strip in global coordinate system
        //  Units are in cm andradians, depending on the layer.
//         static Int_t computeGlobalPhysicalCoordinate(Char_t & layer,
        static Int_t computeGlobalPhysicalCoordinate(Int_t & layer,
                                                     Short_t & strip);
};

inline Int_t StGmtGeom::getNaiveMapping( Int_t idx )  //// WORKING
{
  return mNaiveMapping[ idx ];
}

inline Int_t StGmtGeom::getElectIdFromElecCoord
(
    Int_t rdo, Int_t arm, Int_t apv, Int_t ch
)
{
    return encodeElectronicId(rdo,arm,apv,ch); 
}

inline Int_t StGmtGeom::getElecCoordFromElectId
(
   Int_t eID,
   Int_t& rdo, Int_t& arm, Int_t& apv, Int_t& ch
)
{
  return decodeElectronicId(eID,rdo,arm,apv,ch);
}

//get the octant given the phi in radians
inline Int_t StGmtGeom::getOctant( Double_t phi )
{
  double phiDeg= 75 - ((phi*180)/mPi);
  while ( phiDeg < 0 ) phiDeg+=360;
  while ( phiDeg > 360 ) phiDeg-=360;
  int i8=phiDeg/45;
  return i8;
}

// inline std::string StGmtGeom::getNaiveGeoNameFromElecCoord
// (
//     Int_t rdo, Int_t arm, Int_t apv, Int_t channel
// )
// {
//     return
// 	translateGeoIdToGeoName(
// 	    getNaiveGeoIdFromElecCoord( rdo, arm, apv, channel )
// 	);
// }
// 
// inline Int_t StGmtGeom::getNaivePhysCoordFromElecCoord
// (
//     Int_t rdo, Int_t arm, Int_t apv, Int_t channel,
//     Short_t & module, Char_t & layer
// )
// {
//     return getPhysicalCoordinate(
// 	getNaiveGeoIdFromElecCoord( rdo, arm, apv, channel ),
// 	module, layer);
// }

#endif

