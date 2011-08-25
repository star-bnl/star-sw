/*  StFgtGeom.h
 *
 *  FGT geometry class declaration.
 *
 *  \author W. Witzke (wowitz0@uky.edu)
 *
 */

#ifndef _ST_FGT_GEOM_H_
#define _ST_FGT_GEOM_H_

#include <TObject.h>
#include <string>
#include <sstream>
#include <cstdlib>

#include "StFgtGeomDefs.h"

//  StFgtGeomData stores data on each ordinate associated with each global ID
//  used to index individual geometry elements.
struct StFgtGeomData
{
    Bool_t isPhi;
    Double_t ordinate;
    Double_t lowerSpan;
    Double_t upperSpan;
};

//  StFgtGeom is a "singleton" class. Only one of it needs to exist in any
//  program. However, because the data contained in this class is entirely
//  static, the class itself is also entirely static. No instances of this
//  class can be created.
class StFgtGeom
{
    public:
	/*  Not sure that these have a point anymore.
	static StFgtGeom& getInstance()
	{
	    static StFgtGeom singleton;
	    return singleton;
	}

	//  Shouldn't need to do much here.
	~StFgtGeom() {}
	*/

	//  For all functions where they appear: Disc can be >= 0 (in theory,
	//  although only values 0-5 work at the moment, I believe). Quadrant
	//  is 0-3.  Layer is 'P' or 'R'. Strip is 0-720
	static Short_t encodeGeoId(
	    Int_t disc, Int_t quadrant, Char_t layer, Int_t strip
	)
	{
	    return
	    (
		( disc*kNumFgtQuadrants + quadrant )
		* kNumFgtLayers + ( layer == 'P' )
	    ) * kNumFgtStripsPerLayer + strip;
	}

	static void decodeGeoId(
	    Int_t geoId,
	    Short_t & disc, Short_t & quadrant, Char_t & layer, Short_t & strip
	)
	{
	    strip = geoId % kNumFgtStripsPerLayer;
	    geoId /= kNumFgtStripsPerLayer;

	    layer = ( geoId % kNumFgtLayers ) ? 'P' : 'R';
	    geoId /= kNumFgtLayers;

	    quadrant = geoId % kNumFgtQuadrants;
	    disc = geoId / kNumFgtQuadrants;
	}

	static std::string encodeGeoName(
	    Int_t disc, Int_t quadrant, Char_t layer, Int_t strip
	)
	{
            std::stringstream buff;
            buff << disc << quadrant << layer;
	    strip += 1;
            if ( strip < 10 )
                buff << "00";
            else if ( strip < 100 )
                buff << "0";

            buff << strip;
            return buff.str();
	}

	static void decodeGeoName(
	    const std::string & geoName,
	    Short_t & disc, Short_t & quadrant, Char_t & layer, Short_t & strip
	)
	{
	    disc = geoName[0] - '0';
	    quadrant = geoName[1] - '0';
	    layer = geoName[2];
	    strip = std::atoi( (geoName.substr(3)).c_str() ) - 1;
	}

	static std::string translateGeoIdToGeoName( Int_t geoId )
	{
	    Short_t disc, quadrant, strip;
	    Char_t layer;

	    decodeGeoId( geoId, disc, quadrant, layer, strip );
	    return encodeGeoName( disc, quadrant, layer, strip );
	}

	static Short_t translateGeoNameToGeoId( const std::string & geoName )
	{
	    Short_t disc, quadrant, strip;
	    Char_t layer;

	    decodeGeoName( geoName, disc, quadrant, layer, strip );
	    return encodeGeoId( disc, quadrant, layer, strip );
	}

	//  The ordinate, lowerSpan and upperSpan are all in centimeters or
	//  radians, depending on the layer.
	static void getPhysicalCoordinate(
	    Int_t geoId,
	    Short_t & disc, Short_t & quadrant, Char_t & layer,
	    Double_t & ordinate, Double_t & lowerSpan, Double_t & upperSpan
	)
	{
	    Short_t strip;

	    decodeGeoId( geoId, disc, quadrant, layer, strip );
	    ordinate =
		mStrips[
		    (layer == 'P') * kNumFgtStripsPerLayer + strip
		].ordinate;
	    lowerSpan =
		mStrips[
		    (layer == 'P') * kNumFgtStripsPerLayer + strip
		].lowerSpan;
	    upperSpan =
		mStrips[
		    (layer == 'P') * kNumFgtStripsPerLayer + strip
		].upperSpan;
	}

	//  The ordinate, lowerSpan and upperSpan are all in centimeters or
	//  radians, depending on the layer.
	static void getPhysicalCoordinate(
	    const std::string & geoName,
	    Short_t & disc, Short_t & quadrant, Char_t & layer,
	    Double_t ordinate, Double_t lowerSpan, Double_t upperSpan
	)
	{
	    Short_t strip;

	    decodeGeoName( geoName, disc, quadrant, layer, strip );
	    ordinate =
		mStrips[
		    (layer == 'P') * kNumFgtStripsPerLayer + strip
		].ordinate;
	    lowerSpan =
		mStrips[
		    (layer == 'P') * kNumFgtStripsPerLayer + strip
		].lowerSpan;
	    upperSpan =
		mStrips[
		    (layer == 'P') * kNumFgtStripsPerLayer + strip
		].upperSpan;
	}

	//  Please note that the following functions do NOT access the STAR
	//  database to find mapping information. They assume the most
	//  straight-forward mapping scheme and use that.
	//  For those functions that have them, currently rdo can be 1-2, arm
	//  can be 0-5, apv can be 0-23 (although 10, 11, 22, and 23 are not
	//  technically valid) and channel is 0-127.
	static Short_t getNaiveGeoIdFromElecCoord(
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	)
	{
	    Short_t disc = int(arm/2) + (rdo-1)*3;
	    Short_t quadrant = (arm & 1)*2 + int( apv/12 );

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

	static std::string getNaiveGeoNameFromElecCoord(
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	)
	{
	    return
		translateGeoIdToGeoName(
		    getNaiveGeoIdFromElecCoord( rdo, arm, apv, channel )
		);
	}

	static void getNaivePhysCoordFromElecCoord(
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel,
	    Short_t & disc, Short_t & quadrant, Char_t & layer,
	    Double_t ordinate, Double_t lowerSpan, Double_t upperSpan
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
	static Double_t getNaiveMapping(
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

	static bool isNaiveR(
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	)
	{
	    if ( apv >= 12 )
		return
		    mNaiveMapping[ (apv-12)*128+channel ] >= 720;
	    else
		return
		    mNaiveMapping[ apv*128+channel ] >= 720;
	}

	static const Int_t kNumStrips = 1440;
	static const Int_t kNumChannels = 1280;

    private:
	/*  Not sure that these have a point anymore.
	StFgtGeom() {};

	//  Copy constructors and assignment operators are *not* implemented.
	StFgtGeom( const StFgtGeom& );
	StFgtGeom& operator=( const StFgtGeom& );
	*/

	//  ---Private member variables---
	static StFgtGeomData mStrips[ kNumStrips ];

	static Int_t mNaiveMapping[ kNumChannels ];

    //ClassDef(StFgtGeom, 1)
};

#endif

/*
 *  $ Id: $
 *  $ Log: $
 *
 */
