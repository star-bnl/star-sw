/*  StFgtDb.h
 *
 *  FGT database table observer abstract base class.
 *
 *  \author W. Witzke (wowitz0@uky.edu)
 *
 */

#ifndef _ST_FGT_DB_H_
#define _ST_FGT_DB_H_

#include "StFgtUtil/geometry/StFgtGeom.h"

//  This is an abstract base class for all the table collection observers of
//  the StFgtDbMaker.  It does implement some functionality that should be
//  common to all implementations.
class StFgtDb
{
    public:


	//  The ordinate, lowerSpan and upperSpan are all in centimeters or
	//  radians, depending on the layer.
	virtual Int_t getPhysicalCoordinateFromGeoId(
	    Int_t geoId,
	    Short_t & disc, Short_t & quadrant, Char_t & layer,
	    Double_t & ordinate, Double_t & lowerSpan, Double_t & upperSpan
	    );

	//  The ordinate, lowerSpan and upperSpan are all in centimeters or
	//  radians, depending on the layer.
	virtual Int_t getPhysicalCoordinateFromGeoName(
	    const std::string & geoName,
	    Short_t & disc, Short_t & quadrant, Char_t & layer,
	    Double_t & ordinate, Double_t & lowerSpan, Double_t & upperSpan
	    );


	//geoId is a unique number used to identify a specific strip 
        //on a specific disk/quadrant/layer/strip.  Please NOTE:
        //The set of geoIds IS NOT CONTINUOUS simply becuase strip 
        //number is not continuous. On the R plane strips 280-399 
        //are not implemented.

	virtual Int_t getGeoIdFromElecCoord(
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	) = 0;

	virtual Int_t getElecCoordFromGeoId(
            Int_t geoId, Int_t& rdo, Int_t& arm, Int_t& apv, Int_t& channel
	) = 0;

	virtual std::string getGeoNameFromElecCoord(
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	) = 0;

	//Geoname is human readable form of geoId
	virtual Int_t getElecCoordFromGeoName(
	    const std::string & geoName,
            Int_t& rdo, Int_t& arm, Int_t& apv, Int_t& channel
	) = 0;

	virtual Int_t getPhysCoordFromElecCoord(
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel,
	    Short_t & disc, Short_t & quadrant, Char_t & layer,
	    Double_t & ordinate, Double_t & lowerSpan, Double_t & upperSpan
	) = 0;

	virtual Double_t getPedestalFromGeoId( Int_t geoId ) = 0;

	virtual Double_t getPedestalFromGeoName(
	    const std::string & geoName
	) = 0;

	virtual Double_t getPedestalFromElecCoord( 
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	) = 0;

	//  Electronic Id is determined from the electronic devices rdo/arm/apv/channel
        //  and does form a continuous set of integers. 
	virtual Double_t getPedestalFromElecId(
	    Int_t electId
	) = 0;

	virtual Double_t getPedestalSigmaFromGeoId( Int_t geoId ) = 0;

	virtual Double_t getPedestalSigmaFromGeoName(
	    const std::string & geoName
	) = 0;

	virtual Double_t getPedestalSigmaFromElecCoord( 
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	) = 0;

	virtual Double_t getPedestalSigmaFromElecId(
	    Int_t electId
	) = 0;

	//Pedestal status is not currently used or filled.  If you want to know the 
	//status of a strip use getStatus* functions NOT getPedestalStatus* functions
	virtual Char_t getPedestalStatusFromGeoId( Int_t geoId ) = 0;

	virtual Char_t getPedestalStatusFromGeoName(
	    const std::string & geoName
	) = 0;

	virtual Char_t getPedestalStatusFromElecCoord( 
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	) = 0;

	virtual Char_t getPedestalStatusFromElecId(
	    Int_t electId
	) = 0;

	//These are the functions that tell you the status of strips
	virtual Char_t getStatusFromGeoId( Int_t geoId ) = 0;

	virtual Char_t getStatusFromGeoName( const std::string & geoName ) = 0;

	virtual Char_t getStatusFromElecCoord( 
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	) = 0;

	virtual Char_t getStatusFromElecId(
	    Int_t electId
	) = 0;

	virtual Double_t getGainFromGeoId( Int_t geoId ) = 0;

	virtual Double_t getGainFromGeoName( const std::string & geoName ) = 0;

	virtual Double_t getGainFromElecCoord( 
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	) = 0;

	virtual Double_t getGainFromElecId(
	    Int_t electId
	) = 0;

	virtual Char_t getGainStatusFromGeoId( Int_t geoId ) = 0;

	virtual Char_t getGainStatusFromGeoName( const std::string & geoName ) = 0;

	virtual Char_t getGainStatusFromElecCoord( 
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	) = 0;

	virtual Char_t getGainStatusFromElecId(
	    Int_t electId
	) = 0;

	virtual Double_t getMapping(
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	) = 0;

	virtual bool isR(
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	) = 0;
	virtual Double_t getEloss(Int_t bin)=0;

	//dump of FGT status/peds/pedSigma for each strip
	void printFgtDumpCSV1(TString fname, int myDate, int myTime);

	virtual ~StFgtDb(){}

};


	//  The ordinate, lowerSpan and upperSpan are all in centimeters or
	//  radians, depending on the layer.
inline 	Int_t StFgtDb::getPhysicalCoordinateFromGeoId(
	    Int_t geoId,
	    Short_t & disc, Short_t & quadrant, Char_t & layer,
	    Double_t & ordinate, Double_t & lowerSpan, Double_t & upperSpan
	)
	{
	    return StFgtGeom::getPhysicalCoordinate(
		geoId, disc, quadrant, layer, ordinate, lowerSpan, upperSpan
	    );
	}

	//  The ordinate, lowerSpan and upperSpan are all in centimeters or
	//  radians, depending on the layer.
inline	Int_t StFgtDb::getPhysicalCoordinateFromGeoName(
	    const std::string & geoName,
	    Short_t & disc, Short_t & quadrant, Char_t & layer,
	    Double_t & ordinate, Double_t & lowerSpan, Double_t & upperSpan
	)
	{
	    return StFgtGeom::getPhysicalCoordinate(
		geoName, disc, quadrant, layer, ordinate, lowerSpan, upperSpan
	    );
	}




#endif

/*
 *  $ Id: $
 *  $ Log: $
 *
 */
