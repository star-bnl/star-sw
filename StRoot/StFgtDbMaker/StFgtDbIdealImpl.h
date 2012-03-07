/*  StFgtDbImpl.h
 *
 *  FGT database table observer for real database interactions.
 *
 *  \author W. Witzke (wowitz0@uky.edu)
 *
 */

#ifndef _ST_FGT_DB_IDEAL_IMPL_H_
#define _ST_FGT_DB_IDEAL_IMPL_H_

#include "StFgtDb.h"

class StFgtDbIdealImpl : public StFgtDb
{
    public:
	StFgtDbIdealImpl()
	{ }

	virtual Int_t getGeoIdFromElecCoord(
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	)
	{
	    return StFgtGeom::getNaiveGeoIdFromElecCoord(
		rdo, arm, apv, channel
	    );
	}

	virtual void getElecCoordFromGeoId(
            Int_t geoId, Int_t& rdo, Int_t& arm, Int_t& apv, Int_t& channel
	)
	{
	    StFgtGeom::getNaiveElecCoordFromGeoId(
		geoId, rdo, arm, apv, channel
	    );

	    return;
	}

	virtual std::string getGeoNameFromElecCoord(
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	)
	{
	    return StFgtGeom::getNaiveGeoNameFromElecCoord(
		rdo, arm, apv, channel
	    );
	}

	virtual void getElecCoordFromGeoName(
	    const std::string & geoName,
            Int_t& rdo, Int_t& arm, Int_t& apv, Int_t& channel
	)
	{
	    Int_t geoId =
		StFgtGeom::translateGeoNameToGeoId( geoName );

	    StFgtGeom::getNaiveElecCoordFromGeoId(
		geoId, rdo, arm, apv, channel
	    );

	    return;
	}

	virtual void getPhysCoordFromElecCoord(
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel,
	    Short_t & disc, Short_t & quadrant, Char_t & layer,
	    Double_t & ordinate, Double_t & lowerSpan, Double_t & upperSpan
	)
	{
	    StFgtGeom::getNaivePhysCoordFromElecCoord(
		rdo, arm, apv, channel,
		disc, quadrant, layer, ordinate, lowerSpan, upperSpan
	    );
	}

	//  The value of 20.0 here has no real basis in reality at all.
	virtual Double_t getPedestalFromGeoId( Int_t geoId )
	{
	    return 20.0;
	}

	virtual Double_t getPedestalFromGeoName(
	    const std::string & geoName
	)
	{
	    return 20.0;
	}

	virtual Double_t getPedestalFromElecCoord( 
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	)
	{
	    return 20.0;
	}

	virtual Double_t getPedestalFromElecId( Int_t elecId )
	{
	    return 20.0;
	}


	//  The value of 2.0 being returned here has no real basis in reality
	//  at all.
	virtual Double_t getPedestalSigmaFromGeoId( Int_t geoId )
	{
	    return 2.0;
	}

	virtual Double_t getPedestalSigmaFromGeoName(
	    const std::string & geoName
	)
	{
	    return 2.0;
	}

	virtual Double_t getPedestalSigmaFromElecCoord( 
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	)
	{
	    return 2.0;
	}

	virtual Double_t getPedestalSigmaFromElecId( Int_t elecId )
	{
	    return 2.0;
	}

	//  The pedestal here is always good, so always return 1.
	virtual UChar_t getPedestalStatusFromGeoId( Int_t geoId )
	{
	    return 1;
	}

	virtual UChar_t getPedestalStatusFromGeoName(
	    const std::string & geoName
	)
	{
	    return 1;
	}

	virtual UChar_t getPedestalStatusFromElecCoord( 
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	)
	{
	    return 1;
	}

	virtual UChar_t getPedestalStatusFromElecId( Int_t elecId )
	{
	    return 1;
	}

	//  The status is always good, so return 0.
	virtual UChar_t getStatusFromGeoId( Int_t geoId )
	{
	    return 0;
	}

	virtual UChar_t getStatusFromGeoName( const std::string & geoName )
	{
	    return 0;
	}

	virtual UChar_t getStatusFromElecCoord( 
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	)
	{
	    return 0;
	}

	virtual UChar_t getStatusFromElecId( Int_t elecId )
	{
	    return 0;
	}

	//  The value of 50.0 here has no basis in reality.
	virtual Double_t getGainFromGeoId( Int_t geoId )
	{
	    return 50.0;
	}

	virtual Double_t getGainFromGeoName( const std::string & geoName )
	{
	    return 50.0;
	}

	virtual Double_t getGainFromElecCoord( 
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	)
	{
	    return 50.0;
	}

	virtual Double_t getGainFromElecId( Int_t elecId )
	{
	    return 50.0;
	}

	//  Gains are always good here, so return 1.
	virtual UChar_t getGainStatusFromGeoId( Int_t geoId )
	{
	    return 1;
	}

	virtual UChar_t getGainStatusFromGeoName( const std::string & geoName )
	{
	    return 1;
	}

	virtual UChar_t getGainStatusFromElecCoord( 
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	)
	{
	    return 1;
	}
	virtual UChar_t getGainStatusFromElecId( Int_t elecId )
	{
	    return 1.0;
	}


	virtual Double_t getMapping(
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	)
	{
	    return StFgtGeom::getNaiveMapping( rdo, arm, apv, channel );
	}

	virtual bool isR(
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	)
	{
	    return StFgtGeom::isNaiveR( rdo, arm, apv, channel );
	}

	virtual Float_t eLossTab(int bin)
	{
	  //no naive implementationo
	  return 0;
	}

    //	ClassDef(StFgtDbNaiveImpl, 1)
};

#endif 

/*
 *  $ Id: $
 *  $ Log: $
 *
 */
