/*  StFgtDbImpl.h
 *
 *  FGT database table observer for real database interactions.
 *
 *  \author W. Witzke (wowitz0@uky.edu)
 *
 */

#ifndef _ST_FGT_DB_IMPL_H_
#define _ST_FGT_DB_IMPL_H_

#include <cstdlib>
#include "StFgtDb.h"
#include "tables/St_fgtElosCutoff_Table.h"
#include "tables/St_fgtPedestal_Table.h"
#include "tables/St_fgtMapping_Table.h"
#include "tables/St_fgtGain_Table.h"
#include "tables/St_fgtStatus_Table.h"
#include "StFgtUtil/geometry/StFgtGeom.h"

//#include "fgtGain.h"
//#include "fgtMapping.h"
//#include "fgtPedestal.h"
//#include "fgtStatus.h"

class StFgtDbImpl : public StFgtDb
{
    public:
	//  The following three functions should not normally be called
	//  directly by users. They are for use by the factory methods defined
	//  in the class being observed by this DB interface implementation.

          StFgtDbImpl(
        ) : m_map(NULL), m_rmap(NULL), m_status(NULL), m_pedestal(NULL), m_gain(NULL)
	{ }
	    
	  StFgtDbImpl(
	    fgtMapping_st * map, 
	    fgtMapping_st * rmap, 
	    fgtStatus_st * status,
	    fgtPedestal_st * pedestal,
	    fgtGain_st * gain
	) : m_map(map), m_rmap(rmap), m_status(status),
	    m_pedestal(pedestal), m_gain(gain)
	{ }

	virtual void updateTables(
	    fgtMapping_st * map, 
	    fgtMapping_st * rmap, 
	    fgtStatus_st * status,
	    fgtPedestal_st * pedestal,
	    fgtGain_st * gain,
	    fgtElosCutoff_st* mLossTab
	)
	{
	    m_map = map;
	    m_rmap = rmap;
	    m_status = status;
	    m_pedestal = pedestal;
	    m_gain = gain;
	    m_eLoss=mLossTab;
	}

	//----------------------------------------------------------
	//  User functions start here.

	virtual Int_t getGeoIdFromElecCoord(
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	)
	{
	    Int_t eId = StFgtGeom::encodeElectronicId( rdo, arm, apv, channel );

	    if ( eId < 0 )
		return kFgtError;

	    return m_map->Mapping[ eId ];
	}


	virtual Int_t getElecCoordFromGeoId(
            Int_t geoId, Int_t& rdo, Int_t& arm, Int_t& apv, Int_t& channel
	);

	virtual std::string getGeoNameFromElecCoord(
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	)
	{
	    Int_t geoId = getGeoIdFromElecCoord( rdo, arm, apv, channel );

	    if ( geoId < 0 )
		return kFgtErrorString;

	    return StFgtGeom::translateGeoIdToGeoName( geoId );
	}


	virtual Int_t getElecCoordFromGeoName(
	    const std::string & geoName,
            Int_t& rdo, Int_t& arm, Int_t& apv, Int_t& channel
	);

	virtual Int_t getPhysCoordFromElecCoord(
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel,
	    Short_t & disc, Short_t & quadrant, Char_t & layer,
	    Double_t & ordinate, Double_t & lowerSpan, Double_t & upperSpan
	)
	{
	    return StFgtGeom::getPhysicalCoordinate(
		getGeoIdFromElecCoord( rdo, arm, apv, channel ),
		disc, quadrant, layer, ordinate, lowerSpan, upperSpan
	    );
	}

	virtual Double_t getPedestalFromGeoId( Int_t geoId );

	virtual Double_t getPedestalFromElecId( Int_t elecId);

	virtual Double_t getPedestalFromGeoName( const std::string & geoName )
	{   
	    Int_t geoId = StFgtGeom::translateGeoNameToGeoId( geoName );

	    if ( geoId < 0 )
		return kFgtError;

	    return getPedestalFromGeoId( geoId );
	}   

	virtual Double_t getPedestalFromElecCoord( 
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	)
	{   
	    Int_t eId =
		StFgtGeom::encodeElectronicId( rdo, arm, apv, channel );

	    if ( eId < 0 )
		return kFgtError;

	    return m_pedestal->AdcPedestal[ eId ];
	}

	virtual Double_t getPedestalSigmaFromGeoId( Int_t geoId );

	virtual Double_t getPedestalSigmaFromElecId( Int_t elecId );

	virtual Double_t getPedestalSigmaFromGeoName(
	    const std::string & geoName
	)
	{
	    Int_t geoId = StFgtGeom::translateGeoNameToGeoId( geoName );

	    if ( geoId < 0 )
		return kFgtError;

	    return getPedestalSigmaFromGeoId( geoId );
	}

	virtual Double_t getPedestalSigmaFromElecCoord( 
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	)
	{
	    Int_t eId =
		StFgtGeom::encodeElectronicId( rdo, arm, apv, channel );

	    if ( eId < 0 )
		return kFgtError;

	    return m_pedestal->AdcPedestalRMS[ eId ];
	}

	virtual Char_t getPedestalStatusFromGeoId( Int_t geoId );

	virtual Char_t getPedestalStatusFromElecId( Int_t elecId );


	virtual Char_t getPedestalStatusFromGeoName(
	    const std::string & geoName
	)
	{
	    Int_t geoId = StFgtGeom::translateGeoNameToGeoId( geoName );

	    if ( geoId < 0 )
		return kFgtErrorChar;

	    return getPedestalStatusFromGeoId( geoId );
	}

	virtual Char_t getPedestalStatusFromElecCoord( 
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	)
	{
	    Int_t eId =
		StFgtGeom::encodeElectronicId( rdo, arm, apv, channel );

	    if ( eId < 0 )
		return kFgtErrorChar;

	    return m_pedestal->Status[ eId ];
	}


	virtual Char_t getStatusFromGeoId( Int_t geoId );

	virtual Char_t getStatusFromElecId( Int_t elecId );

	virtual Char_t getStatusFromGeoName( const std::string & geoName )
	{
	    Int_t geoId = StFgtGeom::translateGeoNameToGeoId( geoName );

	    if ( geoId < 0 )
		return kFgtErrorChar;

	    return getStatusFromGeoId( geoId );
	}

	virtual Char_t getStatusFromElecCoord( 
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	)
	{
	    Int_t eId =
		StFgtGeom::encodeElectronicId( rdo, arm, apv, channel );

	    if ( eId < 0 )
		return kFgtErrorChar;

	    return m_status->Status[ eId ];
	}

	virtual Double_t getGainFromGeoId( Int_t geoId );

	virtual Double_t getGainFromElecId( Int_t elecId );

	virtual Double_t getGainFromGeoName( const std::string & geoName )
	{
	    Int_t geoId = StFgtGeom::translateGeoNameToGeoId( geoName );

	    if ( geoId < 0 )
		return kFgtError;

	    return getGainFromGeoId( geoId );
	}

	virtual Double_t getGainFromElecCoord( 
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	)
	{
	    Int_t eId =
		StFgtGeom::encodeElectronicId( rdo, arm, apv, channel );

	    if ( eId < 0 )
		return kFgtError;

	    return m_gain->Gain[ eId ];
	}

	virtual Char_t getGainStatusFromGeoId( Int_t geoId );

	virtual Char_t getGainStatusFromElecId( Int_t elecId );

	virtual Char_t getGainStatusFromGeoName( const std::string & geoName )
	{
	    Int_t geoId = StFgtGeom::translateGeoNameToGeoId( geoName );

	    if ( geoId < 0 )
		return kFgtErrorChar;

	    return getGainStatusFromGeoId( geoId );
	}

	virtual Char_t getGainStatusFromElecCoord( 
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	)
	{
	    Int_t eId =
		StFgtGeom::encodeElectronicId( rdo, arm, apv, channel );

	    if ( eId < 0 )
		return kFgtErrorChar;

	    return m_gain->Status[ eId ];
	}

	virtual Double_t getMapping(
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	);

	virtual bool isR(
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	);


	virtual Double_t getEloss(Int_t bin)
	{
	  return m_eLoss[0].cutoff[bin];
	};



	virtual ~StFgtDbImpl() {}


    private:
	fgtMapping_st * m_map;
	fgtMapping_st * m_rmap; 
	fgtStatus_st * m_status;
	fgtPedestal_st * m_pedestal;
	fgtGain_st * m_gain;
	fgtElosCutoff_st* m_eLoss;

    //	ClassDef(StFgtDbImpl, 1)
};

#endif 

/*
 *  $ Id: $
 *  $ Log: $
 *
 */
