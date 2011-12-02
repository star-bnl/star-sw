/*  StFgtDbImpl.h
 *
 *  FGT database table observer for real database interactions.
 *
 *  \author W. Witzke (wowitz0@uky.edu)
 *
 */

#ifndef _ST_FGT_DB_IMPL_H_
#define _ST_FGT_DB_IMPL_H_

#include "StFgtDb.h"
#include "tables/St_fgtElosCutoff_Table.h"
#include "tables/St_fgtPedestal_Table.h"
#include "tables/St_fgtMapping_Table.h"
#include "tables/St_fgtGain_Table.h"
#include "tables/St_fgtStatus_Table.h"

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

	StFgtDbImpl()
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
	    return m_map->Mapping[ 
		encodeElectronicId( rdo, arm, apv, channel )
	    ];
	}

	virtual void getElecCoordFromGeoId(
            Int_t geoId, Int_t& rdo, Int_t& arm, Int_t& apv, Int_t& channel
	)
	{
	    Int_t elecId = m_rmap->Mapping[ geoId ];

	    decodeElectronicId( elecId, rdo, arm, apv, channel );

	    return;
	}

	virtual std::string getGeoNameFromElecCoord(
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	)
	{
	    Int_t geoId = getGeoIdFromElecCoord( rdo, arm, apv, channel );

	    return StFgtGeom::translateGeoIdToGeoName( geoId );
	}

	virtual void getElecCoordFromGeoName(
	    const std::string & geoName,
            Int_t& rdo, Int_t& arm, Int_t& apv, Int_t& channel
	)
	{
	    Int_t geoId =
		StFgtGeom::translateGeoNameToGeoId( geoName );

	    getElecCoordFromGeoId( geoId, rdo, arm, apv, channel );

	    return;
	}

	virtual void getPhysCoordFromElecCoord(
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel,
	    Short_t & disc, Short_t & quadrant, Char_t & layer,
	    Double_t & ordinate, Double_t & lowerSpan, Double_t & upperSpan
	)
	{
	    StFgtGeom::getPhysicalCoordinate(
		getGeoIdFromElecCoord( rdo, arm, apv, channel ),
		disc, quadrant, layer, ordinate, lowerSpan, upperSpan
	    );
	}

	virtual Double_t getPedestalFromGeoId( Int_t geoId )
	{
	    Int_t elecId = m_rmap->Mapping[ geoId ];
	    return m_pedestal->AdcPedestal[ elecId ];
	}

	virtual Double_t getPedestalFromGeoName(
	    const std::string & geoName
	)
	{
	    Int_t geoId =
		StFgtGeom::translateGeoNameToGeoId( geoName );

	    return getPedestalFromGeoId( geoId );
	}

	virtual Double_t getPedestalFromElecCoord( 
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	)
	{
	    return m_pedestal->AdcPedestal[
		encodeElectronicId( rdo, arm, apv, channel )
	    ];
	}

	virtual Double_t getPedestalSigmaFromGeoId( Int_t geoId )
	{
	    Int_t elecId = m_rmap->Mapping[ geoId ];
	    return m_pedestal->AdcPedestalRMS[ elecId ];
	}

	virtual Double_t getPedestalSigmaFromGeoName(
	    const std::string & geoName
	)
	{
	    Int_t geoId =
		StFgtGeom::translateGeoNameToGeoId( geoName );

	    return getPedestalSigmaFromGeoId( geoId );
	}

	virtual Double_t getPedestalSigmaFromElecCoord( 
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	)
	{
	    return m_pedestal->AdcPedestalRMS[
		encodeElectronicId( rdo, arm, apv, channel )
	    ];
	}

	virtual UChar_t getPedestalStatusFromGeoId( Int_t geoId )
	{
	    Int_t elecId = m_rmap->Mapping[ geoId ];
	    return m_pedestal->Status[ elecId ];
	}

	virtual UChar_t getPedestalStatusFromGeoName(
	    const std::string & geoName
	)
	{
	    Int_t geoId =
		StFgtGeom::translateGeoNameToGeoId( geoName );

	    return getPedestalStatusFromGeoId( geoId );
	}

	virtual UChar_t getPedestalStatusFromElecCoord( 
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	)
	{
	    return m_pedestal->Status[
		encodeElectronicId( rdo, arm, apv, channel )
	    ];
	}

	virtual UChar_t getStatusFromGeoId( Int_t geoId )
	{
	    Int_t elecId = m_rmap->Mapping[ geoId ];
	    return m_status->Status[ elecId ];
	}

	virtual UChar_t getStatusFromGeoName( const std::string & geoName )
	{
	    Int_t geoId =
		StFgtGeom::translateGeoNameToGeoId( geoName );

	    return getStatusFromGeoId( geoId );
	}

	virtual UChar_t getStatusFromElecCoord( 
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	)
	{
	    return m_status->Status[
		encodeElectronicId( rdo, arm, apv, channel )
	    ];
	}

	virtual Double_t getGainFromGeoId( Int_t geoId )
	{
	    Int_t elecId = m_rmap->Mapping[ geoId ];
	    return m_gain->Gain[ elecId ];
	}

	virtual Double_t getGainFromGeoName( const std::string & geoName )
	{
	    Int_t geoId =
		StFgtGeom::translateGeoNameToGeoId( geoName );

	    return getGainFromGeoId( geoId );
	}

	virtual Double_t getGainFromElecCoord( 
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	)
	{
	    return m_gain->Gain[
		encodeElectronicId( rdo, arm, apv, channel )
	    ];
	}

	virtual UChar_t getGainStatusFromGeoId( Int_t geoId )
	{
	    Int_t elecId = m_rmap->Mapping[ geoId ];
	    return m_gain->Status[ elecId ];
	}

	virtual UChar_t getGainStatusFromGeoName( const std::string & geoName )
	{
	    Int_t geoId =
		StFgtGeom::translateGeoNameToGeoId( geoName );

	    return getGainStatusFromGeoId( geoId );
	}

	virtual UChar_t getGainStatusFromElecCoord( 
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	)
	{
	    return m_gain->Status[
		encodeElectronicId( rdo, arm, apv, channel )
	    ];
	}

	virtual Double_t getMapping(
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	);

	virtual bool isR(
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	);



    private:
	fgtMapping_st * m_map;
	fgtMapping_st * m_rmap; 
	fgtStatus_st * m_status;
	fgtPedestal_st * m_pedestal;
	fgtGain_st * m_gain;
	fgtElosCutoff_st* m_eLoss;

	virtual Int_t encodeElectronicId(
	    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
	)
	{
	    if ( apv >= 12 )
	    {
		apv -= 2;
	    }
	    return channel+128*(apv+20*(arm+6*(rdo-1)));
	}

	virtual void decodeElectronicId(
	    Int_t elecId, Int_t &rdo, Int_t &arm, Int_t &apv, Int_t &channel
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
	virtual Float_t eLossTab(int bin)
	{
	  assert(bin>=0);
	  assert(bin<10000);
	  return m_eLoss[0].cutoff[bin];

	};
    //	ClassDef(StFgtDbImpl, 1)
};

#endif 

/*
 *  $ Id: $
 *  $ Log: $
 *
 */
