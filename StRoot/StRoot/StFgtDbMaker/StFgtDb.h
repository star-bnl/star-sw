/*  StFgtDb.h
 *
 *  FGT database table observer for real database interactions.
 *
 *  \author W. Witzke (wowitz0@uky.edu)
 *
 */

#ifndef _ST_FGT_DB_H_
#define _ST_FGT_DB_H_

#include <cstdlib>
#include "tables/St_fgtElosCutoff_Table.h"
#include "tables/St_fgtSimuParams_Table.h"
#include "tables/St_fgtPedestal_Table.h"
#include "tables/St_fgtMapping_Table.h"
#include "tables/St_fgtGain_Table.h"
#include "tables/St_fgtStatus_Table.h"
#include "tables/St_fgtAlignment_Table.h"
#include "StFgtUtil/geometry/StFgtGeom.h"

class StFgtDb 
{
    public:
	//  The following three functions should not normally be called
	//  directly by users. They are for use by the factory methods defined
	//  in the class being observed by this DB interface implementation.

          StFgtDb(
        ) : m_map(NULL), m_rmap(NULL), m_status(NULL), m_pedestal(NULL), m_gain(NULL), m_alignment(NULL)
	{ }
	    
	  StFgtDb(
	    fgtMapping_st * map, 
	    fgtMapping_st * rmap, 
	    fgtStatus_st * status,
	    fgtPedestal_st * pedestal,
	    fgtGain_st * gain,
	    fgtAlignment_st * alignment
	) : m_map(map), m_rmap(rmap), m_status(status),
	    m_pedestal(pedestal), m_gain(gain), m_alignment(alignment)
	{ }

	virtual void updateTables(
	    fgtMapping_st * map, 
	    fgtMapping_st * rmap, 
	    fgtStatus_st * status,
	    fgtPedestal_st * pedestal,
	    fgtGain_st * gain,
	    fgtElosCutoff_st* mLossTab,
	    fgtSimuParams_st* mSimuParTab,
	    fgtAlignment_st* alignment
	)
	{
	    m_map = map;
	    m_rmap = rmap;
	    m_status = status;
	    m_pedestal = pedestal;
	    m_gain = gain;
	    m_eLoss=mLossTab;
	    m_simPar= mSimuParTab;
	    m_alignment = alignment;
	}

	//----------------------------------------------------------
	//  User functions start here.


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

	virtual Double_t getSimuParam(Int_t bin)
	{
	  return m_simPar[0].param[bin];
	};
  
        virtual fgtAlignment_st* getAlignment() {return m_alignment;}

        // This gives XYZ from R and PHI obtained by StFgtGeom::getPhysicalCoordinate(STAR coordinate)
        // If option=0, no alighment parameter is considered and gives ideal position
        // If option=1 (default), then alignment parameter is taken from DB, and it will apply offsets and rotation around StFgtGeom::getQuadCenterXYZ()
        // If option=2, then alignment parameter is taken from last argument, and it will apply offsets and rotation around StFgtGeom::getQuadCenterXYZ()
        virtual void getStarXYZ(Short_t disc, Short_t quad, Double_t r, Double_t phi, TVector3 &xyz, Int_t opt=1, fgtAlignment_st* par=0);         

	//dump of FGT status/peds/pedSigma for each strip
	void printFgtDumpCSV1(TString fname, int myDate, int myTime);

	virtual ~StFgtDb() {}
  

    private:
	fgtMapping_st * m_map;
	fgtMapping_st * m_rmap; 
	fgtStatus_st * m_status;
	fgtPedestal_st * m_pedestal;
	fgtGain_st * m_gain;
	fgtElosCutoff_st* m_eLoss;
	fgtSimuParams_st* m_simPar;
        fgtAlignment_st* m_alignment;
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
