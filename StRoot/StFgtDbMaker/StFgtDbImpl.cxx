#include "StMessMgr.h"
#include "StFgtDbImpl.h"

//  This is not a very efficient implementation of this. It feels like this
//  could be more efficient.

Int_t StFgtDbImpl::getElecCoordFromGeoId(
    Int_t geoId, Int_t& rdo, Int_t& arm, Int_t& apv, Int_t& channel
)
{
    if ( geoId < 0 || geoId >= kFgtNumGeoIds )
    {
        LOG_DEBUG << "GeoId " << geoId << " out of range in StFgtDbImpl::getElecCoordFromGeoId." << endm;
        rdo = kFgtError;
        arm = kFgtError;
        apv = kFgtError;
        channel = kFgtError;

        return kFgtError;
    }

    Int_t elecId = m_rmap->Mapping[ geoId ];

    StFgtGeom::decodeElectronicId( elecId, rdo, arm, apv, channel );

    return 0;
}

Int_t StFgtDbImpl::getElecCoordFromGeoName(
    const std::string & geoName,
    Int_t& rdo, Int_t& arm, Int_t& apv, Int_t& channel
)
{
    Int_t geoId =
	StFgtGeom::translateGeoNameToGeoId( geoName );

    if ( geoId < 0 )
    {
	rdo = kFgtError;
	arm = kFgtError;
	apv = kFgtError;
	channel = kFgtError;

	return kFgtError;
    }

    getElecCoordFromGeoId( geoId, rdo, arm, apv, channel );

    return 0;
}

Double_t StFgtDbImpl::getPedestalFromGeoId( Int_t geoId )
{
    if ( geoId < 0 || geoId >= kFgtNumGeoIds )
    {
        LOG_DEBUG << "GeoId " << geoId << " out of range in StFgtDbImpl::getPedestalFromGeoId." << endm;
        return kFgtError;
    }

    Int_t elecId = m_rmap->Mapping[ geoId ];
    return m_pedestal->AdcPedestal[ elecId ];
}

Double_t StFgtDbImpl::getPedestalFromElecId( Int_t elecId)
{
    if ( elecId < 0 || elecId >= kFgtNumElecIds )
    {
        LOG_DEBUG << "Electronic ID " << elecId << " out of range in StFgtDbImpl::getPedestalFromElecId." << endm;
        return kFgtError;
    }

    return m_pedestal->AdcPedestal[ elecId ];
}

Double_t StFgtDbImpl::getPedestalSigmaFromGeoId( Int_t geoId )
{
    if ( geoId < 0 || geoId >= kFgtNumGeoIds )
    {
        LOG_DEBUG << "GeoId " << geoId << " out of range in StFgtDbImpl::getPedestalSigmaFromGeoId." << endm;
        return kFgtError;
    }

    Int_t elecId = m_rmap->Mapping[ geoId ];
    return m_pedestal->AdcPedestalRMS[ elecId ];
}

Double_t StFgtDbImpl::getPedestalSigmaFromElecId( Int_t elecId )
{
    if ( elecId < 0 || elecId >= kFgtNumElecIds )
    {
        LOG_DEBUG << "Electronic ID " << elecId << " out of range in StFgtDbImpl::getPedestalSigmaFromElecId." << endm;
        return kFgtError;
    }

    return m_pedestal->AdcPedestalRMS[ elecId ];
}

Char_t StFgtDbImpl::getPedestalStatusFromGeoId( Int_t geoId )
{
    if ( geoId < 0 || geoId >= kFgtNumGeoIds )
    {
        LOG_DEBUG << "GeoId " << geoId << " out of range in StFgtDbImpl::getPedestalStatusFromGeoId." << endm;
        return kFgtErrorChar;
    }

    Int_t elecId = m_rmap->Mapping[ geoId ];
    return m_pedestal->Status[ elecId ];
}

Char_t StFgtDbImpl::getPedestalStatusFromElecId( Int_t elecId )
{
    if ( elecId < 0 || elecId >= kFgtNumElecIds )
    {
        LOG_DEBUG << "Electronic ID " << elecId << " out of range in StFgtDbImpl::getPedestalStatusFromElecId." << endm;
        return kFgtErrorChar;
    }

    return m_pedestal->Status[ elecId ];
}

Char_t StFgtDbImpl::getStatusFromGeoId( Int_t geoId )
{
    if ( geoId < 0 || geoId >= kFgtNumGeoIds )
    {
        LOG_DEBUG << "GeoId " << geoId << " out of range in StFgtDbImpl::getStatusFromGeoId." << endm;
        return kFgtErrorChar;
    }

    Int_t elecId = m_rmap->Mapping[ geoId ];
    return m_status->Status[ elecId ];
}

Char_t StFgtDbImpl::getStatusFromElecId( Int_t elecId )
{
    if ( elecId < 0 || elecId >= kFgtNumElecIds )
    {
        LOG_DEBUG << "Electronic ID " << elecId << " out of range in StFgtDbImpl::getStatusFromElecId." << endm;
        return kFgtErrorChar;
    }

    return m_status->Status[ elecId ];
}

Double_t StFgtDbImpl::getGainFromGeoId( Int_t geoId )
{
    if ( geoId < 0 || geoId >= kFgtNumGeoIds )
    {
        LOG_DEBUG << "GeoId " << geoId << " out of range in StFgtDbImpl::getGainFromGeoId." << endm;
        return kFgtError;
    }

    Int_t elecId = m_rmap->Mapping[ geoId ];
    return m_gain->Gain[ elecId ];
}

Double_t StFgtDbImpl::getGainFromElecId( Int_t elecId )
{
    if ( elecId < 0 || elecId >= kFgtNumElecIds )
    {
        LOG_DEBUG << "Electronic ID " << elecId << " out of range in StFgtDbImpl::getGainFromElecId." << endm;
        return kFgtError;
    }

    return m_gain->Gain[ elecId ];
}

Char_t StFgtDbImpl::getGainStatusFromGeoId( Int_t geoId )
{
    if ( geoId < 0 || geoId >= kFgtNumGeoIds )
    {
        LOG_DEBUG << "GeoId " << geoId << " out of range in StFgtDbImpl::getGainStatusFromGeoId." << endm;
        return kFgtErrorChar;
    }

    Int_t elecId = m_rmap->Mapping[ geoId ];
    return m_gain->Status[ elecId ];
}

Char_t StFgtDbImpl::getGainStatusFromElecId( Int_t elecId )
{
    if ( elecId < 0 || elecId >= kFgtNumElecIds )
    {
        LOG_DEBUG << "Electronic ID " << elecId << " out of range in StFgtDbImpl::getGainStatusFromElecId." << endm;
        return kFgtErrorChar;
    }

    return m_gain->Status[ elecId ];
}

Double_t StFgtDbImpl::getMapping(
    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
)
{
	Short_t disc, quadrant;
	Char_t layer;
	Double_t ordinate, lowerSpan, upperSpan;

	if ( getPhysCoordFromElecCoord(
		rdo, arm, apv, channel,
		disc, quadrant, layer,
		ordinate, lowerSpan, upperSpan
	     ) < 0
	)
	    return kFgtError;
     
	return ordinate;
}

//  This, similarly, seems needlessly complicated.
bool StFgtDbImpl::isR(
    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
)
{
	Short_t disc, quadrant;
	Char_t layer;
	Double_t ordinate, lowerSpan, upperSpan;

	//  Can't do any boundry checking return value thing here. Going to
	//  have to let the call here handle the warning message.
	getPhysCoordFromElecCoord(
	    rdo, arm, apv, channel,
	    disc, quadrant, layer,
	    ordinate, lowerSpan, upperSpan
	);
     
	return (layer == 'R');
}




