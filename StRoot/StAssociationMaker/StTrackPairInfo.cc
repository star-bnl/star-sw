/***************************************************************************
 *
 * $Id: StTrackPairInfo.cc,v 1.5 1999/12/14 07:07:41 calderon Exp $
 * $Log: StTrackPairInfo.cc,v $
 * Revision 1.5  1999/12/14 07:07:41  calderon
 * Added Ratio Number of Common Hits / Number of Reconstructed Hits for
 * each detector.
 * Numbering scheme from StEvent & StMcEvent as per SVT request
 * Added Kink, V0 and Xi vertex associations.
 *
 * Revision 1.4  1999/12/08 00:00:25  calderon
 * New version of StAssociationMaker.
 * -Uses new StEvent / StMcEvent
 * -Includes maps using reconstructed and monte carlo objects as keys for:
 *   TPC Hits
 *   SVT Hits
 *   FTPC Hits
 *   Tracks (using all 3 hit multimaps)
 *
 * Revision 1.3  1999/09/23 21:25:24  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/
#include "StTrackPairInfo.hh"
#include "StMcTrack.hh"
#include "StGlobalTrack.h"
#include "StTrackDetectorInfo.h"

static const char rcsid[] = "$Id: StTrackPairInfo.cc,v 1.5 1999/12/14 07:07:41 calderon Exp $";

StTrackPairInfo::StTrackPairInfo(StGlobalTrack* rcTrk,
				 StMcTrack*     mcTrk,
				 unsigned int tpcPings,
				 unsigned int svtPings,
				 unsigned int ftpcPings)
    :
    mPartnerTrack(rcTrk),
    mPartnerMcTrack(mcTrk),
    mCommonTpcHits(tpcPings),
    mCommonSvtHits(svtPings),
    mCommonFtpcHits(ftpcPings)
{
    // Percent of Svt Hits
    unsigned short  numPoints = rcTrk->detectorInfo()->numberOfPoints(kSvtId);
    mRatioCommonToTotalHitsSvt  =
	(numPoints) ? static_cast<float>(mCommonSvtHits)/static_cast<float>(numPoints) : 0;

    // Percent of Tpc Hits
    numPoints = rcTrk->detectorInfo()->numberOfPoints(kTpcId);
    mRatioCommonToTotalHitsTpc  =
	(numPoints) ? static_cast<float>(mCommonTpcHits)/static_cast<float>(numPoints) : 0;

    if (!numPoints) {
	// Percent of Ftpc West Hits in common
	numPoints = rcTrk->detectorInfo()->numberOfPoints(kFtpcWestId);
	mRatioCommonToTotalHitsFtpc =
	    (numPoints) ? static_cast<float>(mCommonFtpcHits)/static_cast<float>(numPoints) : 0;
	
	// If no Ftpc West Hits, try Ftpc East 
	if (!numPoints) {
	    numPoints = rcTrk->detectorInfo()->numberOfPoints(kFtpcEastId);
	    mRatioCommonToTotalHitsFtpc =
		(numPoints) ? static_cast<float>(mCommonFtpcHits)/static_cast<float>(numPoints) : 0;
	}
    }
}

StTrackPairInfo::~StTrackPairInfo() { /* noop */ }

void StTrackPairInfo::setPartnerMcTrack(StMcTrack* val) { mPartnerMcTrack = val; }

void StTrackPairInfo::setPartnerTrack(StGlobalTrack* val) { mPartnerTrack = val; }

void StTrackPairInfo::setCommonTpcHits(unsigned int val) { mCommonTpcHits = val; }

void StTrackPairInfo::setCommonSvtHits(unsigned int val) { mCommonSvtHits = val; }

void StTrackPairInfo::setCommonFtpcHits(unsigned int val) { mCommonFtpcHits = val; }

