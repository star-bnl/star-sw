/***************************************************************************
 *
 * StTrackPairInfo.cc
 *
 **************************************************************************/
#include "StTrackPairInfo.hh"
#include "StMcEvent/StMcTrack.hh"

static const char rcsid[] = "$Id: StTrackPairInfo.cc,v 1.1.1.1 1999/06/30 21:54:36 calderon Exp $";

StTrackPairInfo::StTrackPairInfo(StMcTrack* t, unsigned int pings) 
{
    mPartnerMcTrack = t;
    mCommonHits = pings;
}

StTrackPairInfo::~StTrackPairInfo() {
    /* noop */
}

void StTrackPairInfo::setPartnerMcTrack(StMcTrack* val) { mPartnerMcTrack = val; }

void StTrackPairInfo::setCommonHits(unsigned int val) { mCommonHits = val; }
