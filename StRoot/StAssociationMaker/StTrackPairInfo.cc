/***************************************************************************
 *
 * $Id: StTrackPairInfo.cc,v 1.3 1999/09/23 21:25:24 calderon Exp $
 * $Log: StTrackPairInfo.cc,v $
 * Revision 1.3  1999/09/23 21:25:24  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/
#include "StTrackPairInfo.hh"
#include "StMcTrack.hh"

static const char rcsid[] = "$Id: StTrackPairInfo.cc,v 1.3 1999/09/23 21:25:24 calderon Exp $";

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
